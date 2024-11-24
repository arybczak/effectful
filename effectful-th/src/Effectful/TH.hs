{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
-- | Generate functions for performing operations of dynamically dispatched
-- effects via Template Haskell.
module Effectful.TH
  ( makeEffect
  , makeEffect_
  , makeUniqueEffect
  , makeUniqueEffect_
  ) where

import Control.Monad
import Data.Char (toLower)
import Data.Foldable qualified as F
import Data.Map.Strict qualified as Map
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Datatype.TyVarBndr

import Effectful
import Effectful.Dispatch.Dynamic

-- | For an effect data type @E@, @'makeEffect' E@ generates the appropriate
-- instance of 'DispatchOf' as well as functions for performing operations of
-- @E@ by 'send'ing them to the effect handler.
--
-- >>> :{
--   data E :: Effect where
--     Op1 :: Int -> m a -> E m a
--     Op2 :: IOE :> es => Int -> E (Eff es) ()
--     Op3 :: (forall r. m r -> m r) -> E m Int
--   makeEffect ''E
-- :}
--
-- >>> :kind! DispatchOf E
-- DispatchOf E :: Dispatch
-- ...Dynamic
--
-- >>> :i op1
-- op1 :: (HasCallStack, E :> es) => Int -> Eff es a -> Eff es a
-- ...
--
-- >>> :i op2
-- op2 :: (HasCallStack, E :> es, IOE :> es) => Int -> Eff es ()
-- ...
--
-- >>> :i op3
-- op3 ::
--   (HasCallStack, E :> es) =>
--   (forall r. Eff es r -> Eff es r) -> Eff es Int
-- ...
--
-- The naming rule changes the first uppercase letter in the constructor name to
-- lowercase or removes the @:@ symbol in case of operators. Any fixity
-- annotations defined for the constructors are preserved for the corresponding
-- definitions.
--
-- If the constructor declaration has Haddock, then this is reused for the
-- sending functions, otherwise a simple placeholder is used.
makeEffect :: Name -> Q [Dec]
makeEffect = makeEffectImpl ''(:>) True

-- | Like 'makeEffect', but doesn't generate type signatures. This is useful
-- when you want to attach Haddock documentation to function signatures:
--
-- >>> :{
--   data Noop :: Effect where
--     Noop :: Noop m ()
--   makeEffect_ ''Noop
--   -- | Perform nothing at all.
--   noop :: Noop :> es => Eff es ()
-- :}
--
-- /Note:/ function signatures must be added /after/ the call to 'makeEffect_'.
makeEffect_ :: Name -> Q [Dec]
makeEffect_ = makeEffectImpl ''(:>) False

makeUniqueEffect :: Name -> Q [Dec]
makeUniqueEffect = makeEffectImpl ''(<:>) True

makeUniqueEffect_ :: Name -> Q [Dec]
makeUniqueEffect_ = makeEffectImpl ''(<:>) False

makeEffectImpl :: Name -> Bool -> Name -> Q [Dec]
makeEffectImpl memberOp makeSig effName = do
  checkRequiredExtensions
  info <- reifyDatatype effName
  dispatch <- do
    e <- getEff (ConT $ datatypeName info) (const WildCardT <$> datatypeInstTypes info)
    let dispatchE = ConT ''DispatchOf `AppT` e
        dynamic   = PromotedT 'Dynamic
    pure . TySynInstD $ TySynEqn Nothing dispatchE dynamic
  ops <- traverse (makeCon memberOp makeSig) (constructorName <$> datatypeCons info)
  pure $ dispatch : concat (reverse ops)
  where
    getEff :: Type -> [Type] -> Q Type
    getEff e = \case
      [m, r]   -> do
        checkKind "the next to last" (ArrowT `AppT` StarT `AppT` StarT) m
        checkKind "the last" StarT r
        pure e
      (v : vs) -> getEff (e `AppT` forgetKind v) vs
      _        -> fail "The effect data type needs at least 2 type parameters"
      where
        forgetKind = \case
          SigT v _ -> v
          ty       -> ty

    checkKind which expected = \case
      SigT (VarT _) k
        | k == expected -> pure ()
        | otherwise -> fail
           $ "Expected " ++ which ++ " type parameter to have a kind "
          ++ pprint expected ++ ", got " ++ pprint k
      -- Weird type, let it through and see what happens.
      _ -> pure ()

-- | Generate a single definition of an effect operation.
makeCon :: Name -> Bool -> Name -> Q [Dec]
makeCon memberOp makeSig name = do
  fixity <- reifyFixity name
  typ <- reify name >>= \case
    DataConI _ typ _ -> pure typ
    _ -> fail $ "Not a data constructor: " ++ nameBase name

  (actionParams, (effTy, ename, resTy)) <- extractParams typ

  -- The 'ename' can be either:
  --
  -- - A variable for the monad, in which case we need to generate the @es@
  --   variable and substitute it later for 'Eff es'.
  --
  -- - A variable 'es' for the local 'Eff es' if the monad parameter was locally
  --   substituted in the contructor.
  --
  -- For example in the following effect:
  --
  -- data E :: Effect where
  --   E1 :: Int -> E m ()
  --   E2 :: IOE :> es => E (Eff es) ()
  --
  -- Processing 'E1' will yield 'Right m', but 'E2' will yield 'Left es'.
  --
  -- In the first case we need to substitute the variable ourselves in a few
  -- places, but in the second we're good since it was already substituted.
  (esName, maybeMonadName) <- case ename of
    Left  esName    -> pure (esName, Nothing)
    Right monadName -> (, Just monadName) <$> newName "es"

  let fnName = mkName . toSmartConName $ nameBase name
  fnArgs <- traverse (const $ newName "x") actionParams

  let esVar = VarT esName

      substM :: Type -> Type
      substM = case maybeMonadName of
        Just m  -> applySubstitution . Map.singleton m $ ConT ''Eff `AppT` esVar
        Nothing -> id

      (origActionVars, actionCtx) = extractCtx typ
      actionVars = case maybeMonadName of
        Just m  -> filter ((m /=) . tvName) origActionVars
                ++ [kindedTVSpecified esName $ ListT `AppT` ConT ''Effect]
        Nothing -> origActionVars

#if MIN_VERSION_template_haskell(2,17,0)
  -- In GHC >= 9.0 it's possible to generate the following body:
  --
  -- e x1 .. xN = send (E @ty1 .. @tyN x1 .. xN)
  --
  -- because specificities of constructor variables are exposed.
  --
  -- This allows to generate functions for such effects:
  --
  -- type family F ty :: Type
  -- data AmbEff :: Effect where
  --   AmbEff :: Int -> AmbEff m (F ty)
  --
  -- Sadly the version for GHC < 9 will not compile due to ambiguity error.
  let fnBody =
        let tyApps = (`mapMaybe` origActionVars) $ \v -> case tvFlag v of
              InferredSpec  -> Nothing
              SpecifiedSpec -> Just $ if maybeMonadName == Just (tvName v)
                                      then ConT ''Eff `AppT` esVar
                                      else VarT (tvName v)

            effCon = if makeSig
              then F.foldl' AppTypeE (ConE name) tyApps
              else                  ConE name
        in VarE 'send `AppE` F.foldl' (\f -> AppE f . VarE) effCon fnArgs
#else
  -- In GHC < 9.0, generate the following body:
  --
  -- e :: E v1 .. vN :> es => x1 -> .. -> xK -> E v1 .. vN (Eff es) r
  -- e x1 .. xK = send (E x1 .. xN :: E v1 .. vK (Eff es) r)
  let fnBody =
        let effOp  = F.foldl' (\f -> AppE f . VarE) (ConE name) fnArgs
            effSig = effTy `AppT` (ConT ''Eff `AppT` esVar) `AppT` substM resTy
        in if makeSig
           then VarE 'send `AppE` SigE effOp effSig
           else VarE 'send `AppE`      effOp
#endif
  let fnSig = ForallT actionVars
        (ConT ''HasCallStack : UInfixT effTy memberOp esVar : actionCtx)
        (makeTyp esVar substM resTy actionParams)

  let mkDec fix =
#if MIN_VERSION_template_haskell(2,22,0)
        InfixD fix DataNamespaceSpecifier name
#else
        InfixD fix name
#endif
      rest = FunD fnName [Clause (VarP <$> fnArgs) (NormalB fnBody) []]
           : maybeToList (mkDec <$> fixity)

  (++ rest) <$> withHaddock name [SigD fnName fnSig | makeSig]

----------------------------------------
-- Helpers

toSmartConName :: String -> String
toSmartConName = \case
  (':' : xs) -> xs
  (x : xs)   -> toLower x : xs
  _          -> error "empty constructor name"

extractCtx :: Type -> ([TyVarBndrSpec], Cxt)
extractCtx = \case
  ForallT vars ctx _ -> (vars, ctx)
  ty                 -> error $ "unexpected type: " ++ show ty

extractParams :: Type -> Q ([Type], (Type, Either Name Name, Type))
extractParams = \case
  ForallT _ _ ty -> extractParams ty
  SigT ty _ -> extractParams ty
  ParensT ty -> extractParams ty
  ArrowT `AppT` a `AppT` ty -> do
    (args, ret) <- extractParams ty
    pure (a : args, ret)
#if MIN_VERSION_template_haskell(2,17,0)
  MulArrowT `AppT` _ `AppT` a `AppT` ty -> do
    (args, ret) <- extractParams ty
    pure (a : args, ret)
#endif
  effTy `AppT` monadTy `AppT` resTy -> case monadTy of
    VarT monadName -> pure ([], (effTy, Right monadName, resTy))
    ConT eff `AppT` VarT esName
      | eff == ''Eff -> pure ([], (effTy, Left esName, resTy))
    ty -> fail $ "Invalid instantiation of the monad parameter: " ++ pprint ty
  ty -> fail $ "Unexpected type: " ++ pprint ty

makeTyp :: Type -> (Type -> Type) -> Type -> [Type] -> Type
makeTyp esVar substM resTy = \case
  []       -> ConT ''Eff `AppT` esVar `AppT` substM resTy
  (p : ps) -> ArrowT `AppT` substM p `AppT` makeTyp esVar substM resTy ps

withHaddock :: Name -> [Dec] -> Q [Dec]
#if MIN_VERSION_template_haskell(2,18,0)
withHaddock name decs = do 
  existingHaddock <- getDoc (DeclDoc name)
  let newDoc = 
        case existingHaddock of
          Just doc -> doc
          Nothing -> "Perform the operation '" ++ nameBase name ++ "'."
  withDecsDoc newDoc (pure decs)
#else
withHaddock _ decs = pure decs
#endif

checkRequiredExtensions :: Q ()
checkRequiredExtensions = do
  missing <- filterM (fmap not . isExtEnabled) exts
  let ppMissing = map (\ext -> "{-# LANGUAGE " <> show ext <> " #-}") missing
  unless (null missing) . fail . unlines $
    [ "Generating functions requires additional language extensions.\n"
    , "You can enable them by adding them to the 'default-extensions'"
    , "field in the .cabal file or the following pragmas to the beginning"
    , "of the source file:\n"
    ] ++ ppMissing
  where
    exts = [ FlexibleContexts
           , ScopedTypeVariables
#if MIN_VERSION_template_haskell(2,17,0)
           , TypeApplications
#endif
           , TypeFamilies
           , TypeOperators
           ]
