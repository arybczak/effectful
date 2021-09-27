module Effectful.Environment
  ( -- * Environment effect
    Environment
  , runEnvironment

    -- * Querying the environment
  , getArgs
  , getProgName
  , getExecutablePath
  , getEnv
  , getEnvironment
  , lookupEnv

    -- * Modifying the environment
  , setEnv
  , unsetEnv
  , withArgs
  , withProgName
  ) where

import qualified System.Environment as E

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | An effect for querying and modifying the system environment.
data Environment :: Effect where
  Environment :: Environment m r

runEnvironment :: IOE :> es => Eff (Environment : es) a -> Eff es a
runEnvironment = evalEffect (IdE Environment)

-- | Lifted 'E.getArgs'.
getArgs :: Environment :> es => Eff es [String]
getArgs = unsafeEff_ E.getArgs

-- | Lifted 'E.getEnv'.
getEnv :: Environment :> es => String -> Eff es String
getEnv = unsafeEff_ . E.getEnv

-- | Lifted 'E.getEnvironment'.
getEnvironment :: Environment :> es => Eff es [(String, String)]
getEnvironment = unsafeEff_ E.getEnvironment

-- | Lifted 'E.getExecutablePath'.
getExecutablePath :: Environment :> es => Eff es FilePath
getExecutablePath = unsafeEff_ E.getExecutablePath

-- | Lifted 'E.getProgName'.
getProgName :: Environment :> es => Eff es String
getProgName = unsafeEff_ E.getProgName

-- | Lifted 'E.lookupEnv'.
lookupEnv :: Environment :> es => String -> Eff es (Maybe String)
lookupEnv = unsafeEff_ . E.lookupEnv

-- | Lifted 'E.setEnv'.
setEnv :: Environment :> es => String -> String -> Eff es ()
setEnv n = unsafeEff_ . E.setEnv n

-- | Lifted 'E.unsetEnv'.
unsetEnv :: Environment :> es => String -> Eff es ()
unsetEnv = unsafeEff_ . E.unsetEnv

-- | Lifted 'E.withArgs'.
withArgs :: Environment :> es => [String] -> Eff es a -> Eff es a
withArgs env k = unsafeEff $ \es -> do
  seqUnliftEff es $ \runInIO -> do
    E.withArgs env (runInIO k)

-- | Lifted 'E.withProgName'.
withProgName :: Environment :> es => String -> Eff es a -> Eff es a
withProgName n k = unsafeEff $ \es -> do
  seqUnliftEff es $ \runInIO -> do
    E.withProgName n (runInIO k)
