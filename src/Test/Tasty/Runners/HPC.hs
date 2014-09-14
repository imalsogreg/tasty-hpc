{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Runners.HPC where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import qualified Data.Map                as Map
import           Data.Monoid
import           Data.Typeable
import qualified System.Process          as P
------------------------------------------------------------------------------
import qualified Test.Tasty.Options      as Tasty
import qualified Test.Tasty.Providers    as Tasty
import qualified Test.Tasty.Runners      as Tasty
import qualified Trace.Hpc.Tix           as Hpc
import qualified Trace.Hpc.Mix           as Hpc
------------------------------------------------------------------------------
import Test.Tasty.Runners.HPC.Internal


------------------------------------------------------------------------------
hpcRunner :: Tasty.Ingredient
hpcRunner = Tasty.TestReporter optionDescriptions runner
  where
   optionDescriptions = [ Tasty.Option (Proxy :: Proxy RunHPC)
                        , Tasty.Option (Proxy :: Proxy MixPath)
                        , Tasty.Option (Proxy :: Proxy TixPath)
                        ]
                        
   ----------------------------------------------------------------------------
   runner :: Tasty.OptionSet -> Tasty.TestTree
          -> Maybe (Tasty.StatusMap -> IO Bool)
   runner options testTree = case Tasty.lookupOption options of
     RunHPC False -> Nothing
     RunHPC True  -> Just $ \_ -> do
       talkingStick <- newMVar ()
       let hpcFold = Tasty.trivialFold {
             Tasty.foldSingle   = runSingle talkingStick
             }
       cm@(CodeTests m) <- Tasty.getApp $
                           Tasty.foldTestTree hpcFold options testTree
       putStrLn $ show cm           -- TODO replace with real output format
       return (not . Map.null $ m)  -- TODO check all tests passed?

   ----------------------------------------------------------------------------
   runSingle :: forall t. Tasty.IsTest t => MVar ()
             -> Tasty.OptionSet -> Tasty.TestName -> t
             -> Tasty.Ap IO CodeTests
   runSingle mv opts name test = Tasty.Ap . withMVar mv $ \_ -> do
     let cmd = "dist/build/testsuite/testsuite -p '" ++ name ++ "'"
     res  <- Tasty.run opts test (print . Tasty.progressText)
     tix' <- touchTixWith "testsuite.tix" cmd
     tests <- case tix' of
       Nothing                      -> return mempty
       Just (Hpc.Tix moduleEntries) -> codeMapOfTest moduleEntries name res
     return tests


------------------------------------------------------------------------------
codeMapOfTest :: [Hpc.TixModule] -> String -> Tasty.Result -> IO CodeTests
codeMapOfTest tixMods testName testResult = do
  moduleMappings <- forM tixMods $ \tixMod -> do
    putStrLn $ "Reading mix for " ++ Hpc.tixModuleName tixMod
    rMix  <- try $ Hpc.readMix ["dist/hpc/fib-0.1"] (Right tixMod)
    case rMix of
      Left (e :: SomeException) -> print e >> return mempty
      Right (Hpc.Mix _ _ _ _ exprs) -> do
        let touched :: [Hpc.MixEntry]
            touched = map snd . filter ((>0) . fst) $
                      zip (Hpc.tixModuleTixs tixMod :: [Integer]) exprs
        return . (Hpc.tixModuleName tixMod, ) . ModuleTests $
          Map.fromList [(e, [(testName,testResult)])
                       | e <- touched]
  return . CodeTests $  Map.fromList  moduleMappings


------------------------------------------------------------------------------
touchTixWith :: FilePath -> String -> IO (Maybe Hpc.Tix)
touchTixWith tixFilePath cmd = do
  Hpc.writeTix tixFilePath emptyTix
  h <- P.runCommand cmd
  _ <- P.waitForProcess h
  Hpc.readTix tixFilePath


------------------------------------------------------------------------------
emptyTix :: Hpc.Tix
emptyTix = Hpc.Tix []
