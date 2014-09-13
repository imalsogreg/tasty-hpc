{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Test.Tasty.Runners.HPC where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Map                as Map
import           Data.Monoid
import           Data.Proxy (Proxy(..))
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
hpcRunner = Tasty.TestManager optionDescriptions runner
  where
   optionDescriptions = [Tasty.Option (Proxy :: Proxy MixPath)
                        ,Tasty.Option (Proxy :: Proxy TixPath)
                        ]
                        
   ----------------------------------------------------------------------------
   runner :: Tasty.OptionSet -> Tasty.TestTree -> Maybe (IO Bool)
   runner options testTree = do
     let RunHPC  runHpc  = Tasty.lookupOption options
         MixPath mixPath = Tasty.lookupOption options
         TixPath tixPath = Tasty.lookupOption options
     case runHpc of
       False -> Just (return False)
       True  -> Just $ do
         talkingStick <- newMVar ()
         let hpcFold = Tasty.trivialFold {
                 Tasty.foldSingle   = runSingle talkingStick
               , Tasty.foldResource = (resourceFold talkingStick)
               }
         b <- Tasty.getApp $ Tasty.foldTestTree hpcFold options testTree
         return (not $ b == mempty)

   ----------------------------------------------------------------------------
   resourceFold :: forall a. MVar () -> Tasty.ResourceSpec a ->
                   (IO a -> Tasty.Ap IO CodeTests) -> Tasty.Ap IO CodeTests
   resourceFold mv (Tasty.ResourceSpec acc rel) rFun = undefined

   ----------------------------------------------------------------------------
   hpcRSpec :: MVar () -> Tasty.ResourceSpec ()
   hpcRSpec m = Tasty.ResourceSpec (takeMVar m) (const $ putMVar m ())

   ----------------------------------------------------------------------------
   runSingle :: forall t. Tasty.IsTest t => MVar () -> Tasty.OptionSet ->
                Tasty.TestName -> t -> Tasty.Ap IO CodeTests
   runSingle mv options name test = Tasty.Ap $ do
     let cmd = "dist/build/testsuite/testsuite -p '" ++ name ++ "'"
         act = withMVar mv $ \() -> P.runCommand cmd
     res  <- Tasty.run t --  TODO Fix this
     tix' <- withMVar mv $ \() -> touchTixWith cmd "testsuite.tix"
     case tix' of
       Nothing                  -> return mempty
       Just (Hpc.Tix moduleEntries) ->
         codeMapOfTests moduleEntries res


------------------------------------------------------------------------------
codeMapOfTests :: [Hpc.TixModule] -> Tasty.Result -> IO CodeTests
codeMapOfTests tixMods result = do
  moduleMappings <- forM tixMods $ \tixMod -> do
    (Hpc.Mix fp _ _ _ exprs) <- Hpc.readMix ["dist/hpc"] $ Right tixModule
    let exprCnts = zip (zip exprs (repeat result)) (Hpc.tixModuleTixs tixMod)
    return . (Hpc.tixModuleName tixMod,) .
      Map.fromList . map fst $ filter ((>0) . snd) exprCnts
  return $ undefined



------------------------------------------------------------------------------
touchTixWith :: FilePath -> String -> IO (Maybe Hpc.Tix)
touchTixWith fp cmd = do
  Hpc.writeTix fp emptyTix
  h <- P.runCommand cmd
  _ <- P.waitForProcess h
  Hpc.readTix fp


------------------------------------------------------------------------------
emptyTix :: Hpc.Tix
emptyTix = Hpc.Tix []
