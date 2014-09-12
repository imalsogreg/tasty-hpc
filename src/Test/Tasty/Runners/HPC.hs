{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Runners.HPC where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar
import qualified Data.Map                as Map
import           Data.Monoid
import           Data.Proxy (Proxy(..))
import           Data.Typeable
------------------------------------------------------------------------------
import qualified Test.Tasty.Options      as Tasty
import qualified Test.Tasty.Providers    as Tasty
import qualified Test.Tasty.Runners      as Tasty
import qualified Trace.Hpc.Tix           as Hpc
import qualified Trace.Hpc.Mix           as Hpc

import Test.Tasty.Runners.HPC.Internal

hpcRunner :: Tasty.Ingredient
hpcRunner = Tasty.TestManager optionDescriptions runner
  where
   optionDescriptions = [Tasty.Option (Proxy :: Proxy MixPath)
                        ,Tasty.Option (Proxy :: Proxy TixPath)
                        ]

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
                 Tasty.foldSingle   = runSingle
               , Tasty.foldResource = (resourceFold talkingStick)
               }
         let b = Tasty.foldTestTree hpcFold options testTree :: CodeTests
         return (not $ b == mempty)

   resourceFold :: forall a. MVar () -> Tasty.ResourceSpec a -> (IO a -> CodeTests) -> CodeTests
   resourceFold mv (Tasty.ResourceSpec acc rel) rFun = undefined

   hpcRSpec :: MVar () -> Tasty.ResourceSpec ()
   hpcRSpec m = Tasty.ResourceSpec (takeMVar m) (const $ putMVar m ())

   runSingle :: forall t. Tasty.IsTest t => Tasty.OptionSet -> Tasty.TestName -> t -> CodeTests
   runSingle = undefined
