-- | Run a 'Tasty.TestTree' and produce a summary of which tests cover which
--   parts of the code

module Test.Tasty.Runners.HPC where

------------------------------------------------------------------------------
import           Data.Proxy (Proxy(..))
import           Data.Typeable
------------------------------------------------------------------------------
import qualified Test.Tasty.Options   as Tasty
import qualified Test.Tasty.Providers as Tasty
import qualified Test.Tasty.Runners   as Tasty

import Test.Tasty.Runners.HPC.Internal

hpcRunner :: Tasty.Ingredient
hpcRunner = Tasty.TestReporter optionDescription runner
 where
   optionDescription = [Tasty.Option (Proxy :: Proxy MixPath)
                       ,Tasty.Option (Proxy :: Proxy TixPath)
                       ]

   runner options testTree = do
     MixPath mixPath <- Tasty.lookupOption options
     TixPath tixPath <- Tasty.lookupOption options
     undefined
         