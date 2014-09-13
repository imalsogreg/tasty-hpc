{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Runners.HPC.Internal where

import Control.Applicative
import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy  (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Typeable

import qualified Trace.Hpc.Mix        as Mix
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.Providers as Tasty
import qualified Test.Tasty.Options   as Tasty


------------------------------------------------------------------------------
newtype MixPath = MixPath FilePath
                deriving (Typeable)

instance Tasty.IsOption MixPath where
  defaultValue = MixPath ".hpc"
  parseValue   = Just . MixPath
  optionName   = Tagged "mixpath"
  optionHelp   = Tagged "Base path for hpc's mix files"


------------------------------------------------------------------------------
newtype TixPath = TixPath FilePath
                deriving (Typeable)

instance Tasty.IsOption TixPath where
  defaultValue = TixPath "testtuite.tix"
  parseValue   = Just . TixPath
  optionName   = Tagged "tixpath"
  optionHelp   = Tagged "Location of hpc's tix file. $MYTESTSUITE.tix"


------------------------------------------------------------------------------
newtype RunHPC = RunHPC Bool
               deriving (Typeable)

instance Tasty.IsOption RunHPC where
  defaultValue = RunHPC False
  parseValue s = RunHPC <$> Tasty.safeRead s
  optionName   = Tagged "hpc"
  optionHelp   = Tagged "Map libray code to test code"


newtype CodeTests = CodeTests (Map.Map FilePath
                    (Map.Map Mix.MixEntry [(Tasty.TestName,Tasty.Result)]))
                  deriving (Eq)

------------------------------------------------------------------------------
instance Monoid CodeTests where
  mempty                            = CodeTests $ Map.empty
  CodeTests a `mappend` CodeTests b =
    CodeTests $ Map.unionWith (Map.unionWith (++)) a b
