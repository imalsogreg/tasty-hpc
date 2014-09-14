{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Runners.HPC.Internal where

import Control.Applicative
import qualified Data.Map as Map
import Data.Monoid
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


------------------------------------------------------------------------------
newtype CodeTests = CodeTests (Map.Map FilePath ModuleTests)


newtype ModuleTests =
  ModuleTests (Map.Map Mix.MixEntry [(Tasty.TestName,Tasty.Result)])


------------------------------------------------------------------------------
instance Monoid CodeTests where
  mempty                            = CodeTests $ Map.empty
  CodeTests a `mappend` CodeTests b = CodeTests $ a <> b

instance Monoid ModuleTests where
  mempty = ModuleTests mempty
  ModuleTests a `mappend` ModuleTests b = ModuleTests $ Map.unionWith (++) a b


------------------------------------------------------------------------------
instance Show ModuleTests where
  show (ModuleTests m) = unlines . map entry . Map.toList $ m
    where entry (x,y) = show (snd x) ++  ": Tested by " ++ show (map fst y)

instance Show CodeTests where
  show (CodeTests m) = unlines . map showModule . Map.toList $ m
    where
      showModule (modName, modTests) = unlines ["Module " ++ modName
                                               , show modTests
                                               , ""]
