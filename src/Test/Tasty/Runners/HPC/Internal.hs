{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Runners.HPC.Internal where

import Control.Applicative
import Data.Proxy  (Proxy(..))
import Data.Tagged (Tagged(..))
import Data.Typeable


import qualified Test.Tasty.Options as Tasty

newtype MixPath = MixPath FilePath
                deriving (Typeable)

instance Tasty.IsOption MixPath where
  defaultValue = MixPath ".hpc"
  parseValue   = Just . MixPath
  optionName   = Tagged "mixpath"
  optionHelp   = Tagged "Base path for hpc's mix files"

instance Tasty.IsOption TixPath where
  defaultValue = TixPath "testtuite.tix"
  parseValue   = Just . TixPath
  optionName   = Tagged "tixpath"
  optionHelp   = Tagged "Location of hpc's tix file. $MYTESTSUITE.tix"

newtype TixPath = TixPath FilePath
                deriving (Typeable)