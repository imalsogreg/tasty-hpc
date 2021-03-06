-- | Not sure whether to render code to html by (ab)using hindent here,
--   or though Test/Tasty/HPC/Render.hs

{-# LANGUAGE DeriveDataTypeable #-}

module Test.Tasty.Runners.HPC.Internal where

import Control.Applicative
import qualified Data.Map as Map
import Data.Monoid
import Data.Tagged (Tagged(..))
import Data.Typeable

import qualified Trace.Hpc.Mix                   as Hpc
import qualified Trace.Hpc.Tix                   as Hpc
import qualified Trace.Hpc.Util                  as Hpc
import qualified Test.Tasty                      as Tasty
import qualified Test.Tasty.Providers            as Tasty
import qualified Test.Tasty.Options              as Tasty
import qualified Language.Haskell.Exts.Annotated as HSE

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


newtype CodeTests =
  CodeTests (Map.Map (FilePath, Hpc.MixEntry) [(Tasty.TestName,Tasty.Result)])

------------------------------------------------------------------------------
instance Monoid CodeTests where
  mempty                            = CodeTests $ Map.empty
  CodeTests a `mappend` CodeTests b = CodeTests $ Map.unionWith (<>) a b


------------------------------------------------------------------------------
instance Show CodeTests where
  show (CodeTests m) = unlines . map entry . Map.toList $ m
    where entry (x,y) = show x ++  ": Tested by " ++ show (map fst y)


------------------------------------------------------------------------------
showHpcPos :: Hpc.HpcPos -> String
showHpcPos p = let (r0,c0,r1,c1) = Hpc.fromHpcPos p
                   in show r0 ++ ":" ++ show c0
                      ++ "-" ++
                      show r1 ++ ":" ++ show c1

------------------------------------------------------------------------------
hpcToHSE :: (FilePath, Hpc.HpcPos) -> HSE.SrcSpanInfo
hpcToHSE (f, hpcPos) = let (r0,c0,r1,c1) = Hpc.fromHpcPos hpcPos
                       in HSE.SrcSpanInfo (HSE.SrcSpan f r0 c0 r1 c1) []

hpcFromHSE :: HSE.SrcSpanInfo -> (FilePath, Hpc.HpcPos)
hpcFromHSE (HSE.SrcSpanInfo (HSE.SrcSpan f r0 c0 r1 c1) _) =
  (f, Hpc.toHpcPos (r0,c0,r1,c1));
