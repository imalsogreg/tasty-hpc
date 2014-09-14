module Main where

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util

import Test.Tasty.Runners.HPC
import Test.Tasty.Runners.HPC.Internal

pos1 = toHpcPos (1,1,10,10)
pos2 = toHpcPos (2,2,8,8)
pos3 = toHpcPos (10,1,20,1)

modTestsA :: ModuleTests
modTestsA = ModuleTests $ Map.fromList [(pos1,["testA"])
                                       ,(pos2,["testA"])
                                       ]

modTestsB :: ModuleTests
modTestsB = ModuleTests $ Map.fromList [(pos1,["testB"])
                                       ,(pos3,["testB"])
                                       ]

resA = CodeTests $ Map.fromList [("ModuleA", modTestsA)]
resB = CodeTests $ Map.fromList [("ModuleA", modTestsB)]

main = print "Hello"
