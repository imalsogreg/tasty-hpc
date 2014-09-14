module Test where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Trace.Hpc.Mix
import Trace.Hpc.Tix

import Test.Tasty.Runners.HPC
import Test.Tasty.Runners.HPC.Internal



resultA :: Result
resultA = Result { resultOutcome = Success
                 , resultDescription = "Worked"
                 , resultTime = 1
                 }
