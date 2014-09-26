{-# LANGUAGE OverloadedStrings #-}

module HIndent.Styles.Html where

import Control.Monad
import Control.Monad.State.Class
import Data.Int
import Data.Default.Class
import Data.Maybe
import HIndent.Pretty
import HIndent.Types
import Language.Haskell.Exts.Annotated.Syntax
import Prelude hiding (exp)

import Test.Tasty.Runners.HPC.Internal

-- | Empty state as is Chris Done's style.
data State = State 

testMarkup :: ModuleTests -> Style
testMarkup ts = Style {styleName         = "tasty-hpc-markup"
                      ,styleAuthor       = "Greg Hale"
                      ,styleDescription  = "Tag tested expressions"
                      ,styleInitialState = State
                      ,styleExtenders = [Extender exp]
                      ,styleDefConfig = def
                      }

exp :: t -> Exp NodeInfo -> Printer ()
exp = _ (Exp 
