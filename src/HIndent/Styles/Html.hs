{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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

testMarkup :: CodeTests -> Style
testMarkup ts = Style {styleName         = "tasty-hpc-markup"
                      ,styleAuthor       = "Greg Hale"
                      ,styleDescription  = "Tag tested expressions"
                      ,styleInitialState = State
                      ,styleExtenders = [Extender exp
                                        ,Extender decl
                                        ,Extender rhs
--                                        ,Extender unguardedalt
                                        ,Extender stmt]
                      ,styleDefConfig = def
                      }

decl :: State -> Decl NodeInfo -> Printer ()
decl _ d = do
  write "<span class=\"decl\" tests=\"test1 test2\">"
  pretty d
  write "</span>"


exp :: State -> Exp NodeInfo -> Printer ()
exp _ e = do
  write "<span class=\"exp\" tests=\"test3 test4\">"
  pretty e
  write "</span>"


rhs :: State -> Exp NodeInfo -> Printer ()
rhs _ r = do
  write "<span class=\"rhs\">"
  pretty r
  write "</span>"

--unguardedalt :: t -> GuardedAlts NodeInfo -> Printer ()
--unguardedalt _ u = error "alt"

stmt :: State -> Stmt NodeInfo -> Printer ()
stmt = error "stmt"
