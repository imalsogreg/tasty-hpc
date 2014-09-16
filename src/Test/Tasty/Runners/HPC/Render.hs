{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Runners.HPC.Render where

------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as HSE
import           Prelude                         hiding (head, id, div)
import qualified Text.Blaze.Html                 as Html
import qualified Trace.Hpc.Mix                   as Hpc
import qualified Trace.Hpc.Tix                   as Hpc
import qualified Trace.Hpc.Util                  as Hpc
import qualified Text.Blaze                      as B
import           Text.Blaze
import qualified Text.Blaze.Html5                as B
import           Text.Blaze.Html5.Attributes
------------------------------------------------------------------------------
import           Test.Tasty.Runners.HPC.Internal 

renderModule :: (HSE.Module HSE.SrcSpanInfo) -> Html.Html
renderModule = undefined

srcDeclToHtml :: HSE.Decl HSE.SrcLoc -> ModuleTests -> Html.Html
srcDeclToHtml decl (ModuleTests testMap) =
  let tests = [] in  -- TODO find the tests for this scr node
  case decl of
    (HSE.TypeDecl _ _ _) -> B.div ! href "#TypeDecl" $ B.toHtml (HSE.prettyPrint decl)
  
