module Test.Tasty.Runners.HPC.Render where


------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as HSE
import           Prelude                         hiding (head, id, div)
import qualified Text.Blaze.Html                 as Html
import qualified Trace.Hpc.Mix                   as Hpc
import qualified Trace.Hpc.Tix                   as Hpc
import qualified Trace.Hpc.Util                  as Hpc
------------------------------------------------------------------------------
import           Test.Tasty.Runners.HPC.Internal 


srcDeclToHtml :: HSE.Decl HSE.SrcLoc -> ModuleTests -> Html.Html
srcDeclToHtml src (ModuleTests testMap) =
  let tests = [] in  -- TODO find the tests for this scr node
  
  
