{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Runners.HPC.Render where

------------------------------------------------------------------------------
import           Control.Monad
import qualified Data.Map                        as Map
import           Data.String                     (fromString)
import           Prelude                         hiding (head, id, div)
------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as HSE
import qualified Text.Blaze.Html                 as Html
import qualified Trace.Hpc.Mix                   as Hpc
import qualified Trace.Hpc.Tix                   as Hpc
import qualified Trace.Hpc.Util                  as Hpc
import qualified Text.Blaze                      as B
import           Text.Blaze
import qualified Text.Blaze.Html5                as B
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Renderer.String      as B
import qualified Test.Tasty                      as Tasty
import qualified Test.Tasty.Providers            as Tasty
import qualified Test.Tasty.Options              as Tasty

------------------------------------------------------------------------------
import           Test.Tasty.Runners.HPC.Internal 

testsReport :: CodeTests -> IO ()
testsReport (CodeTests testMap) =
  forM_ (Map.toList testMap) $ \(fp,modTests) -> do
    h <- testsOverSrcFile modTests fp
    writeFile (fp ++ ".html") h
  

------------------------------------------------------------------------------
testsOverSrcFile :: ModuleTests -> FilePath -> IO String
testsOverSrcFile tests srcFile = do
  res <- HSE.parseFileWithComments HSE.defaultParseMode srcFile
  case res of
    HSE.ParseFailed _ _      -> error "Bad file parse"
    HSE.ParseOk (modSrc, comms) -> return . B.renderHtml $
                                   renderModule modSrc

------------------------------------------------------------------------------
renderModule :: (HSE.Module HSE.SrcSpanInfo) -> Html.Html
renderModule (HSE.Module loc modHead modPragmas modImports modDecls) =
  B.docTypeHtml $ do
    B.head $ B.title "Module"
    B.body $ forM_ modDecls (flip srcDeclToHtml emptyModules)


------------------------------------------------------------------------------
srcDeclToHtml :: HSE.Decl HSE.SrcSpanInfo -> ModuleTests -> Html.Html
srcDeclToHtml decl (ModuleTests testMap) =
  let modTestMatches = Map.filterWithKey (\(p,_) _ -> p == srcSpanToHpc (declSrcSpan decl)) testMap :: Map.Map Hpc.MixEntry [(Tasty.TestName, Tasty.Result)]
      matchTestNames = map fst . concat $ Map.elems modTestMatches :: [Tasty.TestName]
      
  in case decl of
    (HSE.TypeSig _ _ _) -> B.a ! href (fromString . concat $ matchTestNames) $
                           B.toHtml (HSE.prettyPrint decl)
    _                   -> B.div . B.toHtml $ HSE.prettyPrint decl

-- for testing
emptyModules = ModuleTests Map.empty

srcSpanToHpc :: HSE.SrcSpanInfo -> Hpc.HpcPos
srcSpanToHpc (HSE.SrcSpanInfo sp _) =
  Hpc.toHpcPos (HSE.srcSpanStartLine sp, HSE.srcSpanStartColumn sp,
                HSE.srcSpanEndLine sp,   HSE.srcSpanEndColumn sp)

------------------------------------------------------------------------------
declSrcSpan :: HSE.Decl HSE.SrcSpanInfo -> HSE.SrcSpanInfo
declSrcSpan decl = case decl of
  HSE.ClosedTypeFamDecl l _ _ _ -> l
  HSE.MinimalPragma l _         -> l
  HSE.TypeDecl l _ _            -> l
  HSE.TypeFamDecl l _ _         -> l
  HSE.DataDecl l _ _ _ _ _      -> l
  HSE.GDataDecl l _ _ _ _ _ _   -> l
  HSE.DataFamDecl l _ _ _       -> l
  HSE.TypeInsDecl l _ _         -> l
  HSE.DataInsDecl l _ _ _ _     -> l
  HSE.GDataInsDecl l _ _ _ _ _  -> l
  HSE.ClassDecl l _ _ _ _       -> l
  HSE.InstDecl l _ _ _          -> l
  HSE.DerivDecl l _ _           -> l
  HSE.InfixDecl l _ _ _         -> l
  HSE.DefaultDecl l _           -> l
  HSE.SpliceDecl l _            -> l
  HSE.TypeSig l _ _             -> l
  HSE.FunBind l _               -> l
  HSE.PatBind l _ _ _           -> l
  HSE.ForImp l _ _ _ _ _        -> l
  HSE.ForExp l _ _ _ _          -> l
  HSE.RulePragmaDecl l _        -> l
  HSE.DeprPragmaDecl l _        -> l
  HSE.WarnPragmaDecl l _        -> l
  HSE.InlineSig l _ _ _         -> l
  HSE.InlineConlikeSig l _ _    -> l
  HSE.SpecSig l _ _ _           -> l
  HSE.SpecInlineSig l _ _ _ _   -> l
  HSE.InstSig l _               -> l
  HSE.AnnPragma l _             -> l
