{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Runners.HPC.Render where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Data.List                       (groupBy,nub)
import           Data.Ord                        (comparing)
import qualified Data.Map                        as Map
import qualified Data.Text                       as T
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

class (HSE.Annotated ast, HSE.ExactP ast) => PageNode ast

showRes :: Tasty.Result -> T.Text
showRes r = "Result"


exprTag :: Hpc.BoxLabel -> [(Tasty.TestName, Tasty.Result)] -> T.Text
exprTag boxLabel ts = let (tNames,tResults) = unzip ts in
  T.concat ["<span "
           ,"class=\"", boxLabelClass boxLabel, "\" "
           ,"test-names=\"",T.unwords . map T.pack $ tNames
           ,"test-results="
           , T.unwords . map showRes $ tResults
           ,"\">"]


type PreLine  = T.Text
type PreBlock = [PreLine]

type TargetFun = Int -> Int
 
------------------------------------------------------------------------------
addTagToLine :: Int
             -> (TargetFun, PreLine)
             -> ((FilePath,Hpc.MixEntry), [(Tasty.TestName,Tasty.Result)])
             -> (TargetFun, PreLine)
addTagToLine lineNum (insFn, line) ((_,(srcPos, boxLabel)), testsResults) =
  let startTag = exprTag boxLabel testsResults
      endTag  = "</span>"
      (sLine,sPos,eLine,ePos) = Hpc.fromHpcPos srcPos
      fI = fromIntegral
      startPos = insFn . fI $ sPos
      endPos   = insFn . fI $ ePos
      (pA,pB,pC) = let (ab, c) = T.splitAt endPos   line
                       (a,  b) = T.splitAt startPos ab
                   in  (a,  b,  c)
      insFn' x
        | x < startPos   = x
        | x < endPos     = x + T.length startTag
        | otherwise      = x + T.length startTag + T.length endTag
      line' = T.concat [pA, startTag, pB, endTag, pC]
  in if lineNum >= sLine && lineNum <= eLine
     then (insFn' . insFn, line')
     else (insFn, line)

-- Yuck!
addTagsToLines :: CodeTests -> PreBlock -> PreBlock
addTagsToLines (CodeTests codeTests) codeLines =
  map snd $ zipWith
  (\i l -> foldl (addTagToLine i) (\x -> x,l) (Map.toList codeTests))
  [0 .. length codeLines] codeLines

------------------------------------------------------------------------------
boxLabelClass :: Hpc.BoxLabel -> T.Text
boxLabelClass (Hpc.ExpBox b)       = T.unwords ["exp-box", T.pack $ show b]
boxLabelClass (Hpc.LocalBox xs)    = T.unwords $ "local-box" : map T.pack xs
boxLabelClass (Hpc.TopLevelBox xs) =
  T.unwords $ "top-level-box" : map T.pack xs
boxLabelClass (Hpc.BinBox t b)     =
  T.unwords ["bin-box", T.pack $ show t, T.pack $ show b]


testsReports :: CodeTests -> IO ()
testsReports (CodeTests testMap) = do
  let moduleNames  = nub . map fst . Map.keys $ testMap :: [FilePath]
  forM_ moduleNames $ \fp -> do
    let thisModuleTests =
          CodeTests $ Map.filterWithKey (\k _ -> fst k == fp) $ testMap
    print $ "Trying to open file: " ++ fp
    let fp' = if fp == "fib-0.1/Fib"  -- TODO fix, make general of course
              then "/home/greghale/Programming/throwaway/fib/src/Fib.hs"
              else ""
    r <- try $ moduleReport fp' thisModuleTests
    case r of
      Left (_ :: SomeException)  -> return ()
      Right h -> writeFile (fp' ++ ".html") (T.unpack h)

moduleReport :: FilePath -> CodeTests -> IO T.Text
moduleReport fp tests =
  return . T.unlines . addTagsToLines tests . T.lines . T.pack =<< readFile fp


{-
------------------------------------------------------------------------------
testsOverSrcFile :: CodeTests -> FilePath -> IO String
testsOverSrcFile tests srcFile = do
  res <- HSE.parseFileWithComments HSE.defaultParseMode srcFile
  case res of
    HSE.ParseFailed _ _      -> error "Bad file parse"
    HSE.ParseOk (modSrc, comms) -> return . B.renderHtml $
                                   renderModule modSrc tests

------------------------------------------------------------------------------
renderModule :: (HSE.Module HSE.SrcSpanInfo) -> CodeTests -> Html.Html
renderModule (HSE.Module loc modHead modPragmas modImports modDecls) mTests =
  B.docTypeHtml $ do
    B.head $ B.title "Module"
    B.body $ forM_ modDecls (flip srcDeclToHtml mTests)



------------------------------------------------------------------------------
srcDeclToHtml :: HSE.Decl HSE.SrcSpanInfo -> CodeTests -> Html.Html
srcDeclToHtml decl (CodeTests testMap) =
  let modTestMatches = flip Map.filterWithKey testMap $ \(pos,_) _ ->
        pos == srcSpanToHpc (declSrcSpan decl)
      matchTestNames = map fst . concat $
                       Map.elems modTestMatches :: [Tasty.TestName]
      
  in case decl of
    (HSE.TypeSig _ _ _) ->
      B.div . B.toHtml $ HSE.prettyPrint decl
--    (HSE.FunBind l matches) ->
----      map (srcMatchToHtml  -- <- Note: I don't want to describe how to render
                           --      the whole tree! Too many constructors.
--    _ -> undefined
--      B.div . B.toHtml $
--      HSE.prettyPrint decl ++ fromString (concat matchTestNames) ++ "Test"


-- for testing
emptyModules = CodeTests Map.empty

srcSpanToHpc :: HSE.SrcSpanInfo -> Hpc.HpcPos
srcSpanToHpc (HSE.SrcSpanInfo sp _) =
  Hpc.toHpcPos (HSE.srcSpanStartLine sp, HSE.srcSpanStartColumn sp,
                HSE.srcSpanEndLine sp,   HSE.srcSpanEndColumn sp)
-}
------------------------------------------------------------------------------
expSrcSpan :: HSE.Exp HSE.SrcSpanInfo -> HSE.SrcSpanInfo
expSrcSpan e = case e of
  HSE.Var l _          -> l
  HSE.IPVar l _        -> l
  HSE.Con l _          -> l
  HSE.Lit l _          -> l
  HSE.InfixApp l _ _ _ -> l
  HSE.App l _ _        -> l
  HSE.NegApp l _       -> l
  HSE.Lambda l _ _     -> l
  HSE.Let l _ _        -> l
  HSE.If l _ _ _       -> l
  _                    -> emptySrcSpan

emptySrcSpan :: HSE.SrcSpanInfo
emptySrcSpan = HSE.SrcSpanInfo (HSE.SrcSpan "" 0 0 0 0) []
{-
if exp then exp else exp
MultiIf l [IfAlt l]	

if | exp -> exp ...
Case l (Exp l) [Alt l]	

case exp of alts
Do l [Stmt l]	

do-expression: the last statement in the list should be an expression.
MDo l [Stmt l]	

mdo-expression
Tuple l Boxed [Exp l]	

tuple expression
TupleSection l Boxed [Maybe (Exp l)]	

tuple section expression, e.g. (,,3)
List l [Exp l]	

list expression
Paren l (Exp l)	

parenthesised expression
LeftSection l (Exp l) (QOp l)	

left section (exp qop)
RightSection l (QOp l) (Exp l)	

right section (qop exp)
RecConstr l (QName l) [FieldUpdate l]	

record construction expression
RecUpdate l (Exp l) [FieldUpdate l]	

record update expression
EnumFrom l (Exp l)	

unbounded arithmetic sequence, incrementing by 1: [from ..]
EnumFromTo l (Exp l) (Exp l)	

bounded arithmetic sequence, incrementing by 1 [from .. to]
EnumFromThen l (Exp l) (Exp l)	

unbounded arithmetic sequence, with first two elements given [from, then ..]
EnumFromThenTo l (Exp l) (Exp l) (Exp l)	

bounded arithmetic sequence, with first two elements given [from, then .. to]
ListComp l (Exp l) [QualStmt l]	

ordinary list comprehension
ParComp l (Exp l) [[QualStmt l]]	

parallel list comprehension
ExpTypeSig l (Exp l) (Type l)	

expression with explicit type signature
VarQuote l (QName l)	

'x for template haskell reifying of expressions
TypQuote l (QName l)	

''T for template haskell reifying of types
BracketExp l (Bracket l)	

template haskell bracket expression
SpliceExp l (Splice l)	

template haskell splice expression
QuasiQuote l String String	

quasi-quotaion: [$name| string |]
XTag l (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]	

xml element, with attributes and children
XETag l (XName l) [XAttr l] (Maybe (Exp l))	

empty xml element, with attributes
XPcdata l String	

PCDATA child element
XExpTag l (Exp l)	

escaped haskell expression inside xml
XChildTag l [Exp l]	

children of an xml element
CorePragma l String (Exp l)	

CORE pragma
SCCPragma l String (Exp l)	

SCC pragma
GenPragma l String (Int, Int) (Int, Int) (Exp l)	

GENERATED pragma
Proc l (Pat l) (Exp l)	

arrows proc: proc pat -> exp
LeftArrApp l (Exp l) (Exp l)	

arrow application (from left): exp -< exp
RightArrApp l (Exp l) (Exp l)	

arrow application (from right): exp >- exp
LeftArrHighApp l (Exp l) (Exp l)	

higher-order arrow application (from left): exp -<< exp
RightArrHighApp l (Exp l) (Exp l)	

higher-order arrow application (from right): exp >>- exp
LCase l [Alt l]	
-}


------------------------------------------------------------------------------
declSrcSpan :: HSE.Decl HSE.SrcSpanInfo -> HSE.SrcSpanInfo
declSrcSpan decl = case decl of
--  HSE.ClosedTypeFamDecl l _ _ _ -> l -- These are in HSE 1.16
--  HSE.MinimalPragma l _         -> l -- Two more w/ diff arity btw .15&.16
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
  HSE.PatBind l _ _ _ _         -> l
  HSE.DefaultDecl l _           -> l
  HSE.SpliceDecl l _            -> l
  HSE.TypeSig l _ _             -> l
  HSE.FunBind l _               -> l
  HSE.ForImp l _ _ _ _ _        -> l
  HSE.ForExp l _ _ _ _          -> l
  HSE.RulePragmaDecl l _        -> l
  HSE.DeprPragmaDecl l _        -> l
  HSE.WarnPragmaDecl l _        -> l
  HSE.InlineSig l _ _ _         -> l
  HSE.InlineConlikeSig l _ _    -> l
  HSE.SpecSig l _ _ _           -> l
  HSE.SpecInlineSig l _ _ _ _   -> l
  HSE.InstSig l _ _             -> l
  HSE.AnnPragma l _             -> l
