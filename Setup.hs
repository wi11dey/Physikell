{-# OPTIONS_GHC -Wno-x-partial #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Data.GraphViz.Commands
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised
import Data.List
import Data.Maybe
import Distribution.Simple hiding (classifyExtension)
import Distribution.Simple.PreProcess
import Distribution.Simple.PreProcess.Unlit
import Distribution.Simple.SrcDist
import Distribution.Simple.Utils
import Distribution.Types.BuildInfo
import Distribution.Types.PackageDescription
import Distribution.Verbosity
import Graphmod
import Language.Haskell.Extension as Cabal hiding (classifyExtension)
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc
import System.IO.Silently
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

main = defaultMainWithHooks simpleUserHooks
  { buildHook = \package localBuildInfo hooks flags → do
      unicodeify package
      buildHook simpleUserHooks package localBuildInfo hooks flags
  , postBuild = \args flags packageDescription localBuildInfo → do
      postBuild simpleUserHooks args flags packageDescription localBuildInfo
      graphModules
  }

unicodeSyntax ∷ Token → Maybe T.Text
unicodeSyntax KW_Forall   = Just "∀"
unicodeSyntax DoubleColon = Just "∷"
unicodeSyntax RightArrow  = Just "→"
unicodeSyntax LeftArrow   = Just "←"
unicodeSyntax DoubleArrow = Just "⇒"
unicodeSyntax _           = Nothing

unicodeify ∷ PackageDescription → IO ()
unicodeify package = do
  let extensions = do
        BuildInfo {..} ← allBuildInfo package
        Cabal.EnableExtension extension ← defaultExtensions ++ otherExtensions
        return $ classifyExtension $ show extension
  packageFiles ← listPackageSources normal "." package knownSuffixHandlers
  (concat → replacements) ← forM packageFiles \file → do
    tokens ← lex extensions file
    return do
      Loc {..} ← tokens
      replacement ← maybeToList $ unicodeSyntax $ unLoc
      return (loc, replacement)
  sequence_ $ reverse $ map replace replacements
    where
      lex extensions parseFilename
        | ".lhs" `isSuffixOf` parseFilename = do
            source ← readFile parseFilename
            preprocessed ← either return (dieWithException normal) $ unlit parseFilename source
            return $
              fromParseResult $
              lexTokenStreamWithMode defaultParseMode { parseFilename, extensions } preprocessed
        | ".hs" `isSuffixOf` parseFilename = do
            source ← readFile parseFilename
            return $
              fromParseResult $
              lexTokenStreamWithMode defaultParseMode { parseFilename, extensions } source
        | otherwise = return []

      replace ∷ (SrcSpan, T.Text) → IO ()
      replace (SrcSpan { srcSpanFilename = filename
                       , srcSpanStartLine   = (subtract 1 → startLine)
                       , srcSpanStartColumn = (subtract 1 → startCol)
                       , srcSpanEndLine     = (subtract 1 → endLine)
                       , srcSpanEndColumn   = (subtract 1 → endCol)
                       },
                replacement) = do
        source ← T.readFile filename
        let (prevLines, splitAt (endLine - startLine + 1) → (spanLines, nextLines)) =
              splitAt startLine $ T.lines source
        T.writeFile filename $
          T.unlines $
          prevLines ++
            [ T.take startCol (head spanLines) <>
              replacement <>
              T.drop endCol (last spanLines)] ++
            nextLines

graphModules ∷ IO ()
graphModules = do
  output ← capture_ $ graphmod ["--quiet"]
  let graph = op $ parseDotGraph (LT.pack output) ∷ DotGraph String
  LT.writeFile "Modules.dot" $ printDotGraph graph

  graphvizInstalled ← isGraphvizInstalled
  if not graphvizInstalled
    then return ()
    else void $ runGraphviz graph Svg "Modules.svg"

    where
      -- |op constructs the op-category of a category by reversing all its arrows
      op ∷ DotGraph n → DotGraph n
      op graph@DotGraph {..} = graph {graphStatements = opStatement <$> graphStatements}

      opStatement (DE edge@DotEdge {..}) =
        DE edge {fromNode = toNode, toNode = fromNode}
      opStatement (SG subGraph@DotSG {..}) =
        SG subGraph {subGraphStmts = opStatement <$> subGraphStmts}
      opStatement statement = statement
