{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Control.Monad (void)
import Data.GraphViz.Commands
import Data.GraphViz.Types
import Data.GraphViz.Types.Generalised
import Distribution.Simple
import Graphmod
import System.IO.Silently
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

main ∷ IO ()
main =
  defaultMainWithHooks simpleUserHooks
  { postBuild = \args flags packageDescription localBuildInfo → do
      postBuild simpleUserHooks args flags packageDescription localBuildInfo

      output ← capture_ $ graphmod ["--quiet"]
      let graph = reverseArrows (parseDotGraph (Text.pack output) ∷ DotGraph String)
      Text.writeFile "Modules.dot" $ printDotGraph graph

      hasGraphviz ← isGraphvizInstalled
      if hasGraphviz then do
        void $ runGraphviz graph Svg "Modules.svg"
      else pure ()
  }

reverseArrows ∷ DotGraph n → DotGraph n
reverseArrows graph = graph {graphStatements = reverseStatement <$> graphStatements graph}
  where
    reverseStatement (DE edge) =
      DE edge {fromNode = toNode edge, toNode = fromNode edge}
    reverseStatement (SG subGraph) =
      SG subGraph {subGraphStmts = reverseStatement <$> subGraphStmts subGraph}
    reverseStatement statement = statement
