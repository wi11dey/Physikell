{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Monad
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
      let graph = op $ parseDotGraph (Text.pack output) ∷ DotGraph String
      Text.writeFile "Modules.dot" $ printDotGraph graph

      hasGraphviz ← isGraphvizInstalled
      if hasGraphviz then do
        void $ runGraphviz graph Svg "Modules.svg"
      else pure ()
  }

op ∷ DotGraph n → DotGraph n
op graph@DotGraph {..} = graph {graphStatements = opStatement <$> graphStatements}
  where
    opStatement (DE edge@DotEdge {..}) =
      DE edge {fromNode = toNode, toNode = fromNode}
    opStatement (SG subGraph@DotSG {..}) =
      SG subGraph {subGraphStmts = opStatement <$> subGraphStmts}
    opStatement statement = statement
