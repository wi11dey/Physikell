{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Data.GraphViz.Commands (GraphvizOutput (Svg), isGraphvizInstalled, runGraphviz)
import Data.GraphViz.Types (parseDotGraph)
import Data.GraphViz.Types.Canonical (DotGraph)
import qualified Data.Text.Lazy as Text
import Distribution.Simple
import Graphmod (graphmod)
import System.IO (hPutStrLn, stderr)
import System.IO.Silently (capture)

main ∷ IO ()
main =
  defaultMainWithHooks simpleUserHooks
  { postBuild = \args flags packageDescription localBuildInfo → do
      postBuild simpleUserHooks args flags packageDescription localBuildInfo

      (output, ()) ← capture $ graphmod ["--quiet"]
      writeFile "Modules.dot" output

      hasGraphviz ← isGraphvizInstalled
      if hasGraphviz then do
        void $ runGraphviz (parseDotGraph (Text.pack output) ∷ DotGraph String) Svg "Modules.svg"
      else pure ()
  }
