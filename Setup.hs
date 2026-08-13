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

main ∷ IO ()
main =
  defaultMainWithHooks simpleUserHooks
  { postBuild = \args flags packageDescription localBuildInfo → do
      postBuild simpleUserHooks args flags packageDescription localBuildInfo

      output ← capture_ $ graphmod ["--quiet"]
      writeFile "Modules.dot" output

      hasGraphviz ← isGraphvizInstalled
      if hasGraphviz then do
        void $ runGraphviz (parseDotGraph (Text.pack output) ∷ DotGraph String) Svg "Modules.svg"
      else pure ()
  }
