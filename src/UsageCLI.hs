{-# LANGUAGE QuasiQuotes #-}
module UsageCLI (progUsage) where

import System.Environment (getArgs)
import System.Console.Docopt

progUsage :: Docopt
progUsage = [docopt|
pandoc-mm

Usage:
    pandoc-mm FILE [ -x ]
    pandoc-mm --help | -h
    pandoc-mm --version

Options:
    -x, --latex            Output raw latex
    -h, --help             Show help
    --version              Show version.

Arguments
    FILE                   Org file containing the mindmap
|]
