{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Test where

import Text.Pandoc
import Text.Pandoc.Error
import Text.Pandoc.Walk (walk)
import Debug.Trace
import Data.List
import Data.String.Interpolate
import System.Process
import System.Directory
import Data.Char (toLower)
import Data.Tree

import MindMap.Data
import MindMap.Print
import Utils

testIssue n = do
  f <- readFile $ "./issues/" ++ (show n) ++ "/Category.org"
  return $ readDoc f


test :: IO Pandoc
test = do
  f <- readFile "./examples/Category.org";
  return $ readDoc f

testmm :: IO MindMap
testmm = do {
  f <- test;
  return $ parseMindMap f
}

