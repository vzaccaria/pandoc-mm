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


testStruct :: IO Structure
testStruct = do {
  f <- readFile "Category.org";
  return $ parseChunks $ getChunks $ readDoc f
}

testDraw = testStruct >>= (putStrLn . drawStruct)


testHierarchy = do {
  x <- testStruct;
  return $ asStructuredData  "root" x
}


p = testHierarchy >>= printWithTemplate
f = testHierarchy >>= (drawWithTemplate "x.pdf")
