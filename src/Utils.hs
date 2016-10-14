{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils where

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
import qualified Data.Map as Map

dasherize :: Inline -> String
dasherize Space = "-"
dasherize (Str s) = map toLower s


readDoc :: String -> Pandoc
readDoc s = case readOrg def s of
  Right doc -> doc
  Left err -> error (show err)

writeDoc :: Pandoc -> String
writeDoc = writeLaTeX def

expandToLatex :: [Block] -> String
expandToLatex b = writeDoc $ Pandoc (Meta Map.empty) b

-- A bit like map but stops when f returns Nothing.
takeWhile' :: ( a -> Bool ) -> [ a ] -> ([a], [a])
takeWhile' f (a:b) = case f a of
    True -> let r = takeWhile' f b
            in (a : fst r, (snd r))
    False -> ([], a:b)
takeWhile' f [] = ([], [])



