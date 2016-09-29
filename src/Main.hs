module Main where

import Text.Pandoc
import Text.Pandoc.Error
import Text.Pandoc.Walk (walk)
import Debug.Trace
import Structure

readDoc :: String -> Pandoc
readDoc s = case readOrg def s of
  Right doc -> doc
  Left err -> error (show err)

writeDoc :: Pandoc -> String
writeDoc doc = writeLaTeX def doc

myf :: [Structure] -> [Structure]
myf x = trace (show x) x

main :: IO ()
main = interact (writeDoc . (onStructure myf) . readDoc)

