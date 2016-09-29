{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.Pandoc
import Text.Pandoc.Error
import Text.Pandoc.Walk (walk)
import Debug.Trace
import Structure
import Data.String.Interpolate

readDoc :: String -> Pandoc
readDoc s = case readOrg def s of
  Right doc -> doc
  Left err -> error (show err)

writeDoc :: Pandoc -> String
writeDoc doc = writePlain def doc

showStruct :: [Structure] -> [Structure]
showStruct x = concatMap myf' x where
  myf' :: Structure -> [Structure]
  myf' x = trace (show x) [x]

_lat x = Block $ RawBlock (Format "latex") x

_str :: String -> Structure
_str t = Block $ Plain $ [Str t]

_inl :: [Inline] -> Structure
_inl l = Block $ Plain l

_node :: [Inline] -> [Structure]
_node x = [p, m, e] where
  p =  _str "node {"   
  m = _inl x
  e = _str "}"

myf :: [Structure] -> [Structure]
myf x = concatMap myf' x where
  myf' :: Structure -> [Structure]
  myf' (Section _ n contents) =
    [ _str "child { " ] ++
    _node n ++
    myf contents ++
    [  _str " } " ]

  myf' q = []

template d = [i|
ciao #{d}
|]

main :: IO ()
-- main = do {
--   dta <- getContents;
--   print $ readDoc dta 
--     }

main = interact (writeDoc . (onStructure myf). readDoc)

