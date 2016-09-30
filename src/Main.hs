{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc
import Text.Pandoc.Error
import Text.Pandoc.Walk (walk)
import Debug.Trace
import Structure
import Data.String.Interpolate
import System.Process
import System.Directory

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

template title dta = [i|
\\documentclass{article}
\\usepackage{tikz}
\\usetikzlibrary{mindmap}
\\pagestyle{empty}
\\begin{document}
\\begin{tikzpicture}[mindmap, grow cyclic, every node/.style=concept, concept color=orange!40,
    level 1/.append style={level distance=5cm,sibling angle=90},
    level 2/.append style={level distance=3cm,sibling angle=45},]
\\node{#{title}}
#{dta};
\\end{tikzpicture}
\\end{document}
|]

main :: IO ()
main = do {
  dta <- getContents;
  draw "final.pdf "$ template "Root" $ docf dta
} where
  doct dta = onStructure myf $ readDoc dta
  docf dta = writeDoc $ doct dta;



draw :: String -> String -> IO ()
draw name exp = do
    system "rm -rf .pandoc-mm";
    createDirectory ".pandoc-mm";
    writeFile ".pandoc-mm/mm.tex" $ exp;
    system ("cd .pandoc-mm && pdflatex mm.tex")
    system ("pdfcrop .pandoc-mm/mm.pdf " ++ name);
    return ();
