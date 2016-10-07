{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module MindMap.Print where

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
import Utils

template dta = [i|
\\documentclass{article}
\\usepackage{fontspec,xunicode,xltxtra}
\\setmainfont[Scale=0.74]{Fira Sans Medium}
\\usepackage{tikz}
\\usetikzlibrary{mindmap}
\\pagestyle{empty}
\\begin{document}
\\begin{tikzpicture}[mindmap, grow cyclic, every node/.style=concept, concept color=orange!40,
    level 1/.append style={level distance=5cm,sibling angle=90},
    level 2/.append style={level distance=3cm,sibling angle=45},]
#{dta};
\\end{tikzpicture}
\\end{document}
|]

asLatex :: Pandoc -> String
asLatex d = template $ writeDoc d


drawStruct :: Structure -> String
drawStruct t = drawTree (fmap f t) where
   f (Concept idn nm c) = nm ++ " - " ++ (show c)


asStructuredData :: String -> Tree StructureLeaf -> String
asStructuredData r node = 
    let contents = concatMap (\x -> asStructuredData r x) (subForest node)
        identifier = getID $ rootLabel node
    in
        if identifier == r
          then [i| \\node{#{identifier}}
                 #{contents} |]
          else [i| child { node (#{identifier}) {#{getName $ rootLabel node}}
                 #{contents}} |]

printWithTemplate :: String -> IO ()
printWithTemplate s = putStrLn (template s)

drawWithTemplate :: String -> String -> IO ()
drawWithTemplate name exp = draw name (template exp)

draw :: String -> String -> IO ()
draw name exp = do
    system "rm -rf .pandoc-mm";
    createDirectory ".pandoc-mm";
    writeFile ".pandoc-mm/mm.tex" $ exp;
    system ("cd .pandoc-mm && xelatex mm.tex")
    system ("pdfcrop .pandoc-mm/mm.pdf " ++ name);
    return ();

