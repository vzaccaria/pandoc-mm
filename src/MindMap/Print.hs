{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module MindMap.Print (drawMindMap, printMindMap, printMindMapLatex) where

import Text.Pandoc
import Text.Pandoc.Error
import Text.Pandoc.Walk (walk)
import Debug.Trace
import Data.List
import qualified Data.Map as Map
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
   f (Concept idn nm mta c) = nm ++ "-" ++ (show mta) ++ " - " ++ (show c) 


asStructuredData :: String -> Tree StructureLeaf -> String
asStructuredData r node = 
    let contents = concatMap (\x -> asStructuredData r x) (subForest node)
        identifier = getID $ rootLabel node
        color = case Map.lookup "color" (getHeadingMeta $ rootLabel node) of
          (Just c) -> [i| color=#{c} |]
          _ -> ""
    in
        if identifier == "root"
          then [i| \\node{#{r}}
  #{contents} |]
          else [i| child[concept #{color}] { node[concept] (#{identifier}) {#{getName $ rootLabel node}}
  #{contents}} |]

draw :: String -> String -> IO ()
draw name exp = do
    system "rm -rf .pandoc-mm";
    createDirectory ".pandoc-mm";
    writeFile ".pandoc-mm/mm.tex" $ exp;
    system ("cd .pandoc-mm && xelatex mm.tex")
    system ("pdfcrop .pandoc-mm/mm.pdf " ++ name);
    return ();

drawMindMap :: String -> MindMap -> IO ()
drawMindMap fn m = let sStruct = asStructuredData (getMindMapName m) (getStructure m)
                       in draw fn $ asMindMapLatex m

asMindMapLatex :: MindMap -> String
asMindMapLatex m = let sStruct = asStructuredData (getMindMapName m) (getStructure m)
                          in (template sStruct)

printMindMapLatex :: MindMap -> IO ()
printMindMapLatex m = putStrLn $ asMindMapLatex m

printMindMap :: MindMap -> String
printMindMap m = drawStruct (getStructure m)

instance Show MindMap where
  show = printMindMap
