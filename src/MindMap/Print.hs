{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module MindMap.Print where

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

template dta ann = [i|
\\documentclass{article}
\\usepackage{mathspec}
\\setallmainfonts(Digits,Latin){Fira Sans Light}
\\usepackage{tikz}
\\usetikzlibrary{mindmap}
\\providecommand{\\tightlist}{%
\\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}
  
\\pagestyle{empty}
\\begin{document}
\\setlength\\abovedisplayskip{5pt}
\\setlength\\belowdisplayskip{5pt}
\\setlength\\abovedisplayshortskip{5pt}
\\setlength\\belowdisplayshortskip{5pt}
\\tikzstyle{every annotation}=[fill=white, thin, draw=black!20]
\\begin{tikzpicture}[mindmap, grow cyclic, every node/.style=concept, concept color=orange!40,
    level 1/.append style={level distance=5cm,sibling angle=90},
    level 2/.append style={level distance=3cm,sibling angle=45},]
#{dta};
#{ann}
\\end{tikzpicture}
\\end{document}
|]

drawStruct :: Structure -> String
drawStruct t = drawTree (fmap f t) where 
   f (Concept idn nm mta c) = nm ++ "-" ++ (show mta) ++ " - " ++ (show c) 

_m node key = Map.lookup key (getHeadingMeta $ rootLabel node)

getConceptNodes :: String -> Tree StructureLeaf -> String
getConceptNodes r node = 
    let contents = concatMap (\x -> getConceptNodes r x) (subForest node)
        identifier = getID $ rootLabel node
        color = case (_m node "color") of
          (Just c) -> [i| color=#{c} |]
          _ -> ""
    in
        if identifier == "root"
          then [i| \\node{#{r}}
  #{contents} |]
          else [i| child[concept #{color}] { node[concept] (#{identifier}) {#{getName $ rootLabel node}}
  #{contents}} |]

getAnnotation node = expandToLatex (contents node)

getAnnotations :: String -> Tree StructureLeaf -> String
getAnnotations title node =
  let cc = concatMap (\x -> getAnnotations title x) (subForest node)
      identifier = getID $ rootLabel node
      an = getAnnotation (rootLabel node)
      in
    if identifier /= "root" && an /= "" then 
    [i|\\node[annotation, right] at (#{identifier}.east) {#{an}};
#{cc}
|] else cc

draw :: String -> String -> IO ()
draw name exp = do
    system "rm -rf .pandoc-mm";
    createDirectory ".pandoc-mm";
    writeFile ".pandoc-mm/mm.tex" $ exp;
    system ("cd .pandoc-mm && xelatex mm.tex")
    system ("pdfcrop .pandoc-mm/mm.pdf " ++ name);
    return ();

drawMindMap :: String -> MindMap -> IO ()
drawMindMap fn m = let sStruct = getConceptNodes (getMindMapName m) (getStructure m)
                       in draw fn $ asMindMapLatex m

asMindMapLatex :: MindMap -> String
asMindMapLatex m =
  let name = getMindMapName m
      struct = getStructure m
      sStruct = getConceptNodes name struct
      sAnn = getAnnotations name struct
  in (template sStruct sAnn)

printMindMapLatex :: MindMap -> IO ()
printMindMapLatex m = putStrLn $ asMindMapLatex m

printMindMap :: MindMap -> String
printMindMap m = drawStruct (getStructure m)

instance Show MindMap where
  show = printMindMap
