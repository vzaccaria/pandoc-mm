{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module MindMap.Print where

import           Data.Char               (toLower)
import           Data.List
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Tree
import           Debug.Trace
import           System.Directory
import           System.Process
import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.Pandoc.Walk        (walk)
import           MindMap.Data
import           Utils
import Paths_pandoc_mm

data PrintConfig  = P {
  body         :: String,
  annotations  :: String,
  connections  :: String,
  font         :: Maybe String,
  monofont     :: Maybe String,
  helpLines    :: Maybe String,
  preamble     :: String,
  tikzpreamble :: String
}

template :: PrintConfig -> String
template pc =
  let
    dta  = body pc
    cann = connections pc
    ann  = annotations pc
    fnt = case (font pc) of
      Nothing -> ""
      (Just x) -> [i| \\setallmainfonts(Digits,Latin){#{x}} |]
    mfnt = case (monofont pc) of
      Nothing -> ""
      (Just x) -> [i| \\setmonofont{#{x}} |]
    hl = case (helpLines pc) of
      Just x -> [i|\\draw [help lines] #{x}; |]
      Nothing -> ""
  in
    [i|
#{preamble pc}

#{fnt}
#{mfnt}

\\begin{document}
#{tikzpreamble pc}

#{dta};
#{ann};
#{hl};
\\begin{pgfonlayer}{background}
#{cann}
\\end{pgfonlayer}
\\end{tikzpicture}
\\end{document}
|]


-- level 1/.append style={level distance=5cm,sibling angle=90},
-- level 2/.append style={level distance=5cm,sibling angle=60},
-- level 3/.append style={level distance=5cm,sibling angle=60},
-- level 4/.append style={level distance=5cm,sibling angle=60},

drawStruct :: Structure -> String
drawStruct t = drawTree (fmap f t) where
   f (Concept idn _ nm mta c) = nm ++ "-" ++ (show mta) ++ " - " ++ (show c)

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
          else [i| child[concept #{color}] { node[concept, name=#{identifier}] {#{getName $ rootLabel node}}
  #{contents}} |]

getAnnotationText node = expandToLatex (contents node)

_get node d s =
  let v = fromMaybe d (_m node s)
  in case v of
       "-" -> d
       _ -> v



getAnnotationsConnections :: Tree StructureLeaf -> String -> String
getAnnotationsConnections node cc =
  let identifier = getID $ rootLabel node
      uid = getUID $ rootLabel node
      text       = getAnnotationText (rootLabel node) -- Latex content of node
  in
    if identifier /= "root" && text /= ""
      then [i|\n\\draw [concept connection] (#{identifier}) edge (ann-#{show uid}); #{cc} |]
      else cc


nodeToAnnotation :: Tree StructureLeaf -> String -> String
nodeToAnnotation node cc =
  let identifier = getID $ rootLabel node -- e.g. "free-monoid-1"
      uid        = getUID $ rootLabel node
      name       = getName $ rootLabel node
      uid'       = uid - 1
      text       = getAnnotationText (rootLabel node) -- Latex content of node
      placement  = _m node "placement"
  in
    if identifier /= "root"
      then
        if text /= "" then
          let nextpos = [i|, below of=ann-#{show uid'}|]
          in
             case placement of
                      Nothing -> [i|\n\\node[annotation, #{nextpos}, anchor=north, yshift=-.5cm]  (ann-#{show uid}) {\\textbf{#{name}}:\\\\#{text}};#{cc} |]
                      Just p -> [i|\n\\node[annotation]  (ann-#{show uid}) at (#{p}) {\\textbf{#{name}}:\\\\#{text}};#{cc} |]
            else
          [i|\n\\node[annotation, below of=ann-#{show uid'}, anchor=north]  (ann-#{show uid}) {};#{cc} |]
      else
        [i|\n\\node[annotation]  (ann-1) at (current page.south east) {};#{cc} |]

mapAnnotations :: (Tree StructureLeaf -> String -> String) -> Tree StructureLeaf -> String
mapAnnotations f node =
  let
    cc = concatMap (mapAnnotations f) (subForest node)
  in
    f node cc



draw :: String -> String -> IO ()
draw name exp = do
    system "rm -rf .pandoc-mm";
    createDirectory ".pandoc-mm";
    writeFile ".pandoc-mm/mm.tex" $ exp;
    system ("cd .pandoc-mm && xelatex -shell-escape -interaction nonstopmode mm.tex")
    system ("pdfcrop .pandoc-mm/mm.pdf " ++ name);
    return ();

readPreambles :: IO (String, String)
readPreambles = do
  preambleName <- getDataFileName "data/preamble.tex";
  tikzPreambleName <- getDataFileName "data/tikz_preamble.tex";
  latexPreamble <- readFile preambleName;  
  tikzPreamble <- readFile tikzPreambleName;
  return (latexPreamble, tikzPreamble)

drawMindMap :: String -> MindMap -> IO ()
drawMindMap fn m = do
  (latexPreamble, tikzPreamble) <- readPreambles 
  draw fn $ asMindMapLatex m latexPreamble tikzPreamble

asMindMapLatex :: MindMap -> String -> String -> String
asMindMapLatex m latexPreamble tikzPreamble =
  let
      name    = getMindMapName m
      struct  = getStructure m
      cfg = P
        (getConceptNodes name struct)
        (mapAnnotations nodeToAnnotation struct)
        (mapAnnotations getAnnotationsConnections struct)
        (otherMeta m "font")
        (otherMeta m "monofont")
        (otherMeta m "helplines")
        latexPreamble
        tikzPreamble
  in template cfg

printMindMapLatex :: MindMap -> IO ()
printMindMapLatex m = do
  (latexPreamble, tikzPreamble) <- readPreambles 
  putStrLn $ asMindMapLatex m latexPreamble tikzPreamble

printMindMap :: MindMap -> String
printMindMap m = drawStruct (getStructure m)

instance Show MindMap where
  show = printMindMap
