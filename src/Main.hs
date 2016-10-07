{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

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

dasherize :: Inline -> String
dasherize Space = "-"
dasherize (Str s) = map toLower s


readDoc :: String -> Pandoc
readDoc s = case readOrg def s of
  Right doc -> doc
  Left err -> error (show err)

writeDoc :: Pandoc -> String
writeDoc doc = writeLaTeX def doc


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


data StructureLeaf = Concept { getID::String, getName::String, contents::[Block] } deriving (Show)
type Structure     = Tree StructureLeaf
type SubStructures = Forest StructureLeaf


data MindMap = M {
  getMapName :: String,
  getAuthor :: Maybe String,
  getStructure :: Structure
  }

-- We need to start from a Heading 1

type Chunk = (Block, [Block])

getLevel :: Chunk -> Int
getLevel (Header n _ _, _) = n
getLevel _ = error "Sorry, chunk is invalid"

add :: Block -> [Chunk] -> [Chunk]
add x@Header{} cs = cs ++ [(x, [])]
add x cs@(_:_)    =
  let i = init cs
      (h, l) = last cs
  in i ++ [(h, l ++ [x])]
add x []          = error "Sorry, the document should begin with at least one top concept"

-- A bit like map but stops when f returns Nothing.
takeWhile' :: ( a -> Bool ) -> [ a ] -> ([a], [a])
takeWhile' f (a:b) = case f a of
    True -> let r = takeWhile' f b
            in (a : fst r, (snd r))
    False -> ([], a:b)
takeWhile' f [] = ([], [])


asDasherized = concatMap dasherize

asPlainString = concatMap f where
  f (Str s) = s
  f Space = " "

-- Need to use unfoldr (anamorphism):
--
--    unfoldr:: ([a] -> Maybe (b, [a])) -> [a] -> [b]
--
-- so we need to build:
--
--    ([Chunk] -> Maybe (Structure, [Chunk]))
--

toStructure :: Int -> [Chunk] -> Maybe (Structure, [Chunk])
toStructure n0 ((h@(Header n1 _ text), bs):cs)
   | n0 == n1 = let
                    getValidBlocks     = takeWhile' (\x -> getLevel x > n1)
                    (valid, remaining) = getValidBlocks cs
                    subFor= unfoldr (toStructure $ n0 + 1) valid
                    currentTree = Node (Concept (asDasherized text) (asPlainString text) bs) subFor
                in
                    Just (currentTree, remaining)
   | n0 /= n1 = Nothing
toStructure _ [] = Nothing


parseChunks :: [Chunk] -> Structure 
parseChunks cs = let subFor= unfoldr (toStructure 1) cs
                     currentTree = Node (Concept "root" "Root" []) subFor
     in currentTree


getChunks :: Pandoc -> [Chunk]
getChunks (Pandoc _ bs) = foldl (flip add) [] bs


