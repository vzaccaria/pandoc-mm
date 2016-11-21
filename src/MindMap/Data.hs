{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module MindMap.Data where

import Text.Pandoc
import Text.Pandoc.Error
import Text.Pandoc.Walk (walk)
import qualified Data.Map as Map
import Debug.Trace
import Data.List
import Data.String.Interpolate
import System.Process
import System.Directory
import Data.Char (toLower)
import Control.Monad.Supply
import Data.Tree
import Utils

type HeadingMeta = Map.Map String String

data StructureLeaf =
  Concept {getID :: String
          ,getUID :: Integer
          ,getName :: String
          ,getHeadingMeta :: HeadingMeta
          ,contents :: [Block]}
  deriving (Show,Eq)

type Structure = Tree StructureLeaf

type SubStructures = Forest StructureLeaf

data MindMap =
  M {getMapName :: Maybe String
    ,getAuthor :: Maybe String
    ,getStructure :: Structure
    ,otherMeta :: String -> Maybe String }

getMindMapName :: MindMap -> String
getMindMapName (M (Just n) _ _ _) = n
getMindMapName (M _ _ _ _) = "No name"

type Chunk = (Block,[Block])

getLevel :: Chunk -> Int
getLevel (Header n _ _,_) = n
getLevel _ = error "Sorry, chunk is invalid"

add :: Block -> [Chunk] -> [Chunk]
add x@Header{} cs = cs ++ [(x,[])]
add x cs@(_:_) =
  let i = init cs
      (h,l) = last cs
  in i ++ [(h,l ++ [x])]
add x [] =
  -- This might happen also when some meta info is present but with a blank value
  -- Pandoc recognizes this as a paragraph and so it appears as some info outside concepts
  -- final decision; change from:
  -- error "Sorry, the document should begin with at least one top concept"
  -- to
  []
  

asDasherized :: [Inline] -> [Char]
asDasherized = concatMap dasherize

asPlainString :: [Inline] -> [Char]
asPlainString = concatMap f
  where f (Str s) = s
        f Space = " "
        f _ = "*"

-- Need to use unfoldr (anamorphism):
--
--    unfoldr:: ([a] -> Maybe (b, [a])) -> [a] -> [b]
--
-- so we need to build:
--
--    ([Chunk] -> Maybe (Structure, [Chunk]))
--
toStructure
  :: Int -> [Chunk] -> Maybe (Structure,[Chunk])
toStructure n0 ((h@(Header n1 (_,_,mt) text),bs):cs)
  | n0 == n1 =
    let getValidBlocks = takeWhile' (\x -> getLevel x > n1)
        (valid,remaining) = getValidBlocks cs
        subFor = unfoldr (toStructure $ n0 + 1) valid
        currentTree =
          Node (Concept (asDasherized text)
                        0 -- to be filled later
                        (asPlainString text)
                        (Map.fromList mt)
                        bs)
               subFor
    in Just (currentTree,remaining)
  | n0 /= n1 = Nothing
toStructure _ [] = Nothing

parseChunks :: [Chunk] -> Structure
parseChunks cs =
  let subFor = unfoldr (toStructure 1) cs
      currentTree = Node (Concept "root" 0 "Root" (Map.empty) []) subFor
  in currentTree

getChunks :: Pandoc -> [Chunk]
getChunks (Pandoc _ bs) = foldl (flip add) [] bs

-- To generate unique identifiers we use the Supply monad:
-- https://hackage.haskell.org/package/monad-supply-0.6/docs/Control-Monad-Supply.html
-- See for example http://stackoverflow.com/questions/22828137/how-do-i-use-the-supply-monad-to-create-a-function-that-generates-globally-uniqu


-- this will give us: evalSupply :: SuppliedStructure -> [Integer] -> Structure
type SuppliedStructure = Supply Integer Structure

-- How to build a SuppliedStructure from a Structure? Let'traverse the tree with mapM and
-- embellish each node with an action that updates it's number
-- 
-- mapM :: Monad m => (a -> m b) -> t a -> m (t b)

buildSuppliedStructure :: Structure -> SuppliedStructure   
buildSuppliedStructure = mapM f where
  f o@(Concept i _ x y z) = do -- we should fill up the id field
    n <- supply  -- get the new supply
    if i /= "root"
      then
        return $ Concept (i ++ "-" ++ show n) n x y z
      else
        return o
  
supplyIDs :: [Integer] -> Structure -> Structure
supplyIDs a s = evalSupply (buildSuppliedStructure s) a 


uniquifyID :: MindMap -> MindMap
uniquifyID (M t a c m) = M t a c' m where
   c' = supplyIDs [1..] c

parseMindMap :: Pandoc -> MindMap
parseMindMap x@(Pandoc meta _) = uniquifyID $
  M (getMV "title")
    (getMV "author")
    (parseChunks $ getChunks x)
    getMV
  where getMV k =
          case Map.lookup k (unMeta meta) of
            Just (MetaInlines i) -> Just $ asPlainString  i
            _ -> Nothing
