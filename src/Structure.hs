-- Copyright (c) 2010-11 Tillmann Rendel <rendel@informatik.uni-marburg.de>
-- This code can be used under the terms of a 3-clause BSD license.
-- See LICENSE for details.

{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}

-- | This module provides a structured view on pandoc documents
--   as trees, with section headers as inner nodes and blocks as
--   leafs.

module Structure
  (
  -- * Example

  -- $example

  -- * A structured view on Pandoc documents

    Structure (..)

  -- * Converting from and to the structured view

  , toStructure
  , fromStructure

  -- * Convenience functions

  , onStructure
  ) where

import Data.Typeable (Typeable ())
import Data.Data (Data ())

import Text.Pandoc.Definition

-- $example
-- The structured view can be convenient for document
-- transformations which act on whole sections or subsections
-- on the input. For example, some document classes for LaTeX
-- expect an abstract in an @abstract@ environment, but in
-- markdown, it is more convenient to write the abstract in a
-- section called @Abstract@. The following script converts
-- the latter into the former:
--
-- > fromStructure . topDown (concatMap transform) . toStructure where
-- >   transform :: Structure -> [Structure]
-- >   transform (Section _ [Str "Abstract"] contents)
-- >     =   [RawBlock "latex" "\\begin{abstract}"]
-- >     ++  contents
-- >     ++  [RawBlock "latex" "\\end{abstract}"]
-- >   transform abstract content
-- >     =  [content]

-- | A pandoc document as a tree of sections and blocks.
data Structure
  =  Block Block
  |  Section Int [Inline] [Structure]
  deriving (Eq, Show, Typeable, Data)

-- | Convert a flat Pandoc document into a document forest.
toStructure :: [Block] -> [Structure]
toStructure = fst . go 0
  where
  go _ [] = ([], [])

  go outer rest@(Header inner _ _ : _) | inner <= outer
    =  ([], rest)

  go outer (Header inner _ text : rest)
    =  (section : structure'', rest'')
    where (structure', rest') = go inner rest
          (structure'',  rest'') = go outer rest'
          section = Section inner text structure'

  go outer (first : rest)
    =  (block : structure', rest')
    where (structure', rest') = go outer rest
          block = Block first

-- | Convert a document tree into a flat Pandoc document
fromStructure :: Structure -> [Block]
fromStructure (Block block)
  = [block]
fromStructure (Section level header structure)
  = Header level nullAttr header : concatMap fromStructure structure

-- | Apply a structure transformer on a full Pandoc document.
onStructure :: ([Structure] -> [Structure]) -> Pandoc -> Pandoc
onStructure f (Pandoc meta blocks)
  = Pandoc meta . concatMap fromStructure . f . toStructure $ blocks

