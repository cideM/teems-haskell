{-# LANGUAGE OverloadedStrings #-}

module Util.Internal
  ( hexColorToRGBA
  , rgbaToHexColor
  , lengthDesc
  , missingColor
  )
where

import           Types
import           Data.Semigroup
import           Data.Text                     as T

lengthDesc :: (Foldable t) => t a -> t a -> Ordering
lengthDesc a b =
  let cmp x y | x < y     = GT
              | x > y     = LT
              | otherwise = EQ
  in  cmp (Prelude.length a) (Prelude.length b)

missingColor :: ColorName -> ThemeName -> T.Text
missingColor cName tName =
  "Could not find color " <> cName <> " in theme " <> tName
