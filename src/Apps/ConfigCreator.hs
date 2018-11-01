{-# LANGUAGE OverloadedStrings #-}

module Apps.ConfigCreator where

import           Lib
import           Data.Text                     as T
import           Data.Map                      as DM
import           Text.Trifecta
import           Util
import           Colors

type LineParser = Parser T.Text

type OldLine = T.Text
type NewLine = T.Text

missingColor :: ColorName -> ThemeName -> T.Text
missingColor cName tName =
  "Could not find color "
    `T.append` cName
    `T.append` " in theme "
    `T.append` tName

-- This isn't tested as it only makes sense in the context of those functions.
-- That's probably a very bad practice but whatever. It's tested in
-- KittySpec.hs, XSpec.hs etc.
configCreator'
  :: LineParser
  -- ^^^ Checks if a line has a valid color name and returns it for lookup of a new value.
  -> (OldLine -> RGBA -> Either T.Text NewLine)
  -- ^^^ Transforms the old line into a new line with the given color value
  -> Theme
  -- ^^^ Map of color name to RGBA color value
  -> Config
  -- ^^^ Config of a terminal emulator (e.g., kitty.config)
  -> Either T.Text Config
configCreator' lineP makeNewLine theme oldConfig =
  fmap T.unlines . traverse mapper $ T.lines oldConfig
 where
  getColorValue key = DM.lookup key (_colors theme)
  mapper :: OldLine -> Either T.Text NewLine
  mapper currentLine = case parseText lineP currentLine of
    (Success colorName) -> maybe noColorMsg newLine newColorValue
     where
      noColorMsg    = Left $ missingColor colorName (_name theme)
      newLine       = makeNewLine currentLine
      newColorValue = getColorValue colorName
    (Failure _) -> Right currentLine
