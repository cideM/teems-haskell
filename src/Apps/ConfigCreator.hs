{-# LANGUAGE OverloadedStrings #-}

module Apps.ConfigCreator where

import           Lib
import           Data.Text                     as T
import           Data.Map                      as DM
import           Text.Trifecta
import           Util
import           Colors
import           Data.Vector                   as Vec
import           Data.List                      ( foldr
                                                )

type LineParser = Parser String

type OldLine = T.Text
type NewLine = T.Text


-- This isn't tested as it only makes sense in the context of those functions.
-- That's probably a very bad practice but whatever. It's tested in
-- KittySpec.hs, XSpec.hs etc.
configCreator'
  :: LineParser
  -- ^^^ Checks if a line has a valid color name and returns it for lookup of a new value.
  -> (RGBA -> OldLine -> Either T.Text NewLine)
  -- ^^^ Transforms the old line into a new line with the given color value
  -> Theme
  -- ^^^ Map of color name to RGBA color value
  -> Config
  -- ^^^ Config of a terminal emulator (e.g., kitty.config)
  -> Either T.Text Config
configCreator' lineP makeNewLine theme oldConfig =
  fmap unlinesVec . Vec.sequence . Data.List.foldr run Vec.empty $ T.lines
    oldConfig
 where
  getColorValue key = DM.lookup key (colors theme)
  run l ls = case parseText lineP l of
    (Success colorName) -> l' `Vec.cons` ls
     where
      colorName' = T.pack colorName
      l' = case getColorValue colorName' of
        (Just val) -> makeNewLine val l
        Nothing ->
          Left
            $          "Could not find color "
            `T.append` colorName'
            `T.append` " in theme "
            `T.append` name theme
    (Failure _) -> Right l `Vec.cons` ls
