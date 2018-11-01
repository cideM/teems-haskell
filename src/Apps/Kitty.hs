{-# LANGUAGE OverloadedStrings #-}

module Apps.Kitty where

import           Lib
import           Data.Text                     as T
import           Text.Trifecta
import           Util
import           Apps.ConfigCreator
import           Colors

kitty :: App
kitty = App "kitty" (configCreator' lineP makeNewLine) ["kitty/kitty.config"]

-- Excluding colorN (color0, color100,...)
kittyColorP :: Parser T.Text
kittyColorP = T.pack <$> choice
  (fmap
    string
    ["foreground", "selection_foreground", "selection_background", "background"]
  )

lineP :: Parser T.Text
lineP = spaces *> choice [colorNP, kittyColorP] <* spaces

lineWithoutColorP :: Parser T.Text
lineWithoutColorP = T.pack <$> manyTill anyChar (char '#')

makeNewLine :: OldLine -> RGBA -> Either T.Text NewLine
makeNewLine l color = case parseText lineWithoutColorP l of
  (Success leading) -> Right $ leading `T.append` hexAsText
    where hexAsText = displayHexColor $ rgbaToHexColor color
  (Failure _) -> Left "Failed to parse leading part of old line"
