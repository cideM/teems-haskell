{-# LANGUAGE OverloadedStrings #-}

module Apps.Kitty where

import           Lib
import           Data.Text                     as T
import           Text.Trifecta
import           Util
import           Apps.ConfigCreator
import           Colors

kitty :: App
kitty = App
  { appName       = "kitty"
  , configCreator = configCreator' lineP makeNewLine
  , configPaths   = fmap getConfigPath ["kitty/kitty.config"]
  }

-- Excluding colorN (color0, color100,...)
kittyColorNames :: [String]
kittyColorNames =
  ["foreground", "selection_foreground", "selection_background", "background"]

lineP :: Parser String
lineP = T.unpack <$> colorNameP kittyColorNames

lineWithoutColorP :: Parser String
lineWithoutColorP = manyTill anyChar (char '#')

makeNewLine :: RGBA -> OldLine -> Either T.Text NewLine
makeNewLine color l = case parseText lineWithoutColorP l of
  (Success leading) -> Right $ T.pack leading `T.append` hexAsText
    where hexAsText = displayHexColor $ rgbaToHexColor color
  (Failure _) -> Left "Failed to parse leading part of old line"
