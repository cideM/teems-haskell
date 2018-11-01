{-# LANGUAGE OverloadedStrings #-}

module Apps.Termite where

import           Lib
import           Data.Text                     as T
import           Text.Trifecta
import           Util
import           Apps.ConfigCreator
import           Colors

termite :: App
termite = App
  { appName       = "termite"
  , configCreator = configCreator' lineP makeNewLine
  , configPaths   = fmap getConfigPath ["termite/config"]
  }

-- Excluding colorN (color0, color100,...)
termiteColorNames :: [String]
termiteColorNames =
  ["foreground", "foreground_bold", "foreground_dim", "background", "cursor"]


lineP :: Parser String
lineP = T.unpack <$> colorNameP termiteColorNames

lineWithoutColorP :: Parser String
lineWithoutColorP = manyTill anyChar (char '#')

makeNewLine :: RGBA -> OldLine -> Either T.Text NewLine
makeNewLine (RGBA (r, g, b, a)) l = case parseText lineWithoutColorP l of
  (Success leading) -> Right $ T.pack leading `T.append` rgbaText
   where
    rgbaText =
      "rgba("
        `T.append` (T.pack . show $ r)
        `T.append` ", "
        `T.append` (T.pack . show $ g)
        `T.append` ", "
        `T.append` (T.pack . show $ b)
        `T.append` ", "
        `T.append` (T.pack . show $ a)
        `T.append` ")"
  (Failure _) -> Left "Failed to parse leading part of old line"
