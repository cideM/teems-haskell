{-# LANGUAGE OverloadedStrings #-}

module Apps.Termite where

import           Lib
import           Data.Text                     as T
import           Text.Trifecta
import           Util
import           Apps.ConfigCreator
import           Colors

termite :: App
termite = App "termite" (configCreator' lineP makeNewLine) ["termite/config"]

-- Excluding colorN (color0, color100,...)
termiteColorP :: Parser T.Text
termiteColorP = T.pack <$> choice
  (fmap
    string
    ["foreground", "foreground_bold", "foreground_dim", "background", "cursor"]
  )


lineP :: Parser T.Text
lineP = spaces *> choice [colorNP, termiteColorP] <* spaces

lineWithoutColorP :: Parser T.Text
lineWithoutColorP = T.pack <$> manyTill anyChar (char '#')

makeNewLine :: OldLine -> RGBA -> Either T.Text NewLine
makeNewLine l (RGBA (r, g, b, a)) = case parseText lineWithoutColorP l of
  (Success leading) -> Right $ leading `T.append` rgbaText
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
