{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Termite where

import           Types
import           Data.Text                     as T
import           Text.Trifecta
import           Data.Semigroup
import           Parser.Internal
import           Apps.Internal.ConfigCreator

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
  (Success leading) -> Right $ leading <> rgbaText
   where
    rgbaText =
      "rgba("
        <> (T.pack . show $ r)
        <> ", "
        <> (T.pack . show $ g)
        <> ", "
        <> (T.pack . show $ b)
        <> ", "
        <> (T.pack . show $ a)
        <> ")"
  (Failure _) -> Left "Failed to parse leading part of old line"
