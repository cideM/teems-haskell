{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Kitty where

import           Types
import           Data.Semigroup
import           Data.Text                     as T
import           Text.Trifecta
import           Parser.Internal
import           Control.Applicative
import           Apps.Internal.ConfigCreator

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
lineP = spaces *> choice [colorNP, kittyColorP] <* (some space <|> string "=")

lineTillColorP :: Parser T.Text
lineTillColorP = T.pack <$> manyTill anyChar (skipSome (char '#') <|> eof)

makeNewLine :: OldLine -> RGBA -> Either T.Text NewLine
makeNewLine l color = case parseText lineTillColorP l of
  (Success leading) -> Right $ leading <> hexAsText
    where hexAsText = displayHexColor $ rgbaToHexColor color
  (Failure errInfo) ->
    Left $ "Failed to parse leading part of old line: " <> T.pack (show errInfo)
