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
kitty = App "kitty" (configCreator' lineP mkLine) ["kitty/kitty.config"]
  where
    lineP =spaces *> choice [colorNP, kittyColorP] <* (some space <|> string "=")
    mkLine l c = case parseText lineTillColorP l of
      (Success leading) -> Right $ leading <> hex
        where hex = displayHexColor $ rgbaToHexColor c
      (Failure errInfo) ->
        Left $ "Failed to parse leading part of old line: " <> T.pack (show errInfo)

-- Excluding colorN (color0, color100,...)
kittyColorP :: Parser T.Text
kittyColorP = T.pack <$> choice
  (fmap
    string
    ["foreground", "selection_foreground", "selection_background", "background"]
  )

lineTillColorP :: Parser T.Text
lineTillColorP = T.pack <$> manyTill anyChar (skipSome (char '#') <|> eof)
