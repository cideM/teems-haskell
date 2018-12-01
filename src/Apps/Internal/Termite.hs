{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Termite where

import           Types
import qualified Data.Text                     as Text
import           Text.Trifecta
import           Data.Semigroup
import           Parser.Internal
import           Control.Applicative
import           Apps.Internal.ConfigCreator

termite :: App
termite = App "termite" (configCreator' lineP mkLine) ["termite/config"]
 where
  mkLine l (RGBA (r, g, b, a)) = case parseText lineTillColorP l of
    (Success leading) -> Right $ leading <> rgbaText
     where
      rgbaText =
        "rgba("
          <> (Text.pack . show $ r)
          <> ", "
          <> (Text.pack . show $ g)
          <> ", "
          <> (Text.pack . show $ b)
          <> ", "
          <> (Text.pack . show $ a)
          <> ")"
    (Failure errInfo) ->
      Left $ "Failed to parse leading part of old line: " <> Text.pack
        (show errInfo)

-- Excluding colorN (color0, color100,...)
termiteColorP :: Parser Text.Text
termiteColorP = Text.pack <$> choice
  (fmap
    string
    ["foreground", "foreground_bold", "foreground_dim", "background", "cursor"]
  )

lineP :: Parser Text.Text
lineP =
  spaces *> choice [colorNP, termiteColorP] <* (some space <|> string "=")

lineTillColorP :: Parser Text.Text
lineTillColorP = Text.pack <$> manyTill anyChar (skipSome (char '#') <|> eof)
