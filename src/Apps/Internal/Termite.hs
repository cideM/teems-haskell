{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Termite where

import           Apps.Internal.ConfigCreator
import           Control.Applicative
import           Data.Semigroup
import           Data.Text                   as Text
import           Parser.Internal
import           Text.Trifecta
import           Types.Internal.Colors       (RGBA (..))
import           Types.Internal.Misc

termite :: App
termite = App "termite" (configCreator' lineP mkLine) ["termite/config"]
  where
    mkLine l (RGBA r g b a) =
      case parseText lineTillColorP l of
        (Success leading) -> Right $ leading <> rgbaText
          where rgbaText =
                  "= rgba(" <> (Text.pack . show $ r) <> ", " <>
                  (Text.pack . show $ g) <>
                  ", " <>
                  (Text.pack . show $ b) <>
                  ", " <>
                  (Text.pack . show $ a) <>
                  ")"
        (Failure errInfo) ->
          Left $
          "Failed to parse leading part of old line: " <>
          Text.pack (show errInfo)

-- | Parses termite specific color values
termiteColorP :: Parser Text
termiteColorP =
  Text.pack <$>
  choice
    (fmap
       string
       [ "foreground"
       , "foreground_bold"
       , "foreground_dim"
       , "background"
       , "cursor"
       ])

lineP :: Parser Text
lineP = spaces *> choice [colorNP, termiteColorP] <* (some space <|> string "=")

lineTillColorP :: Parser Text
lineTillColorP = Text.pack <$> manyTill anyChar (char '=')
