{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Termite where

import           Apps.Internal.MakeTransform
import           Control.Applicative
import           Data.Semigroup
import           Data.Text                   as Text
import           Text.Trifecta
import           Types.Internal.Colors       (RGBA (..))
import           Types.Internal.Misc
import           Parser.Internal             (parseText, colorNP)

termite :: App
termite = App "termite" (makeTransform' options) ["termite/config"]

options :: MakeTransformOptions'
options =
  MakeTransformOptions'
    { _shouldTransformLine' = shouldTransform
    , _getColorName' = getColorName
    , _getNewLine' = mkLine
    }
  where
    getColorName l =
      case parseText lineP l of
        Success color -> Just color
        _             -> Nothing
    shouldTransform l =
      case parseText lineP l of
        Success _ -> True
        _         -> False
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
