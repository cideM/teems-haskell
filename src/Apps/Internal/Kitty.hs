{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Kitty where

import           Apps.Internal.MakeTransform
import           Control.Applicative
import           Data.Semigroup
import           Data.Text                   as Text
import           Text.Trifecta
import qualified Types.Internal.Colors       as Colors
import           Types.Internal.Misc
import           Parser.Internal             (parseText, colorNP)

kitty :: App
kitty = App "kitty" (makeTransform' options) ["kitty/kitty.config"]

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
    lineP =
      spaces *> choice [colorNP, kittyColorP] <* (some space <|> string "=")
    mkLine l rgba =
      case parseText lineTillColorP l of
        (Success leading) -> Right $ leading <> hex
          where hex = Text.pack . show $ Colors.toHex rgba
        (Failure errInfo) ->
          Left $
          "Failed to parse leading part of old line: " <>
          Text.pack (show errInfo)

-- Excluding colorN (color0, color100,...)
kittyColorP :: Parser Text
kittyColorP =
  Text.pack <$>
  choice
    (fmap
       string
       [ "foreground"
       , "selection_foreground"
       , "selection_background"
       , "background"
       ])

lineTillColorP :: Parser Text
lineTillColorP = Text.pack <$> manyTill anyChar (skipSome (char '#') <|> eof)
