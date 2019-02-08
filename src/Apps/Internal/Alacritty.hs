{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apps.Internal.Alacritty
  ( alacritty
  ) where

import           Control.Applicative
import           Data.Functor
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import           Data.Semigroup
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Vector               as Vector
import           Parser.Internal           (parseText)
import           Text.Trifecta
import           Types.Internal.Colors     (HexColor (..))
import qualified Types.Internal.Colors     as Colors
import           Types.Internal.Exceptions (TransformExceptionType (..))
import           Types.Internal.Misc

-- | Mode exists because Alacritty's config has two color blocks, one
-- for normal colors (0-7) and one for bright colors (color8-15) but the color
-- names are the same.
data ModeType
  = Normal
  | Bright
  deriving (Show, Eq, Ord)

data ParseResult
  = Mode ModeType
  | Line { _name  :: ColorName
         , _value :: Text -- ^ By keeping the value we can later just do Text.replace
          }

alacritty :: App
alacritty = App "alacritty" transform ["alacritty/alacritty.yml"]

-- | Map from alacritty's name for a color, to the color name in the themes
-- file. First tuple element is normal mode, second is bright mode. Some names
-- are the same in both modes, hence the duplicate strings.
colorMap :: Map.Map Text (Text, Text)
colorMap =
  Map.fromList
    [ ("black", ("color0", "color8"))
    , ("red", ("color1", "color9"))
    , ("green", ("color2", "color10"))
    , ("yellow", ("color3", "color11"))
    , ("blue", ("color4", "color12"))
    , ("magenta", ("color5", "color13"))
    , ("cyan", ("color6", "color14"))
    , ("white", ("color7", "color15"))
    , ("background", ("background", "background"))
    , ("foreground", ("foreground", "foreground"))
    , ("text", ("text", "text"))
    , ("cursor", ("cursor", "cursor"))
    , ("dim_foreground", ("dim_foreground", "dim_foreground"))
    , ("bright_foreground", ("bright_foreground", "bright_foreground"))
    ]

-- | modeP parses the mode of the current color block
modeP :: Parser ParseResult
modeP =
  Mode <$>
  try
    (spaces *> (string "bright:" $> Bright) <|>
     (string "normal:" $> Normal) <* spaces)

colorLineParser :: Parser ParseResult
colorLineParser =
  let colorNames = Map.keys colorMap
   in Line <$>
      (Text.pack <$>
       (whiteSpace *> choice (string . Text.unpack <$> colorNames))) <*>
      (Text.pack <$>
       (whiteSpace *> char ':' *> whiteSpace *> colorDelimiter *> string "0x" *>
        count 6 alphaNum <*
        colorDelimiter))
  where
    colorDelimiter = string "\'" <|> string "\""

transform :: Theme -> Config -> Either TransformExceptionType Config
transform theme input =
  fmap Text.unlines .
  sequence . Vector.toList . snd . List.foldl' foldFn (Normal, Vector.empty) $
  Text.lines input
  where
    lineParser = modeP <|> colorLineParser
    mapColorName Normal colorName = fst <$> Map.lookup colorName colorMap
    mapColorName Bright colorName = snd <$> Map.lookup colorName colorMap
    foldFn (mode, acc) current =
      case parseText lineParser current of
        Success (Mode newMode) -> (newMode, acc `Vector.snoc` Right current)
        Success Line {..} ->
          case mapColorName mode _name >>= flip Map.lookup (colors theme) of
            Nothing -> (,) mode (acc `Vector.snoc` Left (ColorNotFound _name))
            Just rgba ->
              let (HexColor r g b) = Colors.toHex rgba
                  replacement = r <> g <> b
               in (,)
                    mode
                    (acc `Vector.snoc`
                     (Right $ Text.replace _value replacement current))
        Failure _ -> (mode, acc `Vector.snoc` Right current)
