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
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Parser.Internal           (parseText)
import           Text.Trifecta
import           Types.Internal.Colors     (HexColor (..))
import qualified Types.Internal.Colors     as Colors
import           Types.Internal.Exceptions (TransformExceptionType (..))
import           Types.Internal.Misc

-- | Mode exists because Alacritty's config has two color blocks, one
-- for normal colors (0-7) and one for bright colors (color8-15)
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

-- TODO: Unify all of the below
nonModeColorNames :: [ColorName]
nonModeColorNames =
  [ "background"
  , "foreground"
  , "text"
  , "cursor"
  , "dim_foreground"
  , "bright_foreground"
  ]

-- | colorNames includes all color names we parse and on which lines we replace
-- the color value with a new one from the theme
colorNames :: [ColorName]
colorNames =
  ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"] ++
  nonModeColorNames

nonModeColors :: Map.Map ColorName Text
nonModeColors =
  Map.fromList
    [ ("background", "background")
    , ("foreground", "foreground")
    , ("text", "text")
    , ("cursor", "cursor")
    , ("dim_foreground", "dim_foreground")
    , ("bright_foreground", "bright_foreground")
    ]

modeColors :: Map.Map (ModeType, ColorName) Text
modeColors =
  Map.fromList
    [ ((Normal, "black"), "color0")
    , ((Normal, "red"), "color1")
    , ((Normal, "green"), "color2")
    , ((Normal, "yellow"), "color3")
    , ((Normal, "blue"), "color4")
    , ((Normal, "magenta"), "color5")
    , ((Normal, "cyan"), "color6")
    , ((Normal, "white"), "color7")
    , ((Bright, "black"), "color8")
    , ((Bright, "red"), "color9")
    , ((Bright, "green"), "color10")
    , ((Bright, "yellow"), "color11")
    , ((Bright, "blue"), "color12")
    , ((Bright, "magenta"), "color13")
    , ((Bright, "cyan"), "color14")
    , ((Bright, "white"), "color15")
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
  Line <$>
  (Text.pack <$> (whiteSpace *> choice (string . Text.unpack <$> colorNames))) <*>
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
    mapColorName mode colorName
      | colorName `elem` nonModeColorNames = Map.lookup colorName nonModeColors
      | otherwise = Map.lookup (mode, colorName) modeColors
    foldFn ::
         (ModeType, Vector (Either TransformExceptionType Text))
      -> Text
      -> (ModeType, Vector (Either TransformExceptionType Text))
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
