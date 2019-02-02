{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Alacritty
  ( alacritty
  ) where

import           Apps.Internal.MakeTransform (MakeTransformOptions (..),
                                              NewLine, OldLine, makeTransform)
import           Control.Applicative
import           Data.Functor
import qualified Data.Map.Strict             as MapStrict
import           Data.Semigroup
import           Data.Text                   as Text
import           Text.Trifecta
import           Types.Internal.Colors       (HexColor (..), RGBA (..))
import qualified Types.Internal.Colors       as Colors
import           Types.Internal.Misc
import           Parser.Internal             (parseText)

-- | AlacrittyMode exists because Alacritty's config has two color blocks, one
-- for normal colors (0-7) and one for bright colors (color8-15)
data AlacrittyMode
  = Normal
  | Bright
  deriving (Show, Eq, Ord)

alacritty :: App
alacritty = App "alacritty" (makeTransform options) ["alacritty/alacritty.yml"]

options :: MakeTransformOptions AlacrittyMode
options =
  MakeTransformOptions
    { _shouldTransformLine = shouldTransform
    , _getColorName = getColorName
    , _getNewLine = const mkLine
    , _getNewState = getNewState
    , _initialState = Normal
    }
  where
    getNewState mode l =
      case parseText modeP l of
        Success newMode -> newMode
        _               -> mode
    shouldTransform _ l =
      case parseText lineTillColorP l of
        Success _ -> True
        _         -> False
    getColorName mode l =
      case parseText colorP l of
        Success color -> getVal mode color
        _             -> Nothing

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

nonModeColors :: MapStrict.Map ColorName Text
nonModeColors =
  MapStrict.fromList
    [ ("background", "background")
    , ("foreground", "foreground")
    , ("text", "text")
    , ("cursor", "cursor")
    , ("dim_foreground", "dim_foreground")
    , ("bright_foreground", "bright_foreground")
    ]

modeColors :: MapStrict.Map (AlacrittyMode, ColorName) Text
modeColors =
  MapStrict.fromList
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
modeP :: Parser AlacrittyMode
modeP =
  try
    (spaces *> (string "bright:" $> Bright) <|>
     (string "normal:" $> Normal) <* spaces)

-- | colorP parses the name of a color (e.g., black)
colorP :: Parser ColorName
colorP =
  Text.pack <$>
  (spaces *> choice (string . Text.unpack <$> colorNames) <* char ':')

-- | lineTillColorP parses all characters until the start of the color value.
-- This way we can keep user specific indentation when replacing the color
-- value. The alacritty config is a .yaml file. Might have been better to just
-- use an existing library for this. But I have too many other projects right
-- now.
lineTillColorP :: Parser Text
lineTillColorP =
  mkOut <$> (Text.pack <$> many space) <*>
  (Text.pack <$> choice (string . Text.unpack <$> colorNames)) <*>
  (Text.pack <$>
   manyTill
     (choice [letter, char ':', space])
    -- Starting to veer into "half baked YAML parser territory". yaml can have
    -- single and double quotes.
     (try ((string "\'" <|> string "\"") *> string "0x")))
  where
    mkOut leading colorName filler =
      Text.empty <> leading <> colorName <> filler

-- | getVal returns a new color value if one exists in the current theme. Needs
-- to take the current color block (= mode) into account, so the correct map is
-- queried
getVal :: AlacrittyMode -> ColorName -> Maybe ColorName
getVal m n
  | n `elem` nonModeColorNames = MapStrict.lookup n nonModeColors
  | otherwise = MapStrict.lookup (m, n) modeColors

-- | mkLine creates a new line with which we can replace the old line in the
-- config file
mkLine :: OldLine -> RGBA -> Either ErrMsg NewLine
mkLine l rgba =
  case parseText lineTillColorP l of
    (Success leading) -> Right $ leading <> newVal
      where (HexColor r g b) = Colors.toHex rgba
            newVal = "'0x" <> r <> g <> b <> "'"
    (Failure errInfo) ->
      Left $
      "Failed to parse leading part of old line: " <> Text.pack (show errInfo)
