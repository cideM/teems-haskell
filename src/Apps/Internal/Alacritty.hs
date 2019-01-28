{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Alacritty where

import           Apps.Internal.ConfigCreator (NewLine, OldLine)
import           Control.Applicative
import           Data.Functor
import qualified Data.List                   as List
import qualified Data.Map.Strict             as MapStrict
import           Data.Semigroup
import           Data.Text                   as Text
import qualified Data.Vector                 as Vector
import           Parser.Internal
import           Text.Trifecta               hiding (line)
import           Types.Internal.Colors       (HexColor (..), RGBA (..))
import qualified Types.Internal.Colors       as Colors
import           Types.Internal.Misc
import           Util.Internal

-- | AlacrittyMode exists because Alacritty's config has two color blocks, one
-- for normal colors (0-7) and one for bright colors (color8-15)
data AlacrittyMode
  = Normal
  | Bright
  deriving (Show, Eq, Ord)

-- | Alacritty is the parse result of a line in the Alacritty config.
-- It can either be a color name (e.g., black), or a mode (see above)
data Alacritty
  = ColorName Text
  | Mode AlacrittyMode
  deriving (Show, Eq)

alacritty :: App
alacritty = App "alacritty" configCreator' ["alacritty/alacritty.yml"]
  where
    lineP = choice [modeP, colorP]
    configCreator' t conf =
      fmap
        unlines'
        (Vector.sequence . snd $
         List.foldl run (Normal, Vector.empty) (Text.lines conf))
      where
        run (m, ls) line
          -- cursor: can be both a heading or the start of the cursor:
          -- '0x111111' color declaration ~_~
          | strip line == "cursor:" = (m, ls `Vector.snoc` Right line)
          | otherwise =
            let getVal' = getVal t m
             in case parseText lineP line of
                  (Success (ColorName n)) ->
                    (m, ls `Vector.snoc` maybe noColorMsg newLine newVal)
                    where newVal = getVal' n
                          noColorMsg = Left $ missingColor n (name t)
                          newLine = mkLine line
                  (Success (Mode m')) -> (m', ls `Vector.snoc` Right line)
                  (Failure _) -> (m, ls `Vector.snoc` Right line)

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
modeP :: Parser Alacritty
modeP =
  try $
  Mode <$>
  (spaces *> (string "bright:" $> Bright) <|>
   (string "normal:" $> Normal) <* spaces)

-- | colorP parses the name of a color (e.g., black)
colorP :: Parser Alacritty
colorP =
  ColorName . Text.pack <$>
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
getVal :: Theme -> AlacrittyMode -> ColorName -> Maybe RGBA
getVal t m n
  | n `elem` nonModeColorNames =
    MapStrict.lookup n nonModeColors >>= flip MapStrict.lookup (colors t)
  | otherwise =
    MapStrict.lookup (m, n) modeColors >>= flip MapStrict.lookup (colors t)

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

unlines' :: Vector.Vector Text -> Text
unlines' = Vector.foldl Text.append Text.empty . fmap (<> "\n")
