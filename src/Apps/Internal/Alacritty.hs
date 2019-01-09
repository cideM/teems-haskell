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
import           Types
import           Types.Internal.Colors       (HexColor (..), RGBA (..))
import qualified Types.Internal.Colors       as Colors
import           Util.Internal

-- | AlacrittyMode exists because Alacritty's config has two color blocks, one
-- for normal colors (0-7) and one for bright colors (color8-15)
data AlacrittyMode
  = Normal
  | Bright
  deriving (Show, Eq)

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
        run (m, ls) line =
          let getVal' = getVal t m
           in case parseText lineP line of
                (Success (ColorName n)) ->
                  (m, ls `Vector.snoc` maybe noColorMsg newLine newVal)
                  where newVal = getVal' n
                        noColorMsg = Left $ missingColor n (name t)
                        newLine = mkLine line
                (Success (Mode m')) -> (m', ls `Vector.snoc` Right line)
                (Failure _) -> (m, ls `Vector.snoc` Right line)

-- | colorNames includes all color names we parse and on which lines we replace
-- the color value with a new one from the theme
colorNames :: [String]
colorNames =
  [ "black"
  , "red"
  , "green"
  , "yellow"
  , "blue"
  , "magenta"
  , "cyan"
  , "white"
  , "background"
  , "foreground"
  ]

-- TODO: This should be a single MapStrict.Map (Text, Mode) Text, not two maps.
-- | Map from non-unique color names from Alacritty's config file to the actual
-- color names from the theme, depending on the color block (= mode) we're in
normal :: MapStrict.Map Text Text
normal =
  MapStrict.fromList
    [ ("background", "background")
    , ("foreground", "foreground")
    , ("black", "color0")
    , ("red", "color1")
    , ("green", "color2")
    , ("yellow", "color3")
    , ("blue", "color4")
    , ("magenta", "color5")
    , ("cyan", "color6")
    , ("white", "color7")
    ]

-- | Same as normal but for bright mode colors
bright :: MapStrict.Map Text Text
bright =
  MapStrict.fromList
    [ ("background", "background")
    , ("foreground", "foreground")
    , ("black", "color8")
    , ("red", "color9")
    , ("green", "color10")
    , ("yellow", "color11")
    , ("blue", "color12")
    , ("magenta", "color13")
    , ("cyan", "color14")
    , ("white", "color15")
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
  (spaces *> choice (string <$> colorNames) <* char ':')

-- | lineTillColorP parses all characters until the start of the color value.
-- This way we can keep user specific indentation when replacing the color
-- value. The alacritty config is a .yaml file. Might have been better to just
-- use an existing library for this. But I have too many other projects right
-- now.
lineTillColorP :: Parser Text
lineTillColorP =
  mkOut <$> many space <*> choice (string <$> colorNames) <*>
  manyTill
    (choice [letter, char ':', space])
    -- Starting to veer into "half baked YAML parser territory". yaml can have
    -- single and double quotes.
    (try ((string "\'" <|> string "\"") *> string "0x"))
  where
    mkOut leading colorName filler =
      let leading' = Text.pack leading
          colorName' = Text.pack colorName
          filler' = Text.pack filler
       in Text.empty <> leading' <> colorName' <> filler'

-- | getVal returns a new color value if one exists in the current theme. Needs
-- to take the current color block (= mode) into account, so the correct map is
-- queried
getVal :: Theme -> AlacrittyMode -> ColorName -> Maybe RGBA
getVal t m n =
  let map' =
        case m of
          Bright -> bright
          Normal -> normal
      colors' = colors t
   in do value <- MapStrict.lookup n map'
         MapStrict.lookup value colors'

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
