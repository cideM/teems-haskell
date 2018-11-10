{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.Alacritty where

import           Types
import           Util.Internal
import           Data.Functor
import           Data.Text                     as T
import           Data.Map.Strict               as DM
import           Parser.Internal
import           Text.Trifecta           hiding ( line )
import           Apps.Internal.ConfigCreator    ( OldLine
                                                , NewLine
                                                )
import           Control.Applicative
import           Data.Semigroup
import           Data.List                     as DL
import           Data.Vector                   as Vec

data AlacrittyMode = Normal | Bright deriving (Show, Eq)
data Alacritty = ColorName T.Text | Mode AlacrittyMode deriving (Show, Eq)

alacritty :: App
alacritty = App "alacritty" configCreator' ["alacritty/alacritty.yml"]

alacrittyColorNames :: [String]
alacrittyColorNames =
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

normal :: DM.Map T.Text T.Text
normal = DM.fromList
  [ ("background", "background")
  , ("foreground", "foreground")
  , ("black"     , "color0")
  , ("red"       , "color1")
  , ("green"     , "color2")
  , ("yellow"    , "color3")
  , ("blue"      , "color4")
  , ("magenta"   , "color5")
  , ("cyan"      , "color6")
  , ("white"     , "color7")
  ]

bright :: DM.Map T.Text T.Text
bright = DM.fromList
  [ ("background", "background")
  , ("foreground", "foreground")
  , ("black"     , "color8")
  , ("red"       , "color9")
  , ("green"     , "color10")
  , ("yellow"    , "color11")
  , ("blue"      , "color12")
  , ("magenta"   , "color13")
  , ("cyan"      , "color14")
  , ("white"     , "color15")
  ]

modeP :: Parser Alacritty
modeP =
  try
    $   Mode
    <$> (   spaces
        *>  (string "bright:" $> Bright)
        <|> (string "normal:" $> Normal)
        <*  spaces
        )

colorP :: Parser Alacritty
colorP =
  ColorName
    .   T.pack
    <$> (spaces *> choice (string <$> alacrittyColorNames) <* char ':')

lineWithoutColorValueP :: Parser T.Text
lineWithoutColorValueP =
  buildOutput
    <$> many space
    <*> choice (string <$> alacrittyColorNames)
    <*> manyTill (choice [letter, char ':', space]) (try (string "'0x"))
 where
  buildOutput leading colorName filler =
    let leading'   = T.pack leading
        colorName' = T.pack colorName
        filler'    = T.pack filler
    in  T.empty <> leading' <> colorName' <> filler'

getColorValue :: Theme -> AlacrittyMode -> T.Text -> Maybe RGBA
getColorValue theme mode color =
  let map' = case mode of
        Bright -> bright
        Normal -> normal
      colors' = _colors theme
  in  do
        value <- DM.lookup color map'
        DM.lookup value colors'

makeNewline :: OldLine -> RGBA -> Either T.Text NewLine
makeNewline oldLine color = case parseText lineWithoutColorValueP oldLine of
  (Success leading) -> Right $ leading <> newValue
   where
    newValue = "'0x" <> transform color <> "'"
    transform c = let (HexColor (r, g, b)) = rgbaToHexColor c in r <> g <> b
  (Failure _) -> Left "Failed to parse leading part of old oldLine"

unlines' :: Vec.Vector T.Text -> T.Text
unlines' = Vec.foldl T.append T.empty . fmap (<> "\n")

configCreator' :: Theme -> Config -> Either T.Text Config
configCreator' theme config = fmap unlines' . Vec.sequence . snd $ DL.foldl
  run
  (Normal, Vec.empty)
  (T.lines config)
 where
  lineP = choice [modeP, colorP]
  run (m, ls) line =
    let getColorValue' = getColorValue theme m
    in  case parseText lineP line of
          (Success (ColorName n)) ->
            (m, ls `Vec.snoc` maybe noColorMsg newLine newColorValue)
           where
            newColorValue = getColorValue' n
            noColorMsg    = Left $ missingColor n (_name theme)
            newLine       = makeNewline line
          (Success (Mode m')) -> (m', ls `Vec.snoc` Right line)
          (Failure _        ) -> (m, ls `Vec.snoc` Right line)
