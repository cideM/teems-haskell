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
  ColorName . T.pack <$> (spaces *> choice (string <$> colorNames) <* char ':')

lineTillColorP :: Parser T.Text
lineTillColorP =
  mkOut <$> many space <*> choice (string <$> colorNames) <*> manyTill
    (choice [letter, char ':', space])
    (try (string "'0x"))
 where
  mkOut leading colorName filler =
    let leading'   = T.pack leading
        colorName' = T.pack colorName
        filler'    = T.pack filler
    in  T.empty <> leading' <> colorName' <> filler'

getVal :: Theme -> AlacrittyMode -> ColorName -> Maybe RGBA
getVal t m n =
  let map' = case m of
        Bright -> bright
        Normal -> normal
      colors' = colors t
  in  do
        value <- DM.lookup n map'
        DM.lookup value colors'

mkLine :: OldLine -> RGBA -> Either T.Text NewLine
mkLine l c = case parseText lineTillColorP l of
  (Success leading) -> Right $ leading <> newVal
   where
    (HexColor (r, g, b)) = rgbaToHexColor c
    newVal               = "'0x" <> r <> g <> b <> "'"
  (Failure errInfo) ->
    Left $ "Failed to parse leading part of old line: " <> T.pack
      (show errInfo)

unlines' :: Vec.Vector T.Text -> T.Text
unlines' = Vec.foldl T.append T.empty . fmap (<> "\n")

configCreator' :: Theme -> Config -> Either T.Text Config
configCreator' t conf = fmap
  unlines'
  (Vec.sequence . snd $ DL.foldl run (Normal, Vec.empty) (T.lines conf))
 where
  lineP = choice [modeP, colorP]
  run (m, ls) line =
    let getVal' = getVal t m
    in  case parseText lineP line of
          (Success (ColorName n)) ->
            (m, ls `Vec.snoc` maybe noColorMsg newLine newVal)
           where
            newVal     = getVal' n
            noColorMsg = Left $ missingColor n (name t)
            newLine    = mkLine line
          (Success (Mode m')) -> (m', ls `Vec.snoc` Right line)
          (Failure _        ) -> (m, ls `Vec.snoc` Right line)
