{-# LANGUAGE OverloadedStrings #-}

module Apps.Alacritty where

import           Lib
import           Util
import           Data.Text                     as T
import           Data.Map.Strict               as DM
import           Text.Trifecta
import           Control.Applicative
import           Data.List                     as DL
import           Data.Vector                   as Vec
import           Colors

data AlacrittyMode = Normal | Bright deriving (Show, Eq)
data AlacrittyLine = AlacrittyLine {
  _leading  :: T.Text,
  _color    :: ColorName,
  _middle   :: T.Text,
  _trailing :: T.Text
} deriving (Show, Eq)
data Alacritty =  Line AlacrittyLine | Mode AlacrittyMode deriving (Show, Eq)

alacritty :: App
alacritty = App
  { appName       = "alacritty"
  , configCreator = configCreator'
  , configPaths   = fmap getConfigPath ["alacritty/alacritty.yml"]
  }

modeP :: Parser Alacritty
modeP = try $ do
  skipMany space
  mode <- string "bright:" <|> string "normal:"
  skipMany space
  case mode of
    "bright:" -> return $ Mode Bright
    _         -> return $ Mode Normal

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

colorLineP :: Parser Alacritty
colorLineP = do
  leading <- T.pack <$> many space
  color   <- T.pack <$> choice (string <$> alacrittyColorNames)
  middle  <- T.pack
    <$> manyTill (choice [letter, char ':', space]) (try (string "'0x"))
  -- Skip the actual color we want to replace, as it's discarded anyway
  _        <- Text.Trifecta.count 6 alphaNum
  _        <- char '\''
  trailing <- T.pack <$> manyTill anyChar eof
  return . Line $ AlacrittyLine leading color middle trailing

normal :: DM.Map ColorName ColorName
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

bright :: DM.Map ColorName ColorName
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

getColorValue :: Theme -> AlacrittyMode -> ColorName -> Maybe RGBA
getColorValue theme mode color =
  let map' = case mode of
        Bright -> bright
        Normal -> normal
      colors' = colors theme
  in  do
        value <- DM.lookup color map'
        DM.lookup value colors'

makeNewline :: RGBA -> AlacrittyLine -> T.Text
makeNewline color (AlacrittyLine leading colorName middle trailing) =
  leading
    `T.append` colorName
    `T.append` middle
    `T.append` "'0x"
    `T.append` transform color
    `T.append` "'"
    `T.append` trailing
 where
  transform c =
    let (HexColor (r, g, b)) = rgbaToHexColor c in r `T.append` g `T.append` b

configCreator' :: Theme -> Config -> Either T.Text Config
configCreator' theme config = fmap unlinesVec . Vec.sequence . snd $ DL.foldl
  run
  (Normal, Vec.empty)
  (T.lines config)
 where
  lineP = choice [modeP, colorLineP]
  run (m, ls) l =
    let getColorValue' = getColorValue theme m
    in  case parseText lineP l of
          (Success (Line matchedLine@AlacrittyLine { _color = colorName })) ->
            (m, ls `Vec.snoc` l')
           where
            l' = case getColorValue' colorName of
              (Just cval) -> Right $ makeNewline cval matchedLine
              Nothing ->
                Left
                  $          "Could not find color "
                  `T.append` colorName
                  `T.append` " in theme "
                  `T.append` name theme
          (Success (Mode m')) -> (m', ls `Vec.snoc` Right l)
          (Failure _        ) -> (m, ls `Vec.snoc` Right l)
