{-# LANGUAGE OverloadedStrings #-}

module Alacritty where

import           Lib
import           Data.Text                     as T
import           Data.Map.Strict               as DM
import           Text.Trifecta           hiding ( line )
import           Control.Applicative
import           Data.List                     as DL

type AlacrittyColorName = T.Text
type Config = T.Text

data AlacrittyMode = Normal | Bright deriving (Show, Eq)
data AlacrittyLine = AlacrittyLine {
  _leading  :: T.Text,
  _color    :: AlacrittyColorName,
  _middle   :: T.Text,
  _trailing :: T.Text
} deriving (Show, Eq)
data Alacritty =  Line AlacrittyLine | Mode AlacrittyMode deriving (Show, Eq)

-- instance Eq Alacritty where
--   (==) (color _) (Mode' _) = False
--   (==) (Mode' a) (Mode' b) = a == b
--   (==) (color a) (color b) = a == b
--   (==) (Mode' _) (color _) = False

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
  color   <- T.pack <$> (choice $ string <$> alacrittyColorNames)
  middle  <- T.pack
    <$> manyTill (choice [letter, char ':', space]) (try (string "'0x"))
  -- Skip the actual color we want to replace, as it's discarded anyway
  _        <- Text.Trifecta.count 6 alphaNum
  _        <- char '\''
  trailing <- T.pack <$> manyTill anyChar eof
  return . Line $ AlacrittyLine leading color middle trailing

normal :: DM.Map AlacrittyColorName ColorName
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

bright :: DM.Map AlacrittyColorName ColorName
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

valueFromTheme
  :: Theme -> AlacrittyMode -> AlacrittyColorName -> Maybe ColorValue
valueFromTheme theme mode color =
  let map' = case mode of
        Bright -> bright
        Normal -> normal
      colors' = colors theme
  in  do
        value <- DM.lookup color map'
        DM.lookup value colors'

makeNewline :: ColorName -> AlacrittyLine -> T.Text
makeNewline value (AlacrittyLine leading color middle trailing) =
  leading
    `T.append` color
    `T.append` middle
    `T.append` "'0x"
    `T.append` T.tail value
    `T.append` "'"
    `T.append` trailing

-- TODO Use vector, maybe?
configCreator' :: Theme -> Config -> Config
configCreator' theme config =
  T.unlines . snd $ DL.foldl run (Normal, []) $ T.lines config
 where
  lineP = choice [modeP, colorLineP]
  run (mode, xs) line =
    let getNewValue = valueFromTheme theme mode
    in  case parseText lineP line of
          (Success (Line l@AlacrittyLine { _color = color })) ->
            (mode, xs ++ [line'])
           where
            line' = case getNewValue color of
              (Just value) -> makeNewline value l
              Nothing      -> line
          (Success (Mode m)) -> (m, xs ++ [line])
          (Failure _       ) -> (mode, xs ++ [line])
