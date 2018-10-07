{-# LANGUAGE OverloadedStrings #-}

module Alacritty where

import           Lib
import           Data.Text                     as T
import           Text.Parser.Combinators
import           Data.Map.Strict               as DM
import           Text.Trifecta           hiding ( line )
import           Control.Applicative
import           Data.List                     as DL

data Mode = Normal | Bright deriving (Show, Eq)
type AlacrittyColorName = T.Text
type Config = T.Text
data Alacritty = Color' ColorName | Mode' Mode deriving (Show)

instance Eq Alacritty where
  (==) (Color' _) (Mode' _) = False
  (==) (Mode' a) (Mode' b) = a == b
  (==) (Color' a) (Color' b) = a == b

alacritty :: App
alacritty = App
  { appName       = "alacritty"
  , configCreator = configCreator'
  , configPaths   = fmap getConfigPath ["alacritty/alacritty.yml"]
  }

parseMode :: Parser Alacritty
parseMode = try $ do
  skipMany space
  mode <- string "bright:" <|> string "normal:"
  skipMany space
  case mode of
    "bright:" -> return $ Mode' Bright
    _         -> return $ Mode' Normal

parseColor :: Parser Alacritty
parseColor = try $ do
  skipMany space
  color <- choice
    [ string "black"
    , string "red"
    , string "green"
    , string "yellow"
    , string "blue"
    , string "magenta"
    , string "cyan"
    , string "white"
    , string "background"
    , string "foreground"
    ]
  return . Color' $ T.pack color

parseColorLine :: Parser (T.Text, T.Text)
parseColorLine = do
  leading  <- T.pack <$> manyTill anyChar (try (string "'0x"))
  -- Skip the actual color we want to replace, as it's discarded anyway
  _        <- Text.Trifecta.count 6 alphaNum
  _        <- char '\''
  trailing <- T.pack <$> manyTill anyChar eof
  return (leading, trailing)

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

getThemeColor :: Theme -> AlacrittyColorName -> Mode -> Maybe ColorValue
getThemeColor theme cName mode =
  let map' = case mode of
        Bright -> bright
        _      -> normal
      colors' = colors theme
  in  do
        value <- DM.lookup cName map'
        DM.lookup value colors'

getNewline :: ColorValue -> T.Text -> T.Text
getNewline color old = case parseString parseColorLine mempty $ T.unpack old of
  (Success (leading, trailing)) ->
    leading
      `T.append` "'0x"
      `T.append` T.tail color
      `T.append` "'"
      `T.append` trailing
  (Failure _) -> old

configCreator' :: Theme -> Config -> Config
configCreator' theme config =
  let lines'   = T.lines config
      newLines = DL.foldr run ([], Normal) lines'
  in  T.unlines . fst $ newLines
 where
  parser         = choice [parseMode, parseColor]
  getThemeColor' = getThemeColor theme
  run line (xs, mode) =
    let unchanged = (line : xs, mode)
    in  case parseString parser mempty $ T.unpack line of
          (Success (Mode' newMode)) -> (line : xs, newMode)
          (Success (Color' c)) ->
            let newColor = getThemeColor' c mode
            in  case newColor of
                  (Just c') ->
                    let newLine = getNewline c' line in (newLine : xs, mode)
                  Nothing -> unchanged
          (Failure _) -> unchanged
