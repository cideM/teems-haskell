{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Apps.Alacritty where

import           Data.Text                     as T
import           Data.Text.IO                  as TIO
import           Apps.Util                     as Util
import           Text.RawString.QQ
import           Text.Parser.Combinators
import           Text.Trifecta
import           Control.Applicative

data Mode = Normal | Bright deriving (Show)

type Color = T.Text

type PrecedingWhitespace = T.Text

type ColorAndSpace = (PrecedingWhitespace, Color)

data AlacrittyParseResults = ColorResult ColorAndSpace | ModeResult Mode deriving (Show)

alacritty :: App
alacritty = App
    { appName            = "alacritty"
    , Util.configCreator = Apps.Alacritty.configCreator
    , configPaths        = [makeOsPath "/alacritty/alacritty.yml"]
    }

parseMode :: Parser AlacrittyParseResults
parseMode = try $ do
    skipMany space
    mode <- string "bright:"
    -- TODO: Find a better way of doing this. Surely I don't have to match on
    -- the string I was parsing
    skipMany space
    case mode of
        "bright:" -> return $ ModeResult Bright
        _         -> return $ ModeResult Normal

parseColor :: Parser AlacrittyParseResults
parseColor = try $ do
    spaces <- some space
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
    char ':'
    skipMany space
    return $ ColorResult (T.pack spaces, T.pack color)

ex :: String
ex =
    [r|
      black:   '0x4e4e4e'
      red:     '0xd68787'
      green:   '0x5f865f'
      yellow:  '0xd8af5f'
      blue:    '0x85add4'
      magenta: '0xd7afaf'
      cyan:    '0x87afaf'
      white:   '0xd0d0d0'

# Bright colors
    bright:
      black:   '0x626262'
      red:     '0xd75f87'
      green:   '0x87af87'
      yellow:  '0xffd787'
      blue:    '0xadd4fb'
      magenta: '0xffafaf'
      cyan:    '0x87d7d7'
      white:   '0xe4e4e4'
|]

test = parseString (choice [parseColor]) mempty ex

getThemeColor :: Theme -> T.Text -> Mode -> T.Text
getThemeColor t color m =
    let themeColors = colors t
        color' = case m of
            Normal -> case color of
                "background"   -> Util.color0 themeColors
                "foreground"   -> Util.color15 themeColors
                "black"   -> Util.color0 themeColors
                "red"     -> Util.color1 themeColors
                "green"   -> Util.color2 themeColors
                "yellow"  -> Util.color3 themeColors
                "blue"    -> Util.color4 themeColors
                "magenta" -> Util.color5 themeColors
                "cyan"    -> Util.color6 themeColors
                "white"   -> Util.color7 themeColors
            Bright -> case color of
                "black"   -> Util.color8 themeColors
                "red"     -> Util.color9 themeColors
                "green"   -> Util.color10 themeColors
                "yellow"  -> Util.color11 themeColors
                "blue"    -> Util.color12 themeColors
                "magenta" -> Util.color13 themeColors
                "cyan"    -> Util.color14 themeColors
                "white"   -> Util.color15 themeColors
    in "'0x" `T.append` T.tail color' `T.append` "'"

-- TODO: Vector
-- TODO: The run fn recursively on lines part needs to be extracted into utils
configCreator :: Theme -> T.Text -> Either Util.ParseErr T.Text
configCreator theme config = run theme (T.lines config) [] Normal
  where
    run :: Theme -> [T.Text] -> [T.Text] -> Mode -> Either Util.ParseErr T.Text
    run _ [] output _ = Right $ T.unlines output
    run t input output mode =
        let parser = choice [parseMode, parseColor]
            line   = T.unpack $ Prelude.head input
            result = parseString parser mempty line
            input'  = Prelude.tail input
        in  case result of
                (Success (ModeResult mode)) -> run t input' output' mode
                  where
                    output' = output ++ [Prelude.head input]
                (Success (ColorResult (space, color))) -> run t
                                                     input'
                                                     output'
                                                     mode
                  where
                    output' =
                        output
                            ++ [ space `T.append` color
                                 `T.append` ": "
                                 `T.append` getThemeColor theme color mode
                               ]
                (Failure err) -> run t input' output' mode
                  where
                    output' = output ++ [Prelude.head input]
