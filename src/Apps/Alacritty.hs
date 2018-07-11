{-# LANGUAGE OverloadedStrings #-}

module Apps.Alacritty where

import           Data.Text                     as T
                                         hiding ( foldr )
import           Data.Text.IO                  as TIO
import           Apps.Util                     as Util
import           Text.Parser.Combinators
import           Text.Trifecta
import           Control.Applicative
import           Data.List                      ( foldr )

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
    color  <- choice
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

getThemeColor :: Theme -> T.Text -> Mode -> T.Text
getThemeColor t color m =
    let themeColors = colors t
        color'      = case m of
            Normal -> case color of
                "background" -> Util.color0 themeColors
                "foreground" -> Util.color15 themeColors
                "black"      -> Util.color0 themeColors
                "red"        -> Util.color1 themeColors
                "green"      -> Util.color2 themeColors
                "yellow"     -> Util.color3 themeColors
                "blue"       -> Util.color4 themeColors
                "magenta"    -> Util.color5 themeColors
                "cyan"       -> Util.color6 themeColors
                "white"      -> Util.color7 themeColors
            Bright -> case color of
                "background" -> Util.color0 themeColors
                "foreground" -> Util.color15 themeColors
                "black"      -> Util.color8 themeColors
                "red"        -> Util.color9 themeColors
                "green"      -> Util.color10 themeColors
                "yellow"     -> Util.color11 themeColors
                "blue"       -> Util.color12 themeColors
                "magenta"    -> Util.color13 themeColors
                "cyan"       -> Util.color14 themeColors
                "white"      -> Util.color15 themeColors
    in  "'0x" `T.append` T.tail color' `T.append` "'"

-- TODO: Vector
configCreator :: Theme -> T.Text -> T.Text
configCreator theme config = T.unlines . fst . foldr run ([], Normal) $ T.lines
    config
  where
    -- Keep track of whether we're parsing normal or bright colors
    run line (xs, mode) =
        let parser = choice [parseMode, parseColor]
            result = parseString parser mempty $ T.unpack line
        in  case result of
                (Success (ModeResult  newMode       )) -> (line : xs, newMode)
                (Success (ColorResult (space, color))) -> (newLine : xs, mode)
                  where
                    newLine =
                        -- Keep whitespace until first character because
                        -- indentation matters in yaml
                        space
                            `T.append` color
                            `T.append` ": "
                            `T.append` getThemeColor theme color mode
                (Failure err) -> (line : xs, mode)
