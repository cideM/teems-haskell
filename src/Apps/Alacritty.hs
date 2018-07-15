{-# LANGUAGE OverloadedStrings #-}

module Apps.Alacritty where

import           Data.Text                     as T
                                         hiding ( foldr )
import           Apps.Util                     as Util
import           Text.Parser.Combinators
import           Text.Trifecta
import           Data.List                      ( foldr )

data Mode = Normal | Bright deriving (Show)

type Color = T.Text

type Whitespace = T.Text

data ParsedColor = ParsedColor {
    leadingSpace :: Whitespace,
    spaceBetween :: Whitespace,
    colorValue :: Color
} deriving (Show)

data AlacrittyParseResults = ColorResult ParsedColor | ModeResult Mode deriving (Show)

alacritty :: App
alacritty = App
    { appName            = "alacritty"
    , Util.configCreator = Apps.Alacritty.configCreator
    , configPaths        = fmap getConfigPath ["alacritty/alacritty.yml"]
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
    spaces1 <- some space
    color   <- choice
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
    _ <- char ':'
    spaces2 <- some space
    return . ColorResult $ ParsedColor
        { leadingSpace = T.pack spaces1
        , colorValue   = T.pack color
        , spaceBetween = T.pack spaces2
        }

-- TODO: Throw on unknown color
getThemeColor :: Theme -> T.Text -> Mode -> T.Text
getThemeColor theme color mode =
    let themeColors = colors theme
        getter      = case mode of
            Normal -> case color of
                "background" -> Util.color0
                "foreground" -> Util.color15
                "black"      -> Util.color0
                "red"        -> Util.color1
                "green"      -> Util.color2
                "yellow"     -> Util.color3
                "blue"       -> Util.color4
                "magenta"    -> Util.color5
                "cyan"       -> Util.color6
                "white"      -> Util.color7
            Bright -> case color of
                "background" -> Util.color0
                "foreground" -> Util.color15
                "black"      -> Util.color8
                "red"        -> Util.color9
                "green"      -> Util.color10
                "yellow"     -> Util.color11
                "blue"       -> Util.color12
                "magenta"    -> Util.color13
                "cyan"       -> Util.color14
                "white"      -> Util.color15
                        -- Transform to alacritty format '0xFFFFFF'
    in  "'0x" `T.append` T.tail (getter themeColors) `T.append` "'"

-- TODO: Vector
configCreator :: Theme -> T.Text -> T.Text
configCreator theme config = T.unlines . fst . foldr run ([], Normal) $ T.lines
    config
  where
    run currentLine (xs, mode) =
        let parser         = choice [parseMode, parseColor]
            result         = parseString parser mempty $ T.unpack currentLine
            getThemeColor' = getThemeColor theme
        in  case result of
                -- Keep track of whether we're parsing normal or bright colors
                (Success (ModeResult  newMode    )) -> (currentLine : xs, newMode)
                (Success (ColorResult parsedColor)) -> (newLine : xs, mode)
                  where
                    newLine =
                        -- Keep whitespace until first character because
                        -- indentation matters in yaml
                        leadingSpace parsedColor
                            `T.append` colorValue parsedColor
                            `T.append` ": "
                            `T.append` spaceBetween parsedColor
                            `T.append` getThemeColor'
                                           (colorValue parsedColor)
                                           mode
                (Failure _) -> (currentLine : xs, mode)
