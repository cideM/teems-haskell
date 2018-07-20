{-# LANGUAGE OverloadedStrings #-}

module Apps.Alacritty where

import           Data.Text                     as T
import           Apps.Util                     as Util
import           Text.Parser.Combinators
import           Data.Map.Strict               as DM
import           Text.Trifecta
import           Data.List                      ( foldr )

data Mode = Normal | Bright deriving (Show)

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
    , configPaths        = fmap Util.getConfigPath ["alacritty/alacritty.yml"]
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
    _       <- char ':'
    spaces2 <- some space
    return . ColorResult $ ParsedColor
        { leadingSpace = T.pack spaces1
        , colorValue   = T.pack color
        , spaceBetween = T.pack spaces2
        }

getThemeColor :: Theme' -> T.Text -> Mode -> Maybe T.Text
getThemeColor theme color mode = do
    let c = colors' theme
    newColor <- case mode of
        -- put into separate map
        Normal -> case color of
            "background" -> DM.lookup "background" c
            "foreground" -> DM.lookup "foreground" c
            "black"      -> DM.lookup "color0" c
            "red"        -> DM.lookup "color1" c
            "green"      -> DM.lookup "color2" c
            "yellow"     -> DM.lookup "color3" c
            "blue"       -> DM.lookup "color4" c
            "magenta"    -> DM.lookup "color5" c
            "cyan"       -> DM.lookup "color6" c
            "white"      -> DM.lookup "color7" c
        Bright -> case color of
            "background" -> DM.lookup "color0" c
            "foreground" -> DM.lookup "color15" c
            "black"      -> DM.lookup "color8" c
            "red"        -> DM.lookup "color9" c
            "green"      -> DM.lookup "color10" c
            "yellow"     -> DM.lookup "color11" c
            "blue"       -> DM.lookup "color12" c
            "magenta"    -> DM.lookup "color13" c
            "cyan"       -> DM.lookup "color14" c
            "white"      -> DM.lookup "color15" c
    return $ "'0x" `T.append` T.tail newColor `T.append` "'" -- Transform to alacritty format '0xFFFFFF'

configCreator :: Theme' -> T.Text -> T.Text
configCreator theme config =
    T.unlines . fst . Data.List.foldr run ([], Normal) $ T.lines config
  where
    run currentLine (xs, mode) =
        let parser         = choice [parseMode, parseColor]
            result         = parseString parser mempty $ T.unpack currentLine
            getThemeColor' = getThemeColor theme
        in  case result of
                -- Keep track of whether we're parsing normal or bright colors
                (Success (ModeResult newMode)) -> (currentLine : xs, newMode)
                (Success (ColorResult parsedColor)) -> (newLine : xs, mode)
                  where
                    newLine =
                        let newColor' =
                                getThemeColor' (colorValue parsedColor) mode
                        in  case newColor' of
                                (Just newColor) ->
                                    -- Keep whitespace until first character because
                                    -- indentation matters in yaml
                                    leadingSpace parsedColor
                                        `T.append` colorValue parsedColor
                                        `T.append` ": "
                                        `T.append` spaceBetween parsedColor
                                        `T.append` newColor
                                Nothing -> currentLine
                (Failure _) -> (currentLine : xs, mode)
