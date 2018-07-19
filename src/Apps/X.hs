{-# LANGUAGE OverloadedStrings #-}

module Apps.X where

import           Data.Text                     as T
                                         hiding ( foldr )
import           Data.Text.IO                  as TIO
import           Apps.Util                     as Util
import           Text.Parser.Combinators
import           Text.Trifecta
import           Control.Applicative
import           Data.List                      ( foldr )

x :: App
x = App
    { appName            = "x"
    , Util.configCreator = Apps.X.configCreator
    , configPaths        = fmap Util.getConfigPath [".Xresources"]
    }

type Color = T.Text
type ParseResult = (Color, ThemeColorGetter)

type ThemeColorGetter = Colors -> T.Text

parseColor :: Parser (Maybe ParseResult)
parseColor = do
    let xColorParser = string "color" >> some digit
    skipMany $ string "*."
    color <- choice [string "foreground", string "background", xColorParser]
    skipMany $ some space
    return $ case color of
        "foreground" -> Just ("foreground", Util.color15)
        "background" -> Just ("background", Util.color0)
        "0"          -> Just ("color0", Util.color0)
        "1"          -> Just ("color1", Util.color1)
        "2"          -> Just ("color2", Util.color2)
        "3"          -> Just ("color3", Util.color3)
        "4"          -> Just ("color4", Util.color4)
        "5"          -> Just ("color5", Util.color5)
        "6"          -> Just ("color6", Util.color6)
        "7"          -> Just ("color7", Util.color7)
        "8"          -> Just ("color8", Util.color8)
        "9"          -> Just ("color9", Util.color9)
        "10"         -> Just ("color10", Util.color10)
        "11"         -> Just ("color11", Util.color11)
        "12"         -> Just ("color12", Util.color12)
        "13"         -> Just ("color13", Util.color13)
        "14"         -> Just ("color14", Util.color14)
        "15"         -> Just ("color15", Util.color15)
        _            -> Nothing

configCreator :: Theme -> T.Text -> T.Text
configCreator theme oldConfig = T.unlines . foldr run [] $ T.lines oldConfig
  where
    run line acc =
        let result = parseString parseColor mempty $ T.unpack line
        in  case result of
                (Success (Just (colorName, getter))) ->
                    let newColor = getter (colors theme)
                        newLine =
                            "*."
                                `T.append` colorName
                                `T.append` ": "
                                `T.append` newColor
                    in  newLine : acc
                Failure _ -> line : acc
