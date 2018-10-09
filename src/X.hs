{-# LANGUAGE OverloadedStrings #-}

module X where

import           Lib
import           Data.Text                     as T
import           Data.Map                      as DM
import           Text.Trifecta
import           Control.Applicative
import           Data.List                      ( foldr )

x :: App
x = App
  { appName       = "x"
  , configCreator = configCreator'
  , configPaths   = fmap getConfigPath [".Xresources"]
  }

parseXConfig :: Parser Text
parseXConfig = do
  skipMany $ string "*."
  color <- T.pack <$> choice
    [ string "foreground"
    , string "background"
    , do
      _   <- string "color"
      num <- some digit
      return $ "color" ++ num
    ]
  skipMany $ some space
  _ <- do
    skipMany $ char '#'
    skipMany . some $ letter <|> digit
  return color

configCreator' :: Theme -> T.Text -> T.Text
configCreator' theme oldConfig = T.unlines . Data.List.foldr run [] $ T.lines
  oldConfig
 where
  run currentLine acc
    = let result = parseString parseXConfig mempty $ T.unpack currentLine
      in
        case result of
          (Success colorName) ->
            let newColor' = DM.lookup colorName (colors theme)
            in
              case newColor' of
                (Just newColor) ->
                  let
                    newLine =
                      "*."
                        `T.append` colorName
                        `T.append` ": "
                        `T.append` newColor
                  in  newLine : acc
                Nothing -> currentLine : acc
          (Failure _) -> currentLine : acc
