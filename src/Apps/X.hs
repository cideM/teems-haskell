{-# LANGUAGE OverloadedStrings #-}

module Apps.X where

import           Lib
import           Data.Text                     as T
import           Data.Map                      as DM
import           Text.Trifecta
import           Util
import           Colors
import           Data.Vector                   as Vec
import           Text.Parser.LookAhead
import           Data.List                      ( foldr
                                                , sortBy
                                                )

type NameClassPrefix = T.Text

data XLine = XLine {
  _leading :: T.Text,
  _color :: T.Text
} deriving (Show, Eq)

x :: App
x = App
  { appName       = "x"
  , configCreator = configCreator' ["*."]
  , configPaths   = fmap getConfigPath [".Xresources"]
  }

resourceP :: Parser ColorName
resourceP = T.pack
  <$> choice [string "foreground", string "background", colorWithDigitP]
 where
  colorWithDigitP = do
    _   <- string "color"
    num <- some digit
    return $ "color" Prelude.++ num

-- Xresources syntax boils down to key: value.
-- key is a "name.class.resource" and value can probably be almost anything
-- The key can also contain wildcards (* and ?).
nameClassP
  :: [NameClassPrefix]
  -- ^^^ List of prefixes to add to e.g. "color1" or "foreground"
  -- Given ["*", "foo."] the parser will look for "*color5" and "foo.color5"
  -- Prefixes are sorted by descending length.
  -> Parser T.Text
nameClassP prefixes = T.pack <$> choice prefixChoices <* lookAhead resourceP
 where
  prefixes'     = sortBy lengthDesc $ fmap T.unpack prefixes
  prefixChoices = string <$> prefixes'

xLineP :: [NameClassPrefix] -> Parser XLine
xLineP allowed = do
  leading <- T.pack <$> many space
  nc      <- nameClassP allowed
  color   <- resourceP
  middle  <- T.pack <$> some (choice [space, char ':'])
  _       <- manyTill anyChar eof
  return
    $ XLine (leading `T.append` nc `T.append` color `T.append` middle) color

makeNewLine :: RGBA -> XLine -> T.Text
makeNewLine color XLine { _leading = l } = l `T.append` hexAsText
  where hexAsText = displayHexColor $ rgbaToHexColor color


configCreator' :: [NameClassPrefix] -> Theme -> Config -> Either T.Text Config
configCreator' allowedPrefixes theme oldConfig =
  fmap unlinesVec . Vec.sequence . Data.List.foldr run Vec.empty $ T.lines
    oldConfig
 where
  getColorValue key = DM.lookup key (colors theme)
  run l ls = case parseText (xLineP allowedPrefixes) l of
    (Success parsedLine@XLine { _color = colorName }) -> l' `Vec.cons` ls
     where
      l' = case getColorValue colorName of
        (Just val) -> Right $ makeNewLine val parsedLine
        Nothing ->
          Left
            $          "Could not find color "
            `T.append` colorName
            `T.append` " in theme "
            `T.append` name theme
    (Failure _) -> Right l `Vec.cons` ls
