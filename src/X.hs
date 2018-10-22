{-# LANGUAGE OverloadedStrings #-}

module X where

import           Lib
import           Data.Text                     as T
import           Data.Map                      as DM
import           Text.Trifecta
import           Text.Parser.LookAhead
import           Data.List                      ( foldr
                                                , sortBy
                                                )

type NameClassPrefix = T.Text
type XColorName = T.Text

data XLine = XLine {
  _leading :: T.Text,
  _color :: T.Text
} deriving (Show, Eq)

x :: App
x = App
  { appName       = "x"
  , configCreator = configCreator' []
  , configPaths   = fmap getConfigPath [".Xresources"]
  }

resourceP :: Parser XColorName
resourceP = T.pack
  <$> choice [string "foreground", string "background", colorWithDigitP]
 where
  colorWithDigitP = do
    _   <- string "color"
    num <- some digit
    return $ "color" ++ num

hexP :: Parser T.Text
hexP = do
  _     <- char '#'
  color <- T.pack <$> some alphaNum
  return $ "#" `T.append` color

colorValueP :: Parser T.Text
colorValueP = choice [hexP, (T.pack <$> manyTill anyChar eof)]

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
  middle  <- T.pack <$> (some $ choice [space, char ':'])
  _       <- colorValueP
  return
    $ XLine (leading `T.append` nc `T.append` color `T.append` middle) color

makeNewLine :: ColorValue -> XLine -> T.Text
makeNewLine c (XLine { _leading = l }) = l `T.append` c

configCreator' :: [NameClassPrefix] -> Theme -> T.Text -> T.Text
configCreator' allowed theme oldConfig =
  T.unlines . Data.List.foldr run [] $ T.lines oldConfig
 where
  getNewValue key = DM.lookup key (colors theme)
  run l ls = case parseText (xLineP allowed) l of
    (Success parsedLine@XLine { _color = c }) -> l' : ls
     where
      l' = case getNewValue c of
        (Just value) -> makeNewLine value parsedLine
        Nothing      -> l
    (Failure _) -> l : ls
