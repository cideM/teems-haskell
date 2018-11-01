{-# LANGUAGE OverloadedStrings #-}

module Apps.X where

import           Lib
import           Data.Text                     as T
import           Text.Trifecta
import           Util
import           Colors
import           Apps.ConfigCreator
import           Text.Parser.LookAhead
import           Data.List                      ( sortBy )

type NameClassPrefix = T.Text

x :: App
x = App
  { appName       = "x"
  , configCreator = configCreator' (xLineP allowedPrefixes)
                                   (makeNewLine allowedPrefixes)
  , configPaths   = fmap getConfigPath [".Xresources"]
  }

allowedPrefixes :: [T.Text]
allowedPrefixes = ["*."]

resourceP :: Parser ColorName
resourceP =
  T.pack <$> choice [string "foreground", string "background", colorNP]

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

xLineP :: [NameClassPrefix] -> Parser String
xLineP allowed = T.unpack <$> (spaces *> nameClassP allowed *> resourceP)

lineWithoutColorP :: [NameClassPrefix] -> Parser String
lineWithoutColorP allowed = do
  leading <- T.pack <$> many space
  nc      <- nameClassP allowed
  color   <- resourceP
  middle  <- T.pack <$> some (choice [space, char ':'])
  _       <- manyTill anyChar eof
  return . T.unpack $ leading `T.append` nc `T.append` color `T.append` middle

makeNewLine :: [NameClassPrefix] -> RGBA -> OldLine -> Either T.Text NewLine
makeNewLine allowed color l = case parseText (lineWithoutColorP allowed) l of
  (Success leading) -> Right $ T.pack leading `T.append` hexAsText
    where hexAsText = displayHexColor $ rgbaToHexColor color
  (Failure _) -> Left "Failed to parse leading part of old line"
