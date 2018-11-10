{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.X where

import           Types
import           Data.Text                     as T
import           Text.Trifecta
import Util.Internal
import           Parser.Internal
import           Apps.Internal.ConfigCreator
import           Data.Semigroup
import           Text.Parser.LookAhead
import           Data.List                      ( sortBy )

type NameClassPrefix = T.Text

x :: App
x = App
  "x"
  (configCreator' (xLineP allowedPrefixes) (makeNewLine allowedPrefixes))
  [".Xresources"]

allowedPrefixes :: [T.Text]
allowedPrefixes = ["*."]

resourceP :: Parser ColorName
resourceP = choice
  [T.pack <$> string "foreground", T.pack <$> string "background", colorNP]

-- Xresources syntax boils down to key: value.
-- key is a "name.class.resource" and value can probably be almost anything
-- The key can also contain wildcards (* and ?).
nameClassP
  :: [NameClassPrefix]
  -- ^^^ List of prefixes to add to e.g. "color1" or "foreground"
  -- Given ["*", "foo."] the parser will look for "*color5" and "foo.color5"
  -- Prefixes are sorted by descending length.
  -> Parser T.Text
nameClassP prefixes = T.pack <$> choice ps <* lookAhead resourceP
  where ps = string <$> sortBy lengthDesc (fmap T.unpack prefixes)

xLineP :: [NameClassPrefix] -> Parser T.Text
xLineP allowed = spaces *> nameClassP allowed *> resourceP

lineWithoutColorP :: [NameClassPrefix] -> Parser T.Text
lineWithoutColorP allowed =
  buildOutput
    <$> many space
    <*> nameClassP allowed
    <*> resourceP
    <*> some (choice [space, char ':'])
    <*  manyTill anyChar eof
 where
  buildOutput leading nc res filler =
    let leading' = T.pack leading
        filler'  = T.pack filler
    in  T.empty <> leading' <> nc <> res <> filler'

makeNewLine :: [NameClassPrefix] -> OldLine -> RGBA -> Either T.Text NewLine
makeNewLine allowed l color = case parseText (lineWithoutColorP allowed) l of
  (Success leading) -> Right $ leading <> hexAsText
    where hexAsText = displayHexColor $ rgbaToHexColor color
  (Failure _) -> Left "Failed to parse leading part of old line"
