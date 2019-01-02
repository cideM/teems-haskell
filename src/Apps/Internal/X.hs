{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.X where

import           Apps.Internal.ConfigCreator
import qualified Data.List                   as List
import qualified Data.Ord                    as Ord
import           Data.Semigroup              ((<>))
import           Data.Text                   as Text
import           Parser.Internal
import           Text.Parser.LookAhead
import           Text.Trifecta
import           Types

type NameClassPrefix = Text

x :: App
x =
  App
    "x"
    (configCreator' (xLineP allowedPrefixes) (makeNewLine allowedPrefixes))
    [".Xresources"]

allowedPrefixes :: [Text]
allowedPrefixes = ["*."]

resourceP :: Parser ColorName
resourceP =
  choice
    [ Text.pack <$> string "foreground"
    , Text.pack <$> string "background"
    , colorNP
    ]

-- Xresources syntax boils down to key: value.
-- key is a "name.class.resource" and value can probably be almost anything
-- The key can also contain wildcards (* and ?).
nameClassP ::
     [NameClassPrefix]
  -- ^^^ List of prefixes to add to e.g. "color1" or "foreground"
  -- Given ["*", "foo."] the parser will look for "*color5" and "foo.color5"
  -- Prefixes are sorted by descending length.
  -> Parser Text
nameClassP prefixes = Text.pack <$> choice ps <* lookAhead resourceP
  where
    ps =
      string <$>
      List.sortBy
        (Ord.comparing $ Ord.Down . List.length)
        (fmap Text.unpack prefixes)

xLineP :: [NameClassPrefix] -> Parser Text
xLineP allowed = spaces *> nameClassP allowed *> resourceP

lineWithoutColorP :: [NameClassPrefix] -> Parser Text
lineWithoutColorP allowed =
  buildOutput <$> many space <*> nameClassP allowed <*> resourceP <*>
  some (choice [space, char ':']) <*
  manyTill anyChar eof
  where
    buildOutput leading nc res filler =
      let leading' = Text.pack leading
          filler' = Text.pack filler
       in Text.empty <> leading' <> nc <> res <> filler'

makeNewLine :: [NameClassPrefix] -> OldLine -> RGBA -> Either Text NewLine
makeNewLine allowed l color =
  case parseText (lineWithoutColorP allowed) l of
    (Success leading) -> Right $ leading <> hexAsText
      where hexAsText = displayHexColor $ rgbaToHexColor color
    (Failure errInfo) ->
      Left $
      "Failed to parse leading part of old line: " <> Text.pack (show errInfo)
