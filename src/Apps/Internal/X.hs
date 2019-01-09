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
import           Types.Internal.Colors       (RGBA (..))
import qualified Types.Internal.Colors       as Colors
import           Types.Internal.Misc

-- | Xresources configuration files consist of name class resource declarations.
-- name.Class.resource: value
-- Xcursor.theme: redglass
-- xscreensaver.Dialog.background: #111111
-- The NameClassPrefix is the name.Class. part of such a declaration which, I'm assuming,
-- is always the same text per app. For example, it could be "URxvt.background"
--                                                            ^^^^^ prefix (class is often not used)
type NameClassPrefix = Text

x :: App
x =
  App
    "x"
    (configCreator' (xLineP allowedPrefixes) (makeNewLine allowedPrefixes))
    [".Xresources"]

-- | Matches e.g., *.color0
allowedPrefixes :: [Text]
allowedPrefixes = ["*."]

-- | Parser for the resource part of name.Class.resource
resourceP :: Parser ColorName
resourceP =
  choice
    [ Text.pack <$> string "foreground"
    , Text.pack <$> string "background"
    , colorNP
    ]

-- | Parser for the name.Class part of name.Class.resource
nameClassP ::
     [NameClassPrefix] -- ^ List of prefixes to add to e.g. "color1" or "foreground"
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

-- | Parser for an entire line in Xresources
xLineP :: [NameClassPrefix] -> Parser Text
xLineP allowed = spaces *> nameClassP allowed *> resourceP

-- | Parser for an entire line in Xresources @without@ the color (which will be
-- replaced with a new one)
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

-- | makeNewLine takes and old line and returns a new one. Didn't anticipate
-- that, huh?
makeNewLine :: [NameClassPrefix] -> OldLine -> RGBA -> Either Text NewLine
makeNewLine allowed l rgba =
  case parseText (lineWithoutColorP allowed) l of
    (Success leading) -> Right $ leading <> hexAsText
      where hexAsText = Text.pack . show $ Colors.toHex rgba
    (Failure errInfo) ->
      Left $
      "Failed to parse leading part of old line: " <> Text.pack (show errInfo)
