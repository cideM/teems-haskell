{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apps.Internal.XUtils where

import qualified Data.List                 as List
import qualified Data.Map                  as Map
import qualified Data.Ord                  as Ord
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Parser.Internal           (colorNP, parseText)
import           Text.Trifecta
import           Types.Internal.Colors     (HexColor (..))
import qualified Types.Internal.Colors     as Colors
import           Types.Internal.Exceptions
import           Types.Internal.Misc

-- | Xresources configuration files consist of name class resource declarations.
-- name.Class.resource: value
-- Xcursor.theme: redglass
-- xscreensaver.Dialog.background: #111111
-- The Prefix is the name.Class. part of such a declaration which, I'm assuming,
-- is always the same text per app.
type Prefix = Text

data ParseResult = ParseResult
  { _name  :: ColorName
  , _value :: Text -- ^ By keeping the value we can later just do Text.replace
  }

lineParser :: [Prefix] -> Parser ParseResult
lineParser prefixes =
  ParseResult <$>
  (Text.pack <$>
   (whiteSpace *> choice (string <$> prefixes') *>
    choice [string "foreground", string "background", colorNP]) <* string ":") <*>
  (Text.pack <$> (whiteSpace *> string "#" *> count 6 anyChar))
  where
    prefixes' =
      List.sortBy
        (Ord.comparing $ Ord.Down . List.length)
        (fmap Text.unpack prefixes)

transform :: [Prefix] -> Theme -> Config -> Either TransformExceptionType Config
transform prefixes theme input = fmap Text.unlines . mapM f $ Text.lines input
  where
    f l =
      case parseText (lineParser prefixes) l of
        Failure _ -> Right l
        Success ParseResult {..} ->
          case Map.lookup _name (colors theme) of
            Nothing -> Left $ ColorNotFound _name
            Just rgba ->
              let (HexColor r g b) = Colors.toHex rgba
                  replacement = r <> g <> b
               in Right $ Text.replace _value replacement l
