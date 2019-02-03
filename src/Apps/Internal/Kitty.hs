{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apps.Internal.Kitty where

import qualified Data.Map                  as Map
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Parser.Internal           (colorNP, parseText)
import           Text.Trifecta
import           Types.Internal.Colors     (HexColor (..))
import qualified Types.Internal.Colors     as Colors
import           Types.Internal.Exceptions
import           Types.Internal.Misc

kitty :: App
kitty = App "kitty" transform ["kitty/kitty.config"]

transform :: Theme -> Config -> Either TransformExceptionType Config
transform theme input = fmap Text.unlines . mapM f $ Text.lines input
  where
    f l =
      case parseText lineParser l of
        Failure _ -> Right l
        Success ParseResult {..} ->
          case Map.lookup _name (colors theme) of
            Nothing -> Left $ ColorNotFound _name
            Just rgba ->
              let (HexColor r g b) = Colors.toHex rgba
                  replacement = r <> g <> b
               in Right $ Text.replace _value replacement l

data ParseResult = ParseResult
  { _name  :: ColorName
  , _value :: Text
  }

lineParser :: Parser ParseResult
lineParser =
  ParseResult <$> (Text.pack <$> (whiteSpace *> choice [colorNP, kittyColorP])) <*>
  (Text.pack <$>
   (whiteSpace *> string "=" *> whiteSpace *> string "#" *> some alphaNum))
  where
    kittyColorP =
      choice
        (fmap
           string
           [ "foreground"
           , "selection_foreground"
           , "selection_background"
           , "background"
           ])
