{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apps.Internal.Termite where

import           Control.Applicative       ((<|>))
import qualified Data.Map                  as Map
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Parser.Internal           (colorNP, parseText)
import           Text.Trifecta
import           Types.Internal.Colors     (RGBA (..))
import           Types.Internal.Exceptions
import           Types.Internal.Misc

termite :: App
termite = App "termite" transform ["termite/config"]

transform :: Theme -> Config -> Either TransformExceptionType Config
transform theme input = fmap Text.unlines . mapM f $ Text.lines input
  where
    tshow :: (Show a) => a -> Text
    tshow = Text.pack . show
    f l =
      case parseText lineParser l of
        Failure _ -> Right l
        Success ParseResult {..} ->
          case Map.lookup _name (colors theme) of
            Nothing -> Left $ ColorNotFound _name
            Just (RGBA r g b a) ->
              let replacement =
                    "rgba(" <> tshow r <> ", " <> tshow g <> ", " <> tshow b <> ", " <>
                    tshow a <> ")"
               in Right $ Text.replace _value replacement l

data ParseResult = ParseResult
  { _name  :: ColorName
  , _value :: Text
  } deriving (Show)

lineParser :: Parser ParseResult
lineParser =
  ParseResult <$> (Text.pack <$> (whiteSpace *> choice [colorNP, colorParser])) <*>
  (Text.pack <$>
   (whiteSpace *> string "=" *> whiteSpace *> (rgbaParser <|> hexParser)))
  where
    rgbaParser =
      string "rgba" *> parens (some $ choice [char ',', char '.', digit, space]) >>= \s ->
        return $ "rgba(" <> s <> ")"
    hexParser = whiteSpace *> count 7 (choice [alphaNum, char '#'])
    colorParser =
      choice
        (string <$>
         [ "foreground"
         , "foreground_bold"
         , "foreground_dim"
         , "background"
         , "cursor"
         ])
