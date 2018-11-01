{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Text.Trifecta                 as Trifecta
import           Data.Text                     as T

parseText :: Parser a -> T.Text -> Trifecta.Result a
parseText p = parseString p mempty . T.unpack

-- colorNP parses colorN (color2, color200, ...)
colorNP :: Parser T.Text
colorNP = do
  _ <- string "color"
  d <- some digit
  return . T.pack $ "color" Prelude.++ d
