{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Text.Trifecta                 as Trifecta
import           Data.Text                     as T
import           Data.Vector                     as Vec

parseText :: Parser a -> T.Text -> Trifecta.Result a
parseText p = parseString p mempty . T.unpack

unlinesVec :: Vec.Vector T.Text -> T.Text
unlinesVec = Vec.foldr (\l ls -> l `T.append` "\n" `T.append` ls) ""

-- colorNP parses colorN (color2, color200, ...)
colorNP :: Parser String
colorNP = do
  _ <- string "color"
  d <- some digit
  return $ "color" Prelude.++ d

-- colorNameP uses colorNP and also parses certain app specific names, which can
-- be passed as an argument. Check Termite.hs or Kitty.hs for a usage example.
colorNameP :: [String] -> Parser Text
colorNameP specialNames =
  fmap T.pack $ spaces *> choice (colorNP : fmap string specialNames)
