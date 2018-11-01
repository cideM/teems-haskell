{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Text.Trifecta                 as Trifecta
import           Data.Text                     as T
import           Data.Vector                     as Vec

parseText :: Parser a -> T.Text -> Trifecta.Result a
parseText p = parseString p mempty . T.unpack

unlinesVec :: Vec.Vector T.Text -> T.Text
unlinesVec = Vec.foldr (\l ls -> l `T.append` "\n" `T.append` ls) ""
