{-# LANGUAGE DeriveGeneric #-}


module Lib where

import           Data.Aeson
import           Data.Text as T
import           Text.Trifecta
import           System.Directory
import           GHC.Generics
import           Data.Map.Strict

data App = App
    { appName :: T.Text
    , configCreator :: Theme -> T.Text -> T.Text
    , configPaths :: [IO FilePath]
    }

instance Show App where
    show = show . appName

type ColorValue = T.Text

type ColorName = T.Text

type ThemeName = T.Text

data Theme = Theme
    { name :: ThemeName
    , colors :: Map ColorName ColorValue
    } deriving (Generic, Show)

instance FromJSON Theme

instance ToJSON Theme

getConfigPath :: FilePath -> IO FilePath
getConfigPath = getXdgDirectory XdgConfig

parseText :: Parser a -> T.Text -> Text.Trifecta.Result a
parseText p = parseString p mempty . T.unpack

lengthDesc :: (Foldable t) => t a -> t a -> Ordering
lengthDesc a b =
  let cmp x y | x < y     = GT
              | x > y     = LT
              | otherwise = EQ
  in  cmp (Prelude.length a) (Prelude.length b)

