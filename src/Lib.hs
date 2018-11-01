{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Data.Aeson
import           Data.Text                     as T
import           System.Directory
import           GHC.Generics
import           Colors
import           Data.Map.Strict               as Map

data App = App
    { appName :: T.Text
    , configCreator :: Theme -> T.Text -> Either T.Text T.Text
    , configPaths :: [IO FilePath]
    }

instance Show App where
    show = show . appName

type ColorName = T.Text

type ThemeName = T.Text

data Theme = Theme
    { name :: ThemeName
    , colors :: Map.Map ColorName RGBA
    } deriving (Generic, Show)

instance FromJSON Theme
instance ToJSON Theme

getConfigPath :: FilePath -> IO FilePath
getConfigPath = getXdgDirectory XdgConfig

lengthDesc :: (Foldable t) => t a -> t a -> Ordering
lengthDesc a b =
  let cmp x y | x < y     = GT
              | x > y     = LT
              | otherwise = EQ
  in  cmp (Prelude.length a) (Prelude.length b)
