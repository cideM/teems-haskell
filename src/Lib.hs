{-# LANGUAGE DeriveGeneric #-}


module Lib where

import           Data.Aeson
import           Data.Text as T
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