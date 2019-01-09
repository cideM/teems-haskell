{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Types
Description : Types used across the entire application
Copyright   : (c) Florian Beeres, 2018
License     : GPL-3
Maintainer  : yuuki@protonmail.com
-}
module Types where

import           Control.Exception.Safe
import           Data.Aeson
import           Data.Map.Strict        as Map
import           Data.Text              as Text
import           GHC.Generics
import           Types.Internal.Colors  (RGBA)

type ErrMsg = Text -- ^ Internal error message format

-- | Umbrella data type for all possible exceptions reported to users
data AppException
  = ThemeDecodeException Text -- ^ When failing to decode with aeson
  | ThemeNotFoundException -- ^ When theme is not found in config file passed via args
  | TransformException ErrMsg
                       FilePath -- ^ Error during transformation of a config
  deriving (Show)

instance Exception AppException

-- | The following types are modelled as outlined in the documentation for
-- optparse-applicative
newtype ListThemesOptions = ListThemesOptions
  { _configPath :: FilePath -- ^ Path to configuration file
  }

data ActivateOptions = ActivateOptions
  { _configPath :: FilePath -- ^ Path to configuration file
  , _themeName  :: ThemeName -- ^ Name of theme to activate
  }

-- | Available CLI commands of this app
data Command
  = ListThemes ListThemesOptions
  | Activate ActivateOptions
  | ListApps

newtype Commands =
  Commands Command

-- | Internal representation of a supported app (terminal emulator)
data App = App
  { _appName       :: Text -- ^ Name of the terminal emulator (e.g., alacritty)
  , _configCreator :: Theme -> Text -> Either ErrMsg Text -- ^ A function that returns a an error message or a new config
  , _configPaths   :: [FilePath] -- ^ Paths where the config file can be found. The string is latter
                                 -- traversed with a function from System.Directory, hence no IO here.
                                 -- Existence of config files is checked later on.
  }

instance Show App where
  show = show . _appName

type ColorName = Text

type ThemeName = Text

type Config = Text

data Theme = Theme
  { name   :: ThemeName -- ^ Name of the theme. No underscore here since aeson will try to look for a key named "name", not "_name"
  , colors :: Map.Map ColorName RGBA -- ^ Map from color name to color value. No underscore here since aeson
                                     -- will try to look for a key named "colors", not "_colors"
  } deriving (Generic, Show, Eq)

instance FromJSON Theme
