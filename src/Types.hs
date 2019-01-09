{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import           Control.Exception.Safe
import           Data.Aeson
import           Data.Map.Strict        as Map
import           Data.Text              as Text
import           GHC.Generics
import           Types.Internal.Colors  (RGBA)

type ErrMsg = Text

data AppException
  = ThemeDecodeException Text -- ^ When failing to decode with aeson
  | ThemeNotFoundException -- ^ When theme is not found in config file passed via args
  | TransformException ErrMsg
                       FilePath -- ^ When failing in the transforms function of a particular app (e.g., Apps.Alacritty configCreator)
  deriving (Show)

instance Exception AppException

-- The following types are modelled as outlined in the documentation for
-- optparse-applicative
newtype ListThemesOptions = ListThemesOptions
  { _configPath :: FilePath
  }

data ActivateOptions = ActivateOptions
  { _configPath :: FilePath
  , _themeName  :: ThemeName
  }

data Command
  = ListThemes ListThemesOptions
  | Activate ActivateOptions
  | ListApps

newtype Commands =
  Commands Command

data App = App
      -- | Name of the terminal emulator (e.g., alacritty)
  { _appName       :: Text
      -- | A function that returns a an error message or a new config
  , _configCreator :: Theme -> Text -> Either ErrMsg Text
      -- | Paths where the config file can be found. The string is latter
      -- traversed with a function from System.Directory, hence no IO here.
      -- Existence of config files is checked later on.
  , _configPaths   :: [FilePath]
  }

instance Show App where
  show = show . _appName

type ColorName = Text

type ThemeName = Text

type Config = Text

data Theme = Theme
      -- | Name of the theme. No underscore here since aeson will try to look
      -- for a key named "name", not "_name"
  { name   :: ThemeName
      -- | Map from color name to color value. No underscore here since aeson
      -- will try to look for a key named "colors", not "_colors"
  , colors :: Map.Map ColorName RGBA
  } deriving (Generic, Show, Eq)

instance FromJSON Theme
