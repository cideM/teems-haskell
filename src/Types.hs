{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import           Data.Aeson
import           Data.Semigroup
import           Numeric
import           Data.Text                     as T
import           GHC.Generics
import           Control.Exception.Safe
import           Data.Map.Strict               as Map

type ErrMsg = T.Text

data AppException
  = ThemeDecodeException T.Text -- ^ When failing to decode with aeson
  | ThemeNotFoundException -- ^ When theme is not found in config file passed via args
  | TransformException ErrMsg FilePath -- ^ When failing in the transforms function of a particular app (e.g., Apps.Alacritty configCreator)
  deriving (Show)

instance Exception AppException

-- The following types are modelled as outlined in the documentation for
-- optparse-applicative

newtype ListThemesOptions = ListThemesOptions {
  _configPath :: FilePath
}

data ActivateOptions = ActivateOptions {
  _configPath :: FilePath,
  _themeName :: ThemeName
}

data Command = ListThemes ListThemesOptions | Activate ActivateOptions | ListApps

newtype Commands = Commands Command

data App = App
    { -- | Name of the terminal emulator (e.g., alacritty)
      _appName :: T.Text
      -- | A function that returns a an error message or a new config
    , _configCreator :: Theme -> T.Text -> Either ErrMsg T.Text
      -- | Paths where the config file can be found. The string is latter
      -- traversed with a function from System.Directory, hence no IO here.
      -- Existence of config files is checked later on.
    , _configPaths :: [FilePath]
    }

instance Show App where
    show = show . _appName

type ColorName = T.Text

type ThemeName = T.Text

type Config = T.Text

data Theme = Theme
    {
      -- | Name of the theme. No underscore here since aeson will try to look
      -- for a key named "name", not "_name"
      name :: ThemeName
      -- | Map from color name to color value. No underscore here since aeson
      -- will try to look for a key named "colors", not "_colors"
    , colors :: Map.Map ColorName RGBA
    } deriving (Generic, Show, Eq)

instance FromJSON Theme
instance ToJSON Theme

newtype HexColor = HexColor (T.Text, T.Text, T.Text) deriving (Eq, Generic)

displayHexColor :: HexColor -> T.Text
displayHexColor (HexColor (r, g, b)) = "#" <> r <> g <> b

instance Show HexColor where
  show = show . T.unpack . displayHexColor

rgbaToHexColor :: RGBA -> HexColor
rgbaToHexColor (RGBA (r, g, b, _)) = HexColor
  (showHex' r, showHex' g, showHex' b)
 where
  showHex' x =
    let s = T.pack $ showHex x "" in if T.length s == 1 then "0" <> s else s

newtype RGBA = RGBA (Int, Int, Int, Double) deriving (Show, Eq, Generic)

instance FromJSON RGBA
instance ToJSON RGBA
