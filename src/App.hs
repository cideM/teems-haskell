{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import           Data.Aeson
import           Data.ByteString.Lazy          as BL
import qualified Data.Map                      as Map
import           Data.Text                     as T
import           Data.Text.Encoding            as TEnc
import           Data.Text.IO                  as TIO
import           Data.Traversable
import           Options.Applicative
import           GHC.Generics
import           System.Directory
import           Data.Semigroup                 ( (<>) )
import           System.Environment
import           System.FilePath.Glob
import           System.IO

data App = App
    { appName :: T.Text
    , configName :: T.Text
    } deriving (Show)

data Colors = Colors
    { foreground :: T.Text
    , background :: T.Text
    , color0 :: T.Text
    , color1 :: T.Text
    , color2 :: T.Text
    , color3 :: T.Text
    , color4 :: T.Text
    , color5 :: T.Text
    , color6 :: T.Text
    , color7 :: T.Text
    , color8 :: T.Text
    , color9 :: T.Text
    , color10 :: T.Text
    , color11 :: T.Text
    , color12 :: T.Text
    , color13 :: T.Text
    , color14 :: T.Text
    , color15 :: T.Text
    } deriving (Generic, Show)

instance FromJSON Colors

instance ToJSON Colors

data Theme = Theme
    { name :: T.Text
    , colors :: Colors
    } deriving (Generic, Show)

instance FromJSON Theme

instance ToJSON Theme

type Apps = Map.Map T.Text App

type PathToConfig = String

alacritty :: App
alacritty = App "alacritty" "**/.config/alacritty/alacritty.yml"

apps :: Apps
apps = Map.fromList [(appName alacritty, alacritty)]

getThemes :: PathToConfig -> IO (Either String [Theme])
getThemes fp = do
    contents <- BL.readFile fp
    return $ eitherDecode contents

-- TODO: Vector
getConfigPaths :: [App] -> IO [(App, [FilePath])]
getConfigPaths apps = do
    xdg <- getXdgDirectory XdgConfig ""
    mapM (go xdg) apps
  where
    go dir app = do
        let globPattern = compile . T.unpack $ configName app
        paths <- globDir1 globPattern dir
        return (app, paths)
