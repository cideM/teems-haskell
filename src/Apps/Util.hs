{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Apps.Util where

import           Data.Aeson
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
import           System.IO

type ParseErr = T.Text

data App = App
    { appName :: T.Text
    , configCreator :: Theme -> T.Text -> T.Text
    , configPaths :: [IO FilePath]
    }

instance Show App where
    show = show . appName

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

makeOsPath :: FilePath -> IO FilePath
makeOsPath p = do
    configDir <- getXdgDirectory XdgConfig ""
    return $ configDir ++ p