{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import           Data.Aeson
import           Data.Semigroup
import           Numeric
import           Parser.Internal
import           Control.Monad
import           Text.Trifecta                 as Trifecta
import           Data.Vector                   as Vector
import           Data.Text                     as T
import           GHC.Generics
import           Control.Exception.Safe
import           Data.Map.Strict               as Map

type ErrMsg = T.Text

data AppException = ThemeDecodeException T.Text | ThemeNotFoundException | TransformException ErrMsg FilePath deriving (Show)
instance Exception AppException

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
    { _appName :: T.Text
    , _configCreator :: Theme -> T.Text -> Either T.Text T.Text
    , _configPaths :: [FilePath]
    }

instance Show App where
    show = show . _appName

type ColorName = T.Text

type ThemeName = T.Text

type Config = T.Text

data Theme = Theme
    { name :: ThemeName
    -- ^^^ No underscore here since aeson will try to look for a key named
    -- "name", not "_name"
    , colors :: Map.Map ColorName RGBA
    -- ^^^ No underscore here since aeson will try to look for a key named
    -- "colors", not "_colors"
    } deriving (Generic, Show)

instance FromJSON Theme
instance ToJSON Theme

newtype HexColor = HexColor (T.Text, T.Text, T.Text) deriving (Eq)

displayHexColor :: HexColor -> T.Text
displayHexColor (HexColor (r, g, b)) = "#" <> r <> g <> b

instance Show HexColor where
  show = show . T.unpack . displayHexColor

hexP :: Parser HexColor
hexP = do
  _ <- char '#'
  r <- T.pack <$> Trifecta.count 2 hexDigit
  g <- T.pack <$> Trifecta.count 2 hexDigit
  b <- T.pack <$> Trifecta.count 2 hexDigit
  return $ HexColor (r, g, b)

hexColorToRGBA :: HexColor -> RGBA
hexColorToRGBA (HexColor (r, g, b)) = RGBA
  (readHex' r, readHex' g, readHex' b, 1.0)
  where readHex' = fst . Prelude.head . readHex . T.unpack

rgbaToHexColor :: RGBA -> HexColor
rgbaToHexColor (RGBA (r, g, b, _)) = HexColor
  (showHex' r, showHex' g, showHex' b)
 where
  showHex' x =
    let s = T.pack $ showHex x "" in if T.length s == 1 then "0" <> s else s

newtype RGBA = RGBA (Int, Int, Int, Double) deriving (Show, Eq)

instance FromJSON RGBA where
  parseJSON (Array v)
      | Vector.length v == 4 = do
          r <- parseJSON $ v Vector.! 0
          g <- parseJSON $ v Vector.! 1
          b <- parseJSON $ v Vector.! 2
          a <- parseJSON $ v Vector.! 3
          return $ RGBA (r, g, b, a)
      | otherwise = mzero
  parseJSON (String s) = case parseText hexP s of
    (Trifecta.Success c) -> return $ hexColorToRGBA c
    (Failure _) -> fail "Failed to parse hex color"
  parseJSON _ = mzero

instance ToJSON RGBA where
  toJSON (RGBA (r, g, b, a)) =
    let
        -- Type signature is needed otherwise it's too constrained
        -- for the double at the end
        makeString :: (Show a) => a -> Value
        makeString = String . T.pack . show
        r' = makeString r
        g' = makeString g
        b' = makeString b
        a' = makeString a
    in Array $ Vector.singleton r' `Vector.snoc` g' `Vector.snoc` b' `Vector.snoc` a'
