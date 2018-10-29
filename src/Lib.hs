{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Lib where

import           Data.Aeson
import           Data.Text                     as T
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

data RGBAColor = RGBAColor Int Int Int Float deriving (Show, Eq)

newtype HexColor = HexColor T.Text deriving (Show, Eq)

data ColorValue = ColorValue {
  hex :: HexColor,
  rgba :: RGBAColor
} deriving (Generic, Show, Eq)

rgbaP :: Parser RGBAColor
rgbaP = do
  _  <- string "rgba("
  xs <- Text.Trifecta.count 3 (some digit <* char ',' <* many space)
  a  <- read <$> some (choice [digit, char '.']) <* char ')'
  let [r, g, b] = read <$> xs
  return $ RGBAColor r g b a

hexP :: Parser HexColor
hexP = do
  _     <- char '#'
  color <- T.pack <$> some hexDigit
  return $ HexColor color

instance FromJSON RGBAColor where
  parseJSON = withText "rgba color" $ \x ->
    case parseText rgbaP x of
      (Text.Trifecta.Success a) -> return a
      (Failure _) -> fail "rgba color"

showText :: Show a => a -> Text
showText = pack . show

instance ToJSON RGBAColor where
  toJSON (RGBAColor r g b a) =
    String $ "rgba("
      `T.append` showText r
      `T.append` ","
      `T.append` showText g
      `T.append` ","
      `T.append` showText b
      `T.append` ","
      `T.append` showText a
      `T.append` ")"

instance FromJSON HexColor where
  parseJSON = withText "hex color" $ \x ->
    case parseText hexP x of
      (Text.Trifecta.Success a) -> return a
      (Failure _) -> fail "hex color"

instance ToJSON HexColor where
  toJSON (HexColor x) =
    String $ "#"
      `T.append` showText x

instance FromJSON ColorValue
instance ToJSON ColorValue

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
