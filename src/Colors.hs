{-# LANGUAGE OverloadedStrings #-}

module Colors where

import           Data.Aeson
import           Data.Text                     as T
import           Text.Trifecta                 as Trifecta
import           Control.Monad
import           Numeric
import           Util
import           Data.Vector                   as Vector

newtype HexColor = HexColor (T.Text, T.Text, T.Text) deriving (Eq)

instance Show HexColor where
  show = show . T.unpack . displayHexColor

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


hexColorToRGBA :: HexColor -> RGBA
hexColorToRGBA (HexColor (r, g, b)) = RGBA
  (readHex' r, readHex' g, readHex' b, 1.0)
  where readHex' = fst . Prelude.head . readHex . T.unpack

rgbaToHexColor :: RGBA -> HexColor
rgbaToHexColor (RGBA (r, g, b, _)) = HexColor
  (showHex' r, showHex' g, showHex' b)
 where
  showHex' x =
    let s = T.pack $ showHex x ""
    in  if T.length s == 1 then "0" `T.append` s else s

displayHexColor :: HexColor -> T.Text
displayHexColor (HexColor (r, g, b)) =
  "#" `T.append` r `T.append` g `T.append` b

hexP :: Parser HexColor
hexP = do
  _ <- char '#'
  r <- T.pack <$> Trifecta.count 2 hexDigit
  g <- T.pack <$> Trifecta.count 2 hexDigit
  b <- T.pack <$> Trifecta.count 2 hexDigit
  return $ HexColor (r, g, b)
