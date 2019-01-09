{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Internal.Colors where

import qualified Control.Monad    as Monad
import qualified Data.Aeson       as Aeson
import           Data.Aeson.Types (FromJSON, Parser, Value (..))
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Maybe       as Maybe
import qualified Data.Scientific  as Scientific
import           Data.Semigroup   ((<>))
import           Data.Text        (Text)
import qualified Data.Text        as Text
import qualified Data.Vector      as Vector
import           GHC.Generics     (Generic)
import qualified Numeric

type R = Text

type G = Text

type B = Text

type Alpha = Text

data HexColor =
  HexColor R
           G
           B
  deriving (Eq, Generic)

instance Show HexColor where
  show (HexColor r g b) = show . Text.unpack $ "#" <> r <> g <> b

-- | R G B Alpha channel
data RGBA =
  RGBA !Int
       !Int
       !Int
       !Double
  deriving (Show, Eq, Generic)

-- | The only constraint for internal color values is that they can be converted
-- to rgba and hex colors
class ColorValue a where
  toRGBA :: a -> RGBA
  toHex :: a -> HexColor

instance ColorValue HexColor where
  toRGBA (HexColor r g b) = RGBA (fromHex r) (fromHex g) (fromHex b) 1.0
    where
      fromHex = fst . head . Numeric.readHex . Text.unpack
  toHex = id

instance ColorValue RGBA where
  toRGBA = id
  toHex (RGBA r g b _) = HexColor (showHex' r) (showHex' g) (showHex' b)
    where
      showHex' x =
        let s = Text.pack $ Numeric.showHex x ""
         in if Text.length s == 1
              then "0" <> s
              else s

isScientific :: Value -> Bool
isScientific (Number _) = True
isScientific _          = False

parseFromRGBA :: Value -> Parser RGBA
parseFromRGBA =
  Aeson.withArray "Failed to parse list of RGBA values" $ \values -> do
    Monad.guard $ all isScientific values
    let [r, g, b, a] = Vector.toList $ Vector.map (\(Number x) -> x) values
    return $ RGBA (toInt r) (toInt g) (toInt b) (toFloat a)
  where
    toInt = Maybe.fromJust . Scientific.toBoundedInteger
    toFloat = Scientific.toRealFloat

parseFromHex :: Value -> Parser RGBA
parseFromHex =
  Aeson.withText "Failed to parse Hex color string" $ \text ->
    case Text.length text of
      4 ->
        Monad.fail
          "Hex values must have 6 chars, not 3 (#FFFFFF instead of #FFF)"
      7 ->
        let [r, g, b] = Text.chunksOf 2 $ Text.tail text
         in return $ RGBA (readHex' r) (readHex' g) (readHex' b) 1.0
      _ -> Monad.fail "Hex values must have 6 chars (#FFFFFF)"
  where
    readHex' = fst . head . Numeric.readHex . Text.unpack

instance FromJSON RGBA where
  parseJSON text@(String _) = parseFromHex text
  parseJSON list@(Array _) = parseFromRGBA list
  parseJSON invalid =
    AesonTypes.typeMismatch
      "Invalid data type for color value. Only hex (#FFFFFF) and rgba ([255, 255, 255, 1.0]) are allowed"
      invalid
