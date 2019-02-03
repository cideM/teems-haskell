module Types.Internal.Exceptions where

import           Control.Exception.Safe
import           Data.Text              (Text)

newtype TransformExceptionType =
  ColorNotFound Text deriving (Show, Eq)

-- | Umbrella data type for all possible exceptions reported to users
data AppException
  = ThemeDecodeException Text -- ^ When failing to decode with aeson
  | ThemeNotFoundException -- ^ When theme is not found in config file passed via args
  | TransformException TransformExceptionType
                       FilePath -- ^ Error during transformation of a config
  deriving (Show)

instance Exception AppException
