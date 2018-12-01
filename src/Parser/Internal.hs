module Parser.Internal where

import Data.Text                     as Text
import           Data.Semigroup
import           Text.Trifecta                 as Trifecta

-- | parseText is a convenience wrapper around parseString
parseText :: Parser a -> Text -> Trifecta.Result a
parseText p = parseString p mempty . Text.unpack

-- | colorNP parses colorN (color2, color200, ...)
colorNP :: Parser Text
colorNP = Text.pack . (<>) "color" <$> (string "color" *> some digit)
