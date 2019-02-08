module Parser.Internal where

import           Data.Semigroup
import           Data.Text      as Text
import           Text.Trifecta  as Trifecta

-- | parseText is a convenience wrapper around parseString
parseText :: Parser a -> Text -> Trifecta.Result a
parseText p = parseString p mempty . Text.unpack

-- | colorNP parses colorN (color2, color200, ...)
colorNP :: Parser String
colorNP = (<>) "color" <$> (string "color" *> some digit)
