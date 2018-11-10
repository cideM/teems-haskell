module Parser.Internal where

import           Data.Text                     as T
import           Data.Semigroup
import           Text.Trifecta                 as Trifecta

parseText :: Parser a -> T.Text -> Trifecta.Result a
parseText p = parseString p mempty . T.unpack

-- colorNP parses colorN (color2, color200, ...)
colorNP :: Parser T.Text
colorNP = T.pack . (<>) "color" <$> (string "color" *> some digit)
