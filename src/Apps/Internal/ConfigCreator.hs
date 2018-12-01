module Apps.Internal.ConfigCreator where

import           Types
import qualified Data.Text                     as Text
import qualified Data.Map                      as Map
import           Text.Trifecta
import           Parser.Internal
import           Util.Internal

type LineParser = Parser Text.Text

type OldLine = Text.Text
type NewLine = Text.Text

-- | configCreator' contains code shared by several apps. It transforms a config
-- with the colors from the given theme and returns either the new config or an
-- error message.
--
-- This isn't tested as it only makes sense in the context of those functions.
-- That's probably a very bad practice but whatever. It's tested in
-- KittySpec.hs, XSpec.hs etc.
configCreator'
  :: LineParser
  -- ^^^ Checks if a line has a valid color name and returns it for lookup of a new value.
  -> (OldLine -> RGBA -> Either ErrMsg NewLine)
  -- ^^^ Transforms the old line into a new line with the given color value
  -> Theme
  -- ^^^ Map of color name to RGBA color value
  -> Config
  -- ^^^ Config of a terminal emulator (e.g., contents of kitty.config)
  -> Either ErrMsg Config
configCreator' lineP mkLine t conf = fmap Text.unlines
                                          (traverse f $ Text.lines conf)
 where
  getVal key = Map.lookup key (colors t)
  f :: OldLine -> Either ErrMsg NewLine
  f curr = case parseText lineP curr of
    (Success colorName) -> maybe noColorMsg newLine newVal
     where
      noColorMsg = Left $ missingColor colorName (name t)
      newLine    = mkLine curr
      newVal     = getVal colorName
    (Failure _) -> Right curr
