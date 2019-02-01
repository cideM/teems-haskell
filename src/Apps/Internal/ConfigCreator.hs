{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.ConfigCreator where

import           Control.Monad.State   (State, evalState, execState, get, put,
                                        runState)
import qualified Data.Map              as Map
import           Data.Text             as Text
import           Parser.Internal
import           Text.Trifecta
import           Types.Internal.Colors (ColorValue)
import           Types.Internal.Colors (RGBA)
import           Types.Internal.Misc
import           Util.Internal

-- | LineParser is a function that should successfully parse a line that the app
-- can transform
type LineParser = Parser Text

type OldLine = Text

type NewLine = Text

-- | configCreator' contains code shared by several apps. It transforms a config
-- with the colors from the given theme and returns either the new config or an
-- error message. It goes over the old config line by line. If a line matches
-- the LineParser, that line (with some other stuff, see below) is given to the
-- line transform function. This isn't tested as it only makes sense in the
-- context of those functions. That's probably a very bad practice but whatever.
-- It's tested in KittySpec.hs, XSpec.hs etc.
configCreator' ::
     LineParser
  -- ^^^ Checks if a line has a valid color name and returns it for lookup of a new value.
  -> (OldLine -> RGBA -> Either ErrMsg NewLine)
  -- ^^^ Transforms the old line into a new line with the given color value
  -> Theme
  -- ^^^ Map of color name to RGBA color value
  -> Config
  -- ^^^ Config of a terminal emulator (e.g., contents of kitty.config)
  -> Either ErrMsg Config
configCreator' lineP mkLine t conf =
  fmap Text.unlines (traverse modifyLine $ Text.lines conf)
  where
    getVal key = Map.lookup key (colors t)
    modifyLine :: OldLine -> Either ErrMsg NewLine
    modifyLine curr =
      case parseText lineP curr of
        (Success colorName) -> maybe noColorMsg newLine newVal
          where noColorMsg = Left $ missingColor colorName (name t)
                newLine = mkLine curr
                newVal = getVal colorName
        (Failure _) -> Right curr

transform ::
  (OldLine -> s -> Bool) -- ^ shouldTransformLine
  -> (OldLine -> s -> Maybe ColorName) -- ^ getColorName
  -> (OldLine -> RGBA -> s -> Maybe NewLine) -- ^ getNewLine
  -> (OldLine -> s -> s) -- ^ getNewState
  -> s -- ^ initialState
  -> Theme
  -> Config
  -> Either ErrMsg Config
transform shouldTransformLine getColorName getNewLine getNewState initialState theme input =
  fmap Text.unlines . sequence $
  evalState (mapM stateFn (Text.lines input)) initialState
  where
    stateFn line = do
      state <- get
      let newState = getNewState line state -- ^ Make new state
      put newState
      if shouldTransformLine line newState
        then case getColorName line newState >>= flip Map.lookup (colors theme) of
               Nothing -> return $ Left "No color name bla" -- ^ The JS version just logs this to the console and keeps the old line
               Just newColorValue ->
                 case getNewLine line newColorValue newState of
                   Nothing      -> return $ Left "No color name bla"
                   Just newLine -> return $ Right newLine
        else return $ Right line
