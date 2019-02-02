{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apps.Internal.MakeTransform where

import qualified Data.List             as List
import qualified Data.Map              as Map
import           Data.Semigroup        ((<>))
import           Data.Text             as Text
import qualified Data.Vector           as Vector
import           Text.Trifecta
import           Types.Internal.Colors (RGBA)
import           Types.Internal.Misc

-- | LineParser is a function that should successfully parse a line that the app
-- can transform
type LineParser = Parser Text

type OldLine = Text

type NewLine = Text

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

data MakeTransformOptions' = MakeTransformOptions'
  { _shouldTransformLine' :: OldLine -> Bool
  , _getColorName'        :: OldLine -> Maybe ColorName
  , _getNewLine'          :: OldLine -> RGBA -> Either ErrMsg NewLine
  }

makeTransform' ::
     MakeTransformOptions' -> Theme -> Config -> Either ErrMsg Config
makeTransform' MakeTransformOptions' {..} =
  let state = mempty
   in makeTransform $
      MakeTransformOptions
        (const _shouldTransformLine')
        (const _getColorName')
        (const _getNewLine')
        (\_ s -> s)
        state

data MakeTransformOptions s = MakeTransformOptions
  { _shouldTransformLine :: s -> OldLine -> Bool
  , _getColorName        :: s -> OldLine -> Maybe ColorName
  , _getNewLine          :: s -> OldLine -> RGBA -> Either ErrMsg NewLine
  , _getNewState         :: s -> OldLine -> s
  , _initialState        :: s
  }

makeTransform ::
     MakeTransformOptions s -> Theme -> Config -> Either ErrMsg Config
makeTransform MakeTransformOptions {..} theme input =
  fmap Text.unlines .
  sequence .
  Vector.toList . snd . List.foldl' foldFn (_initialState, Vector.empty) $
  Text.lines input
  where
    lookupColor = flip Map.lookup (colors theme)
    foldFn (s, acc) currentLine =
      let s' = _getNewState s currentLine
       in (,) s' . Vector.snoc acc $
          if _shouldTransformLine s' currentLine
            then case _getColorName s' currentLine of
                   Nothing -> Left "Could not extract color name from line"
                   Just colorName ->
                     case lookupColor colorName of
                       Nothing ->
                         Left $ "Color " <> tshow colorName <> " not found"
                         -- ^ The JS version just logs this to the console and keeps the old line
                       Just newColorValue ->
                         _getNewLine s' currentLine newColorValue
            else Right currentLine
