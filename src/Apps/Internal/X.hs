{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.X
  ( x
  ) where

import           Apps.Internal.MakeTransform
import qualified Apps.Internal.XUtils        as XUtils
import           Data.Text                   as Text
import           Types.Internal.Misc

x :: App
x = App "x" (makeTransform' $ XUtils.makeOptions allowedPrefixes) [".Xresources"]

-- | Matches e.g., *.color0
allowedPrefixes :: [Text]
allowedPrefixes = ["*."]
