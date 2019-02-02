{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.XTerm
  ( xTerm
  ) where

import           Apps.Internal.MakeTransform
import qualified Apps.Internal.XUtils        as XUtils
import           Data.Text                   as Text
import           Types.Internal.Misc

xTerm :: App
xTerm = App "xterm" (makeTransform' $ XUtils.makeOptions allowedPrefixes) [".Xresources"]

-- | See documentation for X in X.hs
allowedPrefixes :: [Text]
allowedPrefixes = ["XTerm*"]
