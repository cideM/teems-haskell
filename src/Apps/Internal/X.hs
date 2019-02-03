{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.X
  ( x
  ) where

import qualified Apps.Internal.XUtils        as XUtils
import           Types.Internal.Misc

x :: App
x = App "x" (XUtils.transform ["*."]) [".Xresources"]

