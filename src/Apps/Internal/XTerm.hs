{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.XTerm
  ( xTerm
  ) where

import qualified Apps.Internal.XUtils        as XUtils
import           Types.Internal.Misc

-- TODO: Lowercase prefixes and compare to lower case Xresources name
xTerm :: App
xTerm = App "xterm" (XUtils.transform ["XTerm*", "xterm*", "Xterm*"]) [".Xresources"]
