{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.XTerm where

import           Apps.Internal.ConfigCreator
import           Apps.Internal.X             as XResources hiding
                                                            (allowedPrefixes)
import           Data.Text                   as Text
import           Types.Internal.Misc

xTerm :: App
xTerm =
  App
    "xterm"
    (configCreator'
       (XResources.xLineP allowedPrefixes)
       (XResources.makeNewLine allowedPrefixes))
    [".Xresources"]

-- | See documentation for X in X.hs
allowedPrefixes :: [Text]
allowedPrefixes = ["XTerm*"]
