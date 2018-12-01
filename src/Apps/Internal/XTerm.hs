{-# LANGUAGE OverloadedStrings #-}

module Apps.Internal.XTerm where

import           Apps.Internal.ConfigCreator
import           Types
import Data.Text                     as Text
import           Apps.Internal.X               as XResources
                                         hiding ( allowedPrefixes )

xTerm :: App
xTerm = App
  "xterm"
  (configCreator' (XResources.xLineP allowedPrefixes)
                  (XResources.makeNewLine allowedPrefixes)
  )
  [".Xresources"]

allowedPrefixes :: [Text]
allowedPrefixes = ["XTerm*"]
