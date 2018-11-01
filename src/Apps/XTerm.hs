{-# LANGUAGE OverloadedStrings #-}

module Apps.XTerm where

import           Apps.ConfigCreator
import           Lib
import           Data.Text                     as T
import           Apps.X                        as XResources
                                         hiding ( allowedPrefixes )

xTerm :: App
xTerm = App
  "xterm"
  (configCreator' (XResources.xLineP allowedPrefixes)
                  (XResources.makeNewLine allowedPrefixes)
  )
  (fmap getConfigPath [".Xresources"])

allowedPrefixes :: [T.Text]
allowedPrefixes = ["XTerm*"]
