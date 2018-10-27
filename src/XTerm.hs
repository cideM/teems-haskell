{-# LANGUAGE OverloadedStrings #-}

module XTerm where

import Lib
import X as XResources

xTerm :: App
xTerm = App
  { appName       = "xterm"
  , configCreator = XResources.configCreator' ["XTerm*"]
  , configPaths   = fmap getConfigPath [".Xresources"]
  }
