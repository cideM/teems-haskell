{-# LANGUAGE OverloadedStrings #-}

module Apps.XTerm where

import Lib
import Apps.X as XResources

xTerm :: App
xTerm = App
  { appName       = "xterm"
  , configCreator = XResources.configCreator' ["XTerm*"]
  , configPaths   = fmap getConfigPath [".Xresources"]
  }
