{-# LANGUAGE OverloadedStrings #-}

module Util.Internal
  ( missingColor
  )
where

import           Types
import           Data.Semigroup
import Data.Text                     as Text

-- | missingColor just appends some texts to create an error message
missingColor :: ColorName -> ThemeName -> Text
missingColor cName tName =
  "Could not find color " <> cName <> " in theme " <> tName
