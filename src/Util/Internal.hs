{-# LANGUAGE OverloadedStrings #-}

module Util.Internal
  ( missingColor
  )
where

import           Types
import           Data.Semigroup
import qualified Data.Text                     as Text

-- | missingColor just appends some texts to create an error message
missingColor :: ColorName -> ThemeName -> Text.Text
missingColor cName tName =
  "Could not find color " <> cName <> " in theme " <> tName
