{-# LANGUAGE OverloadedStrings #-}

module Apps.Kitty where

import           Lib
import           Data.Text                     as T
import           Data.Map                      as DM
import           Text.Trifecta
import           Util
import           Colors
import           Data.Vector                   as Vec

kitty :: App
kitty = App
  { appName       = "kitty"
  , configCreator = configCreator'
  , configPaths   = fmap getConfigPath ["kitty/kitty.config"]
  }

-- Excluding colorN (color0, color100,...)
kittyColorNames :: [String]
kittyColorNames =
  ["foreground", "selection_foreground", "selection_background", "background"]

configCreator' :: Theme -> Config -> Either T.Text Config
configCreator' theme config = fmap unlinesVec . Vec.sequence $ Vec.foldr
  run
  Vec.empty
  (Vec.fromList $ T.lines config)
 where
  getColorValue key = DM.lookup key (colors theme)
  run l ls = case parseText (colorNameP kittyColorNames) l of
    (Success colorName) -> l' `Vec.cons` ls
     where
      l' = case getColorValue colorName of
        (Just (RGBA (r, g, b, a))) ->
          let rgbaText =
                "rgba("
                  `T.append` (T.pack . show $ r)
                  `T.append` ", "
                  `T.append` (T.pack . show $ g)
                  `T.append` ", "
                  `T.append` (T.pack . show $ b)
                  `T.append` ", "
                  `T.append` (T.pack . show $ a)
                  `T.append` ")"
          in  Right $ colorName `T.append` " = " `T.append` rgbaText
        Nothing ->
          Left
            $          "Could not find color "
            `T.append` colorName
            `T.append` " in theme "
            `T.append` name theme
    (Failure _) -> Right l `Vec.cons` ls
