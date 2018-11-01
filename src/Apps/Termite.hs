{-# LANGUAGE OverloadedStrings #-}

module Apps.Termite where

import           Lib
import           Data.Text                     as T
import           Data.Map                      as DM
import           Text.Trifecta
import           Util
import           Colors
import           Data.Vector                   as Vec

data XLine = XLine {
  _leading :: T.Text,
  _color :: T.Text
} deriving (Show, Eq)

termite :: App
termite = App
  { appName       = "termite"
  , configCreator = configCreator'
  , configPaths   = fmap getConfigPath ["termite/config"]
  }

-- Excluding colorN (color0, color100,...)
termiteColorNames :: [String]
termiteColorNames =
  ["foreground", "foreground_bold", "foreground_dim", "background", "cursor"]

colorNP :: Parser String
colorNP = do
  _ <- string "color"
  d <- some digit
  return $ "color" Prelude.++ d

colorNameP :: Parser Text
colorNameP =
  fmap T.pack $ spaces *> choice (colorNP : fmap string termiteColorNames)

configCreator' :: Theme -> Config -> Either T.Text Config
configCreator' theme config = fmap unlinesVec . Vec.sequence $ Vec.foldr
  run
  Vec.empty
  (Vec.fromList $ T.lines config)
 where
  getColorValue key = DM.lookup key (colors theme)
  run l ls = case parseText colorNameP l of
    (Success colorName) -> l' `Vec.cons` ls
     where
      l' = case getColorValue colorName of
        (Just (RGBA (r, g, b, a))) ->
          let rgbaText =
                "rgba("
                  `T.append` (T.pack . show $ r)
                  `T.append` ", " `T.append` (T.pack . show $ g)
                  `T.append` ", " `T.append` (T.pack . show $ b)
                  `T.append` ", " `T.append` (T.pack . show $ a)
                  `T.append` ")"
          in  Right $ colorName `T.append` " = " `T.append` rgbaText
        Nothing ->
          Left
            $          "Could not find color "
            `T.append` colorName
            `T.append` " in theme "
            `T.append` name theme
    (Failure _) -> Right l `Vec.cons` ls
