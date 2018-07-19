{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Text                     as T
import           Data.Map.Strict
import           Data.Text.IO                  as TIO
import           Data.ByteString.Lazy          as BL
import           Data.Foldable
import           Data.List                     as DL
import           Apps.Util                     as Util
import           Apps.Alacritty
import           Apps.X
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           CLI
import           Control.Monad.IO.Class
import           Control.Exception.Safe

data AppException = ThemeDecodeException | ThemeNotFoundException deriving (Show)

instance Exception AppException

apps :: [App]
apps = [alacritty, x]

getThemes :: (MonadThrow m, MonadIO m) => FilePath -> m [Theme']
getThemes p = do
  contents <- liftIO $ BL.readFile p
  case eitherDecode contents of
    (Left  _     ) -> throw ThemeDecodeException
    (Right themes) -> return $ fmap makeTheme' themes
     where
      makeTheme' theme =
        let c  = colors theme
            c' = fromList
              [ ("foreground", foreground c)
              , ("background", background c)
              , ("color0"    , color0 c)
              , ("color1"    , color1 c)
              , ("color2"    , color2 c)
              , ("color3"    , color3 c)
              , ("color4"    , color4 c)
              , ("color5"    , color5 c)
              , ("color6"    , color6 c)
              , ("color7"    , color7 c)
              , ("color8"    , color8 c)
              , ("color9"    , color9 c)
              , ("color10"   , color10 c)
              , ("color11"   , color11 c)
              , ("color12"   , color12 c)
              , ("color13"   , color13 c)
              , ("color14"   , color14 c)
              , ("color15"   , color15 c)
              ]
        in  Theme' {name' = name theme, colors' = c'}

listThemes :: (MonadThrow m, MonadIO m) => FilePath -> m ()
listThemes fp = do
  themes <- getThemes fp
  liftIO $ traverse_ (TIO.putStrLn . name') themes

activateTheme :: (MonadIO m) => Theme' -> m ()
activateTheme theme = liftIO $ traverse_ transform apps
 where
  transform app = do
    let transformFn = Util.configCreator app
    configs <- liftIO . sequence $ configPaths app
    traverse_ (createNewConfig transformFn) configs
  createNewConfig transformFn path = do
    config <- TIO.readFile path
    TIO.writeFile path $ transformFn theme config

findTheme :: (MonadThrow m) => Util.ThemeName -> [Theme'] -> m Theme'
findTheme tn ts = do
  let result = DL.find ((==) tn . name') ts
  case result of
    Nothing  -> throw ThemeNotFoundException
    (Just x) -> return x

handleException :: (MonadIO m) => AppException -> m ()
handleException e = liftIO $ TIO.putStrLn msg
 where
  msg = case e of
    ThemeNotFoundException -> "Theme not found"
    ThemeDecodeException   -> "Could not decode config file"

run :: (MonadThrow m, MonadIO m) => CLIOptions -> m ()
run (CLIOptions path cmd) = case cmd of
  ListThemes                    -> liftIO $ listThemes path

  (ActivateTheme selectedTheme) -> do

    themes <- getThemes path
    theme  <- findTheme (T.pack selectedTheme) themes

    liftIO $ activateTheme theme

    liftIO . TIO.putStrLn $ "Activated " `T.append` T.pack selectedTheme

main :: IO ()
main = do
  opts <- execParser
    (info (helper <*> parseOptions)
          (fullDesc <> header "Teems" <> progDesc "Descriptive foo")
    )
  run opts `catch` handleException
