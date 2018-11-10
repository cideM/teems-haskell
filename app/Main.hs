{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Types
import           Data.Aeson
import           System.Directory
import           Data.Text.IO                  as TIO
import           Data.ByteString.Lazy          as BL
import           Data.Foldable
import           Data.List                     as DL
import           Apps.Apps
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Control.Monad.IO.Class
import           Control.Exception.Safe

getConfigPath :: FilePath -> IO FilePath
getConfigPath = getXdgDirectory XdgConfig

apps :: [App]
apps = [alacritty, x, xTerm, kitty, termite]

getThemes :: (MonadThrow m, MonadIO m) => FilePath -> m [Theme]
getThemes p = do
  contents <- liftIO $ BL.readFile p
  case eitherDecode contents of
    (Left  _     ) -> throw ThemeDecodeException
    (Right themes) -> return themes

listThemes :: (MonadThrow m, MonadIO m) => FilePath -> m ()
listThemes fp = do
  themes <- getThemes fp
  liftIO $ traverse_ (TIO.putStrLn . _name) themes

activateTheme :: (MonadIO m, MonadThrow m) => Theme -> m ()
activateTheme theme = liftIO $ traverse_ transform apps
 where
  transform app = do
    let transformFn = _configCreator app
    configs <- liftIO . traverse getConfigPath $ _configPaths app
    traverse_ (createNewConfig transformFn) configs
  createNewConfig transformFn path = do
    config <- TIO.readFile path
    case transformFn theme config of
      (Left  err    ) -> throw $ TransformException err
      (Right config') -> TIO.writeFile path config'

findTheme :: (MonadThrow m) => ThemeName -> [Theme] -> m Theme
findTheme tn ts = do
  let result = DL.find ((==) tn . _name) ts
  case result of
    Nothing  -> throw ThemeNotFoundException
    (Just a) -> return a

handleException :: (MonadIO m) => AppException -> m ()
handleException e = liftIO $ TIO.putStrLn msg
 where
  msg = case e of
    ThemeNotFoundException -> "Theme not found"
    ThemeDecodeException   -> "Could not decode config file"
    TransformException err -> "Could transform config: " <> err

parseConfigPath :: Parser ConfigPath
parseConfigPath =
  strOption $ short 'f' <> long "config" <> metavar "CONFIGPATH" <> help
    "Path to config file containing the themes"

parseListThemes :: Parser Command
parseListThemes = pure ListThemes

parseActivateTheme :: Parser Command
parseActivateTheme = ActivateTheme <$> argument str (metavar "THEME NAME")

parseCommands :: Parser Command
parseCommands =
  subparser
    $  (command "list" . info parseListThemes $ progDesc "List all themes")
    <> (command "activate" . info parseActivateTheme $ progDesc
         "Activate a theme"
       )

parseOptions :: Parser CLIOptions
parseOptions = CLIOptions <$> parseConfigPath <*> parseCommands

run :: (MonadThrow m, MonadIO m) => CLIOptions -> m ()
run (CLIOptions path cmd) = case cmd of
  ListThemes                    -> liftIO $ listThemes path

  (ActivateTheme selectedTheme) -> do

    themes <- getThemes path
    theme  <- findTheme selectedTheme themes

    liftIO $ activateTheme theme

    liftIO . TIO.putStrLn $ "Activated " <> selectedTheme

main :: IO ()
main = do
  opts <- execParser
    (info (helper <*> parseOptions)
          (fullDesc <> header "Teems" <> progDesc "Descriptive foo")
    )
  run opts `catch` handleException
