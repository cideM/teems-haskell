{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import           Types
import           Data.Aeson
import           System.Directory
import           Data.Text                     as T
import           Data.Text.IO                  as TIO
import           Data.ByteString.Lazy          as BL
import           Data.Foldable
import           Data.List                     as DL
import           Apps.Apps
import           Control.Monad
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
    (Left  err   ) -> throw . ThemeDecodeException $ T.pack err
    (Right themes) -> return themes

listApps :: (MonadThrow m, MonadIO m) => m ()
listApps = traverse_ (liftIO . TIO.putStrLn . _appName) apps

listThemes :: (MonadThrow m, MonadIO m) => FilePath -> m ()
listThemes fp = getThemes fp >>= liftIO . traverse_ (TIO.putStrLn . name)

activateTheme :: (MonadIO m, MonadThrow m) => Theme -> m ()
activateTheme theme = liftIO $ traverse_ transform apps
 where
  transform app =
    let maker = _configCreator app
        fps   = _configPaths app
    in  liftIO
        $   traverse getConfigPath fps
        >>= filterM doesFileExist
        >>= traverse_ (mkConfig maker)
  mkConfig f fp = do
    conf <- TIO.readFile fp
    case f theme conf of
      (Left  err  ) -> throw $ TransformException err fp
      (Right conf') -> TIO.writeFile fp conf'

findTheme :: (MonadThrow m) => ThemeName -> [Theme] -> m Theme
findTheme tn ts = do
  let result = DL.find ((==) tn . name) ts
  case result of
    Nothing  -> throw ThemeNotFoundException
    (Just a) -> return a

handleException :: (MonadIO m) => AppException -> m ()
handleException e = liftIO $ TIO.putStrLn msg
 where
  msg = case e of
    ThemeNotFoundException   -> "Theme not found"
    ThemeDecodeException err -> "Could not decode config file: " <> err
    TransformException err fp ->
      "Could not transform " <> T.pack fp <> "\n" <> err

configPathP :: Parser FilePath
configPathP =
  strOption $ short 'c' <> long "config" <> metavar "CONFIG PATH" <> help
    "Path to config file containing the themes"

listAppsP :: Parser Command
listAppsP = pure ListApps

listThemesP :: Parser Command
listThemesP = ListThemes . ListThemesOptions <$> configPathP

activateP :: Parser Command
activateP = Activate <$> (ActivateOptions <$> configPathP <*> themeNameP)
 where
  themeNameP = strOption
    (short 't' <> long "theme" <> metavar "THEME NAME" <> help
      "Theme to activate"
    )

cmdP :: Parser Command
cmdP =
  subparser
    $  (command "list-themes" . info listThemesP $ progDesc "List all themes")
    <> (command "list-apps" . info listAppsP $ progDesc
         "List all supported apps"
       )
    <> (command "activate" . info activateP $ progDesc "Activate a theme")

parseOptions :: Parser Commands
parseOptions = Commands <$> cmdP

cPathListThemes :: ListThemesOptions -> FilePath
cPathListThemes = _configPath

cPathActivateThemes :: ActivateOptions -> FilePath
cPathActivateThemes = _configPath

run :: (MonadThrow m, MonadIO m) => Commands -> m ()
run (Commands cmd) = case cmd of
  ListThemes opts -> liftIO . listThemes $ cPathListThemes opts

  ListApps        -> liftIO listApps

  Activate opts   -> do
    let n = _themeName opts

    themes <- getThemes $ cPathActivateThemes opts
    theme  <- findTheme n themes

    liftIO $ activateTheme theme

    liftIO . TIO.putStrLn $ "Activated " <> n

main :: IO ()
main = do
  opts <- execParser
    (info
      (helper <*> parseOptions)
      (fullDesc <> header "Teems" <> progDesc
        "Terminal emulator theme management made easy!"
      )
    )
  run opts `catch` handleException
