{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Apps.Apps
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy      as ByteStringLazy
import           Data.Foldable
import           Data.List                 as List
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as Text
import           Data.Text.IO              as TextIO
import           Options.Applicative
import           System.Directory
import           Types.Internal.Exceptions
import           Types.Internal.Misc

newtype Commands =
  Commands Command

-- | The following types are modelled as outlined in the documentation for
-- optparse-applicative
newtype ListThemesOptions = ListThemesOptions
  { _configPath :: FilePath -- ^ Path to configuration file
  }

data ActivateOptions = ActivateOptions
  { _configPath :: FilePath -- ^ Path to configuration file
  , _themeName  :: ThemeName -- ^ Name of theme to activate
  }

-- | Available CLI commands of this app
data Command
  = ListThemes ListThemesOptions
  | Activate ActivateOptions
  | ListApps

getConfigPath :: FilePath -> IO FilePath
getConfigPath = getXdgDirectory XdgConfig

apps :: [App]
apps = [alacritty, x, xTerm, kitty, termite]

getThemes :: (MonadThrow m, MonadIO m) => FilePath -> m [Theme]
getThemes p = do
  contents <- liftIO $ ByteStringLazy.readFile p
  case eitherDecode contents of
    (Left err)     -> throw . ThemeDecodeException $ Text.pack err
    (Right themes) -> return themes

listApps :: (MonadThrow m, MonadIO m) => m ()
listApps = traverse_ (liftIO . TextIO.putStrLn . _appName) apps

listThemes :: (MonadThrow m, MonadIO m) => FilePath -> m ()
listThemes fp = getThemes fp >>= liftIO . traverse_ (TextIO.putStrLn . name)

activateTheme :: (MonadIO m, MonadThrow m) => Theme -> m ()
activateTheme theme = liftIO $ traverse_ transform apps
  where
    transform app =
      let maker = _configCreator app
          fps = _configPaths app
       in liftIO $
          TextIO.putStrLn (_appName app) >> traverse getConfigPath fps >>=
          filterM doesFileExist >>=
          traverse_ (mkConfig maker)
    mkConfig f fp = do
      conf <- TextIO.readFile fp
      case f theme conf of
        (Left err)    -> throw $ TransformException err fp
        (Right conf') -> TextIO.writeFile fp conf'

findTheme :: (MonadThrow m) => ThemeName -> [Theme] -> m Theme
findTheme tn ts = do
  let result = List.find ((==) tn . name) ts
  case result of
    Nothing  -> throw ThemeNotFoundException
    (Just a) -> return a

handleException :: (MonadIO m) => AppException -> m ()
handleException e = liftIO $ TextIO.putStrLn ("\t" <> msg)
  where
    msg =
      case e of
        ThemeNotFoundException -> "Theme not found"
        ThemeDecodeException err -> "Could not decode config file: " <> err
        TransformException err _ ->
          case err of
            ColorNotFound colorName ->
              "Color " <> colorName <> " not found in theme"

configPathP :: Parser FilePath
configPathP =
  strOption $
  short 'c' <> long "config" <> metavar "CONFIG PATH" <>
  help "Path to config file containing the themes"

listAppsP :: Parser Command
listAppsP = pure ListApps

listThemesP :: Parser Command
listThemesP = ListThemes . ListThemesOptions <$> configPathP

activateP :: Parser Command
activateP = Activate <$> (ActivateOptions <$> configPathP <*> themeNameP)
  where
    themeNameP =
      strOption
        (short 't' <> long "theme" <> metavar "THEME NAME" <>
         help "Theme to activate")

cmdP :: Parser Command
cmdP =
  subparser $
  (command "list-themes" . info listThemesP $ progDesc "List all themes") <>
  (command "list-apps" . info listAppsP $ progDesc "List all supported apps") <>
  (command "activate" . info activateP $ progDesc "Activate a theme")

parseOptions :: Parser Commands
parseOptions = Commands <$> cmdP

cPathListThemes :: ListThemesOptions -> FilePath
cPathListThemes = _configPath

cPathActivateThemes :: ActivateOptions -> FilePath
cPathActivateThemes = _configPath

run :: (MonadThrow m, MonadIO m) => Commands -> m ()
run (Commands cmd) =
  case cmd of
    ListThemes opts -> liftIO . listThemes $ cPathListThemes opts
    ListApps -> liftIO listApps
    Activate opts -> do
      let n = _themeName opts
      themes <- getThemes $ cPathActivateThemes opts
      theme <- findTheme n themes
      liftIO $ activateTheme theme
      liftIO . TextIO.putStrLn $ "Activated " <> n

main :: IO ()
main = do
  opts <-
    execParser
      (info
         (helper <*> parseOptions)
         (fullDesc <> header "Teems" <>
          progDesc "Terminal emulator theme management made easy!"))
  run opts `catch` handleException
