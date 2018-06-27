{-# LANGUAGE OverloadedStrings #-}

module CLI where

import           Data.Semigroup                 ( (<>) )
import           Options.Applicative

type ThemeName = String

data Command = ListThemes | ActivateTheme ThemeName

type ConfigPath = String

data CLIOptions = CLIOptions ConfigPath Command

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
