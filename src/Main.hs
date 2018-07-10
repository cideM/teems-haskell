{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                     as T
import           Data.Text.IO                  as TIO
import           Data.Traversable              as TR
import           Data.List                     as DL
import           App
import           Options.Applicative
import           System.Directory
import           Data.Semigroup                 ( (<>) )
import           CLI
import           Control.Monad.IO.Class

run :: CLIOptions -> IO ()
run (CLIOptions path cmd) = case cmd of
  ListThemes -> do

    themes <- getThemes path

    case themes of
      (Left  msg   ) -> TIO.putStrLn $ T.pack msg
      (Right themes) -> mapM_ (TIO.putStrLn . name) themes

  (ActivateTheme theme) -> do

    themes' <- getThemes path

    case themes' of
      (Left  msg   ) -> TIO.putStrLn $ T.pack msg
      (Right themes) -> do

        let t' = DL.find ((==) theme . T.unpack . name) themes

        case t' of
          Nothing  -> TIO.putStrLn . T.pack $ "Could not find " ++ theme
          (Just t) -> do
            configs <- sequence $ configPaths alacritty
            -- TODO: Remove this is only for logging
            mapM_ (TIO.putStrLn . T.pack) configs
            TIO.putStrLn $ T.pack "Activate " `T.append` T.pack theme

main :: IO ()
main =
  execParser
      (info (helper <*> parseOptions)
            (fullDesc <> header "I am foo" <> progDesc "Descriptive foo")
      )
    >>= run
