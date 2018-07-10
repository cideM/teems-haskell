{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Text                     as T
import           Data.Text.IO                  as TIO
import           Data.Traversable              as TR
import           Data.ByteString.Lazy          as BL
import           Data.List                     as DL
import           Apps.Util
import           Apps.Alacritty
import           Options.Applicative
import           System.Directory
import           Data.Semigroup                 ( (<>) )
import           CLI
import           Control.Monad.IO.Class

getThemes :: FilePath -> IO (Either String [Theme])
getThemes p = do
  contents <- BL.readFile p
  return $ eitherDecode contents

run :: CLIOptions -> IO ()
run (CLIOptions path cmd) = case cmd of
  ListThemes -> do

    themes <- getThemes path

    case themes of
      (Left  msg   ) -> TIO.putStrLn $ T.pack msg
      (Right themes) -> mapM_ (TIO.putStrLn . name) themes

  (ActivateTheme selectedTheme) -> do

    themes' <- getThemes path

    case themes' of
      (Left  msg   ) -> TIO.putStrLn $ T.pack msg
      (Right themes) -> do

        let theme' = DL.find ((==) selectedTheme . T.unpack . name) themes

        case theme' of
          Nothing  -> TIO.putStrLn $ "Could not find " `T.append` T.pack selectedTheme
          (Just theme) -> do
            configs <- sequence $ configPaths alacritty
            mapM_ configTransformer configs
            TIO.putStrLn $ "Activated " `T.append` T.pack selectedTheme
           where
            configTransformer config = do
              content <- TIO.readFile config
              let parsed = Apps.Util.configCreator alacritty theme content
              case parsed of
                (Left err) -> TIO.putStrLn err
                (Right newConfig) -> TIO.putStrLn newConfig

main :: IO ()
main =
  execParser
      (info (helper <*> parseOptions)
            (fullDesc <> header "I am foo" <> progDesc "Descriptive foo")
      )
    >>= run
