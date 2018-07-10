{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Text                     as T
import           Data.Text.IO                  as TIO
import           Data.Traversable              as TR
import           Data.ByteString.Lazy          as BL
import           Data.List                     as DL
import           Apps.Util as Util
import           Apps.Alacritty
import           Options.Applicative
import           System.Directory
import           Data.Semigroup                 ( (<>) )
import           CLI
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

getThemes :: FilePath -> ExceptT T.Text IO [Theme]
getThemes p = do
  contents <- liftIO $ BL.readFile p
  -- ExceptT . return $ eitherDecode contents
  -- ^ Does not work because eitherDecode returns String not T.Text
  let decoded = eitherDecode contents
  case decoded of
    (Left err) -> throwE $ T.pack err
    (Right x) -> return x

listThemes :: FilePath -> IO ()
listThemes fp = do
  themes <- runExceptT $ getThemes fp

  case themes of
    (Left  msg   ) -> TIO.putStrLn msg
    (Right themes) -> mapM_ (TIO.putStrLn . name) themes

activateTheme :: Theme -> IO ()
activateTheme theme = do
  -- TODO: remove alacritty and use apps
  configs <- sequence $ configPaths alacritty
  mapM_ configTransformer configs
 where
  configTransformer path = do
    config <- TIO.readFile path
    TIO.putStrLn $ Util.configCreator alacritty theme config

findTheme :: (Monad m) => [Theme] -> Util.ThemeName -> ExceptT T.Text m Theme
findTheme ts tn =
  let theme = DL.find ((==) tn . name) ts
  in case theme of
    Nothing -> throwE "Theme not found"
    (Just theme') -> return theme'

run :: CLIOptions -> ExceptT T.Text IO ()
run (CLIOptions path cmd) = case cmd of
  ListThemes                    -> liftIO $ listThemes path

  (ActivateTheme selectedTheme) -> do

    themes <- getThemes path
    theme <- findTheme themes $ T.pack selectedTheme

    liftIO $ activateTheme theme
    liftIO . TIO.putStrLn $ "Activated " `T.append` T.pack selectedTheme

main :: IO ()
main = do
  opts <- execParser
      (info (helper <*> parseOptions)
            (fullDesc <> header "I am foo" <> progDesc "Descriptive foo")
      )
  x <- runExceptT $ run opts
  case x of
    (Left err) -> TIO.putStrLn err
    (Right x) -> TIO.putStrLn "yay"
