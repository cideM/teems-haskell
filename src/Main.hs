{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Lazy          as BL
import qualified Data.Map                      as Map
import           Data.Text                     as T
import           Data.Text.Encoding            as TEnc
import           Data.Text.IO                  as TIO
import           Data.Traversable
import           App
import           Options.Applicative
import           GHC.Generics
import           System.Directory
import           Data.Semigroup                 ( (<>) )
import           CLI
import           System.Environment
import           System.FilePath.Glob
import           System.IO

run :: CLIOptions -> IO ()
run (CLIOptions path cmd) = case cmd of
  ListThemes -> do
    themes <- getThemes path
    case themes of
      (Left  msg   ) -> TIO.putStrLn $ T.pack msg
      (Right themes) -> mapM_ (TIO.putStrLn . name) themes

  (ActivateTheme theme) ->
    TIO.putStrLn $ (T.pack "Activate") `T.append` (T.pack theme)

main :: IO ()
main =
  execParser
      (info (helper <*> parseOptions)
            (fullDesc <> header "I am foo" <> progDesc "Descriptive foo")
      )
    >>= run
