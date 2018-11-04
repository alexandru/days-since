module AppCmd (
  AppArgs(..),
  getCmdLineArgs) where

import Options.Applicative
import Data.Semigroup ((<>))

-- |Models command line arguments
data AppArgs = AppArgs
  { 
    date :: String
  } deriving (Show)

appArgsParser :: Parser AppArgs
appArgsParser = AppArgs
  <$> argument str (metavar "DATE" <> help "YYYY-MM-DD format")

opts :: ParserInfo AppArgs
opts = info (appArgsParser <**> helper)
  ( fullDesc
    <> progDesc "Calculates how many days have passed since a certain date" )

getCmdLineArgs :: IO AppArgs
getCmdLineArgs = execParser opts
