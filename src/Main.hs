module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import qualified Data.Time as T
import qualified Data.Time.Clock as TC
import qualified Data.Time.Format as TF

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage message e =
  do
    hPutStrLn stderr message
    exitWith e

-- |Models command line arguments
data AppArgs =
  AppArgs
    { startDate :: String,
      endDate :: Maybe String
    }
  deriving (Show)

getCmdLineArgs :: IO AppArgs
getCmdLineArgs = execParser opts
  where
    appArgsParser = AppArgs
      <$> argument str (
        metavar "START_DATE"
        <> help "Start date, YYYY-MM-DD format"
      )
      <*> optional (
        strOption (
          metavar "END_DATE"
          <> short 'e'
          <> long "end-date"
          <> help "Optional end date, YYYY-MM-DD format"
        )
      )

    opts =
      info
        (appArgsParser <**> helper)
        (fullDesc
          <> progDesc "Calculates how many days have passed since a certain date")

main :: IO ()
main = do
  args <- getCmdLineArgs
  let startDateStr = startDate args
  let parsedStartDT = TF.parseTimeM True TF.defaultTimeLocale "%Y-%-m-%-d %-z" startDateStr :: Maybe T.UTCTime

  putStrLn (show parsedStartDT)
  -- case parsedStartDT of
  --   Nothing ->
  --     exitWithErrorMessage ("Error - invalid date: " <> startDateStr) (ExitFailure 1)
  --   Just startDT -> do
  --     now <- TC.getCurrentTime
  --     let diff = T.diffUTCTime now startDT
  --     let days = T.nominalDiffTimeToSeconds $ T.diffUTCTime now startDT
  --     let weeks = fromInteger days / 7 :: Double
  --     let months = fromInteger days / 30.4375 :: Double
  --     let years = fromInteger days / 365.242199 :: Double

  --     putStrLn ""
  --     putStrLn $ "Days:   " <> printf "%6d" days
  --     putStrLn $ "Weeks:  " <> printf "%6.1f" weeks
  --     putStrLn $ "Months: " <> printf "%6.1f" months
  --     putStrLn $ "Years:  " <> printf "%6.1f" years
  --     putStrLn ""
