module Main where

import AppCmd
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import qualified Data.Time as T
import qualified Data.Time.Clock as TC
import qualified Data.Time.Format as TF

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e =
  hPutStrLn stderr str >> exitWith e

main :: IO ()
main = do
  args <- getCmdLineArgs
  let dayStr = date args
  let dayOpt = (TF.parseTimeM True TF.defaultTimeLocale "%Y-%-m-%-d" dayStr) :: Maybe T.Day
  case dayOpt of
    Nothing -> exitWithErrorMessage ("Error - invalid date: " <> dayStr) (ExitFailure 1)
    Just day -> do
      today <- TC.getCurrentTime >>= return . T.utctDay
      let days = toInteger $ T.diffDays today day
      let weeks = (fromInteger days) / 7 :: Double
      let months = (fromInteger days) / 30.4375 :: Double
      putStrLn ""
      putStrLn $ "Days:  " <> (printf "%5d" days)
      putStrLn $ "Weeks: " <> (printf "%5.1f" weeks)
      putStrLn $ "Months:" <> (printf "%5.1f" months)
      putStrLn ""
