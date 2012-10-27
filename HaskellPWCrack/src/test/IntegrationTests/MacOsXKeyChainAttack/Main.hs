module Main where

import System.Process
import System.Exit

main::IO()
main = do
  (exitCode, _, _) <- readProcessWithExitCode pathToPwCrack arguments testStandardInput
  case exitCode of
    ExitSuccess      -> exitSuccess
    ExitFailure _ -> exitFailure

 where
  pathToPwCrack = "../pwcrack/pwcrack"
  arguments = ["-k","keychain"]
  testStandardInput = ""

