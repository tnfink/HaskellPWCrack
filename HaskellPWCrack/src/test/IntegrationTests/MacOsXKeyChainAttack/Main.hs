module Main where

import System.Process
import System.Exit

main::IO()
main = do
  (exitCode, stdout , stderr) <- readProcessWithExitCode pathToPwCrack arguments testStandardInput
  case exitCode of
    ExitSuccess      -> exitSuccess
    ExitFailure code -> do
      putStrLn $ "Exit-Code:" ++ (show code)
      putStrLn $ "stdout:\n" ++ stdout
      putStrLn $ "stderr:\n" ++ stderr
      exitFailure

 where
  pathToPwCrack = "dist/build/pwcrack/pwcrack"
  pathToKeyChain = "src/test/IntegrationTests/MacOsXKeyChainAttack/testKeyChain.keychain"
  arguments = ["-k", pathToKeyChain]
  testStandardInput = ""

