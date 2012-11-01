module Main where

import System.Process
import System.Exit

-- TODO: lock Keychain after cracking it

main::IO()
main = do
  (exitCode, stdout , stderr) <- readProcessWithExitCode pathToPwCrack arguments testStandardInput
  putStrLn $ "Exit-Code:" ++ (show exitCode)
  putStrLn $ "stdout:\n" ++ stdout
  putStrLn $ "stderr:\n" ++ stderr
  case exitCode of
    ExitSuccess   -> exitSuccess
    ExitFailure _ -> exitFailure

 where
  pathToPwCrack = "dist/build/pwcrack/pwcrack"
  pathToKeyChain = "src/test/IntegrationTests/MacOsXKeyChainAttack/testKeyChain.keychain"
  arguments = ["-k", pathToKeyChain]
  testStandardInput = ""

