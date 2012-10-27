module PWCrack.AttackMacOsXKeyChain(attackMacOsXKeyChain) where

import qualified Data.Text as T
import System.Process
import System.Exit


attackMacOsXKeyChain :: FilePath -> IO (Maybe T.Text)
attackMacOsXKeyChain keychain = do
  let password = T.pack "12otto99"
  success <- testPassword keychain password
  if success then return $ Just password
             else return Nothing

testPassword :: FilePath -> T.Text -> IO Bool
testPassword keychain password = do
  putStrLn $ "try " ++ passwordString
  (exitCode, stdout , stderr) <- readProcessWithExitCode security arguments standardInput
  putStrLn $ show exitCode
  putStrLn stdout
  putStrLn stderr
  case exitCode of
    ExitSuccess   -> return True
    ExitFailure _ -> return False
 where
  security = "security"
  arguments = ["unlock-keychain", "-p", passwordString , keychain]
  standardInput = ""
  passwordString = T.unpack password

