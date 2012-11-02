module PWCrack.AttackMacOsXKeyChain(attackMacOsXKeyChain) where

import qualified Data.Text as T
import qualified Data.HashSet as S
import Data.Hashable
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import System.Process
import System.Exit

import PWCrack.Dictionaries

attackMacOsXKeyChain :: FilePath -> IO (Maybe T.Text)
attackMacOsXKeyChain keychain = do
  (result , finalState) <- runStateT (checkPasswords generateListOfPasswords) startAttackState
  liftIO $ putStrLn ("found "++ (show result) ++" skipped " ++ (show $ skippedTests finalState) ++ " doubles")
  return result
 where
  startAttackState = AttackState { testedStringHashes = S.empty
                                 , skippedTests = 0
                                 }

  checkPasswords :: [T.Text] -> Attacker
  checkPasswords [] = return Nothing
  checkPasswords (password : passwords) = do
    currentState <- get
    let passwordHash = hash password
        testedStrings = testedStringHashes currentState
        numberOfTestedStrings = S.size testedStrings
    when (numberOfTestedStrings `mod` 1000 == 0)
      $ liftIO $ putStrLn ("test password #"++(show numberOfTestedStrings) ++ " skipped "++(show $ skippedTests currentState))
    if (S.member passwordHash testedStrings)
      then do
        liftIO $ putStrLn ("skipping "++(T.unpack password))
        put currentState { skippedTests = 1+(skippedTests currentState)}
        checkPasswords passwords
      else do
        foundPassword <- liftIO $ testPassword keychain password
        if (foundPassword)
          then return $ Just password
          else do
            put currentState { testedStringHashes = S.insert passwordHash testedStrings}
            checkPasswords passwords


data AttackState =
  AttackState {  testedStringHashes :: S.HashSet Int
                 -- todo: check if it is ok to use the passwords in the set instead of their hash codes
              ,  skippedTests :: Int
              } deriving (Show)

type Attacker = StateT AttackState IO(Maybe T.Text)

testPassword :: FilePath -> T.Text -> IO Bool
testPassword keychain password = do
  -- putStrLn $ "try " ++ passwordString
  (exitCode, _ , _ ) <- readProcessWithExitCode security arguments standardInput
  case exitCode of
    ExitSuccess   -> return True
    ExitFailure _ -> return False
 where
  security = "security"
  arguments = ["unlock-keychain", "-p", passwordString , keychain]
  standardInput = ""
  passwordString = T.unpack password

