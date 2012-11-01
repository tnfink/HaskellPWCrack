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
attackMacOsXKeyChain keychain =
  evalStateT (checkPasswords generateListOfPasswords) startAttackState
 where
  startAttackState = AttackState { testedStringHashes = S.empty }

  checkPasswords :: [T.Text] -> Attacker
  checkPasswords passwords = do
    state <- get
    case passwords of
      [] -> return Nothing
      password : passwords -> do
        let passwordHash = hash password
            testedStrings = testedStringHashes state
            numberOfTestedStrings = S.size testedStrings
        when (numberOfTestedStrings `mod` 1000 == 0)
          $ liftIO $ putStrLn ("test password #"++(show numberOfTestedStrings))
        if (S.member passwordHash testedStrings)
          then do
            liftIO $ putStrLn ("skipping "++(T.unpack password))
            checkPasswords passwords
          else do
            foundPassword <- liftIO $ testPassword keychain password
            --liftIO $ putStrLn ("not skipping "++(T.unpack password)++ " result="++(show foundPassword))
            if (foundPassword)
              then return $ Just password
              else do
                put state { testedStringHashes = S.insert passwordHash testedStrings}
                checkPasswords passwords





-- todo: check in the hashset, if the password was already used, store only the hashes in the set,
--       hm, oder doch ein Writer? es ist ja nicht wirklich eine kontinuierliche State Transformation...
-- nee, ein monadischer Transformer: http://book.realworldhaskell.org/read/monad-transformers.html

data AttackState = AttackState {
   testedStringHashes :: S.HashSet Int
} deriving (Show)

type Attacker = StateT AttackState IO(Maybe T.Text)

testPassword :: FilePath -> T.Text -> IO Bool
testPassword keychain password = do
  -- putStrLn $ "try " ++ passwordString
  (exitCode, stdout , stderr) <- readProcessWithExitCode security arguments standardInput
  --putStrLn $ show exitCode
  --putStrLn stdout
  --putStrLn stderr
  case exitCode of
    ExitSuccess   -> return True
    ExitFailure _ -> return False
 where
  security = "security"
  arguments = ["unlock-keychain", "-p", passwordString , keychain]
  standardInput = ""
  passwordString = T.unpack password

