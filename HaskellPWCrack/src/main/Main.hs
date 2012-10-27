module Main where

import System.Directory
import System.Environment
import System.Exit
import System.Console.GetOpt

import PWCrack.AttackMacOsXKeyChain

--  This module defines the interface to the command line.
--


data ParsedOption = Help
                  | KeyChain String
  deriving Eq


suppportedOptions :: [OptDescr ParsedOption]
suppportedOptions =
  [ Option ['h'] ["help"] (NoArg Help)
           "show this help message"
  , Option ['k'] ["macKeyChain"] (ReqArg (\s -> KeyChain s) "path")
           "attack a Mac OS X keychain located at path"
  ]

main::IO()
main = do
  argv <- getArgs
  case (getOpt Permute suppportedOptions argv) of
    ([Help],[],[])                    -> printHelp
    ([KeyChain keyChainPath],[],[])   -> checkAndDoAttackKeyChain keyChainPath
    (_,_,_)                           ->
         do putStrLn "Cannot process your configuration, sorry.\n"
            printHelp
  where
    printHelp = putStrLn $ usageInfo "Usage: pwcrack [option]" suppportedOptions

checkAndDoAttackKeyChain :: String -> IO()
checkAndDoAttackKeyChain keyChainPath = do
  exist <- doesFileExist keyChainPath
  if exist then attackMacOsXKeyChain keyChainPath
           else failAndExit $ "The key chain "++keyChainPath ++ " does not exist."

failAndExit :: String -> IO()
failAndExit reason = do
  putStrLn $ "The execution failed!\n"++reason
  exitWith $ ExitFailure 5