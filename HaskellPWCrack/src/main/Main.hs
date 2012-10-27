module Main where

import System.Environment
import System.Console.GetOpt

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

attackKeyChain keyChainPath = undefined


main::IO()
main = do
  argv <- getArgs
  case (getOpt Permute suppportedOptions argv) of
    ([Help],[],[])                    -> printHelp
    ([KeyChain keyChainPath],[],[])   -> attackKeyChain keyChainPath
    (_,_,_)                           ->
         do putStrLn "Cannot process your configuration, sorry.\n"
            printHelp
  where
    printHelp = putStrLn $ usageInfo "Usage: pwcrack [option]" suppportedOptions
