module PWCrack.DictionaryVariations( permutateUpperLowerCase
                                   , addBirthdays
                                   , DictionaryVariation
                                   ) where

import Data.Char
import qualified Data.Text as T

type DictionaryVariation = T.Text -> [T.Text]

-- Permutate upper and lower case
--------------------------------------------------------

permutateUpperLowerCase :: DictionaryVariation
permutateUpperLowerCase word = permutatedWords
  where permutatedList = permutateUpperLowerCaseIter word
        permutatedWords = filter (\w -> w /= word) permutatedList
  
   
permutateUpperLowerCaseIter :: T.Text -> [T.Text]
permutateUpperLowerCaseIter word
  | T.null word = []
  | otherwise =
      (prependChar firstChar permutatedTail) ++
      (prependChar toggledFirstChar permutatedTail)
     where
       firstChar = T.head word
       toggledFirstChar = toggleCharacter firstChar
       permutatedTail = permutateUpperLowerCaseIter $ T.tail word
             
prependChar ::  Char -> [T.Text] -> [T.Text]
prependChar prefix [] = [T.pack [prefix]]
prependChar prefix listOfWords = map (\word -> prefix `T.cons` word) listOfWords

toggleCharacter :: Char -> Char
toggleCharacter c 
   | isUpper c = toLower c
   | isLower c = toUpper c
   | otherwise = c
 

-- add birthdays
------------------------------------------------------------------

addBirthdays :: DictionaryVariation
addBirthdays pword
  | T.null pword = []
  | otherwise =  
          let days = map (T.pack . show) ([1..31] :: [Int]) 
              months = map (T.pack . show) ([1..12] :: [Int])
              years = map (T.pack . show) ([0..99] :: [Int])
              allCombinations = [ (d,m,y) | d <- days, m <- months, y <- years ]
          in
              map (\ (d,m,y) -> T.concat [d,m,pword,y]) allCombinations
