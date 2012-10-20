module PWCrack.DictionaryVariations( permutateUpperLowerCase
                                   , addBirthdays
                                   ) where

import qualified Data.HashSet as S
import Data.Char
import qualified Data.ByteString as B

-- Permutate upper and lower case
--------------------------------------------------------

permutateUpperLowerCase :: String -> S.HashSet String
permutateUpperLowerCase word = permutatedWords
  where permutatedList = permutateUpperLowerCaseIter word
        allPermutatedWords = S.fromList permutatedList
        permutatedWords = S.delete word allPermutatedWords
  
   
permutateUpperLowerCaseIter :: String -> [String]          
permutateUpperLowerCaseIter [] = []
permutateUpperLowerCaseIter (y:ys) =
  (prependChar y permutatedYs) ++
  (prependChar (toggleCharacter y) permutatedYs)
    where permutatedYs = permutateUpperLowerCaseIter ys
             
prependChar ::  Char -> [[Char]] -> [[Char]]
prependChar prefix [] = [[prefix]]
prependChar prefix listOfWords = map (\word -> prefix : word) listOfWords

toggleCharacter :: Char -> Char
toggleCharacter c 
   | isUpper c = toLower c
   | isLower c = toUpper c
   | otherwise = c
 

-- add birthdays
------------------------------------------------------------------

addBirthdays :: String -> S.HashSet String
addBirthdays [] = S.empty
addBirthdays pword = 
  let days = map show ([1..31] :: [Int])
      months = map show ([1..12] :: [Int])
      years = map show ([0..99] :: [Int])
      allCombinations = [ (d,m,y) | d <- days, m <- months, y <- years ]
  in foldl (\ set (d,m,y) -> (S.insert (d++m++pword++y) set)) S.empty allCombinations
      

