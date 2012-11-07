{-# LANGUAGE CPP #-}
module PWCrack.DictionaryVariations( permutateUpperLowerCase
                                   , addBirthdays
                                   , DictionaryVariation
#ifdef TEST
                                   , addBirthdaysWithParameters
#endif                                   
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
addBirthdays pword = 
  foldr (\ dv list -> (dv pword) ++ list) 
        []  allBirthdayVariants
    where
      allBirthdayVariants = [addBirthdaysWithParameters sep upper | sep <- ["","."], upper <- [True, False]]

addBirthdaysWithParameters :: T.Text -> Bool -> DictionaryVariation
addBirthdaysWithParameters separator upper pword
  | T.null pword = []
  | otherwise =  map concatNewPassword allCombinationsOfDayMonthYearWithLeadingZero
  where pword' = if upper then T.cons (toggleCharacter $ T.head pword)
                                      $ T.tail pword
                          else pword
        concatNewPassword (d,m,y) = T.concat [d,separator,m,separator,pword',separator,y]
  
  

firstCiphersWithLeading0 :: [T.Text]
firstCiphersWithLeading0 = map (T.pack . ((++) "0") . show) ([1..9] :: [Int])

days :: [T.Text]
days = map (T.pack . show) ([1..31] :: [Int])

months :: [T.Text] 
months = map (T.pack . show) ([1..12] :: [Int]) 

years :: [T.Text]
years = map (T.pack . show) ([0..99] :: [Int])

allCombinationsOfDayMonthYearWithLeadingZero :: [(T.Text, T.Text, T.Text)]
allCombinationsOfDayMonthYearWithLeadingZero = [ (d,m,y) | d <- days ++ firstCiphersWithLeading0, 
                                                           m <- months ++ firstCiphersWithLeading0, 
                                                           y <- years ]
          