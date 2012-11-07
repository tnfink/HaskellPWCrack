{-# LANGUAGE FlexibleInstances #-}
module PWCrack.DictionaryVariationsSpec(dictionaryVariationsSpecs) where

import Data.List
import Text.Regex.TDFA

import Test.Hspec
import Test.QuickCheck

import qualified Data.Text as T

import PWCrack.DictionaryVariations

dictionaryVariationsSpecs :: [Spec]
dictionaryVariationsSpecs = 
  [ permutateUpperLowerCaseSpec
  , addBirthdaysSpec
  , birthdayPatternSpec
  , birthdayPatternSeparatedSpec
  ]

-- permutateUpperLowerCase
--------------------------------------------------------

permutateUpperLowerCaseSpec :: Spec
permutateUpperLowerCaseSpec = do
  describe "permutateUpperLowerCase" $ do
    it "creates an empty set for an emtpy word" $
      permutateUpperLowerCase "" `shouldSatisfy` null
    it "turns a small a to a capital A" $
          permutateUpperLowerCase "a" `shouldSatisfy` (sameSet ["A"])
    it "turns a capital A to a small a" $
          permutateUpperLowerCase "A" `shouldSatisfy` (sameSet ["a"])
    it "does not change non-letters" $
          permutateUpperLowerCase "a:" `shouldSatisfy` (sameSet ["A:"])
    it "creates at most 2^l - 1 variations for a word of length l, because it does not put the word itself in the result" 
      $ property $ 
          forAll pwords (\word -> let wordText = T.pack word
                                  in length (permutateUpperLowerCase wordText) <=
                                     ( 2 ^ (T.length wordText) - 1))
                                
sameSet :: [T.Text] -> [T.Text] -> Bool
sameSet as bs = (nub as) == (nub bs)

-- addBirthdays
--------------------------------------------------------                            

addBirthdaysSpec :: Spec
addBirthdaysSpec = do
  describe "addBirthdays" $ do
        it "returns an empty set for an empty word" $
          addBirthdays (T.pack "") `shouldSatisfy` null
        it "generates 2-4 numbers in front of the word and one or two after them possibly separated by ." $
          (addBirthdaysWithParameters "" False (T.pack "password"))
          `shouldSatisfy` 
          (checkBirthdayVariantsWithPattern birthdayPattern)
        it "generates 2-4 numbers in front of the word and one or two after them separated by ." $
          (addBirthdaysWithParameters "." True (T.pack "password"))
          `shouldSatisfy` 
          (checkBirthdayVariantsWithPattern birthdayPatternSeparated)

checkBirthdayVariantsWithPattern :: String -> [T.Text] -> Bool          
checkBirthdayVariantsWithPattern pattern variants = null wrongWords
  where wrongWords = filter (\ word -> not $ (T.unpack word)  =~ pattern) variants

birthdayPatternSpec :: Spec
birthdayPatternSpec = do
  describe "birthdayPatternSpec" $ do
    it "accepts 4 digits at the start and 2 digits at the end" $
       "3112asd12" `shouldSatisfy` matchesBirthdayPattern
    it "accepts 3 digits at the start and 2 digits at the end" $
       "112asd12" `shouldSatisfy` matchesBirthdayPattern
    it "accepts 2 digits at the start and 2 digits at the end" $
       "11asd12" `shouldSatisfy` matchesBirthdayPattern
    it "accepts 2 digits at the start and 1 digit at the end" $
       "11asd1" `shouldSatisfy` matchesBirthdayPattern
    it "does not accept empty strings" $ do
       "" `shouldSatisfy` not .  matchesBirthdayPattern
    it "does not accept strings starting with a letter" $ do
       "x1234asd56" `shouldSatisfy` not .  matchesBirthdayPattern
    it "does not accept strings ending with a letter" $ do
       "1234asd56y" `shouldSatisfy` not .  matchesBirthdayPattern

birthdayPattern :: String
birthdayPattern = "^[[:digit:]]?[[:digit:]][[:digit:]]?[[:digit:]].*[[:digit:]]?[[:digit:]]$"

birthdayPatternSeparatedSpec :: Spec
birthdayPatternSeparatedSpec = do
  describe "birthdayPatternSpec" $ do
    it "accepts 4 digits at the start and 2 digits at the end" $
       "31.12.asd.12" `shouldSatisfy` matchesSeparatedBirthdayPattern
    it "accepts 3 digits at the start and 2 digits at the end" $
       "1.12.asd.12"  `shouldSatisfy` matchesSeparatedBirthdayPattern
    it "accepts 2 digits at the start and 2 digits at the end" $
       "1.1.asd.12"  `shouldSatisfy` matchesSeparatedBirthdayPattern
    it "does not accept empty strings" $ do
       "" `shouldSatisfy` not . matchesSeparatedBirthdayPattern
    it "does not accept strings starting with a letter" $ do
       "x12.34.asd.56" `shouldSatisfy` not . matchesSeparatedBirthdayPattern
    it "does not accept strings ending with a letter" $ do
       "12.34.asd.56.y" `shouldSatisfy` not . matchesSeparatedBirthdayPattern
    it "does not accept strings without separators" $ do
       "1234asd56" `shouldSatisfy` not . matchesSeparatedBirthdayPattern

birthdayPatternSeparated :: [Char]
birthdayPatternSeparated = "^[[:digit:]]?[[:digit:]]\\.[[:digit:]]?[[:digit:]]\\..*\\.[[:digit:]]?[[:digit:]]$"

matchesBirthdayPattern :: T.Text -> Bool
matchesBirthdayPattern =  matchesPattern birthdayPattern

matchesSeparatedBirthdayPattern :: T.Text -> Bool
matchesSeparatedBirthdayPattern =  matchesPattern birthdayPatternSeparated

matchesPattern :: String -> T.Text -> Bool 
matchesPattern pattern word = (T.unpack word) =~ pattern
 

-- Testdata
--------------------------------------------------------                            
pwords :: Gen String
pwords = resize 5  $ listOf1 $ choose ('A','z')