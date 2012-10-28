{-# LANGUAGE FlexibleInstances #-}
module PWCrack.DictionaryVariationsSpec(dictionaryVariationsSpecs) where

import Text.Regex.TDFA

import Test.Hspec
import Test.QuickCheck

import qualified Data.HashSet as S
import qualified Data.Text as T

import PWCrack.DictionaryVariations

dictionaryVariationsSpecs :: [Spec]
dictionaryVariationsSpecs = 
  [ permutateUpperLowerCaseSpec
  , addBirthdaysSpec
  , birthdayPatternSpec
  ]

-- permutateUpperLowerCase
--------------------------------------------------------

permutateUpperLowerCaseSpec :: Spec
permutateUpperLowerCaseSpec = do
  describe "permutateUpperLowerCase" $ do
    it "creates an empty set for an emtpy word" $
      permutateUpperLowerCase "" == S.empty
    it "turns a small a to a capital A" $
          permutateUpperLowerCase "a" == S.singleton("A")
    it "turns a capital A to a small a" $
          permutateUpperLowerCase "A" == S.singleton("a")
    it "does not change non-letters" $
          permutateUpperLowerCase "a:" == S.singleton("A:")
    it "creates at most 2^l - 1 variations for a word of length l, because it does not put the word itself in the result" 
      $ property $ 
          forAll pwords (\word -> let wordText = T.pack word
                                  in S.size (permutateUpperLowerCase wordText) <=
                                     ( 2 ^ (T.length wordText) - 1))
                                

-- addBirthdays
--------------------------------------------------------                            

addBirthdaysSpec :: Spec
addBirthdaysSpec = do
  describe "addBirthdays" $ do
        it "returns an empty set for an empty word" $
          addBirthdays (T.pack "") == S.empty
        it "adds 2-4 numbers in front of the word and one or two after them" $
          let variants = addBirthdays (T.pack "password")
              wrongWords = S.filter (\ w -> not $ matchesBirthdayPattern w) variants
          in  (S.size wrongWords) == 0


birthdayPatternSpec :: Spec
birthdayPatternSpec = do
  describe "birthdayPatternSpec" $ do
    it "accepts 4 digits at the start and 2 digits at the end" $
       (matchesBirthdayPattern "3112asd12") == True
    it "accepts 3 digits at the start and 2 digits at the end" $
       (matchesBirthdayPattern "112asd12") == True
    it "accepts 2 digits at the start and 2 digits at the end" $
       (matchesBirthdayPattern "11asd12") == True
    it "accepts 2 digits at the start and 1 digit at the end" $
       (matchesBirthdayPattern "11asd1") == True
    it "does not accept empty strings" $ do
       (matchesBirthdayPattern "") == False
    it "does not accept strings starting with a letter" $ do
       (matchesBirthdayPattern "x1234asd56") == False
    it "does not accept strings ending with a letter" $ do
       (matchesBirthdayPattern "1234asd56y") == False

birthdayPattern :: String
birthdayPattern = "^[[:digit:]]?[[:digit:]][[:digit:]]?[[:digit:]].*[[:digit:]]?[[:digit:]]$"

matchesBirthdayPattern :: T.Text -> Bool
matchesBirthdayPattern word =  (T.unpack word) =~ birthdayPattern

-- Testdata
--------------------------------------------------------                            
pwords :: Gen String
pwords = resize 5  $ listOf1 $ choose ('A','z')