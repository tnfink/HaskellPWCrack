module PWCrack.DictionariesSpec(dictionariesSpecs) where

import Test.Hspec
import qualified Data.HashSet as S


import PWCrack.Dictionaries
import PWCrack.DictionaryVariations

dictionariesSpecs :: [Spec]
dictionariesSpecs = [applyVariationsSpec, generateListOfPasswordsSpec]


-- applyVariations
----------------------------------------------------------

applyVariationsSpec :: Spec
applyVariationsSpec = do
  describe "applyVariations" $ do
    it "returns an empty set for an empty dictionary" $
      applyVariations emptyDictionary `shouldBe` S.empty
    it "returns a set with 8 entries for a dictionary with a 3 letter word and variation permutateUpperLowerCase" $
      applyVariations simpleUpperCaseDictionary `shouldSatisfy` (\ set -> (S.size set) == 8)
    it "returns more than 3 entries for a dictionary with 2 words and two variations" $
      applyVariations simpleCombinedDictionary `shouldSatisfy` (\ set -> (S.size set) > 2)

emptyDictionary :: Dictionary
emptyDictionary = Dictionary { dicWords = S.empty, dicVariations = [] }

simpleUpperCaseDictionary :: Dictionary
simpleUpperCaseDictionary =
  Dictionary { dicWords= S.singleton("abc"), dicVariations = [permutateUpperLowerCase] }

simpleCombinedDictionary :: Dictionary
simpleCombinedDictionary =
  Dictionary { dicWords= S.fromList ["a","b"], dicVariations = [permutateUpperLowerCase, addBirthdays] }

-- generateListOfPasswords
----------------------------------------------------------

generateListOfPasswordsSpec :: Spec
generateListOfPasswordsSpec  = do
  describe "generateListOfPasswords" $ do
    it "creates a non empty list" $
      generateListOfPasswords `shouldSatisfy` (not . null)