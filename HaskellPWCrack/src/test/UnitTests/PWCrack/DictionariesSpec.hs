module PWCrack.DictionariesSpec(dictionariesSpecs) where

import Test.Hspec

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
      applyVariations emptyDictionary `shouldBe` []
    it "returns a set with 8 entries for a dictionary with a 3 letter word and variation permutateUpperLowerCase" $
      applyVariations simpleUpperCaseDictionary `shouldSatisfy` (\ vars -> (length vars) == 8)
    it "returns more than 3 entries for a dictionary with 2 words and two variations" $
      applyVariations simpleCombinedDictionary `shouldSatisfy` (\ vars -> (length vars) > 2)

emptyDictionary :: Dictionary
emptyDictionary = Dictionary { dicWords = [], dicVariations = [] }

simpleUpperCaseDictionary :: Dictionary
simpleUpperCaseDictionary =
  Dictionary { dicWords= ["abc"], dicVariations = [permutateUpperLowerCase] }

simpleCombinedDictionary :: Dictionary
simpleCombinedDictionary =
  Dictionary { dicWords= ["a","b"], dicVariations = [permutateUpperLowerCase, addBirthdays] }

-- generateListOfPasswords
----------------------------------------------------------

generateListOfPasswordsSpec :: Spec
generateListOfPasswordsSpec  = do
  describe "generateListOfPasswords" $ do
    it "creates a non empty list" $
      generateListOfPasswords `shouldSatisfy` (not . null)