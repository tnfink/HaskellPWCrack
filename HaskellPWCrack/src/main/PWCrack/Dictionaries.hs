{-# LANGUAGE CPP #-}

module PWCrack.Dictionaries
  ( generateListOfPasswords
#ifdef TEST
  , applyVariations
  , applyGeneralVariations
  , Dictionary(..)
#endif
  ) where

import qualified Data.Text as T
import qualified Data.HashSet as S
import Data.List

import PWCrack.DictionaryVariations

data Dictionary =
  Dictionary { dicWords :: S.HashSet T.Text
             , dicVariations :: [DictionaryVariation]
             }

nameDictionary :: Dictionary
nameDictionary = Dictionary
  { dicWords = S.fromList
     [ "adam", "broder", "eva", "kilian", "lena", "leslie", "marie", "torsten", "uriel"
     ]
  , dicVariations =
     [ addBirthdays ]
  }

generalVariations :: [DictionaryVariation]
generalVariations = [ permutateUpperLowerCase ]

-- generateListOfPasswords
-----------------------------------------

generateListOfPasswords :: [T.Text]
generateListOfPasswords =
  S.toList nameVariations
 where
  nameVariations = applyVariations nameDictionary;
  -- allVariations = applyGeneralVariations nameVariations;

applyVariations :: Dictionary -> S.HashSet T.Text
applyVariations dictionary =
  applyVariationsToWords (dicWords dictionary) (dicVariations dictionary)

applyGeneralVariations :: S.HashSet T.Text -> S.HashSet T.Text
applyGeneralVariations vwords =
  applyVariationsToWords vwords generalVariations

applyVariationsToWords :: S.HashSet T.Text -> [DictionaryVariation] -> S.HashSet T.Text
applyVariationsToWords dwords variations =
    let combinations =
          [(variation,word) | variation <- variations,
                              word <- S.toList dwords ]
        varWords = foldl'(\ set (variation,word) -> S.union (variation word) set)
                         S.empty combinations
    in -- I have to add to words from the dictionary itself, because the variations avoid them
       S.union dwords varWords

