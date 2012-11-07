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

import PWCrack.DictionaryVariations

data Dictionary =
  Dictionary { dicWords :: [T.Text]
             , dicVariations :: [DictionaryVariation]
             }

nameDictionary :: Dictionary
nameDictionary = Dictionary
  { dicWords =
     [ "broder" -- , "adam", "otto", "eva", "kilian", "lena", "leslie", "marie", "torsten", "uriel"
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
  allVariations
 where
  nameVariations = applyVariations nameDictionary;
  allVariations = applyGeneralVariations nameVariations;

applyVariations :: Dictionary -> [T.Text]
applyVariations dictionary =
  applyVariationsToWords (dicWords dictionary) (dicVariations dictionary)

applyGeneralVariations :: [T.Text] -> [T.Text]
applyGeneralVariations vwords =
  applyVariationsToWords vwords generalVariations

applyVariationsToWords :: [T.Text] -> [DictionaryVariation] -> [T.Text]
applyVariationsToWords dwords variations =
    let combinations =
          [(variation,word) | variation <- variations,
                              word <- dwords ]
        varWords = concat $ map (\ (variation,word) -> variation word) combinations
    in -- I have to add to words from the dictionary itself, because the variations avoid them
        dwords ++ varWords

