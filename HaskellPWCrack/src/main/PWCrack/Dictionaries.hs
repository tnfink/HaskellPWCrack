module PWCrack.Dictionaries(generateListOfPasswords

) where

import qualified Data.Text as T
import qualified Data.HashSet as S

import PWCrack.DictionaryVariations

data Dictionary =
  Dictionary { dicWords :: [T.Text]
             , dicVariations :: [DictionaryVariation]
             }

nameDictionary :: Dictionary
nameDictionary = Dictionary
  { dicWords =
     [ "adam", "broder", "eva", "kilian", "lena", "leslie", "marie", "torsten", "uriel"
     ]
  , dicVariations =
     [ addBirthdays ]
  }

generalVariations :: [DictionaryVariation]
generalVariations = [ permutateUpperLowerCase ]


generateListOfPasswords :: [T.Text]
generateListOfPasswords =
   S.toList allVariations
  where
     nameVariations = applyVariations nameDictionary;
     allVariations = applyGeneralVariations nameVariations;

applyVariations = undefined

applyGeneralVariations = undefined