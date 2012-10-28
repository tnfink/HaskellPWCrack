module PWCrack.Dictionaries(generateListOfPasswords) where

import qualified Data.Text as T

data Dictionary = Dictionary { dicWords :: [T.Text] }

nameDictionary :: Dictionary
nameDictionary = Dictionary
  [ "adam", "broder", "eva", "kilian", "lena", "leslie", "marie", "torsten", "uriel"
  ]


generateListOfPasswords :: [T.Text]
generateListOfPasswords = dicWords nameDictionary