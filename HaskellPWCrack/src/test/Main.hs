module Main where

import Test.Hspec

import PWCrack.DictionaryVariationsSpec

allSpecifications :: [Spec]
allSpecifications = dictionaryVariationsSpecs

main::IO()
main = mapM_ hspec allSpecifications
