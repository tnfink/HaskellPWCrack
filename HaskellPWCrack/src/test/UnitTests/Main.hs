module Main where

import Test.Hspec

import PWCrack.DictionaryVariationsSpec
import PWCrack.DictionariesSpec

allSpecifications :: [Spec]
allSpecifications = dictionaryVariationsSpecs ++ dictionariesSpecs

main::IO()
main = mapM_ hspec allSpecifications
