module WarmupExercisesSpec where

import Test.Hspec
import Test.QuickCheck (property)

import WarmupExercises (wordStartsWithA, selectElementsOfListStartingWithA, everyPossiblePairSorted)

checkAllStartWithA :: [String] -> Bool
checkAllStartWithA x = and $ map wordStartsWithA x

spec :: Spec
spec = do
    describe "selectElementsOfListSTartingWithA" $ do
        it "should only have 'a's in the list" $ property $ do
            checkAllStartWithA . selectElementsOfListStartingWithA
    describe "everyPossiblePairSorted" $ do
        let alphabetStrings = map (:[]) ['a' .. 'd']  -- ["a", "b", "c", "d"]
        let expectedResult = ["ab", "ac", "ad", "bc", "bd", "cd"]
        it "should make every possible pairing of students" $ do
            (everyPossiblePairSorted alphabetStrings) `shouldBe` expectedResult
        context "when provided with non-sorted input" $ do
            it "should still return alphabetically sorted" $ do
                -- the do above is needed (vs good style previously) because we have two statements
                let reversedStrings = reverse alphabetStrings
                (everyPossiblePairSorted reversedStrings) `shouldBe` expectedResult
            it "should not sort within each element" $ do
                everyPossiblePairSorted ["abc", "fed", "hgi"] `shouldBe` ["abcfed", "abchgi", "fedhgi"]
    describe "get the first half of a string" $ do
        it "has half the length" $ property $ do
            pendingWith "have a quickcheck property checking length is half of input String"
        it "contains all characters from first half" $ property $ do
            pending
    describe "counts elements that are palindromes" $ do
        it "sub-function correctly identifies palindromes" $
            pendingWith "only fools leave random tests lying around"
    describe "format a date nicely" $ do
        it "matches a regex" $
            pendingWith "write a regex to match the date"
    describe "anagrams of words that are real words?" $ do
        it "any anagram can be made from the original chars" $
            pending
    describe "flip the key/value of a dict" $ do
        it "yay beans" $
            pendingWith "only fools leave random tests lying around"
