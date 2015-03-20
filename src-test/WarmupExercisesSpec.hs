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
        -- alphabetStrings is ["a", "b", "c", "d"]
        let alphabetStrings = map (:[]) ['a' .. 'd']
        let expectedResult = ["ab", "ac", "ad", "bc", "bd", "cd"]
        it "should make every possible pairing of students" $ do
            (everyPossiblePairSorted alphabetStrings) `shouldBe` expectedResult
        context "when provided with non-sorted input" $ do
            it "should still return alphabetically sorted" $ do
                let reversedStrings = reverse alphabetStrings
                (everyPossiblePairSorted reversedStrings) `shouldBe` expectedResult
            it "should not sort within each element" $ do
                everyPossiblePairSorted ["abc", "fed", "hgi"] `shouldBe` ["abcfed", "abchgi", "fedhgi"]
    describe "third function" $ do
        it "yay beans" $
            pendingWith "only fools leave random tests lying around"
