module WarmupExercisesSpec where

import Test.Hspec
import Test.QuickCheck (property)

import WarmupExercises (wordStartsWithA, selectElementsOfListStartingWithA, everyPossiblePair)

checkAllStartWithA :: [String] -> Bool
checkAllStartWithA x = and $ map wordStartsWithA x

spec :: Spec
spec = do
    describe "selectElementsOfListSTartingWithA" $ do
        it "should only have 'a's in the list" $ property $ do
            checkAllStartWithA . selectElementsOfListStartingWithA
    describe "everyPossiblePair" $ do
        -- alphabetStrings is ["a", "b", "c", "d"]
        let alphabetStrings = map (:[]) ['a' .. 'd']
        let expectedResult = ["ab", "ac", "ad", "bc", "bd", "cd"]
        it "should make every possible pairing of students" $ do
            (everyPossiblePair alphabetStrings) `shouldBe` expectedResult
        it "should make alphabetical no matter what" $ do
            let reversedStrings = reverse alphabetStrings
            (everyPossiblePair reversedStrings) `shouldBe` expectedResult
    describe "third function" $ do
        it "yay beans" $
            1 `shouldBe` 1
