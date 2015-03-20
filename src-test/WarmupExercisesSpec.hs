module WarmupExercisesSpec where

import Test.Hspec
import Test.QuickCheck (property)

import WarmupExercises (wordStartsWithA, selectElementsOfListStartingWithA, everyPossiblePair)

checkAllStartWithA :: [String] -> Bool
checkAllStartWithA x = and $ map wordStartsWithA x

spec :: Spec
spec = do
    describe "selectElementsOfListSTartingWithA" $ do
        it "should only have 'a's in the list" $ property $
            checkAllStartWithA . selectElementsOfListStartingWithA
    describe "everyPossiblePair" $ do
        it "should make every possible pairing of students" $
            (everyPossiblePair ["a", "b", "c"]) `shouldBe` ["ab", "bc", "ac"]
    describe "third function" $ do
        it "yay beans" $
            1 `shouldBe` 1
