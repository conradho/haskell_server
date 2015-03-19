module WarmupExercisesSpec where

import Test.Hspec
import Test.QuickCheck (property)

import WarmupExercises (wordStartsWithA, selectElementsOfListStartingWithA)

checkAllStartWithA :: [String] -> Bool
checkAllStartWithA x = and $ map wordStartsWithA x

spec :: Spec
spec = do
    describe "selectElementsOfListSTartingWithA" $ do
        it "should be blah" $
            1 `shouldBe` 1
        it "should only have 'a's in the list" $ property $
            checkAllStartWithA . selectElementsOfListStartingWithA
    describe "second function" $ do
        it "should only have 'a's in the list" $
            1 `shouldBe` 2
    describe "third function" $ do
        it "yay beans" $
            1 `shouldBe` 1
