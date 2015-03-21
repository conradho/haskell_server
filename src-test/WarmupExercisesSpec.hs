module WarmupExercisesSpec where

import Test.Hspec
import Test.QuickCheck (property)
import Text.Regex.Posix ((=~))
import Data.Time (getCurrentTime)
import qualified Data.Map as Map

import WarmupExercises
    ( wordStartsWithA
    , selectElementsOfListStartingWithA
    , everyPossiblePairSorted
    , getFirstHalf
    , isPalindrome
    , formatDate
    , flipKeyVal
    )

checkAllStartWithA :: [String] -> Bool
checkAllStartWithA x = and $ map wordStartsWithA x

spec :: Spec
spec = do

    describe "selectElementsOfListSTartingWithA" $ do
        it "should only have 'a's in the list" $ property $ do
            -- QuickCheck properties print counter examples if it fails vs hspec doesn't
            checkAllStartWithA . selectElementsOfListStartingWithA


    describe "everyPossiblePairSorted" $ do
        let alphabetStrings = map (:[]) ['a' .. 'd']  -- ["a", "b", "c", "d"]
        let expectedResult = ["ab", "ac", "ad", "bc", "bd", "cd"]

        it "should return every possible pairing of students" $ do
            (everyPossiblePairSorted alphabetStrings) `shouldBe` expectedResult

        context "when provided with non-sorted input" $ do
            it "should still return alphabetically sorted" $ do
                -- the do above is needed (vs good style previously) because we have two statements
                let reversedStrings = reverse alphabetStrings
                (everyPossiblePairSorted reversedStrings) `shouldBe` expectedResult
            it "should not sort within each element" $ do
                everyPossiblePairSorted ["abc", "fed", "hgi"] `shouldBe` ["abcfed", "abchgi", "fedhgi"]


    describe "getFirstHalf" $ do

        -- below we group all the lets into one block, vs separate let statements above
        -- also note the where indentation (which is based off of the matchHead insead of the let)
        let matchHeadOfOriginalProperty :: Eq a => [a] -> Bool
            matchHeadOfOriginalProperty xs = all (==True) (doesEachPositionMatch)
                where doesEachPositionMatch = zipWith (==) (getFirstHalf xs) xs
            halfLengthProperty :: [a] -> Bool
            halfLengthProperty xs = length(getFirstHalf xs) == length xs `div` 2

        -- the QuickCheck `property` function cannot be applied on
        -- functions with just abstract type declarations and without concrete types
        -- instead we declare(?) the concrete types below
        context "when applied to strings" $ do
            it "returns something with half the length" $ property $ do
                halfLengthProperty :: String -> Bool
            it "contains all characters from first half" $ property $ do
                matchHeadOfOriginalProperty :: String -> Bool

        context "when applied to a list of Integers" $ do
            it "returns something with half the length" $ property $ do
                halfLengthProperty :: [Integer] -> Bool
            it "contains all characters from first half" $ property $ do
                matchHeadOfOriginalProperty :: [Integer] -> Bool

        context "when applied to a list of Bools" $ do
            it "returns something with half the length" $ property $ do
                halfLengthProperty :: [Bool] -> Bool
            it "contains all characters from first half" $ property $ do
                matchHeadOfOriginalProperty :: [Bool] -> Bool


    describe "isPalindrome" $ do
        let palindromes = ["abcba", "weffew"]
            nonPalindromes = ["aldskj", "qwe", "qe"]
        it "correctly identifies palindromes" $ do
            palindromes `shouldSatisfy` (all isPalindrome)
        it "correctly identifies non-palindromes" $ do
            nonPalindromes `shouldSatisfy` (not . any isPalindrome)


    describe "formatDate" $ do
        let regexPattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
        it "matches a date regex" $ do
            -- why does this have to be here and can't be one level up?
            rightNow <- getCurrentTime
            regexPattern `shouldSatisfy` (=~) (formatDate rightNow)
        it "has the correct dates" $ do
            pendingWith "need to make regex return year, month, day"


    describe "anagrams of words that are real words?" $ do
        it "any anagram can be made from the original chars" $
            pending


    describe "flipKeyVal" $ do
        it "check flipping twice gives original" $ property $ do
            -- this fails because if there is duplicated value, the the keys they become are not unique
            let doubleFlipProperty :: [Int] -> [Int] -> Bool
                doubleFlipProperty ks vs = flipKeyVal (flipKeyVal mapping) == mapping
                    where mapping = Map.fromList $ zip ks vs
            doubleFlipProperty
        it "flipping a mapping with keys == vals does not change it" $ property $ do
            let keyValueSame :: [Int] -> Bool
                keyValueSame ks = (flipKeyVal mapping) == mapping
                    where mapping = Map.fromList $ zip ks ks
            keyValueSame
        it "check flipped mapping == creating a new mapping using swapped key/value" $ property $ do
            -- this fails because if there is duplicated value, the the keys they become are not unique
            let swappedKeyValueProperty :: [Int] -> [Int] -> Bool
                swappedKeyValueProperty ks vs = (flipKeyVal mapping) == reversedMapping
                    where mapping = Map.fromList $ zip ks vs
                          reversedMapping = Map.fromList $ zip vs ks
            swappedKeyValueProperty
