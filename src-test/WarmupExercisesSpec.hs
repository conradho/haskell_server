module WarmupExercisesSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (property, listOf1, elements, forAll)
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

    describe "selectElementsOfListStartingWithA" $ do
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
            -- note: prop is just a shortcut for it + property.
            -- here you always need to put `do` at the end
            prop "returns something with half the length" $ do
                halfLengthProperty :: [Integer] -> Bool
            prop "contains all characters from first half" $ do
                matchHeadOfOriginalProperty :: [Integer] -> Bool

        context "when applied to a list of Bools" $ do
            prop "returns something with half the length" $ do
                halfLengthProperty :: [Bool] -> Bool
            prop "contains all characters from first half" $ do
                matchHeadOfOriginalProperty :: [Bool] -> Bool


    describe "isPalindrome" $ do
        let palindromes = ["abcba", "weffew"]
            nonPalindromes = ["aldskj", "qwe", "qe"]
            randomWord = listOf1 $ elements ['a'..'z'] -- creates quickcheck testcase generator
        it "correctly identifies palindromes" $ do
            palindromes `shouldSatisfy` (all isPalindrome)
        it "correctly identifies non-palindromes" $ do
            nonPalindromes `shouldSatisfy` (not . any isPalindrome)
        it "does blah" $
            -- specify your own testcase generator (randomWord) for \word
            -- note it does not actually exhaustively test all combinations (obviously)
            -- forAll already wraps the `property` part (converts a function returning Bool into a quickcheck Property)
            forAll randomWord $ \word -> not $ isPalindrome word


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

        -- change to MOAR quickcheck iterations
        modifyMaxSuccess (const 50) $ prop "hilo" $ do
            ((\xs -> xs == xs) :: [Int] -> Bool)


    describe "flipKeyVal" $ do
        prop "check flipping twice gives original" $ do
            -- this fails because if there is duplicated value, the the keys they become are not unique
            let doubleFlipProperty :: [Int] -> [Int] -> Bool
                doubleFlipProperty ks vs = flipKeyVal (flipKeyVal mapping) == mapping
                    where mapping = Map.fromList $ zip ks vs
            doubleFlipProperty
        prop "flipping a mapping with keys == vals does not change it" $ do
            let keyValueSame :: [Int] -> Bool
                keyValueSame ks = (flipKeyVal mapping) == mapping
                    where mapping = Map.fromList $ zip ks ks
            keyValueSame
        prop "check flipped mapping == creating a new mapping using swapped key/value" $ do
            -- this fails because if there is duplicated value, the the keys they become are not unique
            let swappedKeyValueProperty :: [Int] -> [Int] -> Bool
                swappedKeyValueProperty ks vs = (flipKeyVal mapping) == reversedMapping
                    where mapping = Map.fromList $ zip ks vs
                          reversedMapping = Map.fromList $ zip vs ks
            swappedKeyValueProperty
