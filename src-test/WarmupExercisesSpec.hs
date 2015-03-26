module WarmupExercisesSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary
    , arbitrary
    , choose
    , property
    , listOf
    , listOf1
    , elements
    , forAll
    , suchThat
    )
import Text.Regex.Posix ((=~))

import Data.Time -- (getCurrentTime, fromGregorian)
-- somehow the UTCTime import exists in both Data.Time and Data.Time.Clock
-- and trying to specify it fails
-- import Data.Time.Clock as DTC (UTCTime)

import Data.List (intercalate)

import Text.Printf (printf)
import qualified Data.Map as Map
import Control.Monad (liftM, liftM2)

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

stringifyDate :: Integer -> Int -> Int -> String
-- all the zipping/mapping etc require lists, which need homogenous elements
-- so turn year into Num type first
stringifyDate y m d = intercalate "-" $
    zipWith printf ["%04d", "%02d" , "%02d"] [fromIntegral y, m, d]

-- don't need to do `data RandomYear = same thing`
newtype RandomYear = RandomYear Integer deriving (Show)
instance Arbitrary RandomYear where
    arbitrary = RandomYear `liftM` choose (1000, 3000)

newtype RandomMonth = RandomMonth Int deriving (Show)
instance Arbitrary RandomMonth where
    arbitrary = RandomMonth `liftM` choose (1, 12)

newtype RandomDay = RandomDay Int deriving (Show)
instance Arbitrary RandomDay where
    -- otherwise Feb 31 etc would fails
    -- should rewrite so that a single generator makes year month date
    arbitrary = RandomDay `liftM` choose (1, 28)

newtype RandomNonPalindromeWord = RandomNonPalindromeWord String deriving (Show)
instance Arbitrary RandomNonPalindromeWord where
    arbitrary = RandomNonPalindromeWord `liftM` (
        (listOf1 $ elements ['a'..'z'])
            -- need listOf1 above because otherwise cannot split into (x:xs) below
            -- otherwise it will say `non-exhaustive pattern` and the code compiles
            -- but the specific test will fail
            `suchThat` \all@(x:xs) ->
                (length all > 3) && (x /= last xs)
        )  -- note the weird place of closing bracket


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

        -- the QuickCheck cannot generate arbitrary values for abstract types
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

        context "when given certain example data" $ do
            let palindromes = ["abcba", "weffew"]
                nonPalindromes = ["aldskj", "qwe", "qe"]
            it "correctly identifies palindromes" $ do
                palindromes `shouldSatisfy` (all isPalindrome)
            it "correctly identifies nonpalindromes" $ do
                nonPalindromes `shouldSatisfy` (not . any isPalindrome)

        it "does blah" $ do
            -- create quickcheck testcase generator;
            -- excludes empty list, add ! at the end to make it not a palindrome
            let randomWord = liftM2 (++) (listOf1 $ elements ['a'..'z']) (elements ["!"])
            -- specify your own testcase generator (randomWord) for \word
            -- note it does not actually exhaustively test all combinations (just tries maxSuccess times)
            -- forAll already wraps the `property` part (converts a function returning Bool into a quickcheck Property)
            forAll randomWord $ \word -> not $ isPalindrome word

        prop "correctly identifies non-palindromes FOR ALLS" $ do
            \(RandomNonPalindromeWord word) -> not $ isPalindrome word


    describe "formatDate" $ do
        let regexPattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
        it "matches a date regex" $ do
            -- why does this have to be here and can't be one level up?
            rightNow <- getCurrentTime
            regexPattern `shouldSatisfy` (=~) (formatDate rightNow)
        it "has the correct dates" $ property $ do
                let propCorrectDate :: Integer -> Int -> Int -> Bool
                    propCorrectDate y m d =
                        stringDate == formatDate randomUTC
                            where stringDate = stringifyDate y m d
                                  -- UTCTime takes Day type and also needs a seconds from 00:00
                                  randomUTC = UTCTime (fromGregorian y m d) (fromIntegral 123)
                \(RandomYear y) (RandomMonth m) (RandomDay d) -> propCorrectDate y m d


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
