import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.QuickCheck

import qualified Data.ByteString.Lazy                 as B
import           Data.Word                            (Word8)

import           Control.Monad

import qualified Lib

import qualified Set1                                 as S1
import qualified Set1Data                             as S1D

import qualified Set2                                 as S2
import qualified Set2Data                             as S2D


main :: IO ()
main = defaultMain
    [ testCase "Set 1 Challenge 1" test_S1C1
    , testCase "Set 1 Challenge 2" test_S1C2
    , testCase "Set 1 Challenge 3" test_S1C3
    , testCase "Set 1 Challenge 4" test_S1C4
    , testCase "Set 1 Challenge 5" test_S1C5
    , testCase "S1C6 hamming distance" test_S1C6_hamming
    , testCase "Set 1 Challenge 6" test_S1C6
    , testCase "Set 1 Challenge 7" test_S1C7
    , testCase "Set 1 Challenge 8" test_S1C8
    , testCase "Set 2 Challenge 9" test_S2C9
    , testCase "Set 2 Challenge 10" test_S2C10
    , testProperty "Set 2 Challenge 11" test_S2C11
    , testCase "Set 2 Challenge 12 - fixed string" test_S2C12_fixed
    , testProperty "Set 2 Challenge 12 - random data" test_S2C12_random
    ]

test_S1C1 :: Assertion
test_S1C1 = assertEqual "Set 1 Challenge 1" output result
    where result = S1.challenge1 input
          input = S1D.challenge1Input
          output = S1D.challenge1Output

test_S1C2 :: Assertion
test_S1C2 = assertEqual "Set 1 Challenge 2" output result
  where result = S1.challenge2 input
        input = S1D.challenge2Input
        output = S1D.challenge2Output

test_S1C3 :: Assertion
test_S1C3 = assertEqual "Set 1 Challenge 3" output result
  where result = S1.challenge3 input
        input = S1D.challenge3Input
        output = S1D.challenge3Output

test_S1C4 :: Assertion
test_S1C4 = assertEqual "Set 1 Challenge 4" output result
  where result = S1.challenge4 input
        input = S1D.challenge4Input
        output = S1D.challenge4Output

test_S1C5 :: Assertion
test_S1C5 = assertEqual "Set 1 Challenge 5" output result
  where result = S1.challenge5 input
        input = S1D.challenge5Input
        output = S1D.challenge5Output

test_S1C6_hamming :: Assertion
test_S1C6_hamming = assertEqual "Hamming distance" 37 $ Lib.hammingDistance (Lib.stringToBytes "this is a test") (Lib.stringToBytes "wokka wokka!!!")

test_S1C6 :: Assertion
test_S1C6 = assertEqual "Set 1 Challenge 6" output result
    where result = S1.challenge6 input
          input = S1D.challenge6Input
          output = S1D.challenge6Output

test_S1C7 :: Assertion
test_S1C7 = assertEqual "Set 1 Challenge 7" output result
    where result = S1.challenge7 input
          input = S1D.challenge7Input
          output = S1D.challenge7Output


test_S1C8 :: Assertion
test_S1C8 = assertEqual "Set 1 Challenge 8" output result
  where result = S1.challenge8 input
        input = S1D.challenge8Input
        output = S1D.challenge8Output


test_S2C9 :: Assertion
test_S2C9 = assertEqual "Set 2 Challenge 9" output result
  where result = S2.challenge9 input
        input = S2D.challenge9Input
        output = S2D.challenge9Output

test_S2C10 :: Assertion
test_S2C10 = assertEqual "Set 2 Challenge 10" output result
  where result = S2.challenge10 input
        input = S2D.challenge10Input
        output = S2D.challenge10Output

test_S2C11 :: Property
test_S2C11 = forAll (listOfWord8s 57) $ \rand ->
                    case str rand
                    of  "Guessed ECB\nActual  ECB\n" -> True
                        "Guessed CBC\nActual  CBC\n" -> True
                        _                            -> False
  where str rand = case S2.challenge11 [B.pack rand]
                of Left (x, _, _) -> "FAILED: " ++ x
                   Right x        -> Lib.bytesToString x

test_S2C12_fixed :: Assertion
test_S2C12_fixed = assertEqual "Set 2 Challenge 12" output result
 where result = S2.challenge12 input
       input = S2D.challenge12Input
       output = S2D.challenge12Output


test_S2C12_random :: Property
test_S2C12_random = forAll (listOfWord8s 256) $ \rand ->
                       case S2.challenge12 [B.pack $ take 16 rand, plaintext rand]
                         of Right x -> x == Lib.base64ToBytes (plaintext rand)
                            _       -> False
  where plaintext rand = Lib.bytesToBase64 . B.pack $ drop 16 rand


-- Generators
listOfWord8s :: Int -> Gen [Word8]
listOfWord8s n = replicateM n arbitrary
