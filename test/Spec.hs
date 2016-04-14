import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import qualified Data.ByteString.Lazy           as B
import           Data.Word                      (Word8)

import qualified Lib
import qualified Set1                           as S1
import qualified Set1Data                       as S1D


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
