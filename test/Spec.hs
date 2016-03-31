import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Set1 as S1

main :: IO ()
main = defaultMainWithOpts
    [ testCase "Set 1 Challenge 1" test_S1C1
    , testCase "Set 1 Challenge 2" test_S1C2
    , testCase "Set 1 Challenge 3" test_S1C3
    ] mempty

test_S1C1 :: Assertion
test_S1C1 = assertEqual "Set 1 Challenge 1" output result
    where result = S1.challenge1 input
          input = ["49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"]
          output = Right "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t\n"

test_S1C2 :: Assertion
test_S1C2 = assertEqual "Set 1 Challenge 2" output result
  where result = S1.challenge2 input
        input = ["1c0111001f010100061a024b53535009181c", "686974207468652062756c6c277320657965"]
        output = Right "746865206b696420646f6e277420706c6179\n"

test_S1C3 :: Assertion
test_S1C3 = assertEqual "Set 1 Challenge 3" output result
  where result = S1.challenge3 input
        input = ["1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"]
        output = Right "Cooking MC's like a pound of bacon\n"
