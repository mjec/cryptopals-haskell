import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import qualified Data.ByteString.Lazy           as B
import           Data.Word                      (Word8)

import qualified Lib
import qualified Set1                           as S1


main :: IO ()
main = defaultMain
    [ testCase "Set 1 Challenge 1" test_S1C1
    , testCase "Set 1 Challenge 2" test_S1C2
    , testCase "Set 1 Challenge 3" test_S1C3
    , testCase "Set 1 Challenge 4" test_S1C4
    , testCase "Set 1 Challenge 5" test_S1C5
    ]

test_S1C1 :: Assertion
test_S1C1 = assertEqual "Set 1 Challenge 1" output result
    where result = S1.challenge1 input
          input = map Lib.stringToBytes ["49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"]
          output = Right $ Lib.stringToBytes "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t\n"

test_S1C2 :: Assertion
test_S1C2 = assertEqual "Set 1 Challenge 2" output result
  where result = S1.challenge2 input
        input = map Lib.stringToBytes ["1c0111001f010100061a024b53535009181c", "686974207468652062756c6c277320657965"]
        output = Right $ Lib.stringToBytes "746865206b696420646f6e277420706c6179\n"

test_S1C3 :: Assertion
test_S1C3 = assertEqual "Set 1 Challenge 3" output result
  where result = S1.challenge3 input
        input = map Lib.stringToBytes ["1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"]
        output = Right $ Lib.stringToBytes "Cooking MC's like a pound of bacon\n"

test_S1C4 :: Assertion
test_S1C4 = assertEqual "Set 1 Challenge 4" output result
  where result = S1.challenge4 input
        input = map Lib.stringToBytes [ "7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f"
                , "0864eb4935144c501103a71851370719301bec57093a0929ea3f18060e55"
                , "2d395e57143359e80efffb13330633ea19e323077b4814571e5a3de73a1f"
                , "52e73c1d53330846243c422d3e1b374b5209543903e3195c041c251b7c04"
                , "2f3c2c28273a12520b482f18340d565d1fe84735474f4a012e1a13502523"
                , "23340f39064e306a08194d544647522e1443041d5ee81f5a18415e34a45f"
                , "475a392637565757730a0c4a517b2821040e1709e028071558021f164c54"
                , "100b2135190505264254005618f51152136125370eef27383e45350118ed"
                , "3947452914e0223f1d040943313c193f295b221e573e1b5723391d090d1f"
                , "2c33141859392b04155e3d4e393b322526ee3e581d1b3d6817374d0c085b"
                ]
        output = Right $ Lib.stringToBytes "Now that the party is jumping\n"

test_S1C5 :: Assertion
test_S1C5 = assertEqual "Set 1 Challenge 5" output result
  where result = S1.challenge5 input
        input = map Lib.stringToBytes [ "ICE"
                , "Burning 'em, if you ain't quick and nimble"
                , "I go crazy when I hear a cymbal"
                ]
        output = Right $ Lib.stringToBytes $ unlines
            [ "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"
            , "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
            ]
