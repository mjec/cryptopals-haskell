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
    , testCase "Hamming distance" test_S1C6_hamming
    , testCase "Set 1 Challenge 6" test_S1C6
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
        input = [Lib.stringToBytes $ unlines [ "7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f"
                , "0864eb4935144c501103a71851370719301bec57093a0929ea3f18060e55"
                , "2d395e57143359e80efffb13330633ea19e323077b4814571e5a3de73a1f"
                , "52e73c1d53330846243c422d3e1b374b5209543903e3195c041c251b7c04"
                , "2f3c2c28273a12520b482f18340d565d1fe84735474f4a012e1a13502523"
                , "23340f39064e306a08194d544647522e1443041d5ee81f5a18415e34a45f"
                , "475a392637565757730a0c4a517b2821040e1709e028071558021f164c54"
                , "100b2135190505264254005618f51152136125370eef27383e45350118ed"
                , "3947452914e0223f1d040943313c193f295b221e573e1b5723391d090d1f"
                , "2c33141859392b04155e3d4e393b322526ee3e581d1b3d6817374d0c085b"
                ]]
        output = Right $ Lib.stringToBytes "Now that the party is jumping\n"

test_S1C5 :: Assertion
test_S1C5 = assertEqual "Set 1 Challenge 5" output result
  where result = S1.challenge5 input
        input = map Lib.stringToBytes [ "ICE"
                , "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
                ]
        output = Right $ Lib.stringToBytes "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f\n"

test_S1C6_hamming :: Assertion
test_S1C6_hamming = assertEqual "Hamming distance" 37 $ Lib.hammingDistance (Lib.stringToBytes "this is a test") (Lib.stringToBytes "wokka wokka!!!")

test_S1C6 :: Assertion
test_S1C6 = assertEqual "Set 1 Challenge 6" output result
    where result = S1.challenge6 input
          input = [Lib.stringToBytes $ unlines
                [ "HUIfTQsPAh9PE048GmllH0kcDk4TAQsHThsBFkU2AB4BSWQgVB0dQzNTTmVS"
                , "BgBHVBwNRU0HBAxTEjwMHghJGgkRTxRMIRpHKwAFHUdZEQQJAGQmB1MANxYG"
                , "DBoXQR0BUlQwXwAgEwoFR08SSAhFTmU+Fgk4RQYFCBpGB08fWXh+amI2DB0P"
                , "QQ1IBlUaGwAdQnQEHgFJGgkRAlJ6f0kASDoAGhNJGk9FSA8dDVMEOgFSGQEL"
                , "QRMGAEwxX1NiFQYHCQdUCxdBFBZJeTM1CxsBBQ9GB08dTnhOSCdSBAcMRVhI"
                , "CEEATyBUCHQLHRlJAgAOFlwAUjBpZR9JAgJUAAELB04CEFMBJhAVTQIHAh9P"
                , "G054MGk2UgoBCVQGBwlTTgIQUwg7EAYFSQ8PEE87ADpfRyscSWQzT1QCEFMa"
                , "TwUWEXQMBk0PAg4DQ1JMPU4ALwtJDQhOFw0VVB1PDhxFXigLTRkBEgcKVVN4"
                , "Tk9iBgELR1MdDAAAFwoFHww6Ql5NLgFBIg4cSTRWQWI1Bk9HKn47CE8BGwFT"
                , "QjcEBx4MThUcDgYHKxpUKhdJGQZZVCFFVwcDBVMHMUV4LAcKQR0JUlk3TwAm"
                , "HQdJEwATARNFTg5JFwQ5C15NHQYEGk94dzBDADsdHE4UVBUaDE5JTwgHRTkA"
                , "Umc6AUETCgYAN1xGYlUKDxJTEUgsAA0ABwcXOwlSGQELQQcbE0c9GioWGgwc"
                , "AgcHSAtPTgsAABY9C1VNCAINGxgXRHgwaWUfSQcJABkRRU8ZAUkDDTUWF01j"
                , "OgkRTxVJKlZJJwFJHQYADUgRSAsWSR8KIgBSAAxOABoLUlQwW1RiGxpOCEtU"
                , "YiROCk8gUwY1C1IJCAACEU8QRSxORTBSHQYGTlQJC1lOBAAXRTpCUh0FDxhU"
                ]]
          output = Right $ Lib.stringToBytes $ unlines
              [ "I'm back and I'm ringin' the bell "
              , "A rockin' on the mike while the fly girls yell "
              , "In ecstasy in the back of me "
              , "Well that's my DJ Deshay cuttin' all them Z's "
              , "Hittin' hard and the girlies goin' crazy "
              , "Vanilla's on the mike, man I'm not lazy. "
              , ""
              , "I'm lettin' my drug kick in "
              , "It controls my mouth and I begin "
              , "To just let it flow, let my concepts go "
              , "My posse's to the side yellin', Go Vanilla Go! "
              , ""
              , "Smooth 'cause that's the way I will be "
              , "And if you don't give a damn, then "
              , "Why you starin' at me "
              , "So get off 'cause I control the stage "
              , "There's no dissin' allowed "
              , "I'm in my own phase "
              , "The girlies sa y they love me and that is ok "
              , "And I can dance better than any kid n' play "]
