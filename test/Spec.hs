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
    , testCase "S1C6 hamming distance" test_S1C6_hamming
    , testCase "Set 1 Challenge 6" test_S1C6
    , testCase "Set 1 Challenge 7" test_S1C7
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


test_S1C7 :: Assertion
test_S1C7 = assertEqual "Set 1 Challenge 7" output result
    where result = S1.challenge7 input
          input = map Lib.stringToBytes [ "YELLOW SUBMARINE",
                    unlines
                    [ "CRIwqt4+szDbqkNY+I0qbDe3LQz0wiw0SuxBQtAM5TDdMbjCMD/venUDW9BL"
                    , "PEXODbk6a48oMbAY6DDZsuLbc0uR9cp9hQ0QQGATyyCESq2NSsvhx5zKlLtz"
                    , "dsnfK5ED5srKjK7Fz4Q38/ttd+stL/9WnDzlJvAo7WBsjI5YJc2gmAYayNfm"
                    , "CW2lhZE/ZLG0CBD2aPw0W417QYb4cAIOW92jYRiJ4PTsBBHDe8o4JwqaUac6"
                    , "rqdi833kbyAOV/Y2RMbN0oDb9Rq8uRHvbrqQJaJieaswEtMkgUt3P5Ttgeh7"
                    , "J+hE6TR0uHot8WzHyAKNbUWHoi/5zcRCUipvVOYLoBZXlNu4qnwoCZRSBgvC"
                    , "wTdz3Cbsp/P2wXB8tiz6l9rL2bLhBt13Qxyhhu0H0+JKj6soSeX5ZD1Rpilp"
                    , "9ncR1tHW8+uurQKyXN4xKeGjaKLOejr2xDIw+aWF7GszU4qJhXBnXTIUUNUf"
                    , "RlwEpS6FZcsMzemQF30ezSJHfpW7DVHzwiLyeiTJRKoVUwo43PXupnJXDmUy"
                    , "sCa2nQz/iEwyor6kPekLv1csm1Pa2LZmbA9Ujzz8zb/gFXtQqBAN4zA8/wt0"
                    , "VfoOsEZwcsaLOWUPtF/Ry3VhlKwXE7gGH/bbShAIKQqMqqUkEucZ3HPHAVp7"
                    , "ZCn3Ox6+c5QJ3Uv8V7L7SprofPFN6F+kfDM4zAc59do5twgDoClCbxxG0L19"
                    , "TBGHiYP3CygeY1HLMrX6KqypJfFJW5O9wNIF0qfOC2lWFgwayOwq41xdFSCW"
                    , "0/EBSc7cJw3N06WThrW5LimAOt5L9c7Ik4YIxu0K9JZwAxfcU4ShYu6euYmW"
                    , "LP98+qvRnIrXkePugS9TSOJOHzKUoOcb1/KYd9NZFHEcp58Df6rXFiz9DSq8"
                    , "0rR5Kfs+M+Vuq5Z6zY98/SP0A6URIr9NFu+Cs9/gf+q4TRwsOzRMjMQzJL8f"
                    , "7TXPEHH2+qEcpDKz/5pE0cvrgHr63XKu4XbzLCOBz0DoFAw3vkuxGwJq4Cpx"
                    , "kt+eCtxSKUzNtXMn/mbPqPl4NZNJ8yzMqTFSODS4bYTBaN/uQYcOAF3NBYFd"
                    , "5x9TzIAoW6ai13a8h/s9i5FlVRJDe2cetQhArrIVBquF0L0mUXMWNPFKkaQE"
                    , "BsxpMCYh7pp7YlyCNode12k5jY1/lc8jQLQJ+EJHdCdM5t3emRzkPgND4a7O"
                    , "NhoIkUUS2R1oEV1toDj9iDzGVFwOvWyt4GzA9XdxT333JU/n8m+N6hs23MBc"
                    , "Z086kp9rJGVxZ5f80jRz3ZcjU6zWjR9ucRyjbsuVn1t4EJEm6A7KaHm13m0v"
                    , "wN/O4KYTiiY3aO3siayjNrrNBpn1OeLv9UUneLSCdxcUqjRvOrdA5NYv25Hb"
                    , "4wkFCIhC/Y2ze/kNyis6FrXtStcjKC1w9Kg8O25VXB1Fmpu+4nzpbNdJ9LXa"
                    , "hF7wjOPXN6dixVKpzwTYjEFDSMaMhaTOTCaqJig97624wv79URbCgsyzwaC7"
                    , "YXRtbTstbFuEFBee3uW7B3xXw72mymM2BS2uPQ5NIwmacbhta8aCRQEGqIZ0"
                    , "78YrrOlZIjar3lbTCo5o6nbbDq9bvilirWG/SgWINuc3pWl5CscRcgQQNp7o"
                    , "LBgrSkQkv9AjZYcvisnr89TxjoxBO0Y93jgp4T14LnVwWQVx3l3d6S1wlsci"
                    , "dVeaM24E/JtS8k9XAvgSoKCjyiqsawBMzScXCIRCk6nqX8ZaJU3rZ0LeOMTU"
                    , "w6MC4dC+aY9SrCvNQub19mBdtJUwOBOqGdfd5IoqQkaL6DfOkmpnsCs5PuLb"
                    , "GZBVhah5L87IY7r6TB1V7KboXH8PZIYc1zlemMZGU0o7+etxZWHgpdeX6JbJ"
                    , "Is3ilAzYqw/Hz65no7eUxcDg1aOaxemuPqnYRGhW6PvjZbwAtfQPlofhB0jT"
                    , "Ht5bRlzF17rn9q/6wzlc1ssp2xmeFzXoxffpELABV6+yj3gfQ/bxIB9NWjdZ"
                    , "K08RX9rjm9CcBlRQeTZrD67SYQWqRpT5t7zcVDnx1s7ZffLBWm/vXLfPzMaQ"
                    , "YEJ4EfoduSutjshXvR+VQRPs2TWcF7OsaE4csedKUGFuo9DYfFIHFDNg+1Py"
                    , "rlWJ0J/X0PduAuCZ+uQSsM/ex/vfXp6Z39ngq4exUXoPtAIqafrDMd8SuAty"
                    , "EZhyY9V9Lp2qNQDbl6JI39bDz+6pDmjJ2jlnpMCezRK89cG11IqiUWvIPxHj"
                    , "oiT1guH1uk4sQ2Pc1J4zjJNsZgoJDcPBbfss4kAqUJvQyFbzWshhtVeAv3dm"
                    , "gwUENIhNK/erjpgw2BIRayzYw001jAIF5c7rYg38o6x3YdAtU3d3QpuwG5xD"
                    , "fODxzfL3yEKQr48C/KqxI87uGwyg6H5gc2AcLU9JYt5QoDFoC7PFxcE3RVqc"
                    , "7/Um9Js9X9UyriEjftWt86/tEyG7F9tWGxGNEZo3MOydwX/7jtwoxQE5ybFj"
                    , "WndqLp8DV3naLQsh/Fz8JnTYHvOR72vuiw/x5D5PFuXV0aSVvmw5Wnb09q/B"
                    , "owS14WzoHH6ekaWbh78xlypn/L/M+nIIEX1Ol3TaVOqIxvXZ2sjm86xRz0Ed"
                    , "oHFfupSekdBULCqptxpFpBshZFvauUH8Ez7wA7wjL65GVlZ0f74U7MJVu9Sw"
                    , "sZdgsLmnsQvr5n2ojNNBEv+qKG2wpUYTmWRaRc5EClUNfhzh8iDdHIsl6edO"
                    , "ewORRrNiBay1NCzlfz1cj6VlYYQUM9bDEyqrwO400XQNpoFOxo4fxUdd+AHm"
                    , "CBhHbyCR81/C6LQTG2JQBvjykG4pmoqnYPxDyeiCEG+JFHmP1IL+jggdjWhL"
                    , "WQatslrWxuESEl3PEsrAkMF7gt0dBLgnWsc1cmzntG1rlXVi/Hs2TAU3RxEm"
                    , "MSWDFubSivLWSqZj/XfGWwVpP6fsnsfxpY3d3h/fTxDu7U8GddaFRQhJ+0ZO"
                    , "dx6nRJUW3u6xnhH3mYVRk88EMtpEpKrSIWfXphgDUPZ0f4agRzehkn9vtzCm"
                    , "NjFnQb0/shnqTh4Mo/8oommbsBTUKPYS7/1oQCi12QABjJDt+LyUan+4iwvC"
                    , "i0k0IUIHvk21381vC0ixYDZxzY64+xx/RNID+iplgzq9PDZgjc8L7jMg+2+m"
                    , "rxPS56e71m5E2zufZ4d+nFjIg+dHD/ShNPzVpXizRVUERztLuak8Asah3/yv"
                    , "wOrH1mKEMMGC1/6qfvZUgFLJH5V0Ep0n2K/Fbs0VljENIN8cjkCKdG8aBnef"
                    , "EhITdV7CVjXcivQ6efkbOQCfkfcwWpaBFC8tD/zebXFE+JshW16D4EWXMnSm"
                    , "/9HcGwHvtlAj04rwrZ5tRvAgf1IR83kqqiTvqfENcj7ddCFwtNZrQK7EJhgB"
                    , "5Tr1tBFcb9InPRtS3KYteYHl3HWR9t8E2YGE8IGrS1sQibxaK/C0kKbqIrKp"
                    , "npwtoOLsZPNbPw6K2jpko9NeZAx7PYFmamR4D50KtzgELQcaEsi5aCztMg7f"
                    , "p1mK6ijyMKIRKwNKIYHagRRVLNgQLg/WTKzGVbWwq6kQaQyArwQCUXo4uRty"
                    , "zGMaKbTG4dns1OFB1g7NCiPb6s1lv0/lHFAF6HwoYV/FPSL/pirxyDSBb/FR"
                    , "RA3PIfmvGfMUGFVWlyS7+O73l5oIJHxuaJrR4EenzAu4Avpa5d+VuiYbM10a"
                    , "LaVegVPvFn4pCP4U/Nbbw4OTCFX2HKmWEiVBB0O3J9xwXWpxN1Vr5CDi75Fq"
                    , "NhxYCjgSJzWOUD34Y1dAfcj57VINmQVEWyc8Tch8vg9MnHGCOfOjRqp0VGyA"
                    , "S15AVD2QS1V6fhRimJSVyT6QuGb8tKRsl2N+a2Xze36vgMhw7XK7zh//jC2H"
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
                , "And I can dance better than any kid n' play "
                , ""
                , "Stage 2 -- Yea the one ya' wanna listen to "
                , "It's off my head so let the beat play through "
                , "So I can funk it up and make it sound good "
                , "1-2-3 Yo -- Knock on some wood "
                , "For good luck, I like my rhymes atrocious "
                , "Supercalafragilisticexpialidocious "
                , "I'm an effect and that you can bet "
                , "I can take a fly girl and make her wet. "
                , ""
                , "I'm like Samson -- Samson to Delilah "
                , "There's no denyin', You can try to hang "
                , "But you'll keep tryin' to get my style "
                , "Over and over, practice makes perfect "
                , "But not if you're a loafer. "
                , ""
                , "You'll get nowhere, no place, no time, no girls "
                , "Soon -- Oh my God, homebody, you probably eat "
                , "Spaghetti with a spoon! Come on and say it! "
                , ""
                , "VIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino "
                , "Intoxicating so you stagger like a wino "
                , "So punks stop trying and girl stop cryin' "
                , "Vanilla Ice is sellin' and you people are buyin' "
                , "'Cause why the freaks are jockin' like Crazy Glue "
                , "Movin' and groovin' trying to sing along "
                , "All through the ghetto groovin' this here song "
                , "Now you're amazed by the VIP posse. "
                , ""
                , "Steppin' so hard like a German Nazi "
                , "Startled by the bases hittin' ground "
                , "There's no trippin' on mine, I'm just gettin' down "
                , "Sparkamatic, I'm hangin' tight like a fanatic "
                , "You trapped me once and I thought that "
                , "You might have it "
                , "So step down and lend me your ear "
                , "'89 in my time! You, '90 is my year. "
                , ""
                , "You're weakenin' fast, YO! and I can tell it "
                , "Your body's gettin' hot, so, so I can smell it "
                , "So don't be mad and don't be sad "
                , "'Cause the lyrics belong to ICE, You can call me Dad "
                , "You're pitchin' a fit, so step back and endure "
                , "Let the witch doctor, Ice, do the dance to cure "
                , "So come up close and don't be square "
                , "You wanna battle me -- Anytime, anywhere "
                , ""
                , "You thought that I was weak, Boy, you're dead wrong "
                , "So come on, everybody and sing this song "
                , ""
                , "Say -- Play that funky music Say, go white boy, go white boy go "
                , "play that funky music Go white boy, go white boy, go "
                , "Lay down and boogie and play that funky music till you die. "
                , ""
                , "Play that funky music Come on, Come on, let me hear "
                , "Play that funky music white boy you say it, say it "
                , "Play that funky music A little louder now "
                , "Play that funky music, white boy Come on, Come on, Come on "
                ] ++ "Play that funky music \n\EOT\EOT\EOT\EOT"
