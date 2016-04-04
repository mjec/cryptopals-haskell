module Set1
    ( challenge1
    , challenge2
    , challenge3
    , challenge4
    ) where

import Lib
import Help

import System.IO

import Numeric (showHex)

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Word
import Data.Bits
import Data.List

-- Challenge 1
challenge1 :: [String] -> Either Error String
challenge1 (x:[])
    | isHex x = Right $ (hex_to_base64 x) ++ "\n"
    | otherwise = Left ("Your hex string must be actual hex i.e. [A-Za-z0-9]+", ["1-1"], True)
challenge1 [] = Left ("You need to supply exactly one hex string", ["1-1"], True)
--
-- challenge1_test :: String -> String
-- challenge1_test = hex_to_base64

hex_to_base64 :: String -> String
hex_to_base64 = bytes_to_base64 . hex_to_bytes


-- Challenge 2
challenge2 :: [String] -> Either Error String
challenge2 (x:y:[])
    | isHex (x ++ y) = Right $ bytes_to_hex (bitwiseCombine xor (hex_to_bytes x) (hex_to_bytes y)) ++ "\n"
    | otherwise = Left ("Your hex strings must be actual hex i.e. [A-Za-z0-9]+", ["1-2"], True)
challenge2 _ = Left ("You need to supply two hex strings to xor together", ["1-2"], True)


-- Challenge 3
challenge3 :: [String] -> Either Error String
challenge3 (x:y:[])
    | not $ all (`elem` ['0'..'9']) y = Left ("The number of results to show must be a positive integer", ["1-3"], True)
    | not $ yInt > 0 = Left ("The number of results to show must be a positive integer", ["1-3"], True)
    | isHex x = Right $ hex_string_to_guesses x yInt
    | otherwise = Left ("Your hex string must be actual hex i.e. [A-Za-z0-9]+ and the number to show must be a positive integer", ["1-3"], True)
    where yInt = read y::Int
challenge3 (x:[])
    | isHex x = Right $ (hex_string_to_best_guess x) ++ "\n"
    | otherwise = Left ("Your hex string must be actual hex i.e. [A-Za-z0-9]+", ["1-3"], True)
challenge3 _ = Left ("You need to supply eactly one hex string and an optional number", ["1-3"], True)

hex_string_to_guesses :: String -> Int -> String
hex_string_to_guesses x y = unlines $ take y $ list_of_best_guesses probabilities bs
    where probabilities = single_byte_xor_probabilities bs
          bs = hex_to_bytes x

hex_string_to_best_guess :: String -> String
hex_string_to_best_guess x = bytes_to_string $ best_guess bs
    where best_guess = single_byte_xor $ fst $ minimumBy (\(_, x) (_, y) -> compare x y) probabilities
          probabilities = single_byte_xor_probabilities bs
          bs = hex_to_bytes x

single_byte_xor :: Word8 -> B.ByteString -> B.ByteString
single_byte_xor x str = bitwiseCombine xor str (B.pack (repeat x))

list_of_best_guesses :: [(Word8, Float)] -> B.ByteString -> [String]
list_of_best_guesses l bs =
        [  (showHex (keyof x) "")
        ++ ":"
        ++ (show (deltaof x))
        ++ ":"
        ++ (bytes_to_string (plainof x))
        |  x <- ordered_guesses
        ]
    where ordered_guesses = [(fst x, snd x, single_byte_xor (fst x) bs) | x <- (sortOn (\x -> snd x) l)]
          keyof   (x, _, _) = x
          deltaof (_, x, _) = x
          plainof (_, _, x) = x

single_byte_xor_probabilities :: B.ByteString -> [(Word8, Float)]
single_byte_xor_probabilities str =
        [ (x, y)
        | x <- [0::Word8 .. 0xff]
        , let y = (fst (xor_freq_table x)) + (freqTableDelta asciiFreqTableNoNL $ snd (xor_freq_table x))
        ]
    where letter_freq = buildFreqTable $ Map.keys asciiFreqTableNoNL
          -- xor_freq_table x = letter_freq $ single_byte_xor x (bitwiseCombine (.&.) str (B.pack (repeat (complement (32::Word8)))))
          xor_freq_table x = letter_freq $ single_byte_xor x str


-- Challenge 4
challenge4 :: [String] -> Either Error String
challenge4 inputLines
    | null inputLines = Left ("You need to supply hex strings to standard input i.e. [A-Za-z0-9]+ and only one per line", ["1-4"], True)
    | all isHex inputLines = Right $ bytes_to_string $ single_byte_xor (fst $ snd absolute_best) (fst absolute_best)
    | otherwise = Left ("You need to supply hex strings to standard input i.e. [A-Za-z0-9]+ and only one per line", ["1-4"], True)
    where absolute_best = minimumBy (\(_, (_, x)) (_, (_, y)) -> compare x y) [(hex_to_bytes x, best_guess x) | x <- inputLines]
          best_guess = (minimumBy (\(_, x) (_, y) -> compare x y)) . single_byte_xor_probabilities . hex_to_bytes
