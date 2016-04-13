module Set1
    ( challenge1
    , challenge2
    , challenge3
    , challenge4
    , challenge5
    ) where

import           Help
import           Lib

import           System.IO

import           Numeric              (showHex)

import           Data.Bits
import qualified Data.ByteString.Lazy as B
import           Data.List
import qualified Data.Map             as Map
import           Data.Word

-- Challenge 1
challenge1 :: [B.ByteString] -> Either Error B.ByteString
challenge1 [x]
    | isHex x = Right $ plusNL $ hexToBase64 x
    | otherwise = Left ("Your hex string must be actual hex i.e. [A-Za-z0-9]+", [stringToBytes "1-1"], True)
challenge1 [] = Left ("You need to supply exactly one hex string", [stringToBytes "1-1"], True)

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 = bytesToBase64 . hexToBytes


-- Challenge 2
challenge2 :: [B.ByteString] -> Either Error B.ByteString
challenge2 [x,y]
    | isHex (B.append x y) = Right $ plusNL $ bytesToHex (bitwiseCombine xor (hexToBytes x) (hexToBytes y))
    | otherwise = Left ("Your hex strings must be actual hex i.e. [A-Za-z0-9]+", [stringToBytes "1-2"], True)
challenge2 _ = Left ("You need to supply two hex strings to xor together", [stringToBytes "1-2"], True)


-- Challenge 3
challenge3 :: [B.ByteString] -> Either Error B.ByteString
challenge3 [x,y]
    | not $ all (`elem` map charToWord8 ['0'..'9']) (B.unpack y) = Left ("The number of results to show must be a positive integer", [stringToBytes "1-3"], True)
    | yInt <= 0 = Left ("The number of results to show must be a positive integer", [stringToBytes "1-3"], True)
    | isHex x = Right $ hexToGuessList x yInt
    | otherwise = Left ("Your hex string must be actual hex i.e. [A-Za-z0-9]+ and the number to show must be a positive integer", [stringToBytes "1-3"], True)
    where yInt = read (bytesToString y)::Int
challenge3 [x]
    | isHex x = Right $ plusNL $ hexToBestGuess x
    | otherwise = Left ("Your hex string must be actual hex i.e. [A-Za-z0-9]+", [stringToBytes "1-3"], True)
challenge3 _ = Left ("You need to supply eactly one hex string and an optional number", [stringToBytes "1-3"], True)

hexToGuessList :: B.ByteString -> Int -> B.ByteString
hexToGuessList x y = stringToBytes $ unlines $ take y $ map bytesToString $ listOfBestGuesses probabilities bs
    where probabilities = singleByteXorProbabilities bs
          bs = hexToBytes x

hexToBestGuess :: B.ByteString -> B.ByteString
hexToBestGuess x = bestGuess bs
    where bestGuess = singleByteXor $ fst $ minimumBy (\(_, x) (_, y) -> compare x y) probabilities
          probabilities = singleByteXorProbabilities bs
          bs = hexToBytes x

singleByteXor :: Word8 -> B.ByteString -> B.ByteString
singleByteXor x = B.map (xor x)

listOfBestGuesses :: [(Word8, Double)] -> B.ByteString -> [B.ByteString]
listOfBestGuesses l bs =
        [  stringToBytes $ showHex (keyof x) ""
        ++ ":"
        ++ show (deltaof x)
        ++ ":"
        ++ bytesToString (plainof x)
        |  x <- ordered_guesses
        ]
    where ordered_guesses = [(fst x, snd x, singleByteXor (fst x) bs) | x <- sortOn snd l]
          keyof   (x, _, _) = x
          deltaof (_, x, _) = x
          plainof (_, _, x) = x

singleByteXorProbabilities :: B.ByteString -> [(Word8, Double)]
singleByteXorProbabilities str =
        [ (x, y)
        | x <- [0::Word8 .. 0xff]
        , let y = buildDelta (fromIntegral $ B.length str) asciiFreqTableNoNL $ singleByteXor x str
        ]

letterFreq :: Int -> B.ByteString -> (Double, Map.Map Word8 Double)
letterFreq strLen = buildFreqTable (0, strLen, Map.fromList [(n, 0) | n <- Map.keys asciiFreqTableNoNL])

-- Challenge 4
challenge4 :: [B.ByteString] -> Either Error B.ByteString
challenge4 input
    | null input = Left ("You need to supply hex strings to standard input i.e. [A-Za-z0-9]+, one per line", [stringToBytes "1-4"], True)
    | otherwise  = Right $ singleByteXor (fst $ snd absoluteBest) (fst absoluteBest)
    where absoluteBest = minimumBy (\(_, (_, x)) (_, (_, y)) -> compare x y) [(x, bestGuess x) | x <- map hexToBytes $ B.split (charToWord8 '\n') (head input)]
          bestGuess = minimumBy (\(_, x) (_, y) -> compare x y) . singleByteXorProbabilities


-- Challenge 5
challenge5 :: [B.ByteString] -> Either Error B.ByteString
challenge5 input
    | null input = Left ("You need to supply a key", [stringToBytes "1-5"], True)
    | otherwise = Right $ plusNL $ B.concat [ bytesToHex $ bitwiseCombine xor ln (B.cycle key) | ln <- text]
    where key = head input
          text = tail input
