module Set1
    ( challenge1
    , challenge2
    , challenge3
    , challenge4
    , challenge5
    , challenge6
    , challenge7
    , challenge8
    ) where

import           Lib

import qualified Debug.Trace          as Debug

import           Numeric              (showHex)

import           Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Function
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

solveSingleByteXor :: B.ByteString -> (Word8, Double)
solveSingleByteXor = minimumBy (\(_, x) (_, y) -> compare x y) . singleByteXorProbabilities

-- Challenge 5
challenge5 :: [B.ByteString] -> Either Error B.ByteString
challenge5 input
    | null input = Left ("You need to supply a key", [stringToBytes "1-5"], True)
    | otherwise = Right $ plusNL $ B.concat [bytesToHex $ bitwiseCombine xor ln (B.cycle key) | ln <- text]
    where key = head input
          text = tail input


-- Challenge 6
challenge6 :: [B.ByteString] -> Either Error B.ByteString
challenge6 input
    | null input = Left ("You need to supply some input", [stringToBytes "1-6"], True)
    | otherwise = Right $ plusNL $ bitwiseCombine xor str (B.cycle key)
        where key = fst
                    $ minimumBy
                        (Data.Function.on compare snd)
                        [
                            (   B.pack
                                    $ map fst [solveSingleByteXor c | c <- chunks],
                                sum (map snd [solveSingleByteXor c | c <- chunks])
                                    / fromIntegral (length chunks)
                            )
                            | chunks <- possibleChunks]
              possibleChunks = [B.transpose (strToTranspose n) | n <- likelyKeyLengths]
              strToTranspose n = case Map.lookup n segmentedStr
                                   of Just x -> x
                                      _      -> []
              likelyKeyLengths = map snd
                                $ take numberOfLikelyKeyLengths
                                $ Map.toAscList
                                $ Map.foldWithKey
                                    (\size chunks acc
                                        -> Map.insert
                                                (averageNormalizedHammingDistance
                                                    $ take (2 * numberOfSegmentsToCheck) chunks)
                                                size
                                                acc)
                                    Map.empty
                                    segmentedStr
              segmentedStr = Map.fromList [(n, splitBytes n str) | n <- [shortestKeyLength..longestKeyLength]]
              str = base64ToBytes $ head input
              numberOfSegmentsToCheck = 4
              numberOfLikelyKeyLengths = 3
              shortestKeyLength = 2
              longestKeyLength = 40

-- Hamming distance between two byte strings, divided by the length of the shorter string
-- (we use the shorter string because hammingDistance truncates the longer string)
normalizedHammingDistance :: B.ByteString -> B.ByteString -> Double
normalizedHammingDistance x y = realToFrac (hammingDistance x y) / realToFrac (min (B.length x) (B.length y))

-- Gets the average hamming distance of each sequential pair of ByteStrings in the array
averageNormalizedHammingDistance :: [B.ByteString] -> Double
averageNormalizedHammingDistance = averageNormalizedHammingDistance' (0, 0)

averageNormalizedHammingDistance' :: (Double, Int) -> [B.ByteString] -> Double
averageNormalizedHammingDistance' (acc, count) [] = if count == 0 then 0 else acc / fromIntegral count
averageNormalizedHammingDistance' (acc, count) [_] = if count == 0 then 0 else acc / fromIntegral count
averageNormalizedHammingDistance' (acc, count) (x:y:remainder) = averageNormalizedHammingDistance' (acc + h, count + 1) remainder
    where h = normalizedHammingDistance x y


-- Challenge 7
challenge7 :: [B.ByteString] -> Either Error B.ByteString
challenge7 input
    | null input         = Left ("You need to supply a key and standard input", [stringToBytes "1-7"], True)
    | 16 /= B.length key = Left ("Your key must be exactly 16 ASCII bytes (maybe enclose it in quotes?)", [stringToBytes "1-7"], True)
    | otherwise          = Right $ decryptAES128ECB key str
    where key = head input
          str = base64ToBytes $ head $ tail input


-- Challenge 8
challenge8 :: [B.ByteString] -> Either Error B.ByteString
challenge8 input
  | null input         = Left ("You need to supply standard input", [stringToBytes "1-8"], True)
  | otherwise          = Right $ plusNL . fst . minimumBy (compare `Data.Function.on` snd) $ filter (not . B.null . fst) [(i, allPairsHammingDistance $ splitBytes 16 (hexToBytes i)) | i <- B.split (charToWord8 '\n') $ head input]
  -- | otherwise          = Right $ foldl B.append (stringToBytes "\n") $ map (\(a, b) -> B.append (stringToBytes $ show (fromIntegral b / 320) ++ " - ") $ B.append a (stringToBytes "\n")) $ sortOn snd $ filter (not . B.null . fst) [(i, allPairsHammingDistance $ splitBytes 16 (hexToBytes i)) | i <- B.split (charToWord8 '\n') $ head input]
