module Set2
    ( challenge9
    , challenge10
    , challenge11
    , challenge12
    ) where

import           Lib

import qualified Debug.Trace          as Debug

import           Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Function
import           Data.List
import qualified Data.Map             as Map
import           Data.Word


challenge9 :: [B.ByteString] -> Either Error B.ByteString
challenge9 [len, str]
    | not $ all (`elem` map charToWord8 ['0'..'9']) (B.unpack len) = Left ("The padding length must be a positive integer", [stringToBytes "2-9"], True)
    | lenInt <= 0 = Left ("The padding length must be a positive integer", [stringToBytes "2-9"], True)
    | otherwise = Right $ plusNL $ pkcs7Pad lenInt str
    where lenInt = read (bytesToString len)::Int
challenge9 [x] = Left ("You need to supply eactly one length and one ASCII string", [stringToBytes "2-9"], True)
challenge9 _ = Left ("You need to supply eactly one length and one ASCII string", [stringToBytes "2-9"], True)


challenge10 :: [B.ByteString] -> Either Error B.ByteString
challenge10 [key, input]
    | B.length key /= 16 = Left ("You need to supply an ASCII key exactly 16 bytes long (maybe try quoting it?)", [stringToBytes "2-10"], True)
    | otherwise = Right $ plusNL $ decryptAES128CBC (B.replicate 16 (0::Word8)) key $ base64ToBytes input
challenge10 [x] = Left ("You need to supply an ASCII key and base64 encoded data on standard in", [stringToBytes "2-10"], True)
challenge10 _ = Left ("You need to supply an ASCII key and base64 encoded data on standard in", [stringToBytes "2-10"], True)


-- This takes 57 bytes (Word8s) of random input
--  0 - 15  Key (16 bytes)
-- 16 - 31  IV (16 bytes)
-- 32 - 55  Extra material (24 bytes)
-- 56 - 56  Bits are used as follows: (1 byte)
--          .0      CBC if 0 else 1 (1 bit)
--          .1-3    5 less than bytes to add at start (3 bits, noting 00001110 = 14)
--          .4-6    5 less than bytes to add at end (3 bits, noting 01110000 = 112)
--          .7      Reserved
challenge11 :: [B.ByteString] -> Either Error B.ByteString
challenge11 [rand] = Right $ plusNL $ stringToBytes $ "Guessed " ++ (if score == 0 then "ECB" else "CBC") ++ "\nActual  " ++ (if isCBC then "CBC" else "ECB")
    where score = fromIntegral (allPairsHammingDistance (init (tail (init $ splitBytes 16 encrypted))))
          (isCBC, encrypted) = encryptionOracle rand $ B.replicate 100 80
challenge11 _ = Left ("This challenge should not be given any input", [stringToBytes "2-11"], True)

encryptionOracle :: B.ByteString -> B.ByteString -> (Bool, B.ByteString)
encryptionOracle rand input = (testBit choices 0, encrypted)
    where   encrypted = crypt fullData
            fullData  = B.concat [startData, input, endData]
            iv        = B.take 16 $ B.drop 0  rand
            key       = B.take 16 $ B.drop 16 rand
            startPool = B.take 12 $ B.drop 32 rand
            endPool   = B.take 12 $ B.drop 44 rand
            choices   = B.last    $ B.drop 56 rand
            startData = B.take (5 + fromIntegral (shiftR ( 14 .&. choices) 1)) startPool
            endData   = B.take (5 + fromIntegral (shiftR (112 .&. choices) 4)) endPool
            crypt     = if testBit choices 0 then encryptAES128CBC iv key else encryptAES128ECB key


-- This takes 16 bytes (Word8s) of random input to use as the key
challenge12 :: [B.ByteString] -> Either Error B.ByteString
challenge12 [rand, rawInput] = Right $ B.concat $ map (decryptBlock oracle (B.replicate (fromIntegral blockSize) 80)) plainTextBlocks
    where   oracle = encryptAES128ECB rand
            plainTextBlocks = breakIntoBlocksPkcs7 blockSize input
            initialGuess = B.replicate (fromIntegral blockSize) 80
            blockSize = findBlockSize 2 (oracle $ B.replicate 200 80)
            input = base64ToBytes rawInput

findBlockSize :: Int -> B.ByteString -> Int
findBlockSize rawSize cipherText
    | B.take size cipherText == B.take size (B.drop size cipherText) = rawSize
    | otherwise = findBlockSize (rawSize + 1) cipherText
    where size = fromIntegral rawSize

decryptBlock :: (B.ByteString -> B.ByteString) -> B.ByteString -> B.ByteString -> B.ByteString
decryptBlock oracle = B.foldl foldFunc
    where foldFunc acc byte = rollForward acc $ lastByte oracle (cipherText byte acc) (rollForward acc (80::Word8)) (0::Word8)
          cipherText byte acc = oracle (B.append (B.tail acc) (B.singleton byte))
          rollForward acc byte = B.append (B.tail acc) $ B.singleton byte

lastByte :: (B.ByteString -> B.ByteString) -> B.ByteString -> B.ByteString -> Word8 -> Word8
lastByte oracle cipherText currentGuess x
        | cipherText == oracle (B.append (B.init currentGuess) (B.singleton x)) = x
        | otherwise = lastByte oracle cipherText currentGuess (x + 1)
