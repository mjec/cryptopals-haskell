module Set2
    ( challenge9
    , challenge10
    , challenge11
    ) where

import           Lib

import qualified Debug.Trace          as Debug

import           Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Function
import           Data.List
import qualified Data.Map             as Map
import           Data.Word


oracleKey :: B.ByteString
oracleKey = pkcs7Pad 16 $ stringToBytes "I am mjec, yo"

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

   --  Now that you have ECB and CBC working:
   --
   -- Write a function to generate a random AES key; that's just 16 random bytes.
   --
   -- Write a function that encrypts data under an unknown key --- that is, a function that generates a random key and encrypts under it.
   --
   -- The function should look like:
   --
   -- encryption_oracle(your-input)
   -- => [MEANINGLESS JIBBER JABBER]
   --
   -- Under the hood, have the function append 5-10 bytes (count chosen randomly) before the plaintext and 5-10 bytes after the plaintext.
   --
   -- Now, have the function choose to encrypt under ECB 1/2 the time, and under CBC the other half (just use random IVs each time for CBC). Use rand(2) to decide which to use.
   --
   -- Detect the block cipher mode the function is using each time. You should end up with a piece of code that, pointed at a block box that might be encrypting ECB or CBC, tells you which one is happening.
