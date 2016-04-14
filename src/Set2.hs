module Set2
    ( challenge9
    , challenge10
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
