module Set2
    ( challenge9
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
