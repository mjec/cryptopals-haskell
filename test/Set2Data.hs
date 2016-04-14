module Set2Data where

-- WARNING: this file has some very long lines!


import qualified Data.ByteString.Lazy as B
import           Data.Word            (Word8)
import           Lib                  (stringToBytes)

challenge9Input :: [B.ByteString]
challenge9Input = [stringToBytes "20", stringToBytes "YELLOW SUBMARINE"]

challenge9Output :: Either a B.ByteString
challenge9Output = Right $ stringToBytes "YELLOW SUBMARINE\x04\x04\x04\x04\n"
