module Lib
    (
    -- Types
      Error
    , UTF8String
    , UTF8Char

      -- Base64
    , base64ToBytes
    , bytesToBase64

      -- Base16
    , hexToBytes
    , bytesToHex
    , isHex

      -- ByteString helper functions
    , bitwiseCombine
    , hammingDistance
    , bytesToString
    , buildFreqTable
    , buildDelta
    , freqTableDelta
    , word8ToChar
    , charToWord8
    , stringToBytes
    , splitBytes
    , plusNL

    -- Crypto functions
    , decryptAES128ECB
    , encryptAES128ECB
    , encryptAES128CBC
    , decryptAES128CBC
    , pkcs7Pad

      -- Data
    , englishFreqTable
    , asciiFreqTable
    , asciiFreqTableNoNL
    ) where

import           Control.Arrow
import           Data.Bits
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as B
import           Data.List
import qualified Data.Map                    as Map
import qualified Data.Text.Lazy              as Txt
import qualified Data.Text.Lazy.Encoding     as TxtEnc
import           Data.Word

import qualified Codec.Crypto.AES            as AES

-- Error
-- (error message, arguments for return_help, and whether to show usage)
type Error = (String, [B.ByteString], Bool)
type UTF8String = String
type UTF8Char = Char

-- Base64 functions
base64ToBytes :: B.ByteString -> B.ByteString
base64ToBytes  = B64.decodeLenient

bytesToBase64 :: B.ByteString -> B.ByteString
bytesToBase64 = B64.encode


-- Base16 (hex) functions
hexToBytes :: B.ByteString -> B.ByteString
hexToBytes = fst . B16.decode

bytesToHex :: B.ByteString -> B.ByteString
bytesToHex = B16.encode

isHex :: B.ByteString -> Bool
isHex x = all (`elem` map charToWord8 (['A'..'F'] ++ ['a'..'f'] ++ ['0'..'9'])) $ B.unpack x


-- ByteString helper functions
bitwiseCombine :: (Word8 -> Word8 -> Word8) -> B.ByteString -> B.ByteString -> B.ByteString
bitwiseCombine f x y = B.pack $ B.zipWith f x y

-- NB: if the ByteStrings are not of euqal length, this truncates the longer one
hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance x y = B.foldl (\a b -> a + popCount b) 0 $ bitwiseCombine xor x y


buildDelta :: Int -> Map.Map Word8 Double -> B.ByteString -> Double
buildDelta totalCount startingMap haystack = Map.fold (\x y -> abs x + y) 0 $ B.foldl (flip (Map.adjust (\a -> a - (1/realToFrac totalCount)))) startingMap haystack
-- buildFreqTable startingValue haystack = (realToFrac (totalCount - inCount) / realToFrac totalCount, freqMap) -- {-# SCC "build-normalize-map" #-} Map.map (/realToFrac inCount) freqMap)
--         where (inCount, totalCount, freqMap) = {-# SCC "build-perform-fold" #-} B.foldl' buidFreqTableFold startingValue haystack
-- buildDelta startingMap haystack = Map.fold (+) 0 $ B.foldl (\x -> Map.adjust (-1/realToFrac totalCount) x) startingMap haystack
-- buildDelta totalCount startingMap haystack = Map.fold (\x y -> {-# SCC "buildDelta-fold-lambda" #-}x + y) 0 $ B.foldl' (flip (Map.adjust (\a -> {-# SCC "buildDelta-adjust-lambda" #-} abs $ a - (1/realToFrac totalCount)))) startingMap haystack
--    where totalCount = B.length haystack

-- The map that is returned is (k, v) with one k for each needle (first [Word8])
-- so v will be 0 for those not found. Otherwise v is the proportion of k occurring
-- in the subset of haystack which contains only the needles.
-- The Double that is returned is the proportion of the total length of the haystack
-- which is Word8s not in the needles.
buildFreqTable :: (Int, Int, Map.Map Word8 Double) -> B.ByteString -> (Double, Map.Map Word8 Double)
buildFreqTable startingValue haystack = (realToFrac (totalCount - inCount) / realToFrac totalCount, freqMap)
        where (inCount, totalCount, freqMap) = B.foldl' buidFreqTableFold startingValue haystack

buidFreqTableFold :: (Int, Int, Map.Map Word8 Double) -> Word8 -> (Int, Int, Map.Map Word8 Double)
buidFreqTableFold (inCount, totalCount, accumulatorMap) newByte
    | isIn      = (inCount + 1, totalCount, Map.adjust (+(1/realToFrac totalCount)) newByte accumulatorMap)
    | otherwise = (inCount, totalCount, accumulatorMap)
    where isIn = Map.member newByte accumulatorMap

-- buidFreqTableRecursive :: Map.Map Word8 Double -> Double -> Double -> B.ByteString -> (Double, Map.Map Word8 Double)
-- buidFreqTableRecursive accumulatorMap inCount totalCount bytesToAdd
--     | isNull    = (0, accumulatorMap)
--     | isEmpty   = {-# SCC "build-branch-empty" #-} ((totalCount - inCount) / totalCount, {-# SCC "build-normalize-map" #-} Map.map (/inCount) accumulatorMap)
--     | isIn      = {-# SCC "build-branch-in" #-} let newmap = {-# SCC "build-adjust-map" #-} Map.adjust (+1) hd accumulatorMap
--                   in  buidFreqTableRecursive newmap (inCount + 1) (totalCount + 1) tl
--     | otherwise = buidFreqTableRecursive accumulatorMap inCount (totalCount + 1) tl
--     where hd = B.head bytesToAdd
--           tl = B.tail bytesToAdd
--           isIn = {-# SCC "build-isin?" #-} Map.member hd accumulatorMap  -- fix with memoization
--           isEmpty = B.null bytesToAdd
--           isNull = isEmpty && (totalCount == 0)

charToWord8 :: UTF8Char -> Word8
charToWord8 = B.head . TxtEnc.encodeUtf8 . Txt.singleton

word8ToChar :: Word8 -> UTF8Char
word8ToChar = head . Txt.unpack . TxtEnc.decodeUtf8 . B.singleton

bytesToString :: B.ByteString -> UTF8String
bytesToString = Txt.unpack . TxtEnc.decodeUtf8With (\_ _ -> Just '�')

stringToBytes :: UTF8String -> B.ByteString
stringToBytes = TxtEnc.encodeUtf8 . Txt.pack

-- Splits an input byte string into an array of byte strings, each of which is <= n bytes in length
splitBytes :: Int -> B.ByteString -> [B.ByteString]
splitBytes n bytes
    | B.null bytes = []
    | otherwise    = fst split : splitBytes n (snd split)
        where split = B.splitAt (fromIntegral n) bytes

freqTableDelta :: Map.Map Word8 Double -> Map.Map Word8 Double -> Double
freqTableDelta x y = sum [abs (snd (Map.elemAt i x) - snd (Map.elemAt i y)) | i <- [0..Map.size x - 1]]

plusNL :: B.ByteString -> B.ByteString
plusNL x = B.append x $ B.singleton (charToWord8 '\n')

-- Crypto functions
pkcs7Pad :: Int -> B.ByteString -> B.ByteString
pkcs7Pad len input
    | padlen > 0 = B.append input $ B.replicate padlen (fromIntegral padlen::Word8)
    | otherwise  = input
    where padlen = fromIntegral len - B.length input

breakIntoBlocksPkcs7 :: Int -> B.ByteString -> [B.ByteString]
breakIntoBlocksPkcs7 blocksize str = init split ++ [pkcs7Pad blocksize (last split)]
    where split = splitBytes blocksize str

decryptAES128ECB :: B.ByteString -> B.ByteString -> B.ByteString
decryptAES128ECB k = AES.crypt AES.ECB (B.toStrict k) (B.toStrict $ B.replicate 16 (0::Word8)) AES.Decrypt

encryptAES128ECB :: B.ByteString -> B.ByteString -> B.ByteString
encryptAES128ECB k = AES.crypt AES.ECB (B.toStrict k) (B.toStrict $ B.replicate 16 (0::Word8)) AES.Encrypt

encryptAES128CBC :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
encryptAES128CBC iv k input = foldl (\a b -> B.append a $ encryptAES128CBC' k (B.drop (B.length b - 16) b) a) iv blocks
    where blocks = breakIntoBlocksPkcs7 16 input

decryptAES128CBC :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
decryptAES128CBC iv k input = B.concat . snd $ recursiveDecrypt (iv:blocks, [])
    where
        recursiveDecrypt (c, p)
            | length c < 2 = ([], p)
            | otherwise    = recursiveDecrypt(init c, decryptAES128CBC' k (last $ init c) (last c):p)
        blocks = splitBytes 16 input

encryptAES128CBC' :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
encryptAES128CBC' k prev cur = encryptAES128ECB k (bitwiseCombine xor prev cur)

decryptAES128CBC' :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
decryptAES128CBC' k prev cur = bitwiseCombine xor prev $ decryptAES128ECB k cur


-- Data
englishFreqTable :: Map.Map Word8 Double
englishFreqTable = Map.fromList $ map (first charToWord8)
    [ ('A', 0.0651738)
    , ('B', 0.0124248)
    , ('C', 0.0217339)
    , ('D', 0.0349835)
    , ('E', 0.1041442)
    , ('F', 0.0197881)
    , ('G', 0.0158610)
    , ('H', 0.0492888)
    , ('I', 0.0558094)
    , ('J', 0.0009033)
    , ('K', 0.0050529)
    , ('L', 0.0331490)
    , ('M', 0.0202124)
    , ('N', 0.0564513)
    , ('O', 0.0596302)
    , ('P', 0.0137645)
    , ('Q', 0.0008606)
    , ('R', 0.0497563)
    , ('S', 0.0515760)
    , ('T', 0.0729357)
    , ('U', 0.0225134)
    , ('V', 0.0082903)
    , ('W', 0.0171272)
    , ('X', 0.0013692)
    , ('Y', 0.0145984)
    , ('Z', 0.0007836)
    , (' ', 0.1918182)
    ]

asciiFreqTableNoNL :: Map.Map Word8 Double
asciiFreqTableNoNL = freqTableRemove 10 asciiFreqTable

-- Remove an element of a frequency table and re-normalize it
freqTableRemove :: (Ord a) => a -> Map.Map a Double -> Map.Map a Double
freqTableRemove = freqTableSetFreq 0.0

-- Set an element of a frequency table to a new value (<1) and re-normalize
-- The new value will be the new *normalized* frequency
freqTableSetFreq :: (Ord a) => Double -> a -> Map.Map a Double -> Map.Map a Double
freqTableSetFreq vNew k oldMap = Map.map normalizer newMap
    where newMap     = Map.update (\_ -> Just vNew) k oldMap
          normalizer = case Map.lookup k oldMap
                             of Just 1.0  -> id  -- Avoid division by zero
                                Just vOld -> \x -> (x / (1.0 - vOld)) * (1.0 - vNew)
                                _         -> id


asciiFreqTable :: Map.Map Word8 Double
asciiFreqTable = Map.fromList -- This comes from IMDb biographies (~154 MB)
    [ ( 10, 0.0166623841) -- New lines are common!
      -- NB: this uses \n line endings; you should strip \r endings!
    , ( 32, 0.1493452336)
    , ( 33, 0.0000877131)
    , ( 34, 0.0039434812)
    , ( 35, 0.0000242677)
    , ( 36, 0.0000232542)
    , ( 37, 0.0000050115)
    , ( 38, 0.0002297750)
    , ( 39, 0.0036935298)
    , ( 40, 0.0033630778)
    , ( 41, 0.0033603918)
    , ( 42, 0.0000068084)
    , ( 43, 0.0000100105)
    , ( 44, 0.0109746709)
    , ( 45, 0.0018378580)
    , ( 46, 0.0083998790)
    , ( 47, 0.0003024661)
    , ( 48, 0.0029106658)
    , ( 49, 0.0031379039)
    , ( 50, 0.0020484266)
    , ( 51, 0.0006482944)
    , ( 52, 0.0006231376)
    , ( 53, 0.0006974888)
    , ( 54, 0.0006034835)
    , ( 55, 0.0006037695)
    , ( 56, 0.0007166642)
    , ( 57, 0.0024395819)
    , ( 58, 0.0002724035)
    , ( 59, 0.0002067820)
    , ( 60, 0.0000000249)
    , ( 61, 0.0000020270)
    , ( 62, 0.0000000311)
    , ( 63, 0.0000311134)
    , ( 64, 0.0000022322)
    , ( 65, 0.0044852354)
    , ( 66, 0.0029482766)
    , ( 67, 0.0037505026)
    , ( 68, 0.0022110691)
    , ( 69, 0.0013691369)
    , ( 70, 0.0022042669)
    , ( 71, 0.0014638200)
    , ( 72, 0.0030768151)
    , ( 73, 0.0021547430)
    , ( 74, 0.0013806024)
    , ( 75, 0.0008927370)
    , ( 76, 0.0020431788)
    , ( 77, 0.0031132383)
    , ( 78, 0.0015841325)
    , ( 79, 0.0010364217)
    , ( 80, 0.0021186990)
    , ( 81, 0.0000892488)
    , ( 82, 0.0016801151)
    , ( 83, 0.0047621654)
    , ( 84, 0.0038583547)
    , ( 85, 0.0008843866)
    , ( 86, 0.0010261563)
    , ( 87, 0.0015732453)
    , ( 88, 0.0000846415)
    , ( 89, 0.0006201096)
    , ( 90, 0.0001401904)
    , ( 91, 0.0000163028)
    , ( 92, 0.0000004352)
    , ( 93, 0.0000162966)
    , ( 94, 0.0000001741)
    , ( 95, 0.0013607182)
    , ( 96, 0.0000021513)
    , ( 97, 0.0664147438)
    , ( 98, 0.0082465257)
    , ( 99, 0.0227508279)
    , (100, 0.0313071255)
    , (101, 0.0879005501)
    , (102, 0.0150727676)
    , (103, 0.0151489158)
    , (104, 0.0336337629)
    , (105, 0.0583783203)
    , (106, 0.0008706828)
    , (107, 0.0053374890)
    , (108, 0.0310655990)
    , (109, 0.0179510951)
    , (110, 0.0589311109)
    , (111, 0.0548135280)
    , (112, 0.0122177814)
    , (113, 0.0017306401)
    , (114, 0.0527059338)
    , (115, 0.0472023266)
    , (116, 0.0553994775)
    , (117, 0.0181021976)
    , (118, 0.0090335307)
    , (119, 0.0126625084)
    , (120, 0.0011941461)
    , (121, 0.0115988780)
    , (122, 0.0009457739)
    , (123, 0.0000044208)
    , (124, 0.0000000062)
    , (125, 0.0000044332)
    , (126, 0.0000008953)
    , (128, 0.0000000062)
    , (129, 0.0000000062)
    , (130, 0.0000000062)
    , (131, 0.0000000124)
    , (132, 0.0000000062)
    , (139, 0.0000000062)
    , (157, 0.0000000062)
    , (159, 0.0000000062)
    , (160, 0.0000017720)
    , (161, 0.0000004415)
    , (162, 0.0000000746)
    , (163, 0.0000009575)
    , (165, 0.0000000062)
    , (166, 0.0000000062)
    , (167, 0.0000000249)
    , (168, 0.0000008580)
    , (169, 0.0000005409)
    , (170, 0.0000000808)
    , (171, 0.0000015420)
    , (172, 0.0000000497)
    , (173, 0.0000005036)
    , (174, 0.0000048685)
    , (175, 0.0000000124)
    , (176, 0.0000004042)
    , (177, 0.0000000435)
    , (178, 0.0000000684)
    , (179, 0.0000000870)
    , (182, 0.0000000062)
    , (183, 0.0000006529)
    , (184, 0.0000000311)
    , (185, 0.0000000870)
    , (186, 0.0000001990)
    , (187, 0.0000015544)
    , (188, 0.0000000622)
    , (189, 0.0000005409)
    , (190, 0.0000000249)
    , (191, 0.0000002301)
    , (192, 0.0000002238)
    , (193, 0.0000011130)
    , (194, 0.0000000560)
    , (195, 0.0000001803)
    , (196, 0.0000002674)
    , (197, 0.0000005596)
    , (198, 0.0000000187)
    , (199, 0.0000002425)
    , (200, 0.0000001057)
    , (201, 0.0000021700)
    , (202, 0.0000000187)
    , (203, 0.0000000249)
    , (205, 0.0000001057)
    , (206, 0.0000000435)
    , (208, 0.0000000062)
    , (209, 0.0000000373)
    , (210, 0.0000000311)
    , (211, 0.0000003731)
    , (212, 0.0000001554)
    , (213, 0.0000000435)
    , (214, 0.0000006777)
    , (215, 0.0000000622)
    , (216, 0.0000002549)
    , (218, 0.0000002487)
    , (219, 0.0000000373)
    , (220, 0.0000002984)
    , (221, 0.0000000124)
    , (222, 0.0000000311)
    , (223, 0.0000007026)
    , (224, 0.0000033700)
    , (225, 0.0000179878)
    , (226, 0.0000017161)
    , (227, 0.0000048622)
    , (228, 0.0000064913)
    , (229, 0.0000027669)
    , (230, 0.0000009575)
    , (231, 0.0000048498)
    , (232, 0.0000083566)
    , (233, 0.0000646641)
    , (234, 0.0000015917)
    , (235, 0.0000024871)
    , (236, 0.0000002674)
    , (237, 0.0000133618)
    , (238, 0.0000006839)
    , (239, 0.0000011316)
    , (240, 0.0000002549)
    , (241, 0.0000076105)
    , (242, 0.0000005409)
    , (243, 0.0000133867)
    , (244, 0.0000032705)
    , (245, 0.0000006529)
    , (246, 0.0000101162)
    , (248, 0.0000032705)
    , (249, 0.0000003917)
    , (250, 0.0000034570)
    , (251, 0.0000007461)
    , (252, 0.0000090219)
    , (253, 0.0000003668)
    , (254, 0.0000000497)
    , (255, 0.0000000249)
    ]
