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
    , bytesToString
    , buildFreqTable
--    , freqTableDifference
    , buildDelta
    , freqTableDelta
    , word8ToChar
    , charToWord8
    , stringToBytes
    , plusNL

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
-- hexToBytes hex
--         | hex == B.empty = B.empty
--         | otherwise = B.append (B.singleton $ hx (head firstTwo, head (tail firstTwo))) (hexToBytes $ B.drop 2 hex)
--     where firstTwo = B.unpack $ B.take 2 hex
--           hx h     = case Map.lookup h hexToBytesTable
--                      of Just x -> x
--                         _      -> 0::Word8
hexToBytes = fst . B16.decode

bytesToHex :: B.ByteString -> B.ByteString
bytesToHex = B16.encode

isHex :: B.ByteString -> Bool
isHex x = all (`elem` map charToWord8 (['A'..'F'] ++ ['a'..'f'] ++ ['0'..'9'])) $ B.unpack x


-- ByteString helper functions
bitwiseCombine :: (Word8 -> Word8 -> Word8) -> B.ByteString -> B.ByteString -> B.ByteString
bitwiseCombine f x y = B.pack $ B.zipWith f x y


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

freqTableDelta :: Map.Map Word8 Double -> Map.Map Word8 Double -> Double
freqTableDelta x y = sum [abs (snd (Map.elemAt i x) - snd (Map.elemAt i y)) | i <- [0..Map.size x - 1]]


plusNL :: B.ByteString -> B.ByteString
plusNL x = B.append x $ B.singleton (charToWord8 '\n')


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


hexToBytesTable :: Map.Map (Word8, Word8) Word8
hexToBytesTable = Map.fromList $ map (\x -> ((charToWord8 *** charToWord8) (fst x), snd x))
    [ (('0', '0'), 0)
    , (('0', '1'), 1)
    , (('0', '2'), 2)
    , (('0', '3'), 3)
    , (('0', '4'), 4)
    , (('0', '5'), 5)
    , (('0', '6'), 6)
    , (('0', '7'), 7)
    , (('0', '8'), 8)
    , (('0', '9'), 9)
    , (('0', 'a'), 10)
    , (('0', 'b'), 11)
    , (('0', 'c'), 12)
    , (('0', 'd'), 13)
    , (('0', 'e'), 14)
    , (('0', 'f'), 15)
    , (('1', '0'), 16)
    , (('1', '1'), 17)
    , (('1', '2'), 18)
    , (('1', '3'), 19)
    , (('1', '4'), 20)
    , (('1', '5'), 21)
    , (('1', '6'), 22)
    , (('1', '7'), 23)
    , (('1', '8'), 24)
    , (('1', '9'), 25)
    , (('1', 'a'), 26)
    , (('1', 'b'), 27)
    , (('1', 'c'), 28)
    , (('1', 'd'), 29)
    , (('1', 'e'), 30)
    , (('1', 'f'), 31)
    , (('2', '0'), 32)
    , (('2', '1'), 33)
    , (('2', '2'), 34)
    , (('2', '3'), 35)
    , (('2', '4'), 36)
    , (('2', '5'), 37)
    , (('2', '6'), 38)
    , (('2', '7'), 39)
    , (('2', '8'), 40)
    , (('2', '9'), 41)
    , (('2', 'a'), 42)
    , (('2', 'b'), 43)
    , (('2', 'c'), 44)
    , (('2', 'd'), 45)
    , (('2', 'e'), 46)
    , (('2', 'f'), 47)
    , (('3', '0'), 48)
    , (('3', '1'), 49)
    , (('3', '2'), 50)
    , (('3', '3'), 51)
    , (('3', '4'), 52)
    , (('3', '5'), 53)
    , (('3', '6'), 54)
    , (('3', '7'), 55)
    , (('3', '8'), 56)
    , (('3', '9'), 57)
    , (('3', 'a'), 58)
    , (('3', 'b'), 59)
    , (('3', 'c'), 60)
    , (('3', 'd'), 61)
    , (('3', 'e'), 62)
    , (('3', 'f'), 63)
    , (('4', '0'), 64)
    , (('4', '1'), 65)
    , (('4', '2'), 66)
    , (('4', '3'), 67)
    , (('4', '4'), 68)
    , (('4', '5'), 69)
    , (('4', '6'), 70)
    , (('4', '7'), 71)
    , (('4', '8'), 72)
    , (('4', '9'), 73)
    , (('4', 'a'), 74)
    , (('4', 'b'), 75)
    , (('4', 'c'), 76)
    , (('4', 'd'), 77)
    , (('4', 'e'), 78)
    , (('4', 'f'), 79)
    , (('5', '0'), 80)
    , (('5', '1'), 81)
    , (('5', '2'), 82)
    , (('5', '3'), 83)
    , (('5', '4'), 84)
    , (('5', '5'), 85)
    , (('5', '6'), 86)
    , (('5', '7'), 87)
    , (('5', '8'), 88)
    , (('5', '9'), 89)
    , (('5', 'a'), 90)
    , (('5', 'b'), 91)
    , (('5', 'c'), 92)
    , (('5', 'd'), 93)
    , (('5', 'e'), 94)
    , (('5', 'f'), 95)
    , (('6', '0'), 96)
    , (('6', '1'), 97)
    , (('6', '2'), 98)
    , (('6', '3'), 99)
    , (('6', '4'), 100)
    , (('6', '5'), 101)
    , (('6', '6'), 102)
    , (('6', '7'), 103)
    , (('6', '8'), 104)
    , (('6', '9'), 105)
    , (('6', 'a'), 106)
    , (('6', 'b'), 107)
    , (('6', 'c'), 108)
    , (('6', 'd'), 109)
    , (('6', 'e'), 110)
    , (('6', 'f'), 111)
    , (('7', '0'), 112)
    , (('7', '1'), 113)
    , (('7', '2'), 114)
    , (('7', '3'), 115)
    , (('7', '4'), 116)
    , (('7', '5'), 117)
    , (('7', '6'), 118)
    , (('7', '7'), 119)
    , (('7', '8'), 120)
    , (('7', '9'), 121)
    , (('7', 'a'), 122)
    , (('7', 'b'), 123)
    , (('7', 'c'), 124)
    , (('7', 'd'), 125)
    , (('7', 'e'), 126)
    , (('7', 'f'), 127)
    , (('8', '0'), 128)
    , (('8', '1'), 129)
    , (('8', '2'), 130)
    , (('8', '3'), 131)
    , (('8', '4'), 132)
    , (('8', '5'), 133)
    , (('8', '6'), 134)
    , (('8', '7'), 135)
    , (('8', '8'), 136)
    , (('8', '9'), 137)
    , (('8', 'a'), 138)
    , (('8', 'b'), 139)
    , (('8', 'c'), 140)
    , (('8', 'd'), 141)
    , (('8', 'e'), 142)
    , (('8', 'f'), 143)
    , (('9', '0'), 144)
    , (('9', '1'), 145)
    , (('9', '2'), 146)
    , (('9', '3'), 147)
    , (('9', '4'), 148)
    , (('9', '5'), 149)
    , (('9', '6'), 150)
    , (('9', '7'), 151)
    , (('9', '8'), 152)
    , (('9', '9'), 153)
    , (('9', 'a'), 154)
    , (('9', 'b'), 155)
    , (('9', 'c'), 156)
    , (('9', 'd'), 157)
    , (('9', 'e'), 158)
    , (('9', 'f'), 159)
    , (('a', '0'), 160)
    , (('a', '1'), 161)
    , (('a', '2'), 162)
    , (('a', '3'), 163)
    , (('a', '4'), 164)
    , (('a', '5'), 165)
    , (('a', '6'), 166)
    , (('a', '7'), 167)
    , (('a', '8'), 168)
    , (('a', '9'), 169)
    , (('a', 'a'), 170)
    , (('a', 'b'), 171)
    , (('a', 'c'), 172)
    , (('a', 'd'), 173)
    , (('a', 'e'), 174)
    , (('a', 'f'), 175)
    , (('b', '0'), 176)
    , (('b', '1'), 177)
    , (('b', '2'), 178)
    , (('b', '3'), 179)
    , (('b', '4'), 180)
    , (('b', '5'), 181)
    , (('b', '6'), 182)
    , (('b', '7'), 183)
    , (('b', '8'), 184)
    , (('b', '9'), 185)
    , (('b', 'a'), 186)
    , (('b', 'b'), 187)
    , (('b', 'c'), 188)
    , (('b', 'd'), 189)
    , (('b', 'e'), 190)
    , (('b', 'f'), 191)
    , (('c', '0'), 192)
    , (('c', '1'), 193)
    , (('c', '2'), 194)
    , (('c', '3'), 195)
    , (('c', '4'), 196)
    , (('c', '5'), 197)
    , (('c', '6'), 198)
    , (('c', '7'), 199)
    , (('c', '8'), 200)
    , (('c', '9'), 201)
    , (('c', 'a'), 202)
    , (('c', 'b'), 203)
    , (('c', 'c'), 204)
    , (('c', 'd'), 205)
    , (('c', 'e'), 206)
    , (('c', 'f'), 207)
    , (('d', '0'), 208)
    , (('d', '1'), 209)
    , (('d', '2'), 210)
    , (('d', '3'), 211)
    , (('d', '4'), 212)
    , (('d', '5'), 213)
    , (('d', '6'), 214)
    , (('d', '7'), 215)
    , (('d', '8'), 216)
    , (('d', '9'), 217)
    , (('d', 'a'), 218)
    , (('d', 'b'), 219)
    , (('d', 'c'), 220)
    , (('d', 'd'), 221)
    , (('d', 'e'), 222)
    , (('d', 'f'), 223)
    , (('e', '0'), 224)
    , (('e', '1'), 225)
    , (('e', '2'), 226)
    , (('e', '3'), 227)
    , (('e', '4'), 228)
    , (('e', '5'), 229)
    , (('e', '6'), 230)
    , (('e', '7'), 231)
    , (('e', '8'), 232)
    , (('e', '9'), 233)
    , (('e', 'a'), 234)
    , (('e', 'b'), 235)
    , (('e', 'c'), 236)
    , (('e', 'd'), 237)
    , (('e', 'e'), 238)
    , (('e', 'f'), 239)
    , (('f', '0'), 240)
    , (('f', '1'), 241)
    , (('f', '2'), 242)
    , (('f', '3'), 243)
    , (('f', '4'), 244)
    , (('f', '5'), 245)
    , (('f', '6'), 246)
    , (('f', '7'), 247)
    , (('f', '8'), 248)
    , (('f', '9'), 249)
    , (('f', 'a'), 250)
    , (('f', 'b'), 251)
    , (('f', 'c'), 252)
    , (('f', 'd'), 253)
    , (('f', 'e'), 254)
    , (('f', 'f'), 255)
    ]
