module Lib
    (
      -- Base64
      base64_to_bytes
    , bytes_to_base64

      -- Base16
    , hex_to_bytes
    , bytes_to_hex
    , isHex

      -- ByteString helper functions
    , bitwiseCombine
    , bytes_to_string
    , buildFreqTable
    , freqTableDifference
    , freqTableDelta
    , word8ToChar
    , charToWord8

      -- Data
    , englishFreqTable
    , asciiFreqTable
    , asciiFreqTableNoNL
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text.Lazy as Txt
import qualified Data.Text.Lazy.Encoding as TxtEnc
import qualified Data.Map as Map
import Data.Bits
import Data.Word
import Data.List

-- Base64 functions
base64_to_bytes :: String -> B.ByteString
base64_to_bytes  = B64.decodeLenient . TxtEnc.encodeUtf8 . Txt.pack

bytes_to_base64 :: B.ByteString -> String
bytes_to_base64 = Txt.unpack . TxtEnc.decodeUtf8 . B64.encode


-- Base16 (hex) functions
hex_to_bytes :: String -> B.ByteString
hex_to_bytes = fst . B16.decode . TxtEnc.encodeUtf8 . Txt.pack

bytes_to_hex :: B.ByteString -> String
bytes_to_hex = Txt.unpack . TxtEnc.decodeUtf8 . B16.encode

isHex :: String -> Bool
isHex x = all (`elem` ['A'..'F'] ++ ['a'..'f'] ++ ['0'..'9']) x


-- ByteString helper functions
bitwiseCombine :: (Word8 -> Word8 -> Word8) -> B.ByteString -> B.ByteString -> B.ByteString
bitwiseCombine f x y = B.pack $ B.zipWith (\x y -> (x `f` y)) x y

buildFreqTable :: [Word8] -> B.ByteString -> (Float, Map.Map Word8 Float)
-- The map that is returned is (k, v) with one k for each needle (first [Word8])
-- so v will be 0 for those not found. Otherwise v is the proportion of k occurring
-- in the subset of haystack which contains only the needles.
-- The Float that is returned is the proportion of the total length of the haystack
-- which is Word8s not in the needles.
buildFreqTable needles haystack =
        ( genericLength not_in_needles / genericLength hs  -- haystack characters not in needles
        , Map.union the_table $ Map.fromList $ zip needles $ repeat (0::Float)) -- populate with 0s where required
    where hs = B.unpack haystack  -- convert ByteString -> [Word8]
          (in_needles, not_in_needles) = partition (`elem` needles) hs  -- partition on whether contained or not
          table_gen n the_list = Map.fromListWith (\x _ -> x + (1 / n)) the_list  -- we add 1/n for each occurrance
          the_table = table_gen (genericLength in_needles) $ zip hs $ repeat (0::Float)

charToWord8 :: Char -> Word8
charToWord8 = B.head . TxtEnc.encodeUtf8 . Txt.singleton

word8ToChar :: Word8 -> Char
word8ToChar = head . Txt.unpack . TxtEnc.decodeUtf8 . B.singleton

bytes_to_string :: B.ByteString -> String
bytes_to_string = Txt.unpack . TxtEnc.decodeUtf8With (\err wrd -> Just '�')

freqTableDelta :: Map.Map Word8 Float -> Map.Map Word8 Float -> Float
freqTableDelta x y = sum $ Map.elems $ freqTableDifference x y

freqTableDifference :: Map.Map Word8 Float -> Map.Map Word8 Float -> Map.Map Word8 Float
freqTableDifference x y = Map.differenceWith (\a b -> Just $ abs (a - b)) x y


-- Data
englishFreqTable :: Map.Map Word8 Float
englishFreqTable = Map.fromList
    [ (charToWord8 'A', 0.0651738)
    , (charToWord8 'B', 0.0124248)
    , (charToWord8 'C', 0.0217339)
    , (charToWord8 'D', 0.0349835)
    , (charToWord8 'E', 0.1041442)
    , (charToWord8 'F', 0.0197881)
    , (charToWord8 'G', 0.0158610)
    , (charToWord8 'H', 0.0492888)
    , (charToWord8 'I', 0.0558094)
    , (charToWord8 'J', 0.0009033)
    , (charToWord8 'K', 0.0050529)
    , (charToWord8 'L', 0.0331490)
    , (charToWord8 'M', 0.0202124)
    , (charToWord8 'N', 0.0564513)
    , (charToWord8 'O', 0.0596302)
    , (charToWord8 'P', 0.0137645)
    , (charToWord8 'Q', 0.0008606)
    , (charToWord8 'R', 0.0497563)
    , (charToWord8 'S', 0.0515760)
    , (charToWord8 'T', 0.0729357)
    , (charToWord8 'U', 0.0225134)
    , (charToWord8 'V', 0.0082903)
    , (charToWord8 'W', 0.0171272)
    , (charToWord8 'X', 0.0013692)
    , (charToWord8 'Y', 0.0145984)
    , (charToWord8 'Z', 0.0007836)
    , (charToWord8 ' ', 0.1918182)
    ]

asciiFreqTableNoNL :: Map.Map Word8 Float
asciiFreqTableNoNL = Map.update (\_ -> Just 0.0) (10::Word8) asciiFreqTable

asciiFreqTable :: Map.Map Word8 Float
asciiFreqTable = Map.fromList -- This comes from IMDb biographies (~154 MB)
    [ ( 10::Word8, 0.0166623841) -- New lines are common!
      -- NB: this uses \n line endings; you should strip \r endings!
    , ( 32::Word8, 0.1493452336)
    , ( 33::Word8, 0.0000877131)
    , ( 34::Word8, 0.0039434812)
    , ( 35::Word8, 0.0000242677)
    , ( 36::Word8, 0.0000232542)
    , ( 37::Word8, 0.0000050115)
    , ( 38::Word8, 0.0002297750)
    , ( 39::Word8, 0.0036935298)
    , ( 40::Word8, 0.0033630778)
    , ( 41::Word8, 0.0033603918)
    , ( 42::Word8, 0.0000068084)
    , ( 43::Word8, 0.0000100105)
    , ( 44::Word8, 0.0109746709)
    , ( 45::Word8, 0.0018378580)
    , ( 46::Word8, 0.0083998790)
    , ( 47::Word8, 0.0003024661)
    , ( 48::Word8, 0.0029106658)
    , ( 49::Word8, 0.0031379039)
    , ( 50::Word8, 0.0020484266)
    , ( 51::Word8, 0.0006482944)
    , ( 52::Word8, 0.0006231376)
    , ( 53::Word8, 0.0006974888)
    , ( 54::Word8, 0.0006034835)
    , ( 55::Word8, 0.0006037695)
    , ( 56::Word8, 0.0007166642)
    , ( 57::Word8, 0.0024395819)
    , ( 58::Word8, 0.0002724035)
    , ( 59::Word8, 0.0002067820)
    , ( 60::Word8, 0.0000000249)
    , ( 61::Word8, 0.0000020270)
    , ( 62::Word8, 0.0000000311)
    , ( 63::Word8, 0.0000311134)
    , ( 64::Word8, 0.0000022322)
    , ( 65::Word8, 0.0044852354)
    , ( 66::Word8, 0.0029482766)
    , ( 67::Word8, 0.0037505026)
    , ( 68::Word8, 0.0022110691)
    , ( 69::Word8, 0.0013691369)
    , ( 70::Word8, 0.0022042669)
    , ( 71::Word8, 0.0014638200)
    , ( 72::Word8, 0.0030768151)
    , ( 73::Word8, 0.0021547430)
    , ( 74::Word8, 0.0013806024)
    , ( 75::Word8, 0.0008927370)
    , ( 76::Word8, 0.0020431788)
    , ( 77::Word8, 0.0031132383)
    , ( 78::Word8, 0.0015841325)
    , ( 79::Word8, 0.0010364217)
    , ( 80::Word8, 0.0021186990)
    , ( 81::Word8, 0.0000892488)
    , ( 82::Word8, 0.0016801151)
    , ( 83::Word8, 0.0047621654)
    , ( 84::Word8, 0.0038583547)
    , ( 85::Word8, 0.0008843866)
    , ( 86::Word8, 0.0010261563)
    , ( 87::Word8, 0.0015732453)
    , ( 88::Word8, 0.0000846415)
    , ( 89::Word8, 0.0006201096)
    , ( 90::Word8, 0.0001401904)
    , ( 91::Word8, 0.0000163028)
    , ( 92::Word8, 0.0000004352)
    , ( 93::Word8, 0.0000162966)
    , ( 94::Word8, 0.0000001741)
    , ( 95::Word8, 0.0013607182)
    , ( 96::Word8, 0.0000021513)
    , ( 97::Word8, 0.0664147438)
    , ( 98::Word8, 0.0082465257)
    , ( 99::Word8, 0.0227508279)
    , (100::Word8, 0.0313071255)
    , (101::Word8, 0.0879005501)
    , (102::Word8, 0.0150727676)
    , (103::Word8, 0.0151489158)
    , (104::Word8, 0.0336337629)
    , (105::Word8, 0.0583783203)
    , (106::Word8, 0.0008706828)
    , (107::Word8, 0.0053374890)
    , (108::Word8, 0.0310655990)
    , (109::Word8, 0.0179510951)
    , (110::Word8, 0.0589311109)
    , (111::Word8, 0.0548135280)
    , (112::Word8, 0.0122177814)
    , (113::Word8, 0.0017306401)
    , (114::Word8, 0.0527059338)
    , (115::Word8, 0.0472023266)
    , (116::Word8, 0.0553994775)
    , (117::Word8, 0.0181021976)
    , (118::Word8, 0.0090335307)
    , (119::Word8, 0.0126625084)
    , (120::Word8, 0.0011941461)
    , (121::Word8, 0.0115988780)
    , (122::Word8, 0.0009457739)
    , (123::Word8, 0.0000044208)
    , (124::Word8, 0.0000000062)
    , (125::Word8, 0.0000044332)
    , (126::Word8, 0.0000008953)
    , (128::Word8, 0.0000000062)
    , (129::Word8, 0.0000000062)
    , (130::Word8, 0.0000000062)
    , (131::Word8, 0.0000000124)
    , (132::Word8, 0.0000000062)
    , (139::Word8, 0.0000000062)
    , (157::Word8, 0.0000000062)
    , (159::Word8, 0.0000000062)
    , (160::Word8, 0.0000017720)
    , (161::Word8, 0.0000004415)
    , (162::Word8, 0.0000000746)
    , (163::Word8, 0.0000009575)
    , (165::Word8, 0.0000000062)
    , (166::Word8, 0.0000000062)
    , (167::Word8, 0.0000000249)
    , (168::Word8, 0.0000008580)
    , (169::Word8, 0.0000005409)
    , (170::Word8, 0.0000000808)
    , (171::Word8, 0.0000015420)
    , (172::Word8, 0.0000000497)
    , (173::Word8, 0.0000005036)
    , (174::Word8, 0.0000048685)
    , (175::Word8, 0.0000000124)
    , (176::Word8, 0.0000004042)
    , (177::Word8, 0.0000000435)
    , (178::Word8, 0.0000000684)
    , (179::Word8, 0.0000000870)
    , (182::Word8, 0.0000000062)
    , (183::Word8, 0.0000006529)
    , (184::Word8, 0.0000000311)
    , (185::Word8, 0.0000000870)
    , (186::Word8, 0.0000001990)
    , (187::Word8, 0.0000015544)
    , (188::Word8, 0.0000000622)
    , (189::Word8, 0.0000005409)
    , (190::Word8, 0.0000000249)
    , (191::Word8, 0.0000002301)
    , (192::Word8, 0.0000002238)
    , (193::Word8, 0.0000011130)
    , (194::Word8, 0.0000000560)
    , (195::Word8, 0.0000001803)
    , (196::Word8, 0.0000002674)
    , (197::Word8, 0.0000005596)
    , (198::Word8, 0.0000000187)
    , (199::Word8, 0.0000002425)
    , (200::Word8, 0.0000001057)
    , (201::Word8, 0.0000021700)
    , (202::Word8, 0.0000000187)
    , (203::Word8, 0.0000000249)
    , (205::Word8, 0.0000001057)
    , (206::Word8, 0.0000000435)
    , (208::Word8, 0.0000000062)
    , (209::Word8, 0.0000000373)
    , (210::Word8, 0.0000000311)
    , (211::Word8, 0.0000003731)
    , (212::Word8, 0.0000001554)
    , (213::Word8, 0.0000000435)
    , (214::Word8, 0.0000006777)
    , (215::Word8, 0.0000000622)
    , (216::Word8, 0.0000002549)
    , (218::Word8, 0.0000002487)
    , (219::Word8, 0.0000000373)
    , (220::Word8, 0.0000002984)
    , (221::Word8, 0.0000000124)
    , (222::Word8, 0.0000000311)
    , (223::Word8, 0.0000007026)
    , (224::Word8, 0.0000033700)
    , (225::Word8, 0.0000179878)
    , (226::Word8, 0.0000017161)
    , (227::Word8, 0.0000048622)
    , (228::Word8, 0.0000064913)
    , (229::Word8, 0.0000027669)
    , (230::Word8, 0.0000009575)
    , (231::Word8, 0.0000048498)
    , (232::Word8, 0.0000083566)
    , (233::Word8, 0.0000646641)
    , (234::Word8, 0.0000015917)
    , (235::Word8, 0.0000024871)
    , (236::Word8, 0.0000002674)
    , (237::Word8, 0.0000133618)
    , (238::Word8, 0.0000006839)
    , (239::Word8, 0.0000011316)
    , (240::Word8, 0.0000002549)
    , (241::Word8, 0.0000076105)
    , (242::Word8, 0.0000005409)
    , (243::Word8, 0.0000133867)
    , (244::Word8, 0.0000032705)
    , (245::Word8, 0.0000006529)
    , (246::Word8, 0.0000101162)
    , (248::Word8, 0.0000032705)
    , (249::Word8, 0.0000003917)
    , (250::Word8, 0.0000034570)
    , (251::Word8, 0.0000007461)
    , (252::Word8, 0.0000090219)
    , (253::Word8, 0.0000003668)
    , (254::Word8, 0.0000000497)
    , (255::Word8, 0.0000000249)
    ]
