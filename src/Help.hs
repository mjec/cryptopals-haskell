module Help
    ( returnHelp
    ) where

import qualified Data.ByteString.Lazy as B
import           Lib                  (Error, stringToBytes)

-- Public functions
returnHelp :: [B.ByteString] -> Either Error B.ByteString
returnHelp [] = Right $ stringToBytes $ unlines
    [ "Usage: cryptopals challenge [input data]"
    , "       cryptopals help [challenge]"
    , ""
    , "challenge     the cryptopals challenge (e.g. 1-3 for set 1 challenge 3)"
    , "input data    input data for the relevant function (challenge-dependent)"
    , "help          show this help, or help for the specified challenge"
    ]
returnHelp (cmd:args) = case lookup cmd helpDispatch
                         of Just action -> action args
                            Nothing     -> Left ("Unrecognised command", [], True)

-- Functions used by show_help
helpDispatch :: [(B.ByteString, [B.ByteString] -> Either Error B.ByteString)]
helpDispatch = [ (stringToBytes "1-1", s1c1Help)
                , (stringToBytes "1-2", s1c2Help)
                , (stringToBytes "1-3", s1c3Help)
                , (stringToBytes "1-4", s1c4Help)
                , (stringToBytes "1-5", s1c5Help)
                ]

s1c1Help :: [B.ByteString] -> Either Error B.ByteString
s1c1Help _ = Right $ stringToBytes $ unlines
        [ "Usage: cryptopals 1-1 hex_string"
        , ""
        , "Returns the base64 encoding of hex_string"
        ]

s1c2Help :: [B.ByteString] -> Either Error B.ByteString
s1c2Help _ = Right $ stringToBytes $ unlines
        [ "Usage: cryptopals 1-2 hex_string hex_string"
        , ""
        , "Returns the hex encoded xor of the two hex_strings"
        ]

s1c3Help :: [B.ByteString] -> Either Error B.ByteString
s1c3Help _ = Right $ stringToBytes $ unlines
        [ "Usage: cryptopals 1-3 hex_string [num]"
        , ""
        , "Takes a hex encoded string of ASCII-encoded English plain text which"
        , "has been xor'd with one repeating byte."
        , ""
        , "If called without num this returns the most probable plain text."
        , ""
        , "If called with num (a positive integer) this returns (at most) that"
        , "many rows of possibilities in descending order of likelihood. Each"
        , "row contains three fields, each separated by a colon. The first field"
        , "is the xor key (in hex). The second field is the distance score (lower"
        , "is more likely). The third field is the possible plain text."
        ]

s1c4Help :: [B.ByteString] -> Either Error B.ByteString
s1c4Help _ = Right $ stringToBytes $ unlines
        [ "Usage: cryptopals 1-4 < file"
        , ""
        , "Takes hex strings on standard input (one string per line) and figures"
        , "out which line has been 'encrypted' with a single-byte xor. Returns"
        , "one line with the plain text."
        ]


s1c5Help :: [B.ByteString] -> Either Error B.ByteString
s1c5Help _ = Right $ stringToBytes $ unlines
        [ "Usage: cryptopals 1-5 key < file"
        , ""
        , "Takes an ASCII key (not hex!) and then arbitrary standard input."
        , "Returns each line of the input xor'd with the key (repeating as"
        , "necessary). One output line per input line. Each output line is"
        , "hex encoded."
        ]
