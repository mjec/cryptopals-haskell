module Help
    ( return_help
    ) where

-- Public functions
return_help :: [String] -> Either (String, [String], Bool) String
return_help [] = Right $ unlines
    [ "Usage: cryptopals challenge [input data]"
    , "       cryptopals help [challenge]"
    , ""
    , "challenge     the cryptopals challenge (e.g. 1-3 for set 1 challenge 3)"
    , "input data    input data for the relevant function (challenge-dependent)"
    , "help          show this help, or help for the specified challenge"
    ]
return_help (cmd:args) = case lookup cmd help_dispatch
                         of Just action -> action args
                            Nothing     -> Left ("Unrecognised command", [], True)

-- Functions used by show_help
help_dispatch :: [(String, [String] -> Either (String, [String], Bool) String)]
help_dispatch = [ ("1-1", s1c1_help)
                , ("1-2", s1c2_help)
                , ("1-3", s1c3_help)
                , ("1-4", s1c4_help)
                ]

s1c1_help :: [String] -> Either (String, [String], Bool) String
s1c1_help _ = Right $ unlines
        [ "Usage: cryptopals 1-1 hex_string"
        , ""
        , "Returns the base64 encoding of hex_string"
        ]

s1c2_help :: [String] -> Either (String, [String], Bool) String
s1c2_help _ = Right $ unlines
        [ "Usage: cryptopals 1-2 hex_string hex_string"
        , ""
        , "Returns the hex encoded xor of the two hex_strings"
        ]

s1c3_help :: [String] -> Either (String, [String], Bool) String
s1c3_help _ = Right $ unlines
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

s1c4_help :: [String] -> Either (String, [String], Bool) String
s1c4_help _ = Right $ unlines
        [ "Usage: cryptopals 1-4 < file"
        , ""
        , "Takes hex strings on standard input (one string per line) and figures"
        , "out which line has been 'encrypted' with a single-byte xor. Returns"
        , "one line with the plain text."
        ]
