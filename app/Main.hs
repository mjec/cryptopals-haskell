{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Help

import qualified Set1 as S1

import System.Environment
import System.IO
import System.Exit


-- Dispatcher
-- Functions to which we dispatch have this signature:
--    [String] -> Either  (String, [String], Bool)  String
--      ^                     ^        ^       ^      ^-- what to print
--      |                     |        |       |
--      |                     |        |       --- show usage
--      |                     |        |
--      |                     |        --- arguments for help
--      |                     |
--      |                     --- error message
--      |
--      --- command line arguments
--
dispatch :: [(String, ([String] -> Either (String, [String], Bool) String))]
dispatch = [ ("help", return_help)
           , ("1-1", S1.challenge1)
           , ("1-2", S1.challenge2)
           , ("1-3", S1.challenge3)
           -- , ("1-4", S1.challenge4)
           -- , ("1-5", S1.challenge5)
           -- , ("1-6", S1.challenge6)
           -- , ("1-7", S1.challenge7)
           -- , ("1-8", S1.challenge8)
           ]

-- The function takes arguments and (lines getContents) and returns what should
-- be passed to the dispatch function.
needs_stdin :: [(String, [String] -> [String] -> [String])]
needs_stdin = [ ("1-4", \_ stdin -> stdin)
              ]

parse_args :: String -> [String] -> IO ()
parse_args _ []       = putStr $ always_return_help []
parse_args input (cmd:args) = case cmdFunc
                              of Right s -> putStr s
                                 Left (err, hargs, True) -> usage_and_exit err hargs
                                 Left (err, _, False) -> error_and_exit err
      where cmdFunc = case lookup cmd dispatch
                      of Just action -> case lookup cmd needs_stdin
                                        of Nothing    -> action args
                                           Just    fn -> action $ (fn args (lines input))
                         Nothing     -> Left ("Unrecognised command", [], True)

error_and_exit :: String -> IO ()
error_and_exit msg = do
    hPutStrLn stderr msg
    exitFailure

usage_and_exit :: String -> [String] -> IO ()
usage_and_exit msg args = do
    hPutStrLn stderr msg
    putStr $ always_return_help args
    exitFailure

always_return_help :: [String] -> String
always_return_help args = case return_help args
                          of Right s        -> s
                             Left (s, _, _) -> unlines $ s : lines (always_return_help [])

-- Entry point
main :: IO ()
main = do
    input <- getContents
    args <- getArgs
    parse_args input args
