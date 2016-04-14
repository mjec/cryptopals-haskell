{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Help
import           Lib                  (Error, bytesToString, plusNL,
                                       stringToBytes)

import qualified Set1                 as S1

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as B
import           System.Environment
import           System.Exit
import           System.IO


-- Dispatcher
dispatch :: [(B.ByteString, [B.ByteString] -> Either Error B.ByteString)]
dispatch = [ (stringToBytes "help", returnHelp)
           , (stringToBytes "1-1", S1.challenge1)
           , (stringToBytes "1-2", S1.challenge2)
           , (stringToBytes "1-3", S1.challenge3)
           , (stringToBytes "1-4", S1.challenge4)
           , (stringToBytes "1-5", S1.challenge5)
           , (stringToBytes "1-6", S1.challenge6)
           , (stringToBytes "1-7", S1.challenge7)
           ]

-- The function takes arguments and (lines getContents) and returns what should
-- be passed to the dispatch function.
needsStdin :: [(B.ByteString, [B.ByteString] -> [B.ByteString] -> [B.ByteString])]
needsStdin = [ (stringToBytes "1-4", \_ stdin -> stdin)
             , (stringToBytes "1-5", \args stdin -> if null args then [] else head args : stdin)
             , (stringToBytes "1-6", \_ stdin -> stdin)
             , (stringToBytes "1-7", \args stdin -> if null args then [] else head args : stdin)
             ]

parseArgs :: B.ByteString -> [B.ByteString] -> IO ()
parseArgs _ []       = putStr $ bytesToString $ alwaysReturnHelp []
parseArgs input (cmd:args) = case cmdFunc
                              of Right s -> putStr $ bytesToString s
                                 Left (err, _, False) -> exitWithError err
                                 Left (err, hargs, True) -> exitWithUsage err hargs
      where cmdFunc = case lookup cmd dispatch
                      of Just action -> case lookup cmd needsStdin
                                        of Nothing    -> action args
                                           Just    fn -> action $ fn args [input]
                         Nothing     -> Left ("Unrecognised command", [], True)

exitWithError :: String -> IO ()
exitWithError msg = do
    hPutStrLn stderr msg
    exitFailure

exitWithUsage :: String -> [B.ByteString] -> IO ()
exitWithUsage msg args = do
    hPutStrLn stderr msg
    putStr $ bytesToString $ alwaysReturnHelp args
    exitFailure

alwaysReturnHelp :: [B.ByteString] -> B.ByteString
alwaysReturnHelp args = case returnHelp args
                          of Right s        -> s
                             Left (s, _, _) -> B.append (plusNL $ stringToBytes s) $ alwaysReturnHelp []

-- Entry point
main :: IO ()
main = do
    input <- B.hGetContents stdin
    args <- getArgs
    parseArgs input $ map stringToBytes args
