{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Lib

main :: IO ()
main = someFunc

pm :: [a] -> String
pm xs = "The list has " ++ case xs of [] -> "no"
                                      (_:[]) -> "one"
                                      (_:_:[]) -> "two"
                                      (_:_:_) -> "more than two"
                        ++ " element"
                        ++ case xs of (_:[]) -> ""
                                      _ -> "s"


-- It really only makes sense for Integral i
-- but just in case, we permit this and effectively
-- truncate i if it is non-Integral
tk :: (Num i, Ord i) => i -> [a] -> [a]
tk i _
    | i < 1     = []  -- this acts as truncate (i <= 0 would ceil)
tk _ []         = []
tk i (x:xs)     = x : tk (i - 1) xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =     quicksort [a | a <- xs, a <= x]
                    ++ [x]
                    ++ quicksort [a | a <- xs, a > x]

oneOver :: (Floating a) => a -> a
oneOver = (1/)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

type SomeTy = String

pmst :: SomeTy -> Bool
pmst x = True
