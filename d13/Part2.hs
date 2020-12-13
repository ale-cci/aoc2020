module Main where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Bifunctor (second)


solveN :: (Int, Int) -> (Int, Int) -> (Int, Int)
solveN (t, acc) (dt, n) =
    let k = fromJust $ find (\k -> (t + k * acc) `mod` n == (n - dt) `mod` n) [1..n]
     in (t + k * acc, lcm acc n)


solve :: [(Int, Int)] -> Int
solve = fst . foldl solveN (0, 1)


splitOn :: (Char -> Bool) -> String -> [String]
splitOn _ [] = []
splitOn f (x:xs) =
    if f x
       then "":splitOn f xs
       else case splitOn f xs of
              (t:ts) -> (x:t):ts
              [] -> [[x]]


prepareInput :: [String] -> [(Int, Int)]
prepareInput =
    map (second (read :: String -> Int))
    . filter (\(t, b) -> b /= "x")
    . zip [0..]
    . splitOn (==',')
    . (!! 1)


main :: IO ()
main = interact $ show . solve . prepareInput . lines
