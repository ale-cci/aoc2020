module Main where

import Data.List (sort)

solve :: [Int] -> Int
solve input = n sInput (last sInput)
    where sInput = sort input


memon list x
  | x < 0 = 0
  | otherwise = ((map (\i -> n list i) [0..]) !! x)

n :: [Int] -> Int -> Int
n list x
    | x == 0 = 1
    | x `elem` list = fn (x -1) + fn (x -2) + fn (x -3)
    | otherwise = 0
    where fn = memon list



main :: IO ()
main = interact $ show . solve . map (read :: (String -> Int)) . lines
