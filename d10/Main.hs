module Main where

import Data.List (sort)

asSolution :: [Int] -> Int
asSolution jumps = num1 * num3
    where num3 = length $ filter (==3) jumps
          num1 = length $ filter (==1) jumps


countJumps :: [Int] -> [Int]
countJumps inputs = zipWith (-) (sInputs ++ [last sInputs +3]) (0:sInputs)
    where sInputs = sort inputs


solve :: [Int] -> Int
solve = asSolution . countJumps


main :: IO ()
main = interact $ show . solve . map (read :: (String -> Int)) . lines
