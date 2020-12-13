module Main where

import Data.Ord (comparing)
import Data.List (sortBy)


solve :: Int -> [Int] -> Int
solve arriveTime busses = b * t
    where times = map (\b -> (b, b - arriveTime `mod` b)) busses
          (b, t) = head $ sortBy (comparing snd) times


splitOn :: (Char -> Bool) -> String -> [String]
splitOn _ [] = []
splitOn f (x:xs) =
    if f x
       then "":(splitOn f xs)
       else case splitOn f xs of
              (t:ts) -> (x:t):ts
              [] -> [[x]]



parseInput :: [String] -> (Int, [Int])
parseInput input = (arriveTime, busses)
    where readInt = (read :: String -> Int)
          arriveTime = readInt $ head input
          busses = map readInt $ filter (/= "x") $ splitOn (==',') (input !! 1)


main :: IO ()
main = interact $ show . uncurry solve . parseInput . lines
