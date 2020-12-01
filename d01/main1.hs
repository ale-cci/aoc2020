-- liens (String -> [String]) turns input into seq. of lines
-- words String -> [String]
-- read Individual strings to floats
import Data.List

parser :: String -> [Int]
parser input = map read (lines input)

first_halve :: [a] -> [a]
first_halve xs = (case xs of
    [] -> []
    xs -> take ((length xs) `div` 2) xs)

second_halve :: [a] -> [a]
second_halve xs = (case xs of
    [] -> []
    xs -> drop ((length xs) `div` 2) xs)


find_solution :: [Int] -> Maybe Int
find_solution items = find (\x -> 2020 - x `elem` second_halve items) (first_halve items)


solve :: [Int] -> Int
solve items = (case find_solution items of
    Just n -> (2020 - n) * n
    Nothing -> -1)


-- Finds two items which sum is equal to first argument
main = readFile "input.txt" >>= (\content ->  putStrLn $ show . solve $ parser content)
