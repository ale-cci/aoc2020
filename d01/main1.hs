-- liens (String -> [String]) turns input into seq. of lines
-- words String -> [String]
-- read Individual strings to floats
import Data.List

parser :: String -> [Int]
parser input = map read (lines input)

find_solution :: [Int] -> Maybe Int
find_solution [] = Nothing
find_solution (x:xs) = (if (2020 - x) `elem` xs then Just x else find_solution xs)


solve :: [Int] -> Int
solve items = (case find_solution items of
    Just n -> (2020 - n) * n
    Nothing -> -1)


-- Finds two items which sum is equal to first argument
main = readFile "input.txt" >>= (\content ->  putStrLn $ show . solve $ parser content)
