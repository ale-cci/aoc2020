import Data.List


parser :: String -> [Int]
parser input = map read (lines input)


-- Finds two items which sum is equal to first argument
find_subsum :: Int -> [Int] -> Maybe (Int, Int)
find_subsum _ [] = Nothing
find_subsum total (x:xs) = (
    if (total - x) `elem` xs
    then Just (x, total - x)
    else find_subsum total xs)


find_solution :: [Int] -> Maybe Int
find_solution [] = Nothing
find_solution (x:xs) = (
    case find_subsum (2020 -x) xs of
    Just (a, b) -> Just (a * b * x)
    Nothing -> find_solution xs)


solve :: [Int] -> Int
solve items = (case find_solution items of
    Just n -> n
    Nothing -> -1)


main = readFile "input.txt" >>= (\content ->  putStrLn $ show . solve $ parser content)
