import Data.List

-- Break input into voting groups
groups :: [String] -> [[String]]
groups = fmap (filter (/= "")) . groupBy (\a -> \b -> b /= "")

-- Vote of voting group
votes :: [String] -> [Char]
votes (x:xs) = foldr intersect x xs

votes' = length . votes

solve :: [String] -> [Int]
solve = map votes'. groups

main :: IO ()
main = interact $ show . sum . solve . lines
