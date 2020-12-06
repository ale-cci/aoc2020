import Data.List

groupQuestions :: [Char] -> Int
groupQuestions = length . nub

groups :: String -> [String]
groups = fmap (filter (/= ' ') . concat) . groupBy (\a b -> b /= "") . lines

main :: IO ()
main = interact $ show . sum . fmap groupQuestions . groups
