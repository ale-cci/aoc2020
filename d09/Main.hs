
isSumContained :: [Int] -> Int -> Bool
isSumContained [] _ = False
isSumContained (x:xs) total =
    if (total - x) `elem` xs
       then True
       else isSumContained xs total


shiftQueue :: [Int] -> Int -> [Int]
shiftQueue queue x = (tail queue) ++ [x]


firstInvalid :: [Int] -> [Int] -> Int
firstInvalid queue (x:xs) =
    if isSumContained queue x
       then firstInvalid (shiftQueue queue x) xs
       else x


solve :: [Int] -> Int
solve input = firstInvalid start rest
    where (start, rest) = splitAt 25 input


main :: IO ()
main = interact $ show . solve . map (read :: (String -> Int)) . lines
