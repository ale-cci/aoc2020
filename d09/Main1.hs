import Data.List

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


-- NOTE: does not work if the last number of the input is contained in
-- the solution
findSum :: [Int] -> [Int] -> Int -> [Int]
findSum (x:xs) current val
  | x == val = findSum xs current val
  | total == val = current
  | total > val = findSum (x:xs) (tail current) val
  | otherwise = findSum xs (current ++ [x]) val
  where total = sum current


calcResult :: [Int] -> Int
calcResult res' = (head res) + (last res)
    where res = sort res'

solve' :: [Int] -> Int
solve' inputs = calcResult $ findSum inputs [] $ solve inputs


main :: IO ()
main = interact $ show . solve' . map (read :: (String -> Int)) . lines
