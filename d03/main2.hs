
isOccurrence :: (Int, String) -> Bool
isOccurrence (position, line) = line !! (position `mod` (length line) ) == '#'

dropLines :: Int -> [String] -> [String]
dropLines index inputLines = map snd $ filter (\i -> fst i `mod` index == 0) $ zip [0..] inputLines


solve :: (Int, Int) -> [String] -> Int
solve (slopeX, slopeY) inputLines = length $ filter isOccurrence $ zip (map (*slopeX) [0..]) $ dropLines slopeY inputLines

slopes = [(1, 1)
    ,(3, 1)
    ,(5, 1)
    ,(7, 1)
    ,(1, 2)]


solveAll :: [String] -> Int
solveAll input = product $ map (flip solve input) slopes

main :: IO ()
main = readFile "input.txt" >>= putStrLn . show . solveAll . lines
