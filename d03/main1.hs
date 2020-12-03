
isOccurrence :: (Int, String) -> Bool
isOccurrence (position, line) = line !! (position `mod` (length line) ) == '#'


multiples3 = map (*3) [0..]

solve :: [String] -> Int
solve inputLines = length $ filter isOccurrence $ zip multiples3 inputLines


prepareInput input = lines input


main :: IO ()
main = readFile "input.txt" >>= putStrLn . show . solve . prepareInput
