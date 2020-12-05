-- Advent of code [day 5.0]
toBinary' :: Char -> Int
toBinary' character
    | character == 'R' = 1
    | character == 'L' = 0
    | character == 'B' = 1
    | character == 'F' = 0


joinExp :: (Int, Int) -> Int
joinExp (a, b) = b * (2 ^ a)

binaryExponents :: String -> [Int]
binaryExponents = map joinExp . zip [0..] . reverse . map toBinary'

toBinary :: String -> Int
toBinary = sum . binaryExponents

asPosition :: String -> (Int, Int)
asPosition indications = (toBinary rows, toBinary columns)
    where (rows, columns) = splitAt 7 indications

seatId :: String -> Int
seatId = toBinary

-- maximum' :: [Int] -> Int
-- maximum' = foldr (\a -> \b -> if a > b then a else b) 0

main :: IO ()
main = interact $ show . maximum . map seatId . lines
