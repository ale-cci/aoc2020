-- Advent of Code [day 5.1]
import Data.List

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

isSucc :: Int -> Int -> Bool
isSucc a b = abs (b - a) == 1

findMissing :: [Int] -> Int
findMissing [] = -1
findMissing (x:y:xs) = if isSucc x y
                          then findMissing $ [y] ++ xs
                          else x + 1

findSeat :: String -> Int
findSeat input = findMissing . sort. map seatId . lines $ input


main :: IO ()
main = interact $ show . findSeat
