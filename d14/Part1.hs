module Main where

import Data.Char (isSpace, isNumber)

type Memory = ([Maybe Int], [Int])
type Instruction = (Memory -> Memory)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn _ [] = []
splitOn f (x:xs) =
    if f x
       then "": splitOn f xs
       else case splitOn f xs of
              (t:ts) -> (x:t):ts
              [] -> [[x]]


stripL :: String -> String
stripL (x:xs) = if isSpace x
                   then stripR xs
                   else x:xs


stripR :: String -> String
stripR = reverse . stripL . reverse


maskToList :: String -> [Maybe Int]
maskToList = map (\c -> if c == 'X'
                           then Nothing
                           else Just (read (c:[]) :: Int))

getCell :: String -> Int
getCell info = read number :: Int
    where number = filter (isNumber) info


asBinary' :: Int -> [Int]
asBinary' v
  | v == 0 = [0]
  | v == 1 = [1]
  | otherwise = m:asBinary' d
  where m = v `mod` 10
        d = v `div` 10

asBinary = reverse . asBinary'


fromBinary :: [Int] -> Int
fromBinary = sum . zipWith (\ex v -> v * (10^ex)) [0..] . reverse

updateVal :: [Maybe Int] -> Int -> Int
updateVal mask val = fromBinary $
    zipWith (\m bv ->
        case m of
          Nothing -> bv
          Just x -> x) mask $
              asBinary val

updateCell :: Int -> [Maybe Int] -> [Int] -> Int -> [Int]
updateCell cellId mask (c:cs) val
  | cellId == 0 = (updateVal mask val) : cs
  | otherwise   = c : (updateCell (cellId -1) mask cs val)


toInstruction :: String -> Instruction
toInstruction code
  | assignee == "mask" = \(mask, cells) -> (maskToList value, cells)
  | otherwise = \(mask, cells) -> (mask, updateCell (length cells - cell - 1) mask cells (read value :: Int))
  where (assignee':value':[]) = splitOn (== '=') code
        assignee = stripR assignee'
        value = stripL value'
        cell = getCell assignee


execute :: Memory -> [Instruction] -> Memory
execute m instructions = (foldr (.) id instructions) $ m


sumFields :: Memory -> Int
sumFields = undefined


initialMem = ([], map (const 0) [0..36])

main :: IO ()
main = interact $ show . execute initialMem . map toInstruction . lines
