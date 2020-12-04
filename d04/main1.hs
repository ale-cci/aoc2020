import Data.List

emptyTail :: [a] -> [a]
emptyTail [] = []
emptyTail a = tail a

splitPassports :: String -> [String]
splitPassports = splitOneOf ['\n', ' ']


splitOneOf :: String -> String -> [String]
splitOneOf items [] = []
splitOneOf items string = [fst chunks] ++ (splitOneOf items $ emptyTail $ snd chunks)
    where chunks = break (`elem` items) string



joinPassports :: [String] -> [[(String, String)]]
joinPassports passports = normalizePassports $ filterGroups $ groupBy (\a -> \b -> b /= "") passports


parsePassport :: String -> (String, String)
parsePassport passport = (fst chunks, tail $ snd chunks)
    where chunks = break (==':') passport

isNotEmpty :: [l] -> Bool
isNotEmpty item = length item > 0

filterGroups :: [[String]] -> [[String]]
filterGroups = fmap (filter isNotEmpty) $ filter isNotEmpty

normalizePassports :: [[String]] -> [[(String, String)]]
normalizePassports = fmap (map parsePassport)


entries = ["byr"
          ,"iyr"
          ,"eyr"
          ,"hgt"
          ,"hcl"
          ,"ecl"
          ,"pid"]

isValid :: [(String, String)] -> Bool
isValid passport = all (`elem` keys) entries
    where keys = fmap fst passport

solve :: String -> Int
solve input = length $ filter isValid $ joinPassports $ splitPassports input


main :: IO()
main = readFile "input.txt" >>= putStrLn . show . solve
