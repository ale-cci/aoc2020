-- Advent of Code [day 4.1]
import Data.List
import Text.Read

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


numberBetween :: Int -> Int -> String -> Bool
numberBetween lower upper item =
    case (readMaybe item :: Maybe Int) of
             Nothing -> False
             Just n -> lower <= n && n <= upper


isHex :: String -> Bool
isHex string = (all (`elem` "0123456789abcdef") string) && length string == 6

byr :: String -> Bool
byr = numberBetween 1920 2002
iyr = numberBetween 2010 2020
eyr = numberBetween 2020 2030

hgt :: String -> Bool
hgt value
  | measure == "cm" = numberBetween 150 193 size
  | measure == "in" = numberBetween 59 76 size
  | otherwise = False
  where measure = drop (length value - 2) value
        size = take (length value -2) value

hcl :: String -> Bool
hcl color = ((head color) == '#') && (isHex $ tail color)
ecl = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
pid item = isNumber /= Nothing && length item == 9
    where isNumber = readMaybe item :: Maybe Int


-- Entries with validator
entries = [("byr", byr)
          ,("iyr", iyr)
          ,("eyr", eyr)
          ,("hgt", hgt)
          ,("hcl", hcl)
          ,("ecl", ecl)
          ,("pid", pid)]


validate :: (String, String) -> Bool
validate (key, value) = case find (\(name, _) -> name == key) entries of
                          Nothing -> False
                          Just (_, validator) -> validator value


isValid :: [(String, String)] -> Bool
isValid passport = all validate validKeys && length validKeys == 7
    where validKeys = filter (\i -> fst i `elem` (map fst entries)) passport

solve :: String -> Int
solve input = length $ filter isValid $ joinPassports $ splitPassports input


main :: IO()
main = readFile "input.txt" >>= putStrLn . show . solve
