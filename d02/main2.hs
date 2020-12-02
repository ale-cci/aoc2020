data Validable = Validable { lower :: Int
    , upper :: Int
    , entry :: Char
    , code :: String }


-- Convert input line to Validable: from - to - character - string
toValidate :: String -> Validable
toValidate input = Validable (read $ takeWhile (/= '-') (head chunks)) (read $ tail $ dropWhile (/= '-') (head chunks)) (head $ last $ init chunks) (last chunks)
    where chunks = words input


parseInput :: String -> [Validable]
parseInput input = map toValidate $ lines input


solve :: [Validable] -> Int
solve inputs = length $ filter isValid inputs


xor a b = a /= b

isRequestedChar :: Validable -> Int -> Bool
isRequestedChar v pos = (code v) !! (pos -1) == entry v

isValid :: Validable -> Bool
isValid v = (isRequestedChar v (lower v)) `xor` (isRequestedChar v (upper v))



main :: IO()
main = readFile "input.txt" >>= (\c -> putStrLn $ show . solve $ parseInput c)
