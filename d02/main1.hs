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


isValid :: Validable -> Bool
isValid v = (lower v <= occurrences && occurrences <= upper v)
    where occurrences = length $ filter (==(entry v)) (code v)



main :: IO()
main = readFile "input.txt" >>= (\c -> putStrLn $ show . solve $ parseInput c)
