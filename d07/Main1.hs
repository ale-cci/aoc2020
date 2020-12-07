-- Bag parser
import Data.List
import Text.Read

data Bag = EmptyBag
  | QBag (Int, String)
  deriving (Show)

type BagSpec = (Bag, [Bag])

instance (Eq Bag) where
    (==) EmptyBag EmptyBag = True
    (==) (QBag (q1, n1)) (QBag (q2, n2)) = q1 == q2 && n1 == n2
    (==) _ _ = False



parseBag :: [String] -> Bag
parseBag bagData
  | name == "no other" = EmptyBag
  | otherwise = case readMaybe quantity :: Maybe Int of
                  Just q -> QBag (q, name')
                  Nothing -> QBag (1, name)
  where name = (unwords . init) bagData
        (quantity, name') = ((head . words) name, (unwords . tail . words) name)


splitOn :: Char -> String -> [String]
splitOn pattern [] = []
splitOn pattern input = x : (splitOn pattern $ xs')
    where (x, xs) = break (==pattern) input
          xs' = case stripPrefix (pattern:[]) xs of
                  Just t -> t
                  Nothing -> xs


parseLine :: String -> BagSpec
parseLine statement = (parseBag bag , map (parseBag . words) bags)
    where (bag, bags') = break (=="contain") $ words statement
          bags = splitOn ',' $ filter (/= '.') $ unwords $ tail bags'



getBag :: [BagSpec] -> String -> BagSpec
getBag bags [] = (EmptyBag, [])
getBag bags name = case find (\(QBag (_, name'), bs) -> name == name') bags of
                                 Just b -> b

getName :: Bag -> String
getName EmptyBag = ""
getName (QBag (q, name)) = name

getWeight :: Bag -> Int
getWeight EmptyBag = 0
getWeight (QBag (q, n)) = q

containedBags :: [BagSpec] -> Bag -> Int
containedBags bags EmptyBag = 1
containedBags bags (QBag (q, name)) = 1 + sum (fmap (\b -> (getWeight b) * (containedBags bags b)) content)
    where (_, content) = getBag bags name


solve :: String -> [BagSpec] -> Int
solve source bags = containedBags bags (QBag (1, source))


main :: IO ()
main = interact $ show . solve "shiny gold" . fmap parseLine . lines
