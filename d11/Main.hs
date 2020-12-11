-- L: empty
-- #: occupied
-- .: floor



getAt :: [[Char]] -> Int -> Int -> Char
getAt grid x y
  | 0 <= x && x < width && 0 <= y && y < height = grid !! y !! x
  | otherwise = '.'
  where height = length grid
        width  = length (grid !! 0)


directions = [(-1, -1)
             ,(-1,  0)
             ,(-1,  1)
             ,( 0, -1)
             ,( 0,  1)
             ,( 1, -1)
             ,( 1,  0)
             ,( 1,  1)]


nextState' :: [[Char]] -> Int -> Int -> Char
nextState' grid x y
  | item == '#' && neightbours >= 4 = 'L'
  | item == 'L' && neightbours == 0 = '#'
  | otherwise = item
  where neightbours = length $ filter (=='#') $ map (\(dx, dy) -> getAt grid (x + dx) (y + dy)) directions
        item = getAt grid x y



nextState :: [[Char]] -> [[Char]]
nextState grid = zipWith (\y -> \row -> zipWith (\x -> \_ -> nextState' grid x y) [0..] row) [0..] grid


iterate' :: [[Char]] -> [[Char]] -> [[Char]]
iterate' prev curr = if prev == curr
                       then curr
                       else iterate' curr $ nextState curr


countOccupied :: [[Char]] -> Int
countOccupied = sum . map (\x -> if x == '#' then 1 else 0 ) . concat


main :: IO ()
main = interact $ show . countOccupied . iterate' [] . nextState . lines
