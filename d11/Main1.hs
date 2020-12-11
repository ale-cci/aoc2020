-- L: empty
-- #: occupied
-- .: floor


getAt :: [[Char]] -> Int -> Int -> Char
getAt grid x y
  | 0 <= x && x < width && 0 <= y && y < height = grid !! y !! x
  | otherwise = '.'
  where height = length grid
        width  = length (head grid)


directions = [(-1, -1)
             ,(-1,  0)
             ,(-1,  1)
             ,( 0, -1)
             ,( 0,  1)
             ,( 1, -1)
             ,( 1,  0)
             ,( 1,  1)]


isOutSide :: [[Char]] -> Int -> Int -> Bool
isOutSide grid x y
  | 0 <= x && x < width && 0 <= y && y < height = False
  | otherwise = True
  where height = length grid
        width  = length (head grid)


getCharInD :: [[Char]] -> Int -> Int -> (Int, Int) -> Char
getCharInD grid x y dir@(dx, dy)
  | isOutSide grid nx ny = '.'
  | chr == '.' = getCharInD grid nx ny dir
  | otherwise = chr
  where (nx, ny) = (x + dx, y + dy)
        chr = getAt grid nx ny

getNeightbours :: [[Char]] -> Int -> Int -> [Char]
getNeightbours grid x y = map (getCharInD grid x y) directions


nextState' :: [[Char]] -> Int -> Int -> Char
nextState' grid x y
  | item == '#' && occupied >= 5 = 'L'
  | item == 'L' && occupied == 0 = '#'
  | otherwise = item
  where neightbours = getNeightbours grid x y
        occupied = length $ filter (=='#') neightbours
        item = getAt grid x y



nextState :: [[Char]] -> [[Char]]
nextState grid = zipWith (\y row -> zipWith (\x _ -> nextState' grid x y) [0..] row) [0..] grid


iterate' :: [[Char]] -> [[Char]] -> [[Char]]
iterate' prev curr = if prev == curr
                       then curr
                       else iterate' curr $ nextState curr


countOccupied :: [[Char]] -> Int
countOccupied = sum . map (\x -> if x == '#' then 1 else 0 ) . concat


main :: IO ()
main = interact $ show . countOccupied . iterate' [] . nextState . lines
