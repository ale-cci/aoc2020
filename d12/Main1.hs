module Main where

-- x y wayX, wayY
type Boat = (Int, Int, Int, Int)

data Command = Command (Char, Int) deriving (Show)

instance Read Command where
    readsPrec _ input = [(Command (chr, amount), left)]
        where chr = head input
              amount = (read :: String -> Int)  $ tail input
              left = ""

radians :: Floating a => a -> a
radians angle = angle * (pi / 180)

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate (x, y) angle = let deg = radians $ fromIntegral angle
                          cos' = round $ cos deg
                          sin' = round $ sin deg
                       in (x * cos' - y * sin'
                          ,x * sin' + y * cos')

move :: Boat -> Command -> Boat
move (x, y, wx, wy) (Command (cmd, amount))
  | cmd == 'N' = (x, y, wx, wy + amount)
  | cmd == 'S' = (x, y, wx, wy - amount)
  | cmd == 'E' = (x, y, wx + amount, wy)
  | cmd == 'W' = (x, y, wx - amount, wy)
  | cmd == 'L' = let (rx, ry) = rotate (wx, wy) amount in (x, y, rx, ry)
  | cmd == 'R' = let (rx, ry) = rotate (wx, wy) (-amount) in (x, y, rx, ry)
  | cmd == 'F' = (x + wx * amount, y + wy * amount, wx, wy)


dist :: Boat -> Int
dist (x, y, wx, wy) = (abs x) + (abs y)

main :: IO ()
main = interact $ show . dist . foldl move (0, 0, 10, 1) . map (read :: String -> Command) . lines
