module Main where

-- x y facing
type Boat = (Int, Int, Int)

data Command = Command (Char, Int) deriving (Show)

instance Read Command where
    readsPrec _ input = [(Command (chr, amount), left)]
        where chr = head input
              amount = (read :: String -> Int)  $ tail input
              left = ""


directions = "ENWS"


moveY :: Boat -> Int -> Boat
moveY (x, y, f) amount = (x, y + amount, f)

moveX :: Boat -> Int -> Boat
moveX (x, y, f) amount = (x + amount, y, f)

move :: Boat -> Command -> Boat
move boat@(x, y, f) (Command (cmd, amount))
  | cmd == 'N' = moveY boat amount
  | cmd == 'S' = moveY boat (-amount)
  | cmd == 'W' = moveX boat (-amount)
  | cmd == 'E' = moveX boat amount
  | cmd == 'L' = (x, y, (f + amount) `mod` 360)
  | cmd == 'R' = (x, y, (f - amount + 360) `mod` 360)
  | cmd == 'F' = move boat (Command ((directions !! (f `div` 90)), amount))


dist :: Boat -> Int
dist (x, y, _) = (abs x) + (abs y)

main :: IO ()
main = interact $ show . dist . foldl move (0, 0, 0) . map (read :: String -> Command) . lines
