-- State of the accumulator
import Data.Char (isDigit)
import Data.List (find)


data State = State { accumulator :: Int
                   , pc :: Int
                   } deriving (Show)

instance Eq State where
    (==) s1 s2 = (pc s1) == (pc s2)

data Instruction = Instruction { opcode :: String
                               , arg1 :: Int
                               } deriving (Show)

instance Read Instruction where
    readsPrec _ input =
        let (opcode, rest) = span (/= ' ') $ dropWhile (==' ') input
            (sign, rest') = splitAt 1 $ dropWhile (==' ') rest
            (arg, rest'') = span isDigit $ rest'

            arg' = let argVal = read arg :: Int
                    in if sign == "+"
                          then argVal
                          else argVal * (-1)
         in
            [(Instruction opcode arg', rest'')]

type Program = [Instruction]


eval :: State -> Instruction -> State
eval s i
  | opcode i == "nop" = State (accumulator s) (pc s + 1)
  | opcode i == "jmp" = State (accumulator s) (pc s + arg1 i)
  | opcode i == "acc" = State (accumulator s + arg1 i)  (pc s + 1)


evalUntil :: ([State] -> State -> (Bool, [State])) -> [State] -> State -> Program -> (Bool, State)
evalUntil check states s p
  | continue = if instr == length p
                  then (False, s)
                  else evalUntil check states' state'  p
  | not continue = (True, s)
  where instr = pc s
        state' = eval s (p !! instr)
        (continue,states') = check states s


stateNotRepeated :: [State] -> State -> (Bool, [State])
stateNotRepeated states s
  | s `elem` states = (False, states)
  | otherwise = (True, s:states)


alter :: Int -> Program -> Program
alter _ [] = []
alter i (p0:ps)
  | i == 0 = (Instruction (if (opcode p0) == "jmp" then "nop" else "jmp") (arg1 p0)):ps
  | otherwise = p0:alter (i-1) ps


solve' :: Program -> Maybe (Bool, State)
solve' program = find (\(cond, _) -> not cond) $ fmap (\instr -> solve $ alter instr program) $ filter (\p -> opcode (program !! p) `elem` ["jmp", "nop"]) [0..]

solve :: Program -> (Bool, State)
solve = evalUntil stateNotRepeated [] (State 0 0)


main :: IO ()
main = interact $ show . solve' . map (read :: String -> Instruction) . lines
