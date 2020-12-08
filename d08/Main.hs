-- State of the accumulator
import Data.Char (isDigit)


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


evalUntil :: ([State] -> State -> (Bool, [State])) -> [State] -> State -> Program -> State
evalUntil check states s p = let instr = pc s
                                 (continue,states') = check states s
                                 state' = eval s (p !! instr)
                              in
                              if continue
                                 then evalUntil check states' state'  p
                                    else s


stateNotRepeated :: [State] -> State -> (Bool, [State])
stateNotRepeated states s
  | s `elem` states = (False, states)
  | otherwise = (True, s:states)


solve :: Program -> Int
solve = accumulator . evalUntil stateNotRepeated [] (State 0 0)


main :: IO ()
main = interact $ show . solve . map (read :: String -> Instruction) . lines
