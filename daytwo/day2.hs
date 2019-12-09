import qualified Data.Sequence as DS

type Seq = DS.Seq

run :: Int -> (Int -> Int -> Int) -> Seq Int -> Seq Int
run zero f seq = DS.update position result seq
  where
    index = seq `DS.index` (zero + 1)
    index' = seq `DS.index` (zero + 2)
    result = (seq `DS.index` index) `f` (seq `DS.index` index')
    position = seq `DS.index` (zero + 3)

opCode :: Int -> Seq Int -> Seq Int
opCode zero seq =
  case code of
    1 -> run' (+)
    2 -> run' (*)
    99 -> seq
    x -> undefined
  where
    run' f = run zero f seq
    code = seq `DS.index` zero

runIntCode :: Seq Int -> Seq Int
runIntCode seq = iter seq 0 (seq `DS.index` 0)
  where
    iter seq counter 99 = seq
    iter seq counter c =
      let zero = 4 * counter
          next = opCode zero seq
       in iter next (succ counter) (next `DS.index` (zero + 4))

load :: (Int, Int) -> Seq Int -> Seq Int
load (noun, verb) seq = DS.update 2 verb $ DS.update 1 noun seq

prepare :: Seq Int -> Seq Int
prepare = load (2, 12)

split :: Char -> String -> [String]
split _ [] = [""]
split c (x:xs)
  | x == c = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = split c xs

bruteforce :: Seq Int -> Int -> (Int, Int)
bruteforce seq value =
  head $ [(x, x') | x <- [0 .. 99], x' <- [0 .. 99], verify (x, x')]
  where
    verify pair = (getOutput $ load pair seq) == value

getOutput :: Seq Int -> Int
getOutput seq = seq `DS.index` 0

main :: IO ()
main = do
  file <- readFile "input"
  let list = split ',' file
      ints = DS.fromList $ map (read :: String -> Int) list -- puzzle input as a sequence
      input = prepare ints -- the input but noun = 2 and verb = 12
      new = runIntCode input
      output = getOutput new -- part 1
      (noun, verb) = bruteforce ints 19690720 -- part 2
  print "output: "
  print output
  print "value:"
  print (100 * noun + verb)
