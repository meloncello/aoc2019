import qualified Data.Set as DS
import Data.Set ((\\))

data Direction
  = R
  | L
  | D
  | U
  deriving (Eq, Show, Read)

type Line = (Char, Integer)

type Position = (Integer, Integer)

type PointSet = DS.Set Position

manhattanDistance :: Position -> Integer
manhattanDistance (x, y) = (abs x) + (abs y)

finalPosition :: Position -> Line -> Position
finalPosition (x, y) ('R', n) = (x + n, y)
finalPosition (x, y) ('U', n) = (x, y + n)
finalPosition (x, y) ('L', n) = (x - n, y)
finalPosition (x, y) ('D', n) = (x, y - n)

linePoints :: Position -> Position -> PointSet
linePoints (x0, y0) (x', y') = DS.cartesianProduct xSet ySet
  where
    makeSeq a b
      | a == b = DS.singleton a
      | a > b = DS.fromAscList [a .. b]
      | otherwise = DS.fromAscList [a .. b]
    xSet = makeSeq x0 x'
    ySet = makeSeq y0 y'

buildPath :: [Line] -> PointSet
buildPath list = iter list (0, 0) DS.empty
  where
    iter [] _ acc = acc
    iter (x:xs) origin acc =
      let end = finalPosition origin x
          line = linePoints origin end
       in iter xs end (acc `DS.union` line)

split :: Char -> String -> [String]
split _ [] = [""]
split c (x:xs)
  | x == c = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = split c xs

convert :: [String] -> [Line]
convert lst = reverse $ foldl (\acc (x:xs) -> (x, readN xs) : acc) [] lst
  where
    readN :: String -> Integer
    readN = read

parse :: String -> ([Line], [Line])
parse input =
  let [a, b] = fmap (convert . (split ',')) $ lines input
   in (a, b)

main :: IO ()
main = do
  input <- readFile "input"
  let (first, second) = parse input
      firstPath = buildPath first
      secondPath = buildPath second
      intersections =
        firstPath `DS.intersection` firstPath \\ DS.singleton (0, 0)
      distance = DS.findMin $ DS.map manhattanDistance intersections
  print distance
