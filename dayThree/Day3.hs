{-# LANGUAGE OverloadedLists #-}

import qualified Data.Set as Set

data Direction
  = R
  | L
  | D
  | U
  deriving (Eq, Show, Read)

data Line =
  Line Direction Int
  deriving (Show, Eq)

type Position = (Int, Int)

type PointSet = Set.Set Position

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct a b = [(a, b) | a <- a, b <- b]

finalPosition :: Position -> Line -> Position
finalPosition (x, y) (Line R n) = (x + n, y)
finalPosition (x, y) (Line U n) = (x, y + n)
finalPosition (x, y) (Line L n) = (x - n, y)
finalPosition (x, y) (Line D n) = (x, y + n)

linePoints :: Position -> Position -> PointSet
linePoints (x0, y0) (x', y') = Set.cartesianProduct xSet ySet
  where
    makeSeq a b =
      if a <= b
        then Set.fromDescList [b .. a]
        else Set.fromAscList [a .. b]
    xSet = makeSeq x0 x'
    ySet = makeSeq y0 y'

getIntersections :: [Line] -> PointSet
getIntersections (l:ls) =
  let end = finalPosition (0, 0) l
   in iter ls (linePoints (0, 0) end) Set.empty end
  where
    iter [] _ int _ = int
    iter (l:ls) acc intersections newOrigin =
      let end = finalPosition newOrigin l
          line = linePoints newOrigin end
          int =
            line `Set.intersection`
            (acc `Set.difference` Set.singleton newOrigin)
       in iter ls (acc `Set.union` line) (intersections `Set.union` int) end

split :: Char -> String -> [String]
split _ [] = [""]
split c (x:xs)
  | x == c = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = split c xs

separate :: [String] -> [(String, String)]
separate lst = reverse $ foldl (\acc (x:xs) -> (x : "", xs) : acc) [] lst

convert :: [(String, String)] -> [Line]
convert [] = []
convert ((d, l):xs) = (Line (readD d) (readN l)) : (convert xs)
  where
    readD :: String -> Direction
    readD = read
    readN :: String -> Int
    readN = read

getInput :: String -> IO String
getInput input = do
  text <- readFile input
  let lined = lines text
      output = foldl (++) [] lined
  pure output

main :: IO ()
main = do
  aaa <- readFile "input"
  input <- getInput "input"
  let splitted = split ',' input
      separated = separate splitted
      converted = convert separated
      intersections = getIntersections converted
      closest = Set.findMin intersections
      distance = manhattanDistance (0, 0) closest
  print separated
