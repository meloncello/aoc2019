import Data.Foldable
import Data.Monoid

neededModule :: Int -> Int
neededModule mass = mass `div` 3 - 2

neededModule' :: Int -> Int
neededModule' mass = helper mass 0
  where
    helper mass acc
      | mass <= 6 = acc
      | otherwise =
        let value = neededModule mass
         in helper value (acc + value)

main :: IO ()
main = do
  file <- readFile "input"
  let masses = lines file
      numbers = map (read :: String -> Int) masses
      result = getSum $ foldMap (Sum . neededModule) numbers
      result' = getSum $ foldMap (Sum . neededModule') numbers
  print $ "part 1: " ++ (show result)
  print $ "part 2: " ++ (show result')
