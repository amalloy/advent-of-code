import Control.Arrow
import Control.Applicative (liftA2)
import Data.List (nub)

alternate :: [a] -> ([a], [a])
alternate [] = ([], [])
alternate (x:xs) = (right, x:left)
  where (left,right) = alternate xs

move :: Num a => (a,a) -> Char -> (a,a)
move (x,y) c = case c of
  '<' -> (x-1,y)
  '>' -> (x+1,y)
  '^' -> (x, y+1)
  'v' -> (x, y-1)
  _ -> (x,y)

houses :: [Char] -> [(Int, Int)]
houses = scanl move (0,0)

presents :: Eq a => [(a,a)] -> Int
presents = length . nub

main = interact $ show . (part1 &&& part2)
  where part1 = presents . houses
        part2 moves = presents $ houses =<< [santa, robot]
          where (santa, robot) = alternate moves :: ([Int], [Int])
