import Control.Monad (guard)
import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.List (minimumBy)
import qualified Data.Map.Strict as M

parse :: String -> [Integer]
parse = map read . lines

chooseSummingTo :: (Num a, Ord a) => Int -> a -> [a] -> [([a], [a])]
chooseSummingTo 0 0 xs = [([], xs)]
chooseSummingTo 0 _ _ = []
chooseSummingTo _ 0 _ = []
chooseSummingTo _ _ [] = []
chooseSummingTo n sum (x:xs)
  | sum > 0 = let with = chooseSummingTo (n-1) (sum-x) xs
                  without = chooseSummingTo n sum xs
              in [(summed, x:more) | (summed, more) <- without]
                 ++ [(x:summed, more) | (summed, more) <- with]
  | otherwise = []

canSplitInto :: (Num a, Ord a, Enum a) => [a] -> a -> a -> Bool
canSplitInto xs numGroups groupSize = go xs initMap where
  initMap = M.fromList . zip [1..numGroups] . repeat $ groupSize
  go [] m = all (== 0) . M.elems $ m
  go (x:xs) m = any canFit . M.toList $ m
    where canFit (bucket, remain) = x <= remain && go xs (M.insert bucket (remain-x) m)

firstGroups :: (Num a, Ord a, Integral a) => Int -> a -> [a] -> [[a]]
firstGroups numItems numGroups xs = do
  (first, rest) <- chooseSummingTo numItems groupSize xs
  guard $ canSplitInto rest (numGroups-1) groupSize
  return first
  where groupSize = sum xs `div` numGroups

solve :: Integer -> [Integer] -> Integer
solve numGroups xs = go 1
   where go numItems = case firstGroups numItems numGroups xs of
           [] -> go $ numItems + 1
           solutions -> product $ minimumBy preference solutions
         preference = comparing length <> comparing product

main = interact $ show . (solve 3 &&& solve 4) . parse
