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

canSplitInto :: (Num a, Ord a) => [a] -> a -> Bool
xs `canSplitInto` n = go n n xs where
  go 0 0 [] = True
  go _ _ [] = False
  go a b (x:xs) = x <= a && go (a-x) b xs || go a (b-x) xs

firstGroups :: (Num a, Ord a, Integral a) => Int -> [a] -> [[a]]
firstGroups numItems xs = do
  (first, rest) <- chooseSummingTo numItems groupSize xs
  guard $ rest `canSplitInto` groupSize
  return first
  where groupSize = sum xs `div` 3

solve :: [Integer] -> Integer
solve xs = go 1
   where go numItems = case firstGroups numItems xs of
           [] -> go $ numItems + 1
           solutions -> product $ minimumBy preference solutions
         preference = comparing length <> comparing product

main = interact $ show . solve . parse
