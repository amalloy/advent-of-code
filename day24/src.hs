import Data.Monoid
import Data.Ord (comparing)
import Data.List (minimumBy)
import Control.Monad (guard)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M

type Group a = (Sum a, Product a)

parse :: String -> [Group Int]
parse = map ((Sum &&& Product) . read) . lines

-- all the ways to distribute one [a] into n lists of [a],
-- such that each list is considered Perfect by f
solutions :: Monoid a => Int -> (a -> Ordering) -> [a] -> [[[a]]]
solutions n f = go $ M.fromList (zip [1..n] $ repeat mempty)
  where f' = f . mconcat
        go m [] = do
          guard $ all ((EQ ==) . f') (M.elems m)
          return $ M.elems m
        go m (x:xs) = do
          m' <- additions m x
          go m' xs
        additions m x = do
          k <- [1..n]
          let v = m M.! k
              v' = x : v
          guard $ f' v' /= GT
          return $ M.insert k v' m

sizer :: Ord a => a -> (Sum a, b) -> Ordering
sizer n (x, _) = compare (getSum x) n

prod :: Num a => [(t, Product a)] -> a
prod = (getProduct . mconcat . map snd)

best :: (Ord a, Num a) => [(t, Product a)] -> [(t, Product a)] -> Ordering
best = comparing length <> comparing prod

part1 :: [(Sum Int, Product Int)] -> Int
part1 nums = let total = getSum . mconcat . map fst $ nums
                 numSplits = 3
                 f = sizer (total `div` numSplits)
                 splits = solutions numSplits f nums
                 firsts = map head splits
             in prod $ minimumBy best firsts

main = interact $ show . part1 . parse
