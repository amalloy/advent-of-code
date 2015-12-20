import qualified Data.Map as M
import Control.Monad

type Volume = Int
type Container = Int
type Pantry = M.Map Container Int

updateIn :: Ord k => M.Map k v -> k -> (v -> v -> v) -> v -> M.Map k v
updateIn m k f v = M.unionWith f m $ M.singleton k v

frequencies :: Ord a => [a] -> M.Map a Int
frequencies = foldr add M.empty where
  add x m = updateIn m x (+) 1

readPantry :: String -> Pantry
readPantry = frequencies . map read . lines

distribute :: Volume -> Pantry -> [Pantry]
distribute 0 p = [p]
distribute n p = do
  (size, count) <- M.toList p
  guard $ count > 0 && size <= n
  distribute (n - size) (M.adjust (subtract 1) size p)

main = interact $ show . length . distribute 150 . readPantry
