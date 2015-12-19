import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad (foldM)

type Ingredient = String
type Stat = String -- flavor, etc

data Factor = Factor Stat Int
type Impact = [Factor]
type Recipe = M.Map Ingredient Int
type Cookbook = (M.Map Ingredient Impact)
type Bakery = Reader Cookbook

updateIn :: Ord k => M.Map k v -> k -> (v -> v -> v) -> v -> M.Map k v
updateIn m k f v = M.unionWith f m $ M.singleton k v

bake :: Recipe -> Bakery (M.Map Stat Int)
bake = foldM mix M.empty . M.toList where
  mix m (ingredient, amount) = do
    impact <- asks (M.! ingredient)
    return $ foldr add m impact
    where add (Factor stat i) m = updateIn m stat (+) (i * amount)

parse :: String -> (Ingredient, Impact)
parse s = (init ingr, go more) where
  (ingr:more) = words s
  go [k,v] = [Factor k (read v)]
  go (k:v:more) = Factor k (read (init v)) : go more

parseAll :: String -> Cookbook
parseAll = M.fromList . map parse . lines

mixes :: Ord a => Int -> [a] -> [M.Map a Int]
mixes 0 xs = [M.fromList [(x, 0) | x <- xs]]
mixes n [] = []
mixes n (x:xs) = do
  xAmt <- [1..n]
  rest <- mixes (n - xAmt) xs
  return $ M.insert x xAmt rest

main = interact $ show . parseAll
