import qualified Data.Map as M
import Control.Arrow
import Control.Monad.Trans.Reader
import Control.Monad (foldM)

type Ingredient = String
type Stat = String -- flavor, etc

data Factor = Factor Stat Int
type Impact = [Factor]
type Recipe = M.Map Ingredient Int
type Cookbook = M.Map Ingredient Impact
type Cookie = M.Map Stat Int
type Bakery = Reader Cookbook

updateIn :: Ord k => M.Map k v -> k -> (v -> v -> v) -> v -> M.Map k v
updateIn m k f v = M.unionWith f m $ M.singleton k v

bake :: Recipe -> Bakery Cookie
bake = foldM mix M.empty . M.toList where
  mix m (ingredient, amount) = foldr add m <$> asks (M.! ingredient)
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

score :: Cookie -> Int
score = product . map (max 0) . M.elems

makeCookies :: Cookbook -> [Cookie]
makeCookies m = do
  recipe <- mixes 100 (M.keys m)
  return $ runReader (bake recipe) m

solve :: [Cookie] -> Int
solve = maximum . map (score . M.delete "calories")

hasCalories :: Int -> Cookie -> Bool
hasCalories n c = c M.! "calories" == n

main = interact $ show . (solve &&& solve . filter (hasCalories 500)) . makeCookies . parseAll
