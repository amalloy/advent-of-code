import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad (foldM)

type Ingredient = String
type Stat = String -- flavor, etc

data Factor = Factor Stat Int
type Impact = [Factor]
type Recipe = M.Map Ingredient Int
type Bakery = Reader (M.Map Ingredient Impact)

updateIn :: Ord k => M.Map k v -> k -> (v -> v -> v) -> v -> M.Map k v
updateIn m k f v = M.unionWith f m $ M.singleton k v

bake :: Recipe -> Bakery (M.Map Stat Int)
bake = foldM mix M.empty . M.toList where
  mix m (ingredient, amount) = do
    impact <- asks (M.! ingredient)
    return $ foldr add m impact
    where add (Factor stat i) m = updateIn m stat (+) (i * amount)
