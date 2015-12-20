import qualified Data.Map as M
import Control.Applicative
import Data.Maybe (mapMaybe)

data Life coord light = Life {neighbors :: (coord -> [coord]),
                              next :: light -> [light] -> light,
                              grid :: M.Map coord light
                             }

advance :: Ord coord => Life coord light -> Life coord light
advance game = game {grid = M.fromList . map tick . M.toList $ m}
  where m = grid game
        tick (c,light) = (c, let coords = neighbors game c
                                 others = mapMaybe (`M.lookup` m) coords
                             in next game light others)

grid8 :: Num a => [(a,a)]
grid8 = let ds = [0,-1,1]
        in tail $ liftA2 (,) ds ds

applyDelta :: Num a => (a,a) -> (a,a) -> (a,a)
applyDelta (x,y) (x',y') = (x+x',y+y')

neighbors8 :: Num a => (a,a) -> [(a,a)] -> [(a,a)]
neighbors8 delta = fmap (applyDelta delta)

nextBool :: Bool -> [Bool] -> Bool
nextBool True others = length (filter id others) `elem` [2,3]
nextBool False others = length (filter id others) == 3

rows :: String -> [[(Int, Bool)]]
rows s = zipWith mkLight [0..] <$> lines s

mkLight :: Int -> Char -> (Int, Bool)
mkLight x char = (x, char == '#')

mkRow :: Int -> [(Int, Bool)] -> [((Int, Int), Bool)]
mkRow y lights = [((y,x),on) | (x,on) <- lights]

parse :: String -> M.Map (Int,Int) Bool
parse = M.fromList . concat . zipWith mkRow [0..] . rows
