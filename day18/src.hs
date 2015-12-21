import qualified Data.Map as M
import Control.Arrow
import Control.Applicative
import Data.Maybe (mapMaybe)

class Light a where
  on :: a -> Bool
  fromBool :: Bool -> a
  next :: a -> [Bool] -> a

newtype BasicLight = BasicLight Bool
instance Light BasicLight where
  on (BasicLight b) = b
  fromBool = BasicLight
  next (BasicLight b) others = BasicLight $ nextBool b others

data SketchyLight = Working Bool | Stuck
instance Light SketchyLight where
  on Stuck = True
  on (Working b) = b
  fromBool = Working
  next Stuck _ = Stuck
  next (Working b) others = Working $ nextBool b others

data Life coord light = Life {neighbors :: (coord -> [coord]),
                              grid :: M.Map coord light
                             }

advance :: (Ord coord, Light light) => Life coord light -> Life coord light
advance game = game {grid = M.fromList . map tick . M.toList $ m}
  where m = grid game
        tick (c,light) = (c, let coords = neighbors game c
                                 others = mapMaybe (`M.lookup` m) coords
                             in next light (map on others))

grid8 :: Num a => [(a,a)]
grid8 = let ds = [0,-1,1]
        in tail $ liftA2 (,) ds ds

applyDelta :: Num a => (a,a) -> (a,a) -> (a,a)
applyDelta (x,y) (x',y') = (x+x',y+y')

neighbors8 :: Num a => [(a,a)] -> (a,a) -> [(a,a)]
neighbors8 delta coord = fmap (applyDelta coord) delta

nextBool :: Bool -> [Bool] -> Bool
nextBool True others = length (filter id others) `elem` [2,3]
nextBool False others = length (filter id others) == 3

rows :: Light a => String -> [[(Int, a)]]
rows s = zipWith mkLight [0..] <$> lines s

mkLight :: Light a => Int -> Char -> (Int, a)
mkLight x char = (x, fromBool $ char == '#')

mkRow :: Light a => Int -> [(Int, a)] -> [((Int, Int), a)]
mkRow y lights = [((y,x),on) | (x,on) <- lights]

parse :: Light a => String -> M.Map (Int,Int) a
parse = M.fromList . concat . zipWith mkRow [0..] . rows

life8 :: Light a => String -> Life (Int,Int) a
life8 s = Life (neighbors8 grid8) (parse s)

runFor :: (Ord a, Light b) => Int -> Life a b -> Life a b
runFor n game = iterate advance game !! n

solve :: Light a => Life (Int,Int) a -> Int
solve = length . filter on . M.elems . grid . runFor 100

part1 :: Life (Int,Int) BasicLight -> Int
part1 = solve

part2 :: Life (Int,Int) SketchyLight -> Int
part2 game = solve (game {grid = break (grid game)}) where
  break g = foldr stickOn g positions
  positions = liftA2 (,) [0,99] [0,99]
  stickOn coord = M.insert coord Stuck

main = interact $ show . (part1 . life8 &&& part2 . life8)
