import Data.Bool

type Iso a = a -> a

data Coord a = Coord {row :: a, col :: a} deriving (Show, Eq)
data Params a = Params {init :: a,
                        next :: Iso a,
                        goal :: Coord Int}

data Problem a = Problem {curr :: a,
                          coord :: Coord Int,
                          params :: (Params a)}

nextCoord :: Iso (Coord Int)
nextCoord (Coord 1 c) = Coord (c + 1) 1
nextCoord (Coord r c) = Coord (r - 1) (c + 1)

done :: Problem a -> Bool
done (Problem _ c p) = c == goal p

advance :: Iso (Problem a)
advance (Problem x c p) =
  Problem (next p x) (nextCoord c) p

solve :: Iso (Problem a)
solve = (bool <$> solve . advance <*> id <*> done)

adv :: Integer -> Integer
adv x = mod (x * 252533) 33554393

part1 :: Problem Integer
part1 = let start = 20151125
        in Problem start (Coord 1 1) $ Params start adv (Coord 2947 3029)


main = print . curr . solve $ part1
