import qualified Data.Map as M

-- units
type Distance = Int
type Time = Int
type Name = String

-- reindeer descriptors
data State = Resting Time | Running Time deriving Show
data Position = Position {dist :: Distance, points :: Int, state :: State} deriving Show
data Stats = Stats {speed :: Distance, burst :: Time, rest :: Time} deriving Show
data Reindeer = Reindeer {stats :: Stats, position :: Position} deriving Show

start :: Position
start = Position 0 0 (Resting 0)

parse :: String -> Reindeer
parse s = Reindeer (Stats (read v) (read b - 1) (read r - 1)) start
  where [_, _, _, v, _, _, b, _, _, _, _, _, _, r, _] = words s

rDist :: Reindeer -> Distance
rDist = dist . position

addPoints :: Int -> Reindeer -> Reindeer
addPoints n r@(Reindeer {position = pos}) = r {position = pos {points = n + points pos}}

run :: Stats -> Reindeer -> Reindeer
run s r = r {position = pos (position r)}
  where pos (Position d p state) = case state of
          (Resting 0) -> Position (d + speed s) p (Running (burst s))
          (Resting n) -> Position d p (Resting (n - 1))
          (Running 0) -> Position d p (Resting (rest s))
          (Running n) -> Position (d + speed s) p (Running (n - 1))

race :: Time -> [Reindeer] -> [Reindeer]
race secs = undefined

-- main = interact $ show . maximum . map (dist . position) . race 2503 . map parse . lines
