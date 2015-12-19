import qualified Data.Map as M

-- units
type Distance = Int
type Time = Int
type Name = String

-- reindeer descriptors
data State = Resting Time | Running Time deriving Show
data Position = Position {dist :: Distance, state :: State} deriving Show
data Stats = Stats {speed :: Distance, burst :: Time, rest :: Time} deriving Show
data Reindeer = Reindeer {name :: Name, stats :: Stats, position :: Position} deriving Show

start :: Position
start = Position 0 (Resting 0)

parse :: String -> Reindeer
parse s = Reindeer n (Stats (read v) (read b - 1) (read r - 1)) start
  where [n, _, _, v, _, _, b, _, _, _, _, _, _, r, _] = words s

run :: Stats -> Position -> Position
run s (Position d (Resting 0)) = Position (d + speed s) (Running (burst s))
run s (Position d (Resting n)) = Position d (Resting (n - 1))
run s (Position d (Running 0)) = Position d (Resting (rest s))
run s (Position d (Running n)) = Position (d + speed s) (Running (n - 1))

runFor :: Time -> Reindeer -> Reindeer
runFor n r = r {position = newPos}
  where newPos = iterate (run (stats r)) (position r) !! n
