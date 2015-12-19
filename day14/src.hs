import qualified Data.Map as M

-- units
type Distance = Int
type Time = Int
type Name = String

-- reindeer descriptors
data State = Resting Time | Running Time
data Position = Position {dist :: Distance, state :: State}
data Stats = Stats {speed :: Distance, burst :: Time, rest :: Time}
data Reindeer = Reindeer {name :: Name, stats :: Stats, position :: Position}

-- race tracking
type Standings = M.Map Name Reindeer
data Race = Race {timeLeft :: Time, racers :: Standings}

start :: Position
start = Position 0 (Resting 0)

parse :: String -> Reindeer
parse s = Reindeer n (Stats (read v) (read b) (read r)) start
  where [n, _, _, v, _, _, b, _, _, _, _, _, _, r, _] = words s
