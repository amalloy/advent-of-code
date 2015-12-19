import qualified Data.Map as M

type Person = String
type Happiness = Int

data Direction = Gain | Lose deriving Show
data Delta = Delta Direction Happiness deriving Show
data Preference = Prefer {subject :: Person, delta :: Delta, object :: Person}
                deriving Show

type Chart = M.Map (Person, Person) Delta

parseDir :: String -> Direction
parseDir "gain" = Gain
parseDir "lose" = Lose

parse :: String -> Preference
parse s = Prefer subj (Delta (parseDir dir) (read amt)) (init obj)
  where [subj, "would", dir, amt, "happiness", "units", "by", "sitting", "next", "to", obj] = words s

main = interact $ show . head . map parse . lines
