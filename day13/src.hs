import qualified Data.Map as M
import Data.List (permutations, nub)
import Data.Function (on)
import Data.Maybe (mapMaybe)

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
  where [subj, "would", dir, amt,
         "happiness", "units", "by", "sitting", "next", "to",
         obj] = words s

sign :: Direction -> Int
sign Gain = 1
sign Lose = -1

impact :: Delta -> Happiness
impact (Delta dir amt) = sign dir * amt

pairings :: [a] -> [(a,a)]
pairings all@(x:xs) = (x, last xs) : zip all xs

matches :: Chart -> [(Person, Person)] -> [Delta]
matches chart people = mapMaybe (`M.lookup` chart) people ++
                       mapMaybe (`M.lookup` chart) (map rev people)
  where rev (a,b) = (b,a)

plan :: [Preference] -> Chart
plan prefs = M.fromList $ do
  (Prefer subj delt obj) <- prefs
  return ((subj, obj), delt)

persons :: Chart -> [Person]
persons c = nub $ do
  (a, b) <- M.keys c
  [a, b]

solve :: Chart -> Happiness
solve c = maximum $ do
  let ps = persons c
  order <- permutations ps
  return . sum . map impact $ matches c (pairings order)

main = interact $ show . solve . plan . map parse . lines
