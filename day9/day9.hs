import Data.Maybe (catMaybes, isNothing)
import Data.List (nub)
import Control.Arrow

type City = String
type Distance = Int
data Route = Route City City Distance deriving Show
data Trip = Trip City Distance deriving Show
data Progress = Progress {currentCity :: City, distance :: Distance, flights :: [Route]}

parse :: String -> Route
parse s = case words s of
  [a, "to", b, "=", dist] -> Route a b (read dist)

destination :: City -> Route -> Maybe Trip
destination c (Route a b dist)
  | c == a = go b
  | c == b = go a
  | otherwise = Nothing
  where go x = Just $ Trip x dist

cities :: Route -> [City]
cities (Route a b _) = [a,b]

allCities :: [Route] -> [City]
allCities = nub . concatMap cities

trips :: City -> [Route] -> [Trip]
trips from = catMaybes . map (destination from)

advance :: Progress -> [Progress]
advance (Progress city dist routes) = do
  (Trip to dist') <- trips city routes
  return . Progress to (dist + dist') $ filter (isNothing . destination city) routes

exhaust :: Progress -> [Progress]
exhaust state@(Progress city dist routes) =
  case (advance state, routes) of
    ([], []) -> return state -- visited all cities
    ([], _) -> [] -- out of options, but cities left unvisited
    (nexts, _) -> nexts >>= exhaust

solutions :: [Route] -> [Progress]
solutions routes = do
  start <- allCities routes
  solution <- exhaust $ Progress start 0 routes
  return solution

solve :: [Route] -> [Distance]
solve = map distance . solutions

main = interact $ show . (minimum &&& maximum) . solve . map parse . lines
