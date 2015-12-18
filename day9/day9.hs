import Data.Maybe

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

trips :: City -> [Route] -> [Trip]
trips from = catMaybes . map (destination from)

advance :: Progress -> [Progress]
advance (Progress city dist routes) = do
  (Trip to dist') <- trips city routes
  return . Progress to (dist + dist') $ filter (isNothing . destination city) routes
