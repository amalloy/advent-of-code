type City = String
type Distance = Int
data Route = Route City City Distance deriving Show

parse :: String -> Route
parse s = case words s of
  [a, "to", b, "=", dist] -> Route a b (read dist)

destination :: City -> Route -> Maybe City
destination c (Route a b dest)
  | c == a = Just b
  | c == b = Just a
  | otherwise = Nothing
