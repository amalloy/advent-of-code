type City = String
type Distance = Int
data Route = Route City City Distance deriving Show

parse :: String -> Route
parse s = case words s of
  [a, "to", b, "=", dist] -> Route a b (read dist)

includes :: City -> Route -> Bool
includes c (Route a b) = c == a || c == b
