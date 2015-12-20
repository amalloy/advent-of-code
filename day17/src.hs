import Control.Monad

type Volume = Int
type Container = Int
type Pantry = [Container]

readPantry :: String -> Pantry
readPantry = map read . lines

distribute :: Volume -> Pantry -> [Pantry]
distribute 0 p = [p]
distribute n [] = []
distribute n (c:p) = distribute n p ++ ((c:) <$> distribute (n-c) p)

main = interact $ show . length . distribute 150 . readPantry
