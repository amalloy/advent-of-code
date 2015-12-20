import Control.Arrow

type Volume = Int
type Container = Int
type Pantry = [Container]

readPantry :: String -> Pantry
readPantry = map read . lines

distribute :: Volume -> Pantry -> [Pantry]
distribute 0 p = [p]
distribute n [] = []
distribute n (c:p) = distribute n p ++ ((c:) <$> distribute (n-c) p)

part1 :: [Pantry] -> Int
part1 = length

part2 :: [Pantry] -> Int
part2 p = let n = minimum $ map length p
          in length . filter ((== n) . length) $ p

main = interact $ show . (part1 &&& part2) . distribute 150 . readPantry
