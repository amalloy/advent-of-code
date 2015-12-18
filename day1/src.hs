import Control.Arrow

main = interact $ show . (sum &&& basement) . concatMap floor
  where floor '(' = [1]
        floor ')' = [-1]
        floor _ = []
        basement = length . takeWhile (>= 0) . scanl (+) 0
