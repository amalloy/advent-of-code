splitWith p s = case break p s of
  (before, []) -> [before]
  (before, (_:after)) -> before : splitWith p after

parse :: String -> [Int]
parse = map read . splitWith (== 'x')

faces :: [Int] -> [Int]
faces [x,y,z] = [x*y, y*z, x*z]

wrap :: [Int] -> Int
wrap xs = let fs = faces xs
          in (sum fs * 2) + minimum fs

ribbon :: [Int] -> Int
ribbon xs = perim xs + volume xs

perim :: [Int] -> Int
perim xs = 2 * (sum xs - maximum xs)

volume :: [Int] -> Int
volume = product

tally :: Num b => (a -> b) -> [a] -> b
tally f = sum . map f

solve :: [[Int]] -> (Int, Int)
solve xs = (tally wrap xs, tally ribbon xs)

main = interact $ show . solve . map parse . lines
