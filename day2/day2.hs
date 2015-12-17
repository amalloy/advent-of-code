import Control.Arrow
import Control.Applicative

splitWith p s = case break p s of
  (before, []) -> [before]
  (before, (_:after)) -> before : splitWith p after

parse :: String -> [Int]
parse = map (read :: String -> Int) . splitWith (== 'x')

faces :: [Int] -> [Int]
faces [x,y,z] = [x*y, y*z, x*z]

wrap :: [Int] -> Int
wrap = liftA2 (+) ((2 *) . sum) minimum . faces

ribbon :: [Int] -> Int
ribbon = liftA2 (+) perim volume

perim :: [Int] -> Int
perim = (* 2) . liftA2 (-) sum maximum

volume :: [Int] -> Int
volume = product

tally :: Num b => (a -> b) -> [a] -> b
tally f = sum . map f

main = interact $ show . (tally wrap &&& tally ribbon) . map parse . lines
