import Data.List (group)
import Data.Char (digitToInt, isDigit)
import Data.Traversable (sequenceA)
import Control.Arrow

lookAndSay :: [Int] -> [Int]
lookAndSay xs = group xs >>= sequenceA [length, head]

expand :: [Int] -> [[Int]]
expand = iterate lookAndSay

solve :: Int -> [[Int]] -> Int
solve times = length . (!! times)

main = interact $ show . (solve 40 &&& solve 50) . expand . map digitToInt . filter isDigit
