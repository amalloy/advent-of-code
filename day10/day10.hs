import Data.List (group)
import Data.Char (digitToInt, isDigit)
import Control.Arrow

lookAndSay :: [Int] -> [Int]
lookAndSay xs = group xs >>= describe
  where describe x = [length x, head x]

expand :: [Int] -> [[Int]]
expand = iterate lookAndSay

solve :: Int -> [[Int]] -> Int
solve times = length . (!! times)

main = interact $ show . (solve 40 &&& solve 50) . expand . map digitToInt . filter isDigit
