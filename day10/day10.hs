import Data.List (group)
import Data.Char (digitToInt, isDigit)

lookAndSay :: [Int] -> [Int]
lookAndSay xs = group xs >>= describe
  where describe x = [length x, head x]

expand :: [Int] -> [[Int]]
expand = iterate lookAndSay

part1 :: [[Int]] -> Int
part1 = length . (!! 40)

main = interact $ show . part1 . expand . map digitToInt . filter isDigit
