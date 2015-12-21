import qualified Data.Map.Strict as M

deliveries :: Int -> Int -> [(Int, Int)]
deliveries limit amt = zip [amt, amt+amt..limit] (repeat (amt * 10))

deliverAll :: Int -> M.Map Int Int
deliverAll limit = foldr add M.empty $ concatMap (deliveries limit) [1..limit]
  where add (k, v) m = M.insertWith (+) k v m

answers :: Int -> M.Map Int Int -> [Int]
answers goal m = [k | (k, v) <- M.toList m, v >= goal]

solve :: Int -> Int
solve goal = let limits = iterate (* 10) 1000
                 attempts = map deliverAll limits
             in head $ attempts >>= answers goal

main = interact $ show . solve . read
