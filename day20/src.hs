import qualified Data.Map.Strict as M

data Settings a = Settings {route :: [(a,Int)] -> [(a,Int)],
                            presents :: Int -> Int,
                            limit :: Int
                           }

deliveries :: Settings Int -> Int -> [(Int, Int)]
deliveries settings amt = route settings $ zip [amt, amt+amt..limit settings]
                                               (repeat (presents settings amt))

deliverAll :: Settings Int -> M.Map Int Int
deliverAll settings = foldr add M.empty $ concatMap (deliveries settings) [1..limit settings]
  where add (k, v) m = M.insertWith (+) k v m

answers :: Int -> M.Map Int Int -> [Int]
answers goal m = [k | (k, v) <- M.toList m, v >= goal]

solve :: Int -> Settings Int -> Int
solve goal setting = let limits = iterate (* 10) 1000
                         settings = map (\n -> setting {limit=n}) limits
                         attempts = map deliverAll settings
                     in head $ attempts >>= answers goal

part1 :: Settings Int
part1 = Settings id (* 10) 1

part2 :: Settings Int
part2 = Settings (take 50) (* 11) 1

main = interact $ show . flip map [part1,part2] .  solve . read
  where go f = map f [part1,part2]
