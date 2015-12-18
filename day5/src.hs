import Control.Arrow
import Control.Applicative
import Data.List (isInfixOf)

nice :: String -> Bool
nice s = not (null (drop 2 (filter vowel s)))
         && any (uncurry (==)) pairs
         && not (any illegal pairs)
  where pairs = zip s $ tail s
        illegal = (`elem` [(x,y) | [x,y] <- ["ab", "cd", "pq", "xy"]])
        vowel = (`elem` "aeiou")

hasRepeatedPair :: Eq a => [a] -> Bool
hasRepeatedPair (a:b:xs) = [a,b] `isInfixOf` xs || hasRepeatedPair (b:xs)
hasRepeatedPair _ = False

hasABA :: Eq a => [a] -> Bool
hasABA (a:b:c:xs) = a == c || hasABA (b:c:xs)
hasABA _ = False

nice' :: String -> Bool
nice' = liftA2 (&&) hasRepeatedPair hasABA

main = interact $ show . (length . filter nice &&& length . filter nice') . lines
