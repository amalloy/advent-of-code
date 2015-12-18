{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data Rule a = Rule {breaks :: [a] -> Bool, fix :: [a] -> [a]}

newtype Clamped = Clamp {unClamp :: Char} deriving (Eq, Enum, Show)

instance Bounded Clamped where
  minBound = Clamp 'a'
  maxBound = Clamp 'z'

-- increments from the left side of the list, not the right
inc :: (Enum a, Bounded a, Eq a) => [a] -> [a]
inc [] = error "empty list"
inc (x:xs)
  | x == maxBound = minBound : inc xs
  | otherwise = succ x : xs

skipOver :: (Enum a, Bounded a, Eq a) => (a -> Bool) -> [a] -> [a]
skipOver pred orig = go 0 orig
  where go _ [] = orig
        go n (x:xs) | pred x = replicate n minBound ++ skipOver pred (inc (x:xs))
                    | otherwise = go (n + 1) xs

valid :: [Rule a] -> [a] -> Bool
valid rules xs = not (any (flip breaks xs) rules)

iterateUntil :: (a -> a) -> (a -> Bool) -> a -> a
iterateUntil f pred xs = head . dropWhile (not . pred) $ iterate f xs
