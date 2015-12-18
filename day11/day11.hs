{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Maybe (mapMaybe, maybe)
import Data.List (isPrefixOf)

data Rule a = Rule {breaks :: a -> Bool, fix :: a -> a}

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

applyRule :: a -> Rule a -> Maybe a
applyRule x r | breaks r x = Just $ fix r x
              | otherwise = Nothing

runRules :: [Rule a] -> a -> a
runRules rules = go where
  go x = case mapMaybe (applyRule x) rules of
    (new:_) -> go new
    [] -> x

ruleList :: [Rule [Clamped]]
ruleList = [exclude (map Clamp "ilo"), needsStraight 3, needsGroups 2 2]

exclude :: (Enum a, Bounded a, Eq a) => [a] -> Rule [a]
exclude xs = Rule (any (`elem` xs)) (skipOver (`elem` xs))

-- pred should return False if the rule is broken
basicRule :: (Bounded a, Enum a, Eq a) => ([a] -> Bool) -> Rule [a]
basicRule pred = Rule (not . pred) inc

buildStraight :: (Enum a, Bounded a, Eq a) => Int -> a -> Maybe [a]
buildStraight 0 x = Just []
buildStraight 1 x = Just [x]
buildStraight n x | x == minBound = Nothing
                  | otherwise = (x :) <$> buildStraight (n-1) (pred x)

hasStraight :: (Enum a, Bounded a, Eq a) => Int -> [a] -> Bool
hasStraight n = go
  where go [] = False
        go s@(x:xs) = (maybe False (`isPrefixOf` s) $ buildStraight n x)
                      || go xs

needsStraight :: (Enum a, Bounded a, Eq a) => Int -> Rule [a]
needsStraight n = basicRule (hasStraight n)

needsGroups :: Int -> Int -> Rule [Clamped]
needsGroups groupSize numGroups = basicRule (findGroups numGroups)
  where findGroups 0 s = True
        findGroups _ [] = False
        findGroups numGroups s@(x:xs) = replicate groupSize x `isPrefixOf` s
                                        && findGroups (numGroups-1) (drop groupSize s)
                                        || findGroups numGroups xs

solve = reverse . map unClamp . runRules ruleList . map Clamp . reverse

main = interact $ show . solve . head . lines
