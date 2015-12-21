import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Char (isUpper)
import Data.List (nub)
import Control.Arrow

type Atom = String
type Molecule = [Atom]
data Transition a = Transition a [a] deriving Show
type Grammar a = M.Map a [[a]]
data Problem a = Problem (Grammar a) [a] deriving Show

parseAtom :: String -> (Atom, String)
parseAtom [x] = ([x], "")
parseAtom (x:y:more) | isUpper y = ([x], y:more)
                     | otherwise = ([x,y], more)

parseMolecule :: String -> Molecule
parseMolecule [] = []
parseMolecule s = let (a, more) = parseAtom s
                  in a : parseMolecule more

parseTransition :: String -> Transition Atom
parseTransition s = let [from, _, to] = words s
                        (a, "") = parseAtom from
                        m = parseMolecule to
                    in Transition a m

parse :: String -> Problem Atom
parse s = let strs = filter (not . null) $ lines s
              rules = map parseTransition $ init strs
              goal = parseMolecule $ last strs
              grammar = foldr addRule M.empty rules
          in Problem grammar goal
  where addRule (Transition from to) m =
          M.insertWith (++) from [to] m

expansions :: Ord a => Grammar a -> [a] -> [[a]]
expansions m [] = []
expansions m (x:xs) = leaveAlone ++ expandHere where
  leaveAlone = (x:) <$> expansions m xs
  expandHere = (++ xs) <$> M.findWithDefault [] x m

expand :: Problem Atom -> [Molecule]
expand (Problem grammar goal) = expansions grammar $ goal

part1 :: Problem Atom -> Int
part1 = length . nub . expand

distinct :: Ord a => [a] -> [a]
distinct = S.toList . S.fromList

expansionTree :: Ord a => Grammar a -> [a] -> [[[a]]]
expansionTree g m = iterate (filter possible . distinct . (>>= expansions g)) [m]
  where possible solution = length solution <= goalLength
        goalLength = length g

part2 :: Problem Atom -> Int
part2 (Problem grammar goal) =
  let expansions = expansionTree grammar ["e"]
      success = (goal `elem`)
  in length . takeWhile (not . success) $ expansions

main = interact $ show . (part1 &&& part2) . parse
