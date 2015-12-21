import qualified Data.Map as M
import Data.Char

type Atom = String
type Molecule = [Atom]
data Transition a = Transition a [a] deriving Show

parseAtom :: String -> (Atom, String)
parseAtom [x] = ([x], "")
parseAtom (x:y:more) | isUpper y = ([x], y:more)
                     | otherwise = ([x,y], more)

parseMolecule :: String -> Molecule
parseMolecule [] = []
parseMolecule s = let (a, more) = parseAtom s
                  in a : parseMolecule more

parseTransition :: String -> Transition String
parseTransition s = let [from, _, to] = words s
                        (a, "") = parseAtom from
                        m = parseMolecule to
                    in Transition a m
