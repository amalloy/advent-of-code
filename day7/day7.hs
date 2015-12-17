import Data.Bits
import qualified Data.Map as M
import Data.Maybe

type Label = String
data Unary = Id | Not | Shift Int deriving Show
data Binary = And | Or deriving Show

data Source = Const Int
            | Unary Unary Label
            | Binary Binary Label Label
            deriving Show

data Wire = Wire Label Source deriving Show
type Circuit = M.Map Label Source

data Traced a = Fault String [Wire] | Result a deriving Show

instance Functor Traced where
  fmap f (Fault cause trace) = Fault cause trace
  fmap f (Result a) = Result (f a)

instance Applicative Traced where
  pure = Result
  (Fault cause trace) <*> _ = Fault cause trace
  _ <*> (Fault cause trace) = Fault cause trace
  (Result f) <*> (Result x) = Result f x

instance Monad Traced where
  return = pure
  (Fault cause trace) >>= f = Fault cause trace
  (Result x) >>= f = f x

eval :: Circuit -> Label -> Traced Int
eval c s = (case M.lookup s c of
               (Just source) -> go source
               Nothing -> Fault ("No label: " ++ s) []
  where go (Const x) = Result x
        go (Unary op label) = runUnary op (eval c label)
        go (Binary op x y) = runBinary op (eval c x) (eval c y)

runUnary :: Unary -> Int -> Int
runUnary Id = id
runUnary Not = complement
runUnary (Shift offset) = (`shift` offset)

runBinary :: Binary -> Int -> Int -> Int
runBinary And = (.&.)
runBinary Or = (.|.)

parse :: String -> Wire
parse s = case words s of
  [l, "->", r] -> parseUnary l r
  ["NOT", l, "->", r] -> Wire r (Unary Not l)
  [l, op, r, "->", dst] -> parseBinary op l r dst

parseUnary :: String -> Label -> Wire
parseUnary src label = Wire label $
                       case reads src of
                         [(n, "")] -> Const n
                         [] -> Unary Id src
                         _ -> error $ "Unparseable src: " ++ src

parseBinary :: String -> Label -> Label -> Label -> Wire
parseBinary op left right dst = Wire dst $
                                case op of
                                  (dir:"SHIFT") -> parseShift dir right left
                                  "AND" -> Binary And left right
                                  "OR" -> Binary Or left right

parseShift :: Char -> String -> Label -> Source
parseShift dir amt src = Unary (Shift ((case dir of
                                           'L' -> id
                                           'R' -> negate)
                                       $ read amt))
                         src

part1 :: [Wire] -> Int
part1 wires = eval circuit "a"
  where circuit = M.fromList [(label, source) | (Wire label source) <- wires]

main = interact $ show . part1 . map parse . lines
