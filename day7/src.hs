import Data.Bits
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Trans.State

type Label = String
data Unary = Id | Not | Shift Int deriving Show
data Binary = And | Or deriving Show

data Operand = Source Int | From Label deriving Show

data Source = Const Int
            | Unary Unary Label
            | Binary Binary Operand Operand
            deriving Show

data Wire = Wire Label Source deriving Show
type Circuit = M.Map Label Source

eval :: Label -> State Circuit Int
eval s = do
  old <- gets (M.! s)
  new <- go old
  modify (M.insert s (Const new))
  return new
  where go (Const x) = return x
        go (Unary op label) = runUnary op <$> eval label
        go (Binary op x y) = runBinary op <$> process x <*> process y
        process (Source x) = return x
        process (From wire) = eval wire

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
                                  "AND" -> Binary And l r
                                  "OR" -> Binary Or l r
  where [l, r] = map operand [left, right]
        operand x = case reads x of
          [(n, "")] -> Source n
          [] -> From x

parseShift :: Char -> String -> Label -> Source
parseShift dir amt src = Unary (Shift ((case dir of
                                           'L' -> id
                                           'R' -> negate)
                                       $ read amt))
                         src

solve :: Circuit -> Int
solve = evalState (eval "a")

build :: [Wire] -> Circuit
build wires = M.fromList [(label, source) | (Wire label source) <- wires]

parts :: Circuit -> (Int, Int)
parts c = (wireA, solve (M.insert "b" (Const wireA) c))
  where wireA = solve c

main = interact $ show . parts . build . map parse . lines
