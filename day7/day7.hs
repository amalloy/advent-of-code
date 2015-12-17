import Data.Bits
import qualified Data.Map as M
import Data.Maybe

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

eval :: Circuit -> Label -> Int
eval c s = go (case M.lookup s c of
                  (Just source) -> source
                  Nothing -> error $ "No wire named " ++ s)
  where go (Const x) = x
        go (Unary op label) = runUnary op (eval c label)
        go (Binary op x y) = runBinary op (process x) (process y)
        process (Source x) = x
        process (From wire) = eval c wire

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

part1 :: [Wire] -> Int
part1 wires = eval circuit "a"
  where circuit = M.fromList [(label, source) | (Wire label source) <- wires]

main = interact $ show . part1 . map parse . lines
