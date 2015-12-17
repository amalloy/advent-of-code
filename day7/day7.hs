import Data.Bits
import qualified Data.Map as M
import Data.Maybe

type Label = String
data Unary = Id | Not | Shift Int
data Binary = And | Or

data Source = Const Int
            | Unary Unary Label
            | Binary Binary Label Label

data Wire = Wire Label Source
type Circuit = M.Map Label Source

eval :: Circuit -> Label -> Int
eval c s = go (fromJust $ M.lookup s c) where
  go (Const x) = x
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
