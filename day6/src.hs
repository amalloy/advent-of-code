import Control.Arrow
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad (forM_, mapM_)

splitWith p s = case break p s of
  (before, []) -> [before]
  (before, (_:after)) -> before : splitWith p after

data Operation = On | Off | Toggle deriving (Show)

class Light a where
  off :: a
  apply :: Operation -> a -> a
  brightness :: a -> Int

newtype BinLight = BinLight Bool
instance Light BinLight where
  off = BinLight False

  apply On _ = BinLight True
  apply Off _ = BinLight False
  apply Toggle (BinLight x) = BinLight (not x)

  brightness (BinLight False) = 0
  brightness (BinLight True) = 1

newtype MultiLight = MultiLight Int
instance Light MultiLight where
  off = MultiLight 0

  apply On (MultiLight x) = MultiLight (x + 1)
  apply Off (MultiLight x) = MultiLight (max 0 (x - 1))
  apply Toggle (MultiLight x) = MultiLight (x + 2)

  brightness (MultiLight x) = x

type Coord = (Int, Int)
type Display a = M.Map Coord a
data Range = Range Coord Coord deriving (Show)

parse :: String -> (Operation, Range)
parse s = (op, Range lower upper)
  where (op, [x, _, y]) = parseHead (words s)
        [lower, upper] = map parseCoord [x, y]
        parseHead ("turn":x:xs) = (parseOp x, xs)
        parseHead ("toggle":xs) = (Toggle, xs)
        parseOp "on" = On
        parseOp "off" = Off
        parseCoord s = (x,y)
          where [x,y] = map read . splitWith (== ',') $ s

coordList :: Range -> [Coord]
coordList r@(Range (x1,y1) (x2,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

interpret :: Light a => (Operation, Range) -> State (Display a) ()
interpret (op, range) = forM_ (coordList range) $ \coord ->
  modify $ M.alter insert coord
  where insert Nothing = Just $ apply op off
        insert (Just old) = Just $ apply op old

runLights :: Light a => [(Operation, Range)] -> Display a
runLights ops = execState (mapM_ interpret ops) $ M.empty

part1 :: [(Operation, Range)] -> Display BinLight
part1 = runLights

part2 :: [(Operation, Range)] -> Display MultiLight
part2 = runLights

lumens :: Light a => Display a -> Int
lumens = sum . fmap brightness

main = interact $ show . (const 0 &&& lumens . part2) . map parse . lines
