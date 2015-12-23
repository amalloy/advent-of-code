import Control.Monad.Trans.State
import qualified Data.Map.Strict as M

data Result = Success | Failure | Progress
data Problem node cost = Problem {root :: node,
                                  options :: node -> [(cost, node)],
                                  evaluate :: node -> Result
                                  }
type Path node = [node]
type SearchState cost node = M.Map cost [Path node]
type SearchResult cost node = Maybe (cost, Path node)
type Search cost node a = (State (SearchState cost node)) a

shortestPath :: (Num cost, Ord cost) => Problem node cost -> SearchResult cost node
shortestPath (Problem root options eval) = evalState (solve options eval) (M.singleton 0 [[root]])

solve :: (Num cost, Ord cost) => (node -> [(cost, node)])
                                 -> (node -> Result)
                                 -> Search cost node (SearchResult cost node)
solve nexts eval = go
  where go = do
          empty <- gets M.null
          if empty
            then return Nothing
            else do
              (cost, (path:paths)) <- gets M.findMin
              let (node:parents) = path
              modify $ M.updateMin tailOrDelete
              case eval node of
                Success -> return . Just $ (cost, path)
                Failure -> go
                Progress -> mapM_ explore (nexts node) >> go
                  where explore (cost', node) = modify $ M.insertWith (++) (cost + cost') [(node:path)]


tailOrDelete [x] = Nothing
tailOrDelete (x:xs) = Just xs

example :: Int -> Int -> Problem Int Int
example from to = Problem from choices check
  where choices n = [(1, n - 1), (3, n * 2)]
        check x | x == to = Success
                | x == 0 = Failure
                | otherwise = Progress
