import System.IO
import System.Environment
import qualified Data.Map as M
import Control.Arrow

type Info = M.Map String Int

slurp :: FilePath -> IO [String]
slurp p = lines <$> (hGetContents =<< openFile p ReadMode)

parseGoal :: [String] -> Info
parseGoal = M.fromList . map parsePiece
  where parsePiece s = let [k,v] = words s
                       in (init k, read v)

parse :: String -> (Int, Info)
parse s = let ("Sue":n:more) = words s
          in (read (init n), M.fromList (entries more))
  where entries [k, v] = [(init k, read v)]
        entries (k:v:more) = (init k, read (init v)) : entries more

type KeyPred = String -> Int -> Int -> Bool

satisfies :: KeyPred -> Info -> Info -> Bool
satisfies f goal obj = all match (M.toList obj)
  where match (k, v) = f k v (goal M.! k)

part1 :: KeyPred
part1 = const (==)

part2 :: KeyPred
part2 "cats" = (>)
part2 "trees" = (>)
part2 "pomeranians" = (<)
part2 "goldfish" = (<)
part2 _ = (==)

solve :: KeyPred -> Info -> [(Int, Info)] -> Int
solve f goal = fst . head . filter (satisfies f goal . snd)

main = do
  [goalFile, searchFile] <- getArgs
  goal <- parseGoal <$> slurp goalFile
  sues <- map parse <$> slurp searchFile
  print . (solve part1 goal &&& solve part2 goal) $ sues
