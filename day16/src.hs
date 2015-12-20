import System.IO
import System.Environment
import qualified Data.Map as M

type Info = M.Map String Int

slurp :: FilePath -> IO [String]
slurp p = lines <$> (hGetContents =<< openFile p ReadMode)

parseGoal :: [String] -> Info
parseGoal = M.fromList . map parsePiece
  where parsePiece s = (init k, read v)
          where [k, v] = words s

parse :: String -> (Int, Info)
parse s = (read (init n), M.fromList (entries more))
  where ("Sue":n:more) = words s
        entries [k, v] = [(init k, read v)]
        entries (k:v:more) = (init k, read (init v)) : entries more

satisfies :: Info -> Info -> Bool
satisfies goal obj = all match (M.toList obj)
  where match (k, v) = goal M.! k == v

main = do
  [goalFile, searchFile] <- getArgs
  goal <- parseGoal <$> slurp goalFile
  sues <- map parse <$> slurp searchFile
  print . fst . head . filter (satisfies goal . snd) $ sues
