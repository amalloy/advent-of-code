import System.IO
import System.Environment
import qualified Data.Map as M

type Info = M.Map String Int

slurp :: FilePath -> IO [String]
slurp p = lines <$> (hGetContents =<< openFile p ReadMode)

parseGoal :: [String] -> Info
parseGoal = undefined

parse :: String -> Info
parse = undefined

satisfies :: Info -> Info -> Bool
satisfies goal obj = undefined

main = do
  [goalFile, searchFile] <- getArgs
  goal <- parseGoal <$> slurp goalFile
  sues <- map parse <$> slurp searchFile
  print . head . filter (satisfies goal) $ sues
