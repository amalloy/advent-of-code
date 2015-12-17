import Control.Applicative
import Control.Arrow

stringOverhead :: String -> Int
stringOverhead "" = 2
stringOverhead ('\\':'x':_:_:xs) = 3 + stringOverhead xs
stringOverhead ('\\':_:xs) = 1 + stringOverhead xs
stringOverhead (_:xs) = stringOverhead xs

stringInflation :: String -> Int
stringInflation xs = 2 + (length $ filter (`elem` "\\\"") xs)

main = interact $ show . (sum . map stringOverhead &&& sum . map stringInflation). lines
