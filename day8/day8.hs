import Control.Applicative


stringOverhead :: String -> Int
stringOverhead "" = 2
stringOverhead ('\\':'x':_:_:xs) = 3 + stringOverhead xs
stringOverhead ('\\':_:xs) = 1 + stringOverhead xs
stringOverhead (_:xs) = stringOverhead xs

main = interact $ show . sum . map stringOverhead . lines
