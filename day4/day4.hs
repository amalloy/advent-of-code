import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List (isPrefixOf)
import Numeric (showHex)
import Control.Arrow

digits word = pad $ showHex word ""
  where pad x@[_] = '0' : x
        pad x@[_,_] = x

hex :: B.ByteString -> String
hex = concat . map digits . B.unpack

valid :: String -> B.ByteString -> Bool
valid prefix s = prefix `isPrefixOf` (hex s)

hash :: String -> Int -> B.ByteString
hash key n = MD5.hash . C.pack $ key ++ show n

main = interact $ show . (part1 &&& part2) . head . lines
  where [part1, part2] = map part [5,6]
        part len key = head . filter (valid (replicate len '0') . (hash key)) $ [1..]
