import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (elems)
import Data.Scientific (toBoundedInteger)
import Data.Maybe (maybeToList, fromJust)

nums :: Value -> [Int]
nums (Object o) = concatMap nums (elems o)
nums (Array a) = concatMap nums a
nums (Number n) = maybeToList . toBoundedInteger $ n
nums _ = []

main = (print . sum . nums . fromJust . decode) =<< B.getContents
