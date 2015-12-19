{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (elems)
import Data.Scientific (toBoundedInteger)
import Data.Maybe (maybeToList, fromJust)
import Data.Text (Text)
import Control.Arrow

nums :: (Text -> Bool) -> Value -> [Int]
nums pred = go where
  pred' (String s) = pred s
  pred' _ = False
  go (Object o) = if any pred' (elems o) then [] else concatMap go (elems o)
  go (Array a) = concatMap go a
  go (Number n) = maybeToList . toBoundedInteger $ n
  go _ = []

main = (print . (sum . (nums (const False))
                 &&&
                 (sum . (nums (== "red"))))
        . fromJust . decode)
       =<< B.getContents
