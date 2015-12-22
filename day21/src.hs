import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import qualified Data.Map as M
import Control.Applicative (liftA3)
import System.Environment
import System.IO

data Slot = Weapon | Armor | Ring deriving (Eq, Ord, Show)
data Range = Range Int Int deriving Show

data Item = Item {cost :: Int,
                  damage :: Int,
                  armor :: Int}
            deriving Show

instance Monoid Item where
  mempty = Item 0 0 0
  mappend (Item a b c) (Item x y z) = Item (a + x) (b + y) (c + z)

data Character = Character {health :: Int,
                            attack :: Int,
                            ac :: Int}
                 deriving Show

type Shop = M.Map Slot [Item]
type Constraints = M.Map Slot Range

dec :: Range -> Range
dec (Range 0 upper) = Range 0 (upper - 1)
dec (Range lower upper) = Range (lower - 1) (upper - 1)

choose :: [a] -> Range -> [[a]]
choose [] (Range 0 _) = [[]] -- okay to choose the 0 remaining elements
choose [] _ = [] -- no elements left, but we have to choose something
choose _ (Range _ 0) = [[]] -- whatever's left, we can choose none of them
choose (x:xs) r = include ++ exclude where
  include = (x :) <$> choose xs (dec r)
  exclude = choose xs r

hit :: Character -> Character -> Character
hit hero villain = villain {health = health villain - amt}
  where amt = max 1 $ attack hero - ac villain

winsFight :: Character -> Character -> Bool
winsFight hero villain = health hero > 0 && (health villain <= 0 || not (winsFight (hit hero villain) hero))

shop :: Shop -> Constraints -> [[Item]]
shop s c = do
  (slot, range) <- M.toList c
  (s M.! slot) `choose` range

shopParser :: CharParser () Shop
shopParser = M.fromList <$> shopSection `sepBy` newline

shopSection :: CharParser () (Slot, [Item])
shopSection = (,) <$> sectionHeader <*> many shopItem

sectionHeader :: CharParser () Slot
sectionHeader = do
  slot <- many1 letter
  manyTill anyChar newline
  return $ readSlot slot
  where readSlot "Weapons" = Weapon
        readSlot "Armor" = Armor
        readSlot "Rings" = Ring

shopItem :: CharParser () Item
shopItem = do
  many1 alphaNum
  -- gross, but finding a better separator would be work
  optional (try (char ' ' >> many1 (choice [digit, (char '+')])))
  skip
  [cost, damage, armor] <- num `sepBy` skip
  newline
  return $ Item cost damage armor
  where skip = many1 (char ' ')
        num = read <$> many1 digit

characterParser :: CharParser () Character
characterParser = liftA3 Character stat stat stat where
  stat = do
    manyTill anyChar (char ':')
    many space
    read <$> many digit <* newline

slurp :: FilePath -> IO String
slurp p = hGetContents =<< openFile p ReadMode

-- partial function, only works because input is always well formed
doParse :: CharParser () a -> String -> a
doParse p s = case runParser p () "input" s of
  (Left e) -> error ":("
  (Right x) -> x

parseShop :: String -> Shop
parseShop = doParse shopParser

parseBoss :: String -> Character
parseBoss = doParse characterParser

main = do
  [shopFile, bossFile] <- getArgs
  shop <- parseShop <$> slurp shopFile
  boss <- parseBoss <$> slurp bossFile
  print boss
