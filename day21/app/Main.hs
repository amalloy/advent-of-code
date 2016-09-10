module Main where

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

player :: Character
player = Character 100 0 0

human :: Constraints
human = M.fromList [(Weapon, Range 1 1), (Armor, Range 0 1), (Ring, Range 0 2)]

wearing :: Character -> Item -> Character
(Character hp attack ac) `wearing` (Item cost damage armor) =
  Character hp (attack + damage) (ac + armor)

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

-- returns a single "composite" item representing the sum of all chosen items
goShopping :: Shop -> Constraints -> [Item]
goShopping shop c = let buy (slot, range) = (shop M.! slot) `choose` range
                        sets = mapM buy $ M.toList c
                    in map (mconcat . mconcat) sets

boughtEnough :: Character -> Item -> Bool
boughtEnough boss item = player `wearing` item `winsFight` boss

part1 :: Constraints -> Character -> Shop -> Int
part1 constr boss shop = minimum . map cost . filter (boughtEnough boss) $ goShopping shop constr

part2 :: Constraints -> Character -> Shop -> Int
part2 constr boss shop = maximum . map cost . filter (not . boughtEnough boss) $ goShopping shop constr

main = do
  [shopFile, bossFile] <- getArgs
  shop <- parseShop <$> slurp shopFile
  boss <- parseBoss <$> slurp bossFile
  print $ part1 human boss shop
  print $ part2 human boss shop

shopParser :: CharParser () Shop
shopParser = M.fromList <$> shopSection `sepBy` newline

shopSection :: CharParser () (Slot, [Item])
shopSection = (,) <$> sectionHeader <*> many shopItem

sectionHeader :: CharParser () Slot
sectionHeader = readSlot <$> many1 letter <* manyTill anyChar newline
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
