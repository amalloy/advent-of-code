module Day22.Game where

type MP = Int
type Turn = Int

data Effect = Shield | Poison | Recharge deriving (Enum, Eq)
data Instant = Missile | Drain deriving (Enum, Eq)
data Spell = Effect Effect Turn | Instant Instant
data Spellbook = M.Map Spell MP
