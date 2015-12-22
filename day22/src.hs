type MP = Int
type Turn = Int

data Effect = Shield Int | Poison Int | Recharge Int
data Instant = Missile Int | Drain Int
data Spell = Effect Effect Turn | Instant Instant
data Spellbook = M.Map Spell MP
