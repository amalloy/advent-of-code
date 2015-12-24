module Day22.Game where

import Day22.Search
import Data.Bool (bool)
import Control.Monad

type MP = Int
type Turns = Int

data Effect = Shield | Poison | Recharge deriving (Enum, Eq, Show)
data Instant = Missile | Drain deriving (Enum, Eq, Show)
data Spell = Effect Effect Turns | Instant Instant deriving Show
type Spellbook = [(MP, Spell)]

wizardSpells :: Spellbook
wizardSpells = [(53, Instant Missile),
                (73, Instant Drain),
                (113, Effect Shield 6),
                (173, Effect Poison 6),
                (229, Effect Recharge 5)]

data Boss = Boss {health :: Int,
                  attack :: Int} deriving Show
data Player = Player {hp :: Int,
                      mp :: MP} deriving Show

data Turn = PlayerTurn | BossTurn deriving Show
data Combat = Combat {turn :: Turn,
                      player :: Player,
                      boss :: Boss,
                      activeEffects :: [(Effect, Turns)],
                      hardMode :: Bool -- what a lame hack
                     }
              deriving Show

adjustPlayerHP :: Int -> Combat -> Combat
adjustPlayerHP amt combat@(Combat {player = p}) = combat {player = p {hp = hp p + amt}}

adjustPlayerMP :: Int -> Combat -> Combat
adjustPlayerMP amt combat@(Combat {player = p}) = combat {player = p {mp = mp p + amt}}

adjustBossHP :: Int -> Combat -> Combat
adjustBossHP amt combat@(Combat {boss = b}) = combat {boss = b {health = health b + amt}}

outcome :: Combat -> Result
outcome c | hp (player c) <= 0 = Failure
          | health (boss c) <= 0 = Success
          | otherwise = Progress

applyHardMode :: Combat -> Combat
applyHardMode c = case (hardMode c, turn c) of
  (False, _) -> c
  (True, PlayerTurn) -> c
  (True, BossTurn) -> adjustPlayerHP (-1) c

runEffects :: Combat -> Combat
runEffects combat = applyHardMode . decrementDurations $
                    foldr runEffect combat (fst <$> activeEffects combat)
  where decrementDurations c =
          let effects = activeEffects c
              effects' = filter ((/= 0) . snd) . map (fmap pred) $ effects
          in c {activeEffects = effects'}

runEffect effect combat@(Combat {player = p, boss = b}) =
  case effect of
    Shield -> combat -- effect is considered during attack phase
    Poison -> adjustBossHP (-3) combat
    Recharge -> adjustPlayerMP 101 combat

switchTurns c@(Combat {turn=turn}) = c {turn = case turn of
                                           BossTurn -> PlayerTurn
                                           PlayerTurn -> BossTurn}

nexts :: Combat -> [(MP, Combat)]
nexts c = map (runEffects . switchTurns <$>) $ case turn c of
  BossTurn -> [(0, runBossTurn c)]
  PlayerTurn -> runPlayerTurn c

playerAC :: Combat -> Int
playerAC combat = bool 0 7 $ Shield `elem` (map fst $ activeEffects combat)

runBossTurn :: Combat -> Combat
runBossTurn combat = let ac = playerAC combat
                         atk = attack (boss combat)
                         damage = max 1 (atk - ac)
                     in adjustPlayerHP (- damage) combat

applySpell :: Combat -> Spell -> Combat
applySpell c spell = case spell of
  (Effect e numTurn) -> c {activeEffects = (e, numTurn) : activeEffects c}
  (Instant Missile) -> adjustBossHP (-4) c
  (Instant Drain) -> adjustBossHP (-2) . adjustPlayerHP 2 $ c

applicableSpells :: Combat -> [(MP, Spell)]
applicableSpells c = do
  s@(mana, spell) <- wizardSpells
  guard $ (mp (player c)) >= mana
  case spell of
    (Instant _) -> return s
    (Effect e _) -> bool [s] [] $ e `elem` (map fst (activeEffects c))

runPlayerTurn :: Combat -> [(MP, Combat)]
runPlayerTurn c = do
  (mp, spell) <- applicableSpells c
  return $ (mp, adjustPlayerMP (negate mp) $ applySpell c spell)
