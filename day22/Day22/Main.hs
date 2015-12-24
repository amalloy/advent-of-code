module Day22.Main where

import Day22.Game
import Day22.Search
import Data.Maybe (fromJust)

initPlayer :: Player
initPlayer = Player 50 500

initBoss :: Boss
initBoss = Boss 51 9

initCombat :: Bool -> Combat
initCombat = Combat PlayerTurn initPlayer initBoss []

part1 = initCombat False
part2 = adjustPlayerHP (-1) $ initCombat True -- hard mode is the only effect that has to apply before the player initiates it, so we "cheat" its first application here

mkProblem :: Combat -> Problem Combat Int
mkProblem c = Problem c nexts outcome

main = do
  print $ map (fst . fromJust . shortestPath . mkProblem) [part1,part2]
