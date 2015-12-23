module Day22.Main where

import Day22.Game
import Day22.Search
import Data.Maybe (fromJust)

initPlayer :: Player
initPlayer = Player 50 500

initBoss :: Boss
initBoss = Boss 51 9

initCombat :: Combat
initCombat = Combat PlayerTurn initPlayer initBoss []

main = do
  print . fst . fromJust . shortestPath $ Problem initCombat nexts outcome
