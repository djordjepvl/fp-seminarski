module Minimax where

import Mancala
import RoseTree

--za svaki moguci potez igraca, broji se koliko ce imati kamencica u svojoj velikoj posudi nakon tog poteza
--heuristic je suma svih tih iznosa za P1 minus suma svih tih iznosa za P2
--ako je rezultat pozitivan, P1 pobedjuje, ako je negativan, P2 pobedjuje 
heuristic :: GameState -> Int
heuristic gs = 
        sum (map (!!6) [board $ applyMove m p1s | m <- (moveOptions p1s)]) -
        sum (map (!!13) [board $ applyMove m p2s | m <- (moveOptions p2s)])
    where 
        p1s = GameState (board gs) P1
        p2s = GameState (board gs) P2


minimax :: Rose GameState -> Int
minimax (Node gs []) = heuristic gs
minimax (Node gs children)
    | turn gs == P1 = maximum (map minimax children)
    | otherwise = minimum (map minimax children)

bestNextMove :: Int -> GameState -> Int
bestNextMove depth gs = bestMove
    where
        tree = gameTree depth gs
        Node _ children = tree
                
        childrenScores = (map minimax children)
        bestScore
            | turn gs == P1 = maximum childrenScores
            | otherwise = minimum childrenScores
        indexedScores = zip (moveOptions gs) childrenScores
        bestMoves = filter (\(_,y)->y==bestScore) indexedScores
        (bestMove, _) = head bestMoves
        


