module Mancala where

import RoseTree 

data Player = P1 | P2 deriving (Eq, Show)
data GameState = GameState {board :: [Int], turn :: Player}
data Outcome = Winner Player | Tie deriving (Eq, Show)

updateVal :: Int -> Int -> [Int] -> [Int]
updateVal idx new xs = (take (idx) xs) ++ [new] ++ (drop (idx+1) xs)

printGame :: GameState -> String
printGame (GameState (a1:b1:c1:d1:e1:f1:store1:a2:b2:c2:d2:e2:f2:store2:[]) _) = 
    unlines [
        "Player 1 pits " ++ showPits a1 b1 c1 d1 e1 f1,
        "Player 1 store: " ++ show store1,
        "Player 2 pits " ++ showPits a2 b2 c2 d2 e2 f2,
        "Player 2 store: " ++ show store2
        ] where
            showPits a b c d e f = concat ((show a) : map format [b,c,d,e, f])  
                where format num = ", " ++ (show num)
printGame _ = "Invalid board!"

moveOptions :: GameState -> [Int]
moveOptions gs
    | (turn gs) == P1 = filter isValidMove [0..5]
    | otherwise = filter isValidMove [7..12]
    where isValidMove n = ((board gs) !! n) > 0

applyMove :: Int -> GameState -> GameState
applyMove m (GameState b t)
    | (t==P1) && (finalIdx==6) = GameState nb2 P1
    | (t==P2) && (finalIdx==13) = GameState nb2 P2
    | otherwise = GameState nb2 (switch t)
    where
        switch P1 = P2
        switch P2 = P1
        stones = b!!m
        nextIdx :: Int -> Int
        nextIdx idx
            | t==P1 && idx == 12 = 0
            | t==P2 && idx == 5 = 7
            | otherwise = mod (idx+1) 14
        
        hlpr xs cnt currIdx 
            | cnt == 1 = ( (updateVal currIdx (1 + xs!!currIdx) xs), currIdx )
            | otherwise = (hlpr (updateVal currIdx (1 + xs!!currIdx) xs) (cnt-1) (nextIdx currIdx))
        
        (nb1, finalIdx) = hlpr (updateVal m 0 b) stones (m+1)
        
        oppIdx = 12-finalIdx
        nb2
            | t==P1 && (elem finalIdx [0..5]) && (nb1!!finalIdx == 1) && (nb1!!oppIdx/=0) = 
                updateVal 6 ((nb1!!6)+(nb1!!oppIdx)) (updateVal oppIdx 0 (updateVal finalIdx 0 nb1))
            | t==P2 && (elem finalIdx [7..12]) && (nb1!!finalIdx == 1) && (nb1!!oppIdx/=0) =
                updateVal 13 ((nb1!!13)+(nb1!!oppIdx)) (updateVal oppIdx 0 (updateVal finalIdx 0 nb1))
            | otherwise = nb1

gameIsFinished :: GameState -> Bool
gameIsFinished (GameState b _)
    | sum(map (b!!) [0..5])==0 || sum(map (b!!) [7..12])==0 = True
    | otherwise = False

winnerOfGame :: GameState -> Outcome
winnerOfGame (GameState b _)
    | p1Score > p2Score = Winner P1
    | p1Score < p2Score = Winner P2
    | otherwise = Tie
    where
    p1Score = sum (map (b!!) [0..6])
    p2Score = sum (map (b!!) [7..13])


gameTree :: Int -> GameState -> Rose GameState
gameTree depth gs
    | (depth == 0) || (gameIsFinished gs) = Node gs []
    | otherwise = Node gs children
    where children = [gameTree (depth-1) (applyMove m gs) | m<-moveOptions gs]