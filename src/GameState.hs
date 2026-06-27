module GameState where

import Mancala

newtype GameStateOp a = GameStateOp (GameState -> (a, GameState))

instance Functor GameStateOp where
    fmap f (GameStateOp g) = GameStateOp (\gs -> let (val, ngs) = g gs in (f val, ngs))

instance Applicative GameStateOp where
    pure x = GameStateOp (\gs -> (x,gs))
    GameStateOp ff <*> (GameStateOp g) = GameStateOp (\gs ->
            let
                (f, ngs1) = ff gs
                (val, ngs2) = g ngs1
            in
                (f val, ngs2) 
        )

instance Monad GameStateOp where
    (GameStateOp g) >>= f = GameStateOp (\gs ->
        let
            (val1, ngs1) = g gs
            (GameStateOp ng) = f val1
        in
            ng ngs1
        )

applyMove :: Int -> GameStateOp Bool
applyMove m = GameStateOp (\gs ->
        let
            ngs = Mancala.applyMove m gs
        in
            (gameIsFinished ngs, ngs)
    )

newtype GameStateOpHistory a = GameStateOpHistory (GameState -> (a ,[GameState]))

instance Functor GameStateOpHistory where
    fmap f (GameStateOpHistory g) = GameStateOpHistory (\gs -> let (val, ngs) = g gs in (f val, ngs))

instance Applicative GameStateOpHistory where
    pure x = GameStateOpHistory (\gs -> (x, []))
    (GameStateOpHistory ff) <*> (GameStateOpHistory g) = GameStateOpHistory (\gs ->
            let
                (f, hist1) = ff gs
                currgs = if null hist1 then gs else (head hist1)
                (val, hist2) = g currgs
            in
                (f val, hist2 ++ hist1)
        )

instance Monad GameStateOpHistory where
    (GameStateOpHistory g) >>= f = GameStateOpHistory (\gs ->
            let
                (val, hist1) = g gs
                currgs = if null hist1 then gs else (head hist1)
                (GameStateOpHistory ng) = f val
                (val2, hist2) = ng currgs
            in
                (val2, hist2++hist1)
        )

applyMoveH :: Int -> GameStateOpHistory Bool
applyMoveH m = GameStateOpHistory (\gs ->
        let 
            ngs = Mancala.applyMove m gs
        in
            (gameIsFinished ngs, [ngs])
    )

runGameState :: GameStateOp a -> GameState -> (a, GameState)
runGameState (GameStateOp g) gs = g gs

runGameStateH :: GameStateOpHistory a -> GameState -> (a, [GameState])
runGameStateH (GameStateOpHistory g) gs = g gs

initialize :: GameStateOpHistory ()
initialize = GameStateOpHistory (\gs -> ( () , [] ))