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

