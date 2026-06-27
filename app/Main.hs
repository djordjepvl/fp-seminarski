module Main where

import Mancala
import Minimax
import GameState

import Text.Read (readMaybe)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

getInputMove :: [Int] -> IO Int
getInputMove options = do
  putStr "Your turn: (1-6): "
  ans <- getLine
  case readMaybe ans of
    Just num | (elem (num-1) options) -> return (num-1)
    _ -> do
      putStrLn "Invalid move!"
      getInputMove options


playLoop :: Int -> GameState -> IO ()
playLoop minimaxDepth gs = do
  putStrLn "Current board: "
  putStrLn (printGame gs)
  let gameOver = gameIsFinished gs
  if gameOver 
    then do
      case winnerOfGame gs of
        Winner P1 -> putStrLn "You win!"
        Winner P2 -> putStrLn "You lose!"
        Tie -> putStrLn "Tie!"
      putStrLn "Play again? (y/n): "
      ans <- getLine
      if elem ans ["y","Y"]
        then playLoop minimaxDepth mancalaInitialState
        else return ()
    else do
      ngs <-
        if (turn gs == P1) 
          then do
            ans <- getInputMove (moveOptions gs)
            return (Mancala.applyMove ans gs)
          else do
            putStrLn "Bot's turn... "
            let bestMove = Minimax.bestNextMove minimaxDepth gs
            putStrLn ("Bot plays pit " ++ (show bestMove) ++ "!")
            return (Mancala.applyMove bestMove gs)
      playLoop minimaxDepth ngs
    

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Play against mancalabot? (y/n): "
  ans <- getLine
  if (elem ans ["y","Y"])
    then playLoop 7 mancalaInitialState
    else return ()
