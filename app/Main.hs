module Main (main) where

import Lib
--import qualified Graphics.UI.Threepenny as UI
--import Graphics.UI.Threepenny.Core

main :: IO ()
main = do 
    putStrLn "Hello, Haskell!"
    let board = initBoard 1 1
    printBoard board
    board' <- insertMines 0 board
    printBoard board'
    print $ gameWon board'
    print $ gameLost board'
    putStrLn "Uncovering 0,0"
    -- uncover and save the new board
    board <- return $ uncover 0 0 board'
    printBoard board
    print $ gameWon board
    print $ gameLost board
    



--main = putStrLn "Hello, Haskell!"