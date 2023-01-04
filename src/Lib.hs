module Lib
    ( 
        Cell(..),
        Board(..),
        initBoard,
        printBoard,
        insertMines,
        gameWon,
        gameLost,
        uncover
    ) where

import System.Random

data Cell = HiddenMine | HiddenInt | Revealed Int | Mine deriving (Show, Eq)

newtype Board = Board [[Cell]] deriving (Show, Eq)

initBoard :: Int -> Int -> Board 
initBoard height width = Board $ replicate height $ replicate width (HiddenInt)

-- helper function to update a list at a given index (Data,List.Lens ???)
updateAt :: Int -> a -> [a] -> [a]
updateAt 0 x (_:xs) = x:xs
updateAt n x (y:ys) = y:updateAt (n-1) x ys
updateAt _ _ [] = []

insertMines :: Int -> Board -> IO Board
insertMines 0 board = return board
insertMines n (Board rows) = do
    x <- randomRIO (0, length rows - 1)
    y <- randomRIO (0, length (rows !! x) - 1)
    putStrLn $ "Inserting mine at " ++ show x ++ ", " ++ show y
    --check if mine already exists
    if rows !! x !! y == HiddenMine
      then insertMines n (Board rows)
      else insertMines (n-1) $ Board $ updateAt x (updateAt y HiddenMine (rows !! x)) rows


-- no elements of the board are HiddenInt
gameWon :: Board -> Bool 
gameWon (Board rows) = not $ any (any (== HiddenInt)) rows

-- TODO
gameLost :: Board -> Bool
gameLost (Board rows) = any (any (== Mine)) rows

-- get a List of all the neighbours of a cell
neighbours :: Int -> Int -> Board -> [Cell]
neighbours x y (Board rows) = do
    let height = length rows
    let width = length (rows !! 0)
    let x' = [x-1, x, x+1]
    let y' = [y-1, y, y+1]
    [rows !! i !! j | i <- y', i >= 0, j <- x', j >= 0, i < height, j < width]

-- check how many hiddenMines are in the neighbours of a cell 
-- don't have to check for mines because if they are already revealed the game is over
neighbourMines :: Int -> Int -> Board -> Int 
neighbourMines x y board = length $ filter (== HiddenMine) $ neighbours x y board


-- uncover a cell permanently on the board
uncover :: Int -> Int -> Board -> Board
uncover x y (Board rows) = if rows !! y !! x == HiddenMine
    then Board $ updateAt y (updateAt x Mine (rows !! y)) rows
    -- else count the number of mines and hiddenMines around the cell
    else Board $ updateAt y (updateAt x (Revealed $ neighbourMines x y (Board rows)) (rows !! y)) rows
    

-- debugging 
printBoard :: Board -> IO ()
printBoard (Board rows) = do
    mapM_ print rows


-- TODOS:
-- 1. add a flag to a cell ?? isnt this just cosmetic?
-- 2. test the gameWon and gameLost functions
-- 3. add a simple UI using threepenny-gui

