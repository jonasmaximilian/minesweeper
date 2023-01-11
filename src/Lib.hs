module Lib
    ( 
        Cell(..),
        height, 
        width,
        initialize,
        play, 
        runMinesweeper,
        prettyPrint
    ) where

import System.Random
import Control.Monad.State
import Data.List (concat)

--TODO
-- 1. change Cell type to Mine| Revealed Int | Hidden 
-- 1.1  find the value of the revealed number in the reveal function
--2 make Lib file more pure #
-- change state to board, numRevealed
--3. make State wwork!!!

type MinesweeperState = (Board, Int)

type Board = [[Cell]]

data Cell = Mine | Revealed Int | Hidden deriving (Show, Eq)

type Minesweeper a = State MinesweeperState a

initialize :: Int -> Int -> Minesweeper ()
initialize numMines seed = do
    let board = replicate height $ replicate width Hidden
    let (mines, _) = randomPlacement numMines seed board
    put (mines, 0) -- 0 is the number of revealed cells

height :: Int
height = 5

width :: Int
width = 5

size :: Int
size = width * height


randomPlacement :: Int -> Int -> Board -> (Board, StdGen)
randomPlacement 0 seed board = (board, mkStdGen seed)
randomPlacement n seed board =
  let gen = mkStdGen seed
      (x, gen') = randomR (0, height - 1) gen
      (y, gen'') = randomR (0, width - 1) gen'
  in if board !! x !! y == Mine
     then randomPlacement n seed board
     else let board' = setAt x y Mine board
          in randomPlacement (n - 1) (seed + 1) board'

countMinesAt :: Int -> Int -> Board -> Int
countMinesAt x y board =
  length $ filter (== Mine) $ concat $ map (\(x', y') -> [board !! x' !! y']) (neighbors x y)

-- Get the indices of the surrounding cells.
neighbors :: Int -> Int -> [(Int, Int)]
neighbors x y =
  [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x' >= 0, y' >= 0, x' < height, y' < width, (x', y') /= (x, y)]




-- Set the value at a given position on the board.
setAt :: Int -> Int -> Cell -> Board -> Board
setAt x y value board =
  let row = board !! x
      row' = take y row ++ [value] ++ drop (y + 1) row
  in take x board ++ [row'] ++ drop (x + 1) board

-- Reveal a cell on the board, potentially ending the game if it is a mine.
reveal :: Int -> Int -> Minesweeper ()
reveal x y = do
  (board, numValues) <- get
  case board !! x !! y of
    Hidden -> do let board' = setAt x y (Revealed $ countMinesAt x y board) board
                 let numValues' = numValues + 1
                 put (board', numValues')
    _ -> put (board, numValues)

-- Reveal a cell and its surrounding cells if it is empty.
revealCell :: Int -> Int -> Board -> Board
revealCell x y board =
  let cell = board !! x !! y
  in case cell of
    Hidden -> let board' = setAt x y (Revealed $ countMinesAt x y board) board
              in if countMinesAt x y board == 0
                 then foldl (\board'' (x', y') -> revealCell x' y' board'') board' (neighbors x y)
                 else board'
    _ -> board
    
    
-- Count the number of mines in the surrounding cells.
countMines :: Board -> Int
countMines board =
  let cells = concat board
  in length $ filter (== Mine) cells

-- end the game if there are no Number cells left
endGame :: Minesweeper ()
endGame = do
  (board, mines) <- get
  let cells = concat board
  if length (filter (== Hidden) cells) == 0
  then put (board, 0)
  else when (mines == 0) $ put (board, 0)

-- Play the game by revealing a cell.
play :: Int -> Int -> Minesweeper ()
play x y = do
  reveal x y
  endGame

runMinesweeper :: Minesweeper a -> MinesweeperState -> (a, MinesweeperState)
runMinesweeper = runState

-- debug
prettyPrint :: MinesweeperState -> IO ()
prettyPrint (board, _) = do
    let printCell cell =
            case cell of
            Mine -> putStr "X "
            Revealed n -> putStr $ show n ++ " "
            Hidden -> putStr "H "
        printRow row = do
             forM_ row printCell
             putStrLn ""
    forM_ board printRow