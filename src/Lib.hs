module Lib
    ( 
        Cell(..),
        Action(..),
        height, 
        width,
        initialize,
        reveal,
        runMinesweeper,
        toggleAction,
        setVal,
        findSafeMove,
        printLiveBoard,
        prettyPrint
    ) where

import System.Random
import Control.Monad.State
import Data.List (concat)

-- Board and number of revealed cells
type MinesweeperState = (Board, Int) 

-- 2D array of cells
type Board = [[Cell]] 

-- Revealed Int is the number of mines around the cell
data Cell = Mine | Revealed Int | Hidden deriving (Show, Eq) 

-- action type to give the click context (flag or reveal)
data Action = Reveal | Flag deriving (Show, Eq)

-- State monad for the game
type Minesweeper a = State MinesweeperState a 

initialize :: Int -> Int -> Minesweeper () -- Initialize the board with mines
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

-- seed is randomly generated in Main.hs
randomPlacement :: Int -> Int -> Board -> (Board, StdGen)
randomPlacement 0 seed board = (board, mkStdGen seed)
randomPlacement n seed board =
  let gen = mkStdGen seed
      (x, gen') = randomR (0, height - 1) gen
      (y, gen'') = randomR (0, width - 1) gen'
  in if board !! x !! y == Mine -- if there is already a mine at this position, try again with a different seed
     then randomPlacement n (seed + 1) board
     else let board' = setAt x y Mine board
          in randomPlacement (n - 1) (seed) board'

-- count the number of mines around a given cell
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
  (board, numRevealedCells) <- get
  case board !! x !! y of
    Hidden -> do let board' = setAt x y (Revealed $ countMinesAt x y board) board
                 let numRevealedCells' = numRevealedCells + 1
                 put (board', numRevealedCells')
    _ -> put (board, numRevealedCells)

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

toggleAction :: Action -> Action
toggleAction action =
  case action of
    Reveal -> Flag
    Flag -> Reveal

-- runMinesweeper is a helper function to run the Minesweeper monad
runMinesweeper :: Minesweeper a -> MinesweeperState -> (a, MinesweeperState)
runMinesweeper = runState


-- set a value on a 2D array
setVal :: Int -> Int -> a -> [[a]] -> [[a]]
setVal x y val array = 
    let (before, row:after) = splitAt x array
        row' = setValRow y val row
    in before ++ row':after

setValRow :: Int -> a -> [a] -> [a]
setValRow n val row = let (before, _:after) = splitAt n row
                    in before ++ val:after

findSafeMove :: [[Int]] -> IO (Int, Int)
findSafeMove board = do
    let height = length board
    let width = length (head board)
    -- find a 0 value and return any value around it that isnt revealed yet -> -1
    -- if there are no 0 values, return any value that isnt revealed yet -> -1
    let findSafeMove' :: Int -> Int -> IO (Int, Int)
        findSafeMove' x y =
            case board !! x !! y of
                0 -> do
                    let neighbors = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]
                    let safeNeighbors = filter (\(x, y) -> x >= 0 && x < height && y >= 0 && y < width) neighbors
                    let safeNeighbors' = filter (\(x, y) -> board !! x !! y == -1) safeNeighbors
                    if safeNeighbors' == [] then -- if the neighbors are all revealed, try the next cell
                        findSafeMove' x (y+1)
                    else do
                        let (x', y') = head safeNeighbors'
                        return (x', y')
                _ -> findSafeMove' x (y+1)
    findSafeMove' 0 0
           

-- debug
printLiveBoard :: [[Int]] -> IO ()
printLiveBoard board = do
    forM_ [0..height-1] $ \x -> do
        forM_ [0..width-1] $ \y -> do
            let cellValue = board !! x !! y
            case cellValue of
                (-1) -> do
                    putStr "."
                n -> do
                    putStr $ show n
        putStrLn ""

-- debug
prettyPrint :: MinesweeperState -> IO ()
prettyPrint (board, _) = do
    let printCell cell =
            case cell of
            Mine -> putStr "X "
            _ -> putStr ". "
        printRow row = do
             forM_ row printCell
             putStrLn ""
    forM_ board printRow