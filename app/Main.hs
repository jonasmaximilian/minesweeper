module Main (main) where

import Lib
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Control.Monad                (forM_, forM)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import System.Random
import Data.IORef


setAt :: Int -> Int -> a -> [[a]] -> [[a]]
setAt x y val array = 
    let (before, row:after) = splitAt x array
        row' = setAtRow y val row
    in before ++ row':after

setAtRow :: Int -> a -> [a] -> [a]
setAtRow n val row = let (before, _:after) = splitAt n row
                    in before ++ val:after

printClickedBoard :: [[Bool]] -> IO ()
printClickedBoard board = do
    forM_ [0..height-1] $ \x -> do
        forM_ [0..width-1] $ \y -> do
            let cellValue = board !! x !! y
            case cellValue of
                True -> do
                    putStr "X"
                False -> do
                    putStr "."
        putStrLn ""

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"
    clickedBoard <- liftIO $ newIORef (replicate height $ replicate width False)
    -- start game with 5 mines 5x5 board (height and width are defined in Lib.hs)
    let mines = 5
    seed <- liftIO $ randomIO :: UI Int
    let (result, finalState) = runMinesweeper (initialize mines seed) (undefined, undefined)
    liftIO $ prettyPrint finalState

    -- GUI
    board <- UI.table
    
    forM_ [0..height-1] $ \x -> do
        rowElement <- UI.tr
        forM_ [0..width-1] $ \y -> do
                cell <- UI.button # set UI.text "."
                element cell # UI.set UI.id_ (show x ++ show y)
                element cell # UI.set UI.style [("border", "1px solid black"), ("width", "30px"), ("height", "30px")]
                getBody window #+ [element cell]
                

                
                -- add event handler and game logic
                on UI.click cell $ \_ -> do
                    let (result, newFinalState) = runMinesweeper (play x y) finalState
                    let (board, numMines) = newFinalState
                    let finalState = newFinalState
                    liftIO $ prettyPrint finalState
                    let cellValue = board !! x !! y
                    case cellValue of
                        Mine -> do
                            element cell # set UI.text "X"
                            element cell # UI.set UI.style [("background-color", "red")]
                            -- end game
                            liftIO $ threadDelay 1000000
                            liftIO $ exitSuccess
                        Revealed n -> do
                            element cell # set UI.text (show n)
                            element cell # UI.set UI.style [("background-color", "green")]
                            -- update clickedBoard
                            -- read
                            clickedBoard' <- liftIO $ readIORef clickedBoard
                            clickedBoard'' <- liftIO $ atomicModifyIORef clickedBoard (\clickedBoard' -> (setAt x y True clickedBoard', clickedBoard'))
                            clickedBoard <- return clickedBoard''
                            liftIO $ print (length (filter (== False) (concat clickedBoard)))
                            if length (filter (== False) (concat clickedBoard)) == (mines + 1) then do
                                div <- UI.div # set UI.text "You win!"
                                getBody window #+ [element div]
                                liftIO $ threadDelay 1000000
                                div <- UI.div # set UI.text "Refresh to play again"
                                getBody window #+ [element div]
                                return ()
                                
                            else do
                                return ()
                            liftIO $ printClickedBoard clickedBoard
                            return ()
                        


        getBody window #+ [element rowElement]
        getBody window #+ [element board]
