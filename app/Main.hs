module Main (main) where

import Lib
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Control.Monad                (forM_, forM)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import System.Random
import Data.IORef




main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"

    -- start game with 5 mines 5x5 board (height and width are defined in Lib.hs)
    let (result, finalState) = runMinesweeper (initialize 5 0) (undefined, undefined)
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
                        Number n -> do
                            element cell # set UI.text (show n)
                            element cell # UI.set UI.style [("background-color", "green")]
                        


        getBody window #+ [element rowElement]
        getBody window #+ [element board]

        
    makeMoveBtn <- UI.button # set UI.text "Make Move"
    getBody window #+ [element makeMoveBtn]

    on UI.click makeMoveBtn $ \_ -> do
        liftIO $ putStrLn "Make Move"
        gen <- liftIO $ newStdGen
        let (x, _) = randomR (0, height - 1) gen
        let (y, _) = randomR (0, width - 1) gen
        liftIO $ putStrLn $ "x: " ++ show x ++ " y: " ++ show y
        let (result, newFinalState) = runMinesweeper (play x y) finalState
        let (board, numMines) = newFinalState
        let finalState = newFinalState
        -- get cell by id and set background color to yellow
        cell <- getBody window # UI.find (UI.byId (show x ++ show y))
        element cell # UI.set UI.style [("background-color", "yellow")]




