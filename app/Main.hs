module Main (main) where

import Lib
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Control.Monad                (forM_, forM)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import System.Random
import Data.IORef
import Control.Monad 

-- TODO till sleep (at least) lets go 90 15


-- 11:30 work -> 12 pm break
-- 12 pm -> 1:30 pm work -> 2 pm break
-- 2 pm -> 3:30 pm work -> 4 pm bed?

-- safe current state on github  --------- DONE
-- change clickedBoard to Ints (rewrite) -- DONE
-- try to fix clickedBoard order (rewrite) -- DONE
-- fix bug that sometimes gmaboard doesnt load? some infinite loop problem maybe seed related --- DONE



-- try to implement the suggest move logic (when theres a 0 val choose any square) maybe also for 1s
-- refactor code:
--      -- get rid of Hidden and Flagged in Cell data type
--      -- 
-- write Documentation




setAt :: Int -> Int -> a -> [[a]] -> [[a]]
setAt x y val array = 
    let (before, row:after) = splitAt x array
        row' = setAtRow y val row
    in before ++ row':after

setAtRow :: Int -> a -> [a] -> [a]
setAtRow n val row = let (before, _:after) = splitAt n row
                    in before ++ val:after


printClickedBoard :: [[Int]] -> IO ()
printClickedBoard board = do
    forM_ [0..height-1] $ \x -> do
        forM_ [0..width-1] $ \y -> do
            let cellValue = board !! x !! y
            case cellValue of
                (-1) -> do
                    putStr "."
                n -> do
                    putStr $ show n
        putStrLn ""

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"

     -- initialize clickedBoard
    clickedBoard <- liftIO $ newIORef (replicate height $ replicate width (-1))
    liftIO $ printClickedBoard =<< readIORef clickedBoard

    -- start game with 5 mines 5x5 board (height and width are defined in Lib.hs)
    let mines = 5
    liftIO $ print "generating seed..."
    seed <- liftIO $ randomIO :: UI Int
    liftIO $ print seed

    let (result, finalState) = runMinesweeper (initialize mines seed) (undefined, undefined)
    liftIO $ prettyPrint finalState

    -- set the play action to Reveal
    currAction <- liftIO $ newIORef Reveal

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
                    -- get action type
                    action <- liftIO $ readIORef currAction
                    liftIO $ print action

                    -- check if action is Reveal or Flag 🚩
                    case action of
                        Flag -> do
                            element cell # set UI.text "🚩"
                        Reveal -> do
                            -- play the cell
                            let (result, newFinalState) = runMinesweeper (play x y) finalState
                            let (board, numMines) = newFinalState
                            let finalState = newFinalState
                            liftIO $ prettyPrint finalState
                            let cellValue = board !! x !! y
                            case cellValue of
                                Mine -> do -- place mine 💣
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
                                    clickedBoard'' <- liftIO $ return $ setAt x y n clickedBoard'
                                    -- write
                                    liftIO $ writeIORef clickedBoard clickedBoard''

                                    clickedBoard <- liftIO $ readIORef clickedBoard
                                    liftIO $ print (length (filter (<0) (concat clickedBoard)))
                                    liftIO $ printClickedBoard clickedBoard
                                    return cell
                                    -- check if game is over
                                    if (length (filter (<0) (concat clickedBoard))) == mines then do
                                        liftIO $ putStrLn "You win!"
                                        winMsg <- UI.p # set UI.text "You win!"
                                        getBody window #+ [element winMsg]
                                        refreshBtn <- UI.button # set UI.text "Refresh"
                                        getBody window #+ [element refreshBtn]
                                        on UI.click refreshBtn $ \_ -> do
                                            liftIO $ exitSuccess
                                        return cell
                                    else do
                                        return cell
   
        getBody window #+ [element rowElement]
        getBody window #+ [element board]

    -- Suggest move button and logic
    randomMoveBtn <- UI.button # set UI.text "Suggest Move"
    getBody window #+ [element randomMoveBtn]
    on UI.click randomMoveBtn $ \_ -> do
        x <- liftIO $ randomRIO (0, height-1)
        y <- liftIO $ randomRIO (0, width-1)
        cell <- getElementById window (show x ++ show y)
        case cell of
            Just cell -> do
                 element cell # set UI.style [("background-color", "yellow")]
            Nothing -> do
                error "Error in bot move"
        return ()

    -- toggleFlag button and logic
    toggleFlagOn <- UI.button # set UI.text "Flag is off"
    getBody window #+ [element toggleFlagOn]
    on UI.click toggleFlagOn $ \_ -> do
        liftIO $ atomicModifyIORef currAction (\action -> (toggleAction action, action))
        action <- liftIO $ readIORef currAction
        liftIO $ print action
        case action of
            Flag -> do
                element toggleFlagOn # set UI.text "Flag is on"
            Reveal -> do
                element toggleFlagOn # set UI.text "Flag is off"
        return () 

    return ()
