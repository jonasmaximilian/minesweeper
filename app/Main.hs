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
-- write Documentation


-- curr log:
-- get game bot working, so we dont get a random move


main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"

     -- initialize the liveBoard, which is the board that is displayed to the user
    liveBoard <- liftIO $ newIORef (replicate height $ replicate width (-1))
    liftIO $ printLiveBoard =<< readIORef liveBoard

    -- start game with 5 mines 5x5 board (height and width are defined in Lib.hs)
    -- this board is used to calculate the values we display to the user. (number of mines around the given cell)
    let mines = 5
    liftIO $ print "generating seed..."
    -- but first we must generate a seed
    seed <- liftIO $ randomIO :: UI Int
    liftIO $ print seed

    -- initialize the game with the seed
    let (result, finalState) = runMinesweeper (initialize mines seed) (undefined, undefined)
    liftIO $ prettyPrint finalState

    -- set the play action to Reveal, which is the default action (the user can change to flag by clicking the flag button)
    -- it seems intuitiv however to have this set to Reveal at the start / as default
    currAction <- liftIO $ newIORef Reveal

    -- GUI
    board <- UI.table
    
    forM_ [0..height-1] $ \x -> do
        rowElement <- UI.tr
        forM_ [0..width-1] $ \y -> do

                -- create the cells in the UI
                cell <- UI.button # set UI.text "."
                element cell # UI.set UI.id_ (show x ++ show y)
                element cell # UI.set UI.style [("border", "1px solid black"), ("width", "30px"), ("height", "30px")]
                getBody window #+ [element cell]
                
                -- add event handler and game logic
                on UI.click cell $ \_ -> do
                    -- get action type
                    -- this tells us if we actually have to calculate stuff or can just put a flag
                    action <- liftIO $ readIORef currAction
                    liftIO $ print action

                    -- check if action is Reveal or Flag 🚩
                    case action of
                        Flag -> do
                            element cell # set UI.text "🚩"
                        Reveal -> do
                            -- play the cell
                            -- refactor this LMAO
                            let (result, (board, numMines)) = runMinesweeper (reveal x y) finalState
                            
                            -- cellValue is whatever the user clicked on
                            let cellValue = board !! x !! y
                            case cellValue of

                                -- clicked on a mine
                                -- => end the game and refresh after 1 second
                                Mine -> do 
                                    element cell # set UI.text "X"
                                    element cell # UI.set UI.style [("background-color", "red")]
                                    -- end game
                                    liftIO $ threadDelay 1000000
                                    liftIO $ exitSuccess

                                -- clicked on a number
                                -- => display the number and update the liveBoard
                                Revealed n -> do
                                    
                                    element cell # set UI.text (show n)
                                    element cell # UI.set UI.style [("background-color", "green")]

                                    -- update liveBoard
                                    -- read
                                    liveBoard' <- liftIO $ readIORef liveBoard
                                    liveBoard'' <- liftIO $ return $ setVal x y n liveBoard'
                                    -- write to liveBoard
                                    liftIO $ writeIORef liveBoard liveBoard''

                                    liveBoard <- liftIO $ readIORef liveBoard
                                    liftIO $ print (length (filter (<0) (concat liveBoard)))
                                    liftIO $ printLiveBoard liveBoard
                                    return cell


                                    -- check if game is over
                                    -- this is the case if all cells are revealed
                                    -- all cells are revealed if the number of cells with value -1 is equal to the number of mines 
                                    -- (liveBoard gets initialized with -1 values and updated after clicks with the actual values)
                                    if (length (filter (<0) (concat liveBoard))) == mines then do

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
   
        -- add the board to the UI
        getBody window #+ [element rowElement]
        getBody window #+ [element board]

    -- Suggest move button and logic
    randomMoveBtn <- UI.button # set UI.text "Suggest Move"
    getBody window #+ [element randomMoveBtn]
    on UI.click randomMoveBtn $ \_ -> do
        liveBoard' <- liftIO $ readIORef liveBoard
        (x, y) <- liftIO $ findSafeMove liveBoard'
        liftIO $ print (x, y)
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


