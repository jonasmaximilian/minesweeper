module Main (main) where

import Lib
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Control.Monad                (forM_, forM)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

--import qualified Text.XHtml as UI
--import Text.XHtml (HTMLTABLE(cell))

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Minesweeper"
    board <- UI.table 
    let game = initBoard 5 5
    game' <- liftIO $ insertMines 5 game

    forM_ [0..4] $ \row -> do 
        rowElem <- UI.tr
        forM_ [0..4] $ \col -> do
            cell <- UI.button # set UI.text "." # set UI.id_ (show row ++ show col)
            element cell # UI.set UI.style [("border", "1px solid black"), ("width", "30px"), ("height", "30px")]
            getBody window #+ [element cell]
            on UI.click cell $ \_ -> do
                let cell' = uncover2 row col game'
                let cell'' = case cell' of
                                HiddenMine -> Mine
                                HiddenInt -> Revealed $ neighbourMines row col game'
                                Revealed n -> Revealed n
                                Mine -> Mine
                let text = case cell'' of
                                HiddenMine -> "H"
                                HiddenInt -> "."
                                Revealed n -> show n
                                Mine -> "M"
                element cell # set UI.text text
                if cell'' == Mine
                    then do 
                        element cell # set UI.style [("background-color", "red")]
                        liftIO $ threadDelay (2 * 1000000)
                        liftIO exitSuccess
                    else element cell # set UI.style [("background-color", "green")]
        getBody window #+ [element rowElem]
        
        getBody window #+ [element board]
    