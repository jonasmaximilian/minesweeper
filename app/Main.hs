module Main (main) where

import Lib
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Control.Monad                (forM_, forM)
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

    forM_ [0..9] $ \row -> do 
        rowElem <- UI.tr
        forM_ [0..9] $ \col -> do
            cell <- UI.td # set UI.text " "
            element cell # UI.set UI.style [("border", "1px solid black"), ("width", "30px"), ("height", "30px")]
            getBody window #+ [element cell]
        getBody window #+ [element rowElem]
        getBody window #+ [element board]



    
    