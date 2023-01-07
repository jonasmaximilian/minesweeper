module Main (main) where

import Lib
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Control.Monad                (forM_, forM)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    let (result, finalState) = runMinesweeper (initialize 5 0 >> play 0 0) (undefined, undefined)
    print result -- ()
    prettyPrint finalState -- Board with mines and numbers
    -- print finalState -- (Board with mines and numbers, 80)
    let (_, nums) = finalState
    print nums -- 90
