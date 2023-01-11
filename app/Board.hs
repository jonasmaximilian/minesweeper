module Board (
    Board
    , Cell(..)
    , createBoard
    , isMine
    , isRevealed
    , revealCell
    , countAdjacentMines
) where

import Data.Array
import System.Random
import Utils

-- Contains the functions and types for representing the game board and its cells
