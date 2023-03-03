module Cells where

import qualified Data.Vector as V
import Data.Vector.Storable (Storable)
import Data.Foldable (foldr')

data GameOfLifeCell = 
    AliveCell (Int,Int) 
    | DeadCell (Int, Int) 
    | AboutToDieCell (Int, Int)
    | AboutToRessurect (Int, Int)
  deriving (Show, Eq)

cellPosition:: GameOfLifeCell -> (Int, Int)
cellPosition (AliveCell pos) = pos
cellPosition (DeadCell pos) = pos
cellPosition (AboutToDieCell pos) = pos
cellPosition (AboutToRessurect pos) = pos

numOfDeadAndAliveCells:: [GameOfLifeCell] -> (Int, Int)
numOfDeadAndAliveCells = foldr' helper (0,0) 
    where 
        helper (AliveCell _) (alive,dead) = (alive + 1, dead)
        helper (DeadCell _) (alive,dead) = (alive, dead + 1)
        helper (AboutToDieCell _) (alive,dead) = (alive, dead + 1)
        helper (AboutToRessurect _) (alive,dead) = (alive + 1, dead)

neighbours:: (Int, Int) -> (Int,Int) -> [(Int, Int)]
neighbours (xLimit, yLimit) (xCellPosition, yCellPosition) = 
    filter (\(x,y) -> x >= 0 && y >= 0 && x < xLimit && y < yLimit) 
        [topLeft, top, topRight, right, bottomRight, bottom, bottomLeft, left]
    where
        topLeft = (xCellPosition - 1, yCellPosition + 1)
        top = (xCellPosition, yCellPosition + 1) 
        topRight = (xCellPosition + 1, yCellPosition + 1)
        right = (xCellPosition+1, yCellPosition) 
        bottomRight = (xCellPosition + 1, yCellPosition - 1)
        bottom = (xCellPosition, yCellPosition - 1) 
        bottomLeft = (xCellPosition - 1, yCellPosition - 1)
        left = (xCellPosition-1, yCellPosition ) 