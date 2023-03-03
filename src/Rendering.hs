module Rendering where

import Drawable
import Cells
import Board
import qualified Data.Vector as V
import Control.Monad (join)
import GHC.Float (int2Float)
import Data.Sequence as S
import Data.Foldable (toList)

cellsColorRepresentation:: GameOfLife -> [Float]
cellsColorRepresentation gm@GameOfLife{cellBuffer = pcb} = 
    concat $ V.toList $ join $ matrixMap (\cell -> colorsArray cell) pcb


cellBoardInOpengl :: (Int, Int) -> GameOfLife -> ([Int], [Float])
cellBoardInOpengl (width, height) gm@GameOfLife{cellBuffer = pcb, numOfCells = (numOfXCells, numOfYCells)} = 
    (concat $ toList calculatedIndexArray, concat $ toList calculatedVertexArray) 
        where
            (floatScreenWidth, floatScreenHeight) = (int2Float width, int2Float height)
            (cellWidth, cellHeight) =  (floatScreenWidth/ (int2Float numOfXCells), floatScreenHeight/ (int2Float numOfYCells))
            (minSize, maxSize) = (min cellWidth cellHeight, max cellWidth cellHeight)
            
            -- You can use this aspect ratio to keep cells in the same size
            aspectRatio = minSize / maxSize

            (calculatedIndexArray, calculatedVertexArray, _) = V.ifoldl' (\acc ix row -> 
                                V.ifoldl' (\(accI, accV, accumIndex) iy cell -> 
                                            let (x,y) = cellPosition cell in
                                            let ia = indexArray cell in

                                            -- * 4 because 4 corners
                                            -- We could optimize this as "single mesh" but we accumulate every cell as single object
                                            let resultsAccumIndexA = map (+ (accumIndex * 4)) ia in 

                                            let (ixf, iyf) = (int2Float ix, int2Float iy) in
                                            -- Here we calculate cell width and height, based on indices and screen size
                                            let resultsAccumVertexA = [
                                                        ixf * cellWidth , iyf * cellHeight,
                                                        (ixf + 1) * cellWidth, iyf * cellHeight,
                                                        ixf * cellWidth, (iyf + 1) * cellHeight,
                                                        (ixf + 1) * cellWidth, (iyf + 1) * cellHeight
                                                    ] in
                                                (accI S.|> resultsAccumIndexA , accV S.|> resultsAccumVertexA, accumIndex + 1)
                                        ) acc row 
                                    
                            -- Accumulators are sequences, because appending to end of a sequence is O(1)
                            ) (S.empty, S.empty, 0) pcb


-- We can have less cells than the board size, we need to map mouse position to board positions
mousePositionRelativeToBoard :: (Int, Int) -> (Int, Int) -> GameOfLife -> Maybe (Int, Int)
mousePositionRelativeToBoard (screenWidth, screenHeight) (mouseX, mouseY) GameOfLife { numOfCells = (numOfCellsX, numOfCellsY )} = 
    if resultX > numOfCellsX || resultY > numOfCellsY || resultX < 0 || resultY < 0 then Nothing else Just (resultX, resultY)
        where 
            (cellWidth, cellHeight) =  (screenWidth `div` numOfCellsX, screenHeight `div` numOfCellsY)
            -- (minSize, maxSize) = (min cellWidth cellHeight, max cellWidth cellHeight)
            (resultX, resultY) = (mouseX `div` cellWidth, mouseY `div` cellHeight)