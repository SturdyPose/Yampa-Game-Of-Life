{-# LANGUAGE ScopedTypeVariables #-}
module Board where
import qualified Data.Vector as V
import Cells 
import System.Random (getStdGen, Random (randoms, randomRs))
import Data.List.Split
import Data.IORef
import Control.Monad.State 

type Matrix a = V.Vector (V.Vector a)

matrixMap:: (a -> b) -> Matrix a -> Matrix b
matrixMap f = V.map (V.map f) 

data GameOfLife = GameOfLife {
    cellBuffer :: Matrix GameOfLifeCell
    , numOfCells:: (Int, Int)
} deriving (Show)

defaultGame:: (Int, Int) -> IO GameOfLife
defaultGame numOfCells@(x,y) = do 

    gen <- getStdGen

    let generatedBools = chunksOf y $ map (\(x:: Int) -> x < 5) $ take (x * y) $ randomRs (0,9) gen
    let generatedBoolsVector = V.fromList $ map V.fromList generatedBools

    let generatedCells = V.imap (\ix row -> V.imap
            (\iy b -> if b then AliveCell (ix, iy) else DeadCell (ix, iy)) row) generatedBoolsVector

    return GameOfLife {
        cellBuffer= generatedCells,
        numOfCells = numOfCells
    }


calculateNextFrame:: GameOfLife -> GameOfLife 
calculateNextFrame f@GameOfLife{ cellBuffer = pcb, numOfCells = nc } = nextGameOfLive
    where 
      nextBuffer = matrixMap (\cell ->

                    let cellNeighBours = neighbours nc (cellPosition cell) in
                    let (numOfNeighbourAliveCells, numOfNeighbourDeadCells) = numOfDeadAndAliveCells $ map (\(xPos,yPos) -> pcb V.! xPos V.! yPos) cellNeighBours in case cell of
                      AliveCell pos -> if numOfNeighbourAliveCells >= 2 && numOfNeighbourAliveCells <= 3 then AliveCell pos else AboutToDieCell pos
                      DeadCell pos -> if numOfNeighbourAliveCells == 3 then AboutToRessurect pos else DeadCell pos
                      AboutToDieCell pos -> if numOfNeighbourAliveCells == 3 then AliveCell pos else DeadCell pos 
                      AboutToRessurect pos -> if numOfNeighbourAliveCells >= 2 && numOfNeighbourAliveCells <= 3 then AliveCell pos else AboutToDieCell pos

                  ) pcb

      nextGameOfLive = f{cellBuffer= nextBuffer}