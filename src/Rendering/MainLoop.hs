{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering.MainLoop where

import Control.Monad
import Control.Monad.State
import Foreign.C
import Data.Word (Word32, Word8)
import FRP.Yampa 
import qualified SDL
import qualified SDL.Vect as SDL
import GHC.Float (int2Double, double2Float, int2Float)
import Control.Arrow (arr)
import Data.IORef (newIORef)
import qualified GHC.Event.Windows.Clock as SDL
import Control.Concurrent
import FRP.Yampa (identity)
import Board (defaultGame, GameOfLife (cellBuffer, numOfCells, GameOfLife), calculateNextFrame)
import Rendering.BindGraphics (initSDL, initProgramWithShaders, bindValuesToOpenGL, draw)
import Rendering (cellBoardInOpengl, cellsColorRepresentation, mousePositionRelativeToBoard)
import qualified Data.Vector.Storable as V
import qualified Data.Vector as VP
import qualified Graphics.Rendering.OpenGL as GL
import Data.Int (Int32)
import Control.Lens
import Cells
import Data.Maybe (isJust)
import Data.Tuple
import qualified SDL.Raw as SDLR
import Utils.Utils

screenSize@(screenWidth, screenHeight) = (600, 600)
(numOfCellsX, numOfCellsY) = (40, 40)

data RenderLoop a = 
    RenderLoop {  
          appState:: a
        , frames:: Word32
        , paused:: Bool
    } deriving (Show)

data AppInput = 
    AppInput {
          mouseLocation:: (Int, Int)
        , leftMouseButtonDown:: Bool
        , pausePressed:: Bool
} deriving Show 

newAppInput:: IO AppInput
newAppInput = do
    mapping <- SDL.getMouseButtons 
    loc <- SDL.getAbsoluteMouseLocation >>= \v -> return (fromEnum $ v ^. SDL._x, fromEnum $ v ^. SDL._y) 
    let spaceCode = SDL.Scancode SDLR.SDL_SCANCODE_SPACE
    wasSpacePressed <- SDL.getKeyboardState ?? spaceCode
    
    return AppInput {
    -- events = SDL.pollEvents,
    -- keyBoardState = SDL.getKeyboardState,
    -- mouseLocation = SDL.getRelativeMouseLocation >>= (\(SDL.V2 x y) -> return (x,y))
    leftMouseButtonDown = mapping SDL.ButtonLeft,
    mouseLocation = loc,
    pausePressed = wasSpacePressed
}

defaultRenderLoop:: a -> RenderLoop a
defaultRenderLoop appState = RenderLoop {
    appState = appState,
    frames = 0,
    paused = False 
}

yampaMain:: IO ()
yampaMain = do
    window <- initSDL screenSize 
    program <- initProgramWithShaders 
    g <- defaultGame (numOfCellsX, numOfCellsY)
    let renderLoop = defaultRenderLoop g

    let (indicesL, verticesL) = cellBoardInOpengl (fromEnum screenWidth, fromEnum screenHeight) g
    let colors = cellsColorRepresentation g
    bindValuesToOpenGL screenSize (V.fromList $ map toEnum indicesL, V.fromList verticesL, V.fromList colors) program

    globalTime <- newMVar =<< getSDLTime 
    reactimate
        (do 
            newAppInput
        )
        (inputSense globalTime )
        (output window program $ fromIntegral $ length indicesL)
        (mainArrows renderLoop)
    


inputSense:: MVar Double-> Bool -> IO (DTime, Maybe AppInput)
inputSense time someVal  = do 
    lastTime <- readMVar time
    currentTime <- getSDLTime 
    when ((currentTime - lastTime) < 1000/30) 
        (do 
            let sleepTime = 1000/30 - (currentTime - lastTime)
            threadDelay $ floor $ sleepTime * 1000)

    dt <- (currentTime -) <$> swapMVar time currentTime
    input <- newAppInput
    return (dt, Just input)


output :: SDL.Window -> GL.Program -> Int32 -> Bool -> RenderLoop GameOfLife -> IO Bool
output window program numOfIndices _ val = do

    GL.clearColor GL.$= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer]
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))
    location0 <- GL.uniformLocation program "u_viewPort"
    GL.uniform location0 GL.$= GL.Vector2 (realToFrac screenWidth :: GL.GLfloat) (realToFrac screenWidth :: GL.GLfloat)
    
    timeTicks <- SDL.ticks
    location1 <- GL.uniformLocation program "u_ticks"
    GL.uniform location1 GL.$= GL.Vector1 (timeTicks :: GL.GLuint)

    draw numOfIndices (V.fromList $ cellsColorRepresentation $ appState val)

    events <- SDL.pollEvents
    let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

    SDL.glSwapWindow window
    return quit 
    

mainArrows:: RenderLoop GameOfLife -> SF AppInput (RenderLoop GameOfLife) 
mainArrows renderLoop = loopPre renderLoop sf
    where 
        sf = proc (input, rLoop) -> do 
            let gamePaused = paused rLoop
            let previousGameOfLife = appState rLoop
            let frameNum = frames rLoop

            let pause = whenMaybe $ pausePressed input

            pauseDetected <- edgeJust -< pause
            let isTheGamePaused = if isEvent pauseDetected then not gamePaused else gamePaused

            calculatedGameOfLife <- arr calculateNextFrame -< previousGameOfLife

            nextGameOfLife <- drawUsingMouse -< (input, if isTheGamePaused then previousGameOfLife else calculatedGameOfLife)

            let nextRenderLoop = rLoop { frames = frameNum + 1, appState = nextGameOfLife,  paused = isTheGamePaused }

            returnA -< (nextRenderLoop, nextRenderLoop)


mouseDrawRelativeToBoard:: SF (AppInput,GameOfLife) (Event (Int, Int))
mouseDrawRelativeToBoard = proc (input, g) -> do
    (loc, isMouseDown) <- arr mouseLocation &&& arr leftMouseButtonDown -< input
    returnA -< maybeToEvent $ mousePositionRelativeToBoard (fromEnum screenWidth, fromEnum screenHeight) loc g  >>= toMaybe isMouseDown

drawUsingMouse:: SF (AppInput, GameOfLife) GameOfLife
drawUsingMouse = proc (input, g) -> do
    drawing <- mouseDrawRelativeToBoard -< (input,g)

    case drawing of 
        NoEvent -> returnA -< g
        Event mousePos -> writeFromMousePosition -< (mousePos,g)



writeFromMousePosition:: SF ((Int, Int), GameOfLife) GameOfLife
-- x,y are mouse positions relative to drawn board
writeFromMousePosition = proc ((x,y), g) -> do 
    let cb = cellBuffer g
    -- update the row
    let val = cb ^. ix x & ix y .~ AliveCell (x,y)
    -- then find the row and update the whole thing
    let newBoard = cb & ix x .~ val
    returnA -< g { cellBuffer = newBoard}