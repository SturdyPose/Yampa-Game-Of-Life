{-# LANGUAGE OverloadedStrings #-}

module Rendering.BindGraphics where

import Control.Monad
import Data.Foldable (toList)
import qualified Data.Vector.Storable as V
import Foreign
import GHC.Float (int2Float)
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import qualified Data.Text
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import Data.String (fromString)
import Linear (ortho)
import Foreign.C

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initSDL:: (CInt,CInt) -> IO SDL.Window 
initSDL (screenWidth, screenHeight) = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
  do
    renderQuality <- SDL.get SDL.HintRenderScaleQuality
    when (renderQuality /= SDL.ScaleLinear) $
      putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight,
          SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        }

  SDL.showWindow window
  _ <- SDL.glCreateContext window

  return window

bindValuesToOpenGL:: (CInt,CInt) -> (V.Vector GL.GLuint, V.Vector Float, V.Vector Float) -> GL.Program -> IO ()
bindValuesToOpenGL (screenWidth, screenHeight) (indices, vertices, vertexColors) prog = do
  let ort = (ortho 0 (int2Float $ fromEnum screenWidth) (int2Float $ fromEnum screenHeight) 0 (-1) 1) :: (SDL.M44 GL.GLfloat)
  let ort_list = (concat $ toList $ fmap toList ort) :: [GL.GLfloat]

  GL.attribLocation prog "position" GL.$= (GL.AttribLocation 0)
  GL.attribLocation prog "vertex_color" GL.$= (GL.AttribLocation 1)

  GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Enabled

  location2 <- GL.uniformLocation prog "u_MVP"
  pMatrix <- (GL.newMatrix GL.RowMajor ort_list) :: IO (GL.GLmatrix GL.GLfloat)
  GL.uniform location2 GL.$= pMatrix

  arrayBuffer <- GL.genObjectName

  let vertexBufferSize = (V.length vertices) * sizeOf (V.head vertices)
  V.unsafeWith
    vertices
    ( \pVertices -> do
        GL.bindBuffer GL.ArrayBuffer GL.$= Just arrayBuffer

        GL.bufferData GL.ArrayBuffer GL.$= (toEnum vertexBufferSize, pVertices, GL.StaticDraw)
        GL.vertexAttribPointer (GL.AttribLocation 0)
          GL.$= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 (bufferOffset 0))

        return id
    )

  indexBufferName <- GL.genObjectName
  let indexArraySize = (V.length indices) * sizeOf (V.head indices)
  V.unsafeWith indices $ \pIndices ->
    ( do
        GL.bindBuffer GL.ElementArrayBuffer GL.$= Just indexBufferName

        GL.bufferData GL.ElementArrayBuffer GL.$= (toEnum indexArraySize, pIndices, GL.StaticDraw)
        return id
    )


  colorBuffer <- GL.genObjectName
  let colorBufferSize = (V.length vertexColors) * sizeOf (V.head vertexColors)
  V.unsafeWith vertexColors (\pColors -> do
      GL.bindBuffer GL.ArrayBuffer GL.$= Just colorBuffer 

      GL.bufferData GL.ArrayBuffer GL.$= (toEnum colorBufferSize , pColors , GL.DynamicDraw)
      GL.vertexAttribPointer (GL.AttribLocation 1) GL.$=
        (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 (bufferOffset 0))
      
      return id 
    )

  return ()


initProgramWithShaders:: IO GL.Program
initProgramWithShaders = do
    -- compile vertex shader
    vs <- GL.createShader GL.VertexShader
    vertexShader <- readFile "./shaders/shader.vertex" 
    GL.shaderSourceBS vs GL.$= fromString vertexShader
    GL.compileShader vs
    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        er <- GL.get $ GL.shaderInfoLog vs
        mapM_ (putStrLn . show) $ Data.Text.split (== '\n') $ Data.Text.pack er
        exitFailure

    -- Do it again for the fragment shader
    fs <- GL.createShader GL.FragmentShader
    fragmentShader <- readFile "./shaders/shader.fragment"
    GL.shaderSourceBS fs GL.$= fromString fragmentShader 
    GL.compileShader fs
    fsOK <- GL.get $ GL.compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        er <- GL.get $ GL.shaderInfoLog fs
        mapM_ (putStrLn . show) $ Data.Text.split (== '\n') $ Data.Text.pack er
        exitFailure

    program <- GL.createProgram

    GL.attachShader program vs
    GL.attachShader program fs

    GL.linkProgram program
    linkOK <- GL.get $ GL.linkStatus program
    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless (linkOK && status) $ do
        hPutStrLn stderr "GL.linkProgram error"
        exitFailure
    GL.currentProgram GL.$= Just program

    return program


draw :: GL.NumArrayIndices -> V.Vector Float -> IO ()
draw numOfIndices vertexColors = do
    V.unsafeWith vertexColors (\pColors -> do
        let colorBufferSize = (V.length vertexColors) * sizeOf (V.head vertexColors)
        GL.bufferData GL.ArrayBuffer GL.$= (toEnum colorBufferSize , pColors , GL.DynamicDraw)
      )
    err <- GL.get GL.errors 
    when (length err > 0) $ print $ "ERROR IN DRAW: " ++ show err

    -- GL.drawArrays GL.Triangles 0 (fromIntegral $ V.length indices ) -- 3 is the number of vertices
    GL.drawElements GL.Triangles numOfIndices GL.UnsignedInt nullPtr