module Drawable where 
import Cells

class OpenGLDrawable a where
    vertexArray:: a -> [Float]
    colorsArray:: a -> [Float]
    indexArray:: a -> [Int]

instance OpenGLDrawable GameOfLifeCell where
    vertexArray _ = [
            0.0 , 0.0 ,
            0.0 , 1.0 ,
            1.0 , 0.0 ,
            1.0 , 1.0 
        ]  
    colorsArray (DeadCell _) = [
            1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,
            1.0, 1.0, 1.0,
            1.0, 1.0, 1.0
        ]
    colorsArray (AliveCell _) = [
            0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,
            0.0, 0.0, 0.0
        ]
    colorsArray (AboutToDieCell _) = [
            0.8, 0.5, 0.5,
            0.8, 0.5, 0.5,
            0.8, 0.5, 0.5,
            0.8, 0.5, 0.5
        ]
    colorsArray (AboutToRessurect _) = [
            0.5, 0.8, 0.5,
            0.5, 0.8, 0.5,
            0.5, 0.8, 0.5,
            0.5, 0.8, 0.5
        ]
    indexArray _ = [
            0,1,2,
            1,2,3
        ]
