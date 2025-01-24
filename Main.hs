module Main where

import Graphics.Gloss
import Views.Renderer (render)
import Common
import Events.Events (handleEvent)
-- import Logic.Logic (generateBoard)

-- Configuración de la ventana principal
window :: Display
window = InWindow "Sudoku" (1670, 850) (100, 100)

lighterLila = makeColorI 245 235 255 255 -- Tonalidad más clara de violeta

backgroundColor :: Color
backgroundColor = lighterLila

fps :: Int
fps = 60

-- Estado inicial del juego
initialState :: GameState
initialState = GameState
    {  
        screen = StartScreen
        , board = 
            [ 
            [Just 5, Nothing, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Just 3]
            , [Nothing, Nothing, Nothing, Just 1, Nothing, Just 9, Nothing, Nothing, Nothing]
            , [Nothing, Just 8, Nothing, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing]
            , [Just 1, Nothing, Just 7, Nothing, Nothing, Nothing, Just 4, Nothing, Nothing]
            , [Nothing, Just 2, Nothing, Nothing, Just 5, Nothing, Nothing, Just 8, Nothing]
            , [Nothing, Nothing, Just 6, Nothing, Nothing, Nothing, Just 9, Nothing, Just 1]
            , [Nothing, Just 3, Nothing, Nothing, Nothing, Nothing, Nothing, Just 7, Nothing]
            , [Nothing, Nothing, Nothing, Just 6, Nothing, Just 2, Nothing, Nothing, Nothing]
            , [Just 4, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing, Nothing, Just 9]
            ] 
        , initialBoard = 
            [ 
            [Just 5, Nothing, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Just 3]
            , [Nothing, Nothing, Nothing, Just 1, Nothing, Just 9, Nothing, Nothing, Nothing]
            , [Nothing, Just 8, Nothing, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing]
            , [Just 1, Nothing, Just 7, Nothing, Nothing, Nothing, Just 4, Nothing, Nothing]
            , [Nothing, Just 2, Nothing, Nothing, Just 5, Nothing, Nothing, Just 8, Nothing]
            , [Nothing, Nothing, Just 6, Nothing, Nothing, Nothing, Just 9, Nothing, Just 1]
            , [Nothing, Just 3, Nothing, Nothing, Nothing, Nothing, Nothing, Just 7, Nothing]
            , [Nothing, Nothing, Nothing, Just 6, Nothing, Just 2, Nothing, Nothing, Nothing]
            , [Just 4, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing, Nothing, Just 9]
            ]
        , selectedCell = (0, 0)
        , message = ""
        , isWin = False
        , images = []
        , autoSolveRunning = False
    }

-- Actualización del estado (para este juego, no hacemos nada en cada frame).
update :: Float -> GameState -> GameState
update _ state = state

main :: IO ()
main = do
    -- Cargar imágenes del juego
    startTitle <- loadBMP "./title.bmp"
    winTitle <- loadBMP "./win.bmp"
    schoolLogo <- loadBMP "./matcom.bmp"
    sudokuElements <- loadBMP "./elements.bmp"

    -- Crear el estado inicial con imágenes cargadas
    let initialStateWithImages = initialState
            { images = 
                [ ("startTitle", startTitle)
                , ("winTitle", winTitle)
                , ("schoolLogo", schoolLogo)
                , ("sudokuElements", sudokuElements)
                ]
            }

    -- Ejecutar el juego
    play
        window
        backgroundColor
        fps
        initialStateWithImages
        render
        handleEvent
        update
