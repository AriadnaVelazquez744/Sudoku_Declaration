module Main where

import Graphics.Gloss
import Views.Renderer (render)
import Common.Types
import Events.Events (handleEvent)
import Logic.GameLogic (generateNewBoard, isBoardComplete, isWinCondition, isValidMove)

-- Configuración de la ventana principal
window :: Display
window = InWindow "Sudoku" (1670, 850) (100, 100)

lighterLila = makeColorI 245 235 255 255 -- Tonalidad más clara de violeta

backgroundColor :: Color
backgroundColor = lighterLila

fps :: Int
fps = 60

-- Actualización del estado (para este juego, no hacemos nada en cada frame).
update :: Float -> GameState -> GameState
update _ state = state

main :: IO ()
main = do
    -- Cargar imágenes del juego
    startTitle <- loadBMP "./Img/title.bmp"
    winTitle <- loadBMP "./Img/win.bmp"
    schoolLogo <- loadBMP "./Img/matcom.bmp"
    sudokuElements <- loadBMP "./Img/elements.bmp"
    sudokuBoard <- generateNewBoard

    let initialState = GameState
            { screen = StartScreen  -- Pantalla inicial para pruebas
            , board = sudokuBoard
            , initialBoard = sudokuBoard
            , selectedCell = (0, 0)
            , message = ""
            , isWin = False
            , images = 
                [ ("startTitle", startTitle)
                , ("winTitle", winTitle)
                , ("schoolLogo", schoolLogo)
                , ("sudokuElements", sudokuElements)
                ]
            , autoSolveRunning = False
            }

    -- Ejecutar el juego
    play
        window
        backgroundColor
        fps
        initialState
        render
        handleEvent
        update
