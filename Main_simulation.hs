module Main where

import Graphics.Gloss
import Graphics.Renderer (render)
import Common

-- Configuración de la ventana para pruebas gráficas
window :: Display
window = InWindow "Sudoku (Simulation)" (1670, 850) (100, 100)

lighterLila = makeColorI 245 235 255 255 -- Tonalidad más clara de violeta

backgroundColor :: Color
backgroundColor = lighterLila

fps :: Int
fps = 60

-- -- Prueba pantalla de juego
-- mockState :: GameState
-- mockState = GameState
--   { 
--     screen = GameScreen, -- Cambia para probar otras pantallas
--     board = 
--       [ 
--         [Just 5, Nothing, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Just 3]
--       , [Nothing, Nothing, Nothing, Just 1, Nothing, Just 9, Nothing, Nothing, Nothing]
--       , [Nothing, Just 8, Nothing, Nothing, Just 4, Nothing, Nothing, Just 6, Nothing] -- +4 en (2, 4)
--       , [Just 1, Nothing, Just 7, Nothing, Nothing, Nothing, Just 4, Nothing, Nothing]
--       , [Nothing, Just 2, Nothing, Just 3, Just 5, Nothing, Nothing, Just 8, Nothing] -- +3 en (4, 3)
--       , [Nothing, Nothing, Just 6, Nothing, Nothing, Just 7, Just 9, Nothing, Just 1] -- +7 en (5, 5)
--       , [Nothing, Just 3, Nothing, Nothing, Nothing, Nothing, Nothing, Just 7, Just 8] -- +8 en (6, 8)
--       , [Nothing, Nothing, Nothing, Just 6, Nothing, Just 2, Nothing, Nothing, Nothing]
--       , [Just 4, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing, Nothing, Just 9]
--       ], 
--     initialBoard = 
--       [ 
--         [Just 5, Nothing, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Just 3]
--       , [Nothing, Nothing, Nothing, Just 1, Nothing, Just 9, Nothing, Nothing, Nothing]
--       , [Nothing, Just 8, Nothing, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing]
--       , [Just 1, Nothing, Just 7, Nothing, Nothing, Nothing, Just 4, Nothing, Nothing]
--       , [Nothing, Just 2, Nothing, Nothing, Just 5, Nothing, Nothing, Just 8, Nothing]
--       , [Nothing, Nothing, Just 6, Nothing, Nothing, Nothing, Just 9, Nothing, Just 1]
--       , [Nothing, Just 3, Nothing, Nothing, Nothing, Nothing, Nothing, Just 7, Nothing]
--       , [Nothing, Nothing, Nothing, Just 6, Nothing, Just 2, Nothing, Nothing, Nothing]
--       , [Just 4, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing, Nothing, Just 9]
--       ], 
--     selectedCell = (0, 1), 
--     message = "Mensaje de Prueba", 
--     isWin = False,
--     images = []

--   }

-- main :: IO ()
-- main = play
--   window
--   backgroundColor
--   fps
--   mockState  -- Usa el estado simulado
--   render     -- Renderiza la pantalla
--   (\_ state -> state) -- No maneja eventos
--   (\_ state -> state) -- No actualiza el estado



-- -- Prueba pantalla de inicio
-- main :: IO ()
-- main = do
--     startTitle <- loadBMP "./title.bmp"

--     let initialState = GameState
--           { screen = StartScreen
--           , board = replicate 9 (replicate 9 Nothing)
--           , initialBoard = replicate 9 (replicate 9 Nothing)
--           , selectedCell = (0, 0)
--           , message = ""
--           , isWin = False
--           , images = [("startTitle", startTitle)]
--           }

--     play
--       window
--       backgroundColor
--       fps
--       initialState
--       render
--       (\_ state -> state)
--       (\_ state -> state)




-- -- Prueba pantalla de victoria
-- main :: IO ()
-- main = do
--     winTitle <- loadBMP "./win.bmp"

--     let initialState = GameState
--           { screen = EndScreen
--           , board = replicate 9 (replicate 9 Nothing)
--           , initialBoard = replicate 9 (replicate 9 Nothing)
--           , selectedCell = (0, 0)
--           , message = ""
--           , isWin = False
--           , images = [("winTitle", winTitle)]
--           }

--     play
--       window
--       backgroundColor
--       fps
--       initialState
--       render
--       (\_ state -> state)
--       (\_ state -> state)


-- -- Prueba pantalla de créditos
-- main :: IO ()
-- main = do
--     schoolLogo <- loadBMP "./matcom.bmp"

--     let initialState = GameState
--           { screen = CreditsScreen
--           , board = replicate 9 (replicate 9 Nothing)
--           , initialBoard = replicate 9 (replicate 9 Nothing)
--           , selectedCell = (0, 0)
--           , message = ""
--           , isWin = False
--           , images = [("schoolLogo", schoolLogo)]
--           }

--     play
--       window
--       backgroundColor
--       fps
--       initialState
--       render
--       (\_ state -> state)
--       (\_ state -> state)



-- Prueba pantalla de reglas
main :: IO ()
main = do
    sudokuElements <- loadBMP "./elements.bmp"

    let initialState = GameState
          { screen = RulesScreen
          , board = replicate 9 (replicate 9 Nothing)
          , initialBoard = replicate 9 (replicate 9 Nothing)
          , selectedCell = (0, 0)
          , message = ""
          , isWin = False
          , images = [("sudokuElements", sudokuElements)]
          }

    play
      window
      backgroundColor
      fps
      initialState
      render
      (\_ state -> state)
      (\_ state -> state)

