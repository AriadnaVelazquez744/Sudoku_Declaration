-- module Main where

-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.Pure.Game
-- import Common
-- import Graphics.Renderer (render)
-- import Events (handleEvent)
-- import Logic (generateBoard)

-- -- Configuración de la ventana principal
-- window :: Display
-- window = InWindow "Sudoku" (1670, 850) (100, 100)

--lighterLila = makeColorI 245 235 255 255 -- Tonalidad más clara de violeta

-- backgroundColor :: Color
-- backgroundColor = white

-- fps :: Int
-- fps = 60

-- -- Estado inicial del juego
-- initialState :: GameState
-- initialState = GameState
-- {  
--     screen = StartScreen
--     , board = replicate 9 (replicate 9 Nothing)  -- Tablero vacío al inicio.
--     , initialBoard = replicate 9 (replicate 9 Nothing) -- Tablero inicial vacío.
--     , selectedCell = (0, 0) -- Celda seleccionada inicial.
--     , message = ""          -- Sin mensaje al inicio.
--     , isWin = False         -- Juego no ganado al inicio.
--     , images = [("startTitle", startTitle)]
-- }

-- -- Actualización del estado (para este juego, no hacemos nada en cada frame).
-- update :: Float -> GameState -> GameState
-- update _ state = state

-- -- Función principal
-- main :: IO ()
-- main = do
--     -- Cargar imágenes del juego
--     startTitle <- loadBMP "./title.bmp"
--     -- Genera un tablero inicial válido (debe implementarse en Logic/Board.hs).
--     initialBoard <- generateBoard
--     let stateWithBoard = initialState { board = initialBoard, initialBoard = initialBoard }
--     play 
--         window 
--         backgroundColor 
--         fps 
--         stateWithBoard 
--         render 
--         handleEvent 
--         update
