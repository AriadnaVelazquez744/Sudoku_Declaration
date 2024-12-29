module Graphics.Board where

import Graphics.Gloss
import Common

-- Renderiza el tablero de Sudoku.
drawBoard :: Board -> Board -> (Int, Int) -> Picture
drawBoard currentBoard initialBoard selectedCell = pictures
  [ drawCell x y val (cellStyle (x, y) selectedCell initialBoard)
  | (y, row) <- zip [0..] currentBoard
  , (x, val) <- zip [0..] row
  ]

-- Dibuja una casilla individual con su estilo.
drawCell :: Int -> Int -> Maybe Int -> Color -> Picture
drawCell x y val cellColor =
  translate (fromIntegral x * cellSize) (fromIntegral (-y) * cellSize) $
  pictures
    [ color cellColor $ rectangleSolid cellSize cellSize
    , color black $ rectangleWire cellSize cellSize
    , drawValue val
    ]

-- Dibuja el valor de una casilla (número).
drawValue :: Maybe Int -> Picture
drawValue Nothing = blank
drawValue (Just n) =
  translate (-10) (-10) $
  scale 0.2 0.2 $
  text (show n)

-- Estilo dinámico de las casillas.
cellStyle :: (Int, Int) -> (Int, Int) -> Board -> Color
cellStyle (x, y) (selX, selY) initialBoard
  | (x, y) == (selX, selY) = greyN 0.8           -- Gris claro para la casilla seleccionada.
  | initialBoard !! y !! x /= Nothing = greyN 0.5 -- Gris oscuro para las celdas iniciales.
  | otherwise = white                            -- Blanco para celdas vacías.
