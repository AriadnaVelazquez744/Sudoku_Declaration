module Views.Board where

import Graphics.Gloss
import Common

-- Usa esta función junto al tablero.
drawBoardWithGrid :: Board -> Board -> (Int, Int) -> Float -> Float -> Float -> Picture
drawBoardWithGrid currentBoard initialBoard selectedCell offsetX offsetY cellSize =
  pictures
    [ 
        drawBoard currentBoard initialBoard selectedCell,
        drawGrid offsetX offsetY cellSize
    ]

drawBoard :: Board -> Board -> (Int, Int) -> Picture
drawBoard currentBoard initialBoard selectedCell =
  let boardOffsetX = -3 * cellSize  -- Centrar en X
      boardOffsetY = 3.92 * cellSize  -- Centrar en Y
  in translate boardOffsetX boardOffsetY $ pictures
      [ drawCell x y val (cellStyle (x, y) selectedCell initialBoard)
      | (y, row) <- zip [0..] currentBoard
      , (x, val) <- zip [0..] row
      ]

-- Dibuja una casilla individual con su estilo.
drawCell :: Int -> Int -> Maybe Int -> Color -> Picture
drawCell x y val cellColor =
    translate (fromIntegral x * cellSize) (fromIntegral (-y) * cellSize) $
    pictures
      [ 
        color cellColor $ rectangleSolid cellSize cellSize, 
        color black $ rectangleWire cellSize cellSize, 
        drawValue val
      ]

-- Dibuja el valor de una casilla (número).
drawValue :: Maybe Int -> Picture
drawValue Nothing = blank
drawValue (Just n) =
    let intenseBlack = makeColor 0.0 0.0 0.0 1.0 -- Color negro más intenso.
        thickness = 0.9                          -- Grosor simulado.
        baseText = scale 0.4 0.4 $ text (show n) -- Texto básico escalado.
    in pictures
        [ translate (-15 + dx) (-23 + dy) $ color intenseBlack baseText
        | dx <- [-thickness, 0, thickness]
        , dy <- [-thickness, 0, thickness]
        ]

lightLila = makeColorI 230 210 255 255

-- Estilo dinámico de las casillas.
cellStyle :: (Int, Int) -> (Int, Int) -> Board -> Color
cellStyle (x, y) (selX, selY) initialBoard
    | (x, y) == (selX, selY) && initialBoard !! y !! x == Nothing = lightLila -- Gris claro si es seleccionada y no es inicial.
    | initialBoard !! y !! x /= Nothing = greyN 0.85                           -- Gris oscuro para las celdas iniciales.
    | otherwise = white                                                       -- Blanco para celdas vacías.


-- Ajusta las líneas para las cuadrículas 3x3 del tablero.
drawGrid :: Float -> Float -> Float -> Picture
drawGrid offsetX offsetY cellSize =
  let lineThickness = 3.0 -- Grosor de las líneas
      gridColor = makeColorI 0 0 0 255 -- Color negro intenso
      -- Líneas verticales (ajuste en Y)
      verticalLines = 
        [ translate (offsetX + fromIntegral x * cellSize * 3) (offsetY - cellSize * 7.48) $
            color gridColor $ rectangleSolid lineThickness (cellSize * 9)
        | x <- [1, 2, 3, 4]
        ]
      -- Líneas horizontales (ajuste en X)
      horizontalLines = 
        [ translate (offsetX - cellSize * (-7.5)) (offsetY - fromIntegral y * cellSize * 3) $
            color gridColor $ rectangleSolid (cellSize * 9) lineThickness
        | y <- [1, 2, 3, 4]
        ]
  in pictures (verticalLines ++ horizontalLines)
