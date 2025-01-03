module EventHelpers where

import Graphics.Gloss.Interface.Pure.Game
import Common

handleBoardClick :: Float -> Float -> GameState -> GameState
handleBoardClick mx my state =
    let (x, y) = screenToCell mx my
    in if x >= 0 && x < 9 && y >= 0 && y < 9
        then state { selectedCell = (x, y) }
        else state

moveSelection :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveSelection (dx, dy) (x, y) =
    (min 8 (max 0 (x + dx)), min 8 (max 0 (y + dy)))

insertValue :: Int -> GameState -> GameState
insertValue val state =
    let (x, y) = selectedCell state
        initial = initialBoard state !! y !! x
    in if initial == Nothing
        then state { board = updateBoard (board state) x y (Just val) }
        else state { message = "No puedes modificar esta casilla." }

screenToCell :: Float -> Float -> (Int, Int)
screenToCell mx my = (floor (mx / cellSize), floor (-my / cellSize))

isButtonClicked :: Float -> Float -> Float -> Float -> Bool
isButtonClicked mx my bx by =
    mx >= bx - 100 && mx <= bx + 100 && my >= by - 25 && my <= by + 25

resetGame :: GameState -> GameState
resetGame state = state { board = initialBoard state, message = "" }

updateBoard :: Board -> Int -> Int -> Maybe Int -> Board
updateBoard b x y val =
    take y b ++ [take x (b !! y) ++ [val] ++ drop (x + 1) (b !! y)] ++ drop (y + 1) b


-- Detecta si un clic ocurre dentro del área del tablero
isCellClicked :: Float -> Float -> Bool
isCellClicked mx my =
    let x = floor ((mx - boardOffsetX) / cellSize - 1)
        y = floor ((boardOffsetY - my) / cellSize )
    in x >= 0 && x < 9 && y >= 0 && y < 9
    where
    boardWidth = 9 * cellSize
    boardHeight = 9 * cellSize
    boardOffsetX = -boardWidth / 2
    boardOffsetY = boardHeight / 2



-- Actualiza el estado seleccionando una celda con base en las coordenadas del clic
selectCell :: Float -> Float -> GameState -> GameState
selectCell mx my state =
    let x = floor ((mx - boardOffsetX) / cellSize - 1) 
        y = floor ((boardOffsetY - my) / cellSize ) 
    in state { selectedCell = (x, y) }
    where
    boardWidth = 9 * cellSize
    boardHeight = 9 * cellSize
    boardOffsetX = -boardWidth / 2
    boardOffsetY = boardHeight / 2

deleteValue :: GameState -> GameState
deleteValue state =
    let (x, y) = selectedCell state
        initial = initialBoard state !! y !! x
        current = board state !! y !! x
    in if initial == Nothing && current /= Nothing -- Solo elimina si no es parte del tablero inicial y hay un valor
            then state { board = updateBoard (board state) x y Nothing }
            else state { message = "No puedes modificar esta casilla." }




