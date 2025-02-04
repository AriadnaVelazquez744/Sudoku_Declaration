module Events.EventHelpers where

import Graphics.Gloss.Interface.Pure.Game
import Common.Types
import Logic.GameLogic (generateNewBoard, isBoardComplete, isWinCondition, isValidMove)
import System.IO.Unsafe (unsafePerformIO)

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
        currentBoard = board state
    in if initial /= Nothing
        then state { message = "You cannot modify this cell." } -- Celda protegida
        else
            let newBoard = updateBoard currentBoard x y (Just val)
            in if isValidCurrentState newBoard (x, y)
                then state { board = newBoard, message = "" } -- Movimiento válido, limpia mensaje
                else state { message = "Invalid Move: Number " ++ show val ++ " already in column, row or block." } -- Movimiento inválido

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
            else state { message = "You cannot modify this cell." }

generateNewGame :: GameState -> IO GameState
generateNewGame state = do
    newBoard <- generateNewBoard
    return state
        { board = newBoard
        , initialBoard = newBoard
        , message = "New board generated"
        }

isValidCurrentState :: Board -> (Int, Int) -> Bool
isValidCurrentState board (x, y) =
    case board !! y !! x of
        Nothing -> True -- Si la celda está vacía, es válida
        Just val ->
            let -- Filtra los valores de la fila y la columna excluyendo la celda actual
                rowValues = [v | (col, Just v) <- zip [0..] (board !! y), col /= x]
                colValues = [v | (row, Just v) <- zip [0..] (map (!! x) board), row /= y]
                -- Filtra los valores dentro del bloque 3x3 correspondiente
                blockValues = [v | (bx, by) <- getBlockCells (x, y), Just v <- [board !! by !! bx], (bx, by) /= (x, y)]
            in notElem val rowValues && notElem val colValues && notElem val blockValues

-- Función auxiliar para obtener todas las celdas del bloque 3x3 al que pertenece (x, y)
getBlockCells :: (Int, Int) -> [(Int, Int)]
getBlockCells (x, y) =
    let blockStartX = (x `div` 3) * 3
        blockStartY = (y `div` 3) * 3
    in [(bx, by) | bx <- [blockStartX .. blockStartX + 2], by <- [blockStartY .. blockStartY + 2]]
