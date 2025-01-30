module Logic.Logic where

import System.Random (randomRIO)
import Data.List (nub)


type Board = [[Maybe Int]]
type Plantilla = [[Bool]]

-- Genera un tablero inicial válido de Sudoku
generateNewBoard :: IO Board
generateNewBoard = do
    let emptyBoard = replicate 9 (replicate 9 Nothing) -- Tablero vacío
    solvedBoard <- solveBoard emptyBoard               -- Llena el tablero usando Backtracking
    removeCells solvedBoard                        -- Elimina celdas para crear el desafío

-- Llena el tablero usando Backtracking
solveBoard :: Board -> IO Board
solveBoard board = solveHelper board 0 0

solveHelper :: Board -> Int -> Int -> IO Board
solveHelper board 9 _ = return board -- Si hemos llenado todas las filas, el tablero está completo
solveHelper board row col
    | col >= 9 = solveHelper board (row + 1) 0 -- Pasa a la siguiente fila
    | board !! row !! col /= Nothing = solveHelper board row (col + 1) -- Si la celda ya está llena, avanza
    | otherwise = do
        numbers <- shuffle [1..9] -- Mezcla los números para aleatoriedad
        tryNumbers board row col numbers
  where
    tryNumbers :: Board -> Int -> Int -> [Int] -> IO Board
    tryNumbers board _ _ [] = return board -- Si no hay más números que probar, regresa el tablero
    tryNumbers board row col (n:ns)
        | isValidMove board n (row, col) = do
            let newBoard = updateBoard board row col (Just n)
            result <- solveHelper newBoard row (col + 1)
            if isBoardComplete result then return result else tryNumbers board row col ns
        | otherwise = tryNumbers board row col ns

-- Modifica el tablero según una plantilla
removeCells :: Board -> IO Board
removeCells board = do
    -- Seleccionar una plantilla al azar
    plantilla <- selectRandomTemplate
    -- Aplicar la plantilla al tablero
    return (applyTemplate board plantilla)

-- Verifica si un movimiento es válido
isValidMove :: Board -> Int -> (Int, Int) -> Bool
isValidMove board val (row, col) =
    notElem val (getRow row board) &&
    notElem val (getColumn col board) &&
    notElem val (getSubGrid (row, col) board)

getRow :: Int -> Board -> [Int]
getRow row board = [v | Just v <- board !! row]

getColumn :: Int -> Board -> [Int]
getColumn col board = [v | Just v <- map (!! col) board]

getSubGrid :: (Int, Int) -> Board -> [Int]
getSubGrid (row, col) board =
    let startRow = (row `div` 3) * 3
        startCol = (col `div` 3) * 3
    in [v | r <- take 3 (drop startRow board), Just v <- take 3 (drop startCol r)]

-- Actualiza una celda en el tablero
updateBoard :: Board -> Int -> Int -> Maybe Int -> Board
updateBoard board row col val =
    take row board ++
    [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

-- Comprueba si el tablero está completo
isBoardComplete :: Board -> Bool
isBoardComplete board = all (all (/= Nothing)) board

-- Mezcla una lista (para aleatoriedad en Backtracking)
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    idx <- randomRIO (0, length xs - 1)
    let (before, x:after) = splitAt idx xs
    rest <- shuffle (before ++ after)
    return (x : rest)

-- Comprueba solución válida
isWinCondition :: Board -> Bool
isWinCondition board =
    isBoardComplete board && -- Verifica que el tablero esté lleno
    all isUnique (map (`getRow` board) [0..8]) && -- Todas las filas son únicas
    all isUnique (map (`getColumn` board) [0..8]) && -- Todas las columnas son únicas
    all isUnique [getSubGrid (x, y) board | x <- [0, 3, 6], y <- [0, 3, 6]] -- Todos los subcuadros son únicos

-- Verifica si una lista no tiene duplicados
isUnique :: [Int] -> Bool
isUnique xs = length xs == length (nub xs)

-- Plantillas disponibles
plantillas :: [Plantilla]
plantillas = [plantilla_1, plantilla_2, plantilla_3, plantilla_4, plantilla_5]

-- Selecciona una plantilla al azar
selectRandomTemplate :: IO Plantilla
selectRandomTemplate = do
    index <- randomRIO (0, length plantillas - 1)
    return (plantillas !! index)

-- Aplica la plantilla al tablero
applyTemplate :: Board -> Plantilla -> Board
applyTemplate board plantilla =
    zipWith (zipWith applyCell) board plantilla
  where
    applyCell :: Maybe Int -> Bool -> Maybe Int
    applyCell cell keep
        | keep      = cell   -- Si el valor en la plantilla es True, conserva la celda.
        | otherwise = Nothing -- Si es False, elimina el valor.


--Plantillas para generar diferentes tipos de sudoku
plantilla_1 = [[True, False, False, False, False, True, False, True, False],
               [True, True, False, True, False, False, True, True, False],
               [False, True, True, False, True, False, True, False, False],
               [True, False, False, True, False, True, False, True, False],
               [False, False, True, False, False, False, True, False, False],
               [False, True, False, True, False, True, False, False, True],
               [False, False, True, False, True, False, True, True, False],
               [False, True, True, False, False, True, False, True, True],
               [False, True, False, True, False, False, False, False, False]]

plantilla_2 = [[False, False, True, False, False, True, True, True, False],
               [True, True, False, True, False, True, False, True, False],
               [True, False, False, False, False, True, False, False, True],
               [True, True, True, False, False, False, False, True, False],
               [False, False, False, False, True, False, False, False, False],
               [False, True, False, False, False, False, True, True, True],
               [True, False, False, True, False, False, False, False, True],
               [False, True, False, True, False, True, False, True, True],
               [False, True, True, True, False, False, True, False, False]]   

plantilla_3 = [[True, False, False, False, False, True, True, False, True],
               [False, True, False, False, True, True, False, True, True],
               [False, True, False, True, False, False, False, False, False],
               [False, False, True, False, False, False, False, False, True],
               [True, False, False, False, True, False, True, False, False],
               [False, True, False, False, False, False, False, False, True],
               [False, True, False, True, False, False, False, False, False],
               [False, True, False, False, True, True, False, True, True],
               [True, False, False, False, False, True, True, False, True]]

plantilla_4 = [[False, True, False, False, True, True, True, False, False],
               [False, False, False, True, False, False, True, False, True],
               [True, True, False, False, False, False, False, False, False],
               [True, False, False, True, False, True, False, True, False],
               [True, False, False, False, True, False, False, False, True],
               [False, True, False, True, False, True, False, False, True],
               [False, False, False, False, False, False, False, True, True],
               [True, False, True, False, False, True, False, False, False],
               [False, False, True, True, True, False, False, True, False]]

plantilla_5 = [[True, True, False, False, True, True, False, False, True],
               [False, False, False, True, True, False, True, False, True],
               [False, True, False, False, False, True, False, False, False],
               [True, False, True, True, False, True, False, True, False],
               [True, True, False, False, False, False, False, True, True],
               [False, True, False, True, False, True, True, False, True],
               [False, False, False, True, False, False, False, True, False],
               [True, False, True, False, True, True, False, False, False],
               [True, False, False, True, True, False, False, True, True]]

--Función para elegir una plantilla al azar
generate :: IO [[Bool]]
generate = do
    let plantillas = [plantilla_1, plantilla_2, plantilla_3, plantilla_4, plantilla_5]
    i <- randomRIO (0, length plantillas - 1)
    return (plantillas !! i)

