module Logic.CSP (generateSolutionSteps) where

import Common (GameState(..), Board)
-- import Logic (isValidMove)
import Data.List (sortBy, intersect, (\\))
import Data.Maybe (catMaybes)
import System.Random (randomRIO)

-- Encuentra las celdas vacías en el tablero
findEmptyCells :: Board -> [(Int, Int)]
findEmptyCells board =
  [(r, c) | r <- [0..8], c <- [0..8], board !! r !! c == Nothing]

-- Calcula los valores válidos para una celda específica (intersección fila, columna, bloque)
validValues :: Board -> (Int, Int) -> [Int]
validValues board (row, col) =
  let allValues = [1..9]
      rowValues = getRow row board
      colValues = getCol col board
      blockValues = getBox (row, col) board
  in allValues \\ (rowValues ++ colValues ++ blockValues)

-- Heurística para encontrar la celda más prometedora
findBestCell :: Board -> Maybe ((Int, Int), [Int])
findBestCell board =
  let emptyCells = findEmptyCells board
      cellDomains = [(cell, validValues board cell) | cell <- emptyCells]
      -- Ordenar celdas por tamaño de dominio (MRV), filas y columnas más completas
      rankedCells = sortBy (compareCells board) cellDomains
  in case rankedCells of
       [] -> Nothing
       (cell, domain):_ -> Just (cell, domain)

-- Comparador heurístico: Intersección de filas, columnas y bloques
compareCells :: Board -> ((Int, Int), [Int]) -> ((Int, Int), [Int]) -> Ordering
compareCells board ((r1, c1), d1) ((r2, c2), d2) =
  let score1 = completionScore board r1 c1
      score2 = completionScore board r2 c2
  in compare score2 score1 <> compare (length d1) (length d2)

-- Puntuación por intersección de fila, columna y bloque
completionScore :: Board -> Int -> Int -> Int
completionScore board row col =
  let rowScore = length (getRow row board)
      colScore = length (getCol col board)
      blockScore = length (getBox (row, col) board)
  in rowScore + colScore + blockScore

-- Genera pasos válidos para resolver el Sudoku
generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
generateSolutionSteps initialState = solve (board initialState) []
  where
    solve :: Board -> [(Int, Int, Int)] -> IO [(Int, Int, Int)]
    solve board steps =
      case findBestCell board of
        Nothing -> return steps -- Tablero completo o sin solución
        Just ((row, col), possibleValues) -> do
          shuffledValues <- shuffle possibleValues -- Mezcla los valores posibles
          let validSteps = [(row, col, val) | val <- shuffledValues, isValidMove board val (row, col)]
          case validSteps of
            [] -> return steps -- No se encontraron movimientos válidos
            (step:_) -> do
              let (r, c, v) = step
              let newBoard = updateBoard board (r, c) (Just v)
              solve newBoard (steps ++ [step])


-- Actualiza el valor en una celda específica del tablero
updateBoard :: Board -> (Int, Int) -> Maybe Int -> Board
updateBoard board (row, col) val =
  take row board ++
  [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board

-- Mezcla una lista de valores aleatoriamente
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (before, x:after) = splitAt i xs
  rest <- shuffle (before ++ after)
  return (x : rest)

-- Helpers para obtener filas, columnas y bloques
getRow :: Int -> Board -> [Int]
getRow row board = catMaybes (board !! row)

getCol :: Int -> Board -> [Int]
getCol col board = catMaybes [board !! r !! col | r <- [0..8]]

getBox :: (Int, Int) -> Board -> [Int]
getBox (row, col) board =
  let boxRow = (row `div` 3) * 3
      boxCol = (col `div` 3) * 3
  in catMaybes [board !! r !! c | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]

-- Verifica si un número puede colocarse en una celda
isValidMove :: Board -> Int -> (Int, Int) -> Bool
isValidMove board val (row, col) =
  let rowValues = getRow row board
      colValues = getCol col board
      blockValues = getBox (row, col) board
  in val `notElem` (rowValues ++ colValues ++ blockValues)





