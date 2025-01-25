module Logic.Logic where

import System.Random
import Data.List (transpose)
import Control.Monad
import Data.Maybe (fromJust)

type Board = [[Maybe Int]]

-- Función para imprimir el tablero de Sudoku
printBoard :: Board -> IO ()
printBoard board = do
    let line = replicate 21 '-'
    putStrLn line
    forM_ board $ \row -> do
        putStr "|"
        forM_ row $ \cell ->
            case cell of
                Just n -> putStr $ show n ++ " "
                Nothing -> putStr "  "
        putStrLn "|"
    putStrLn line


-- Función para verificar si un número es válido en una posición dada
isValid :: Board -> Int -> Int -> Int -> Bool
isValid board row col num =
  not (
    any (\n -> board !! row !! n == Just num) [0..8] || -- Verificar fila -- Verificar fila
     -- Verificar fila
    any (\n -> board !! n !! col == Just num) [0..8] || -- Verificar columna -- Verificar columna
    any (\r -> any (\c -> board !! ((row `div` 3) * 3 + r) !! ((col `div` 3) * 3 + c) == Just num) [0..2]) [0..2]
  )


-- Función para encontrar la siguiente celda vacía
findEmpty :: Board -> Maybe (Int, Int)
findEmpty board =
  foldr (\(r, row) acc ->
    case acc of
       Just _ -> acc -- Si ya encontró una celda vacóa
       Nothing ->
           foldr (\(c, cell) acc' ->
               case acc' of
                   Just _ -> acc' -- Si ya encontró una celda vacía
                   Nothing -> if cell == Nothing then Just (r,c) else Nothing
              ) Nothing (zip [0..8] row)
   ) Nothing (zip [0..8] board)


-- Función principal de resolución usando backtracking
solveSudoku :: Board -> IO (Maybe Board)
solveSudoku board = do
  case findEmpty board of
    Nothing -> return (Just board)  -- El tablero está resuelto
    Just (row, col) -> do
        nums <- shuffle [1..9]  -- Números a probar en orden aleatorio
        foldM (\acc num ->
               case acc of
                  Just result -> return $ Just result
                  Nothing ->
                      if isValid board row col num
                          then do
                              solveSudoku $ replaceCell board row col (Just num)
                          else return Nothing
           ) Nothing nums


-- Función para insertar el elemento en el tablero
replaceCell :: Board -> Int -> Int -> Maybe Int -> Board
replaceCell board row col value =
  let (beforeRow, currentRow:afterRows) = splitAt row board
      (beforeCell, _ : afterCells) = splitAt col currentRow
  in beforeRow ++ [beforeCell ++ [value] ++ afterCells] ++ afterRows


-- Función para crear un tablero vacio
emptyBoard :: Board
emptyBoard = replicate 9 (replicate 9 Nothing)


-- Función para mezclar una lista de forma aleatoria
shuffle :: [a] -> IO [a]
shuffle xs = do
  g <- newStdGen
  return $ shuffle' g xs

shuffle' :: StdGen -> [a] -> [a]
shuffle' _ [] = []
shuffle' g xs =
  let (i, g') = randomR (0, length xs - 1) g
      (left, x:right) = splitAt i xs
  in x : shuffle' g' (left ++ right)


-- Función para generar un tablero de Sudoku aleatorio
generateRandomSudoku :: IO (Maybe Board)
generateRandomSudoku = do
  solveSudoku emptyBoard

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

--Función para eliminar elementos del sudoku según la plantilla elegida
replaceWithZeros :: Board -> [[Bool]] -> Board
replaceWithZeros = zipWith (zipWith replace)
  where
    replace :: Maybe Int -> Bool -> Maybe Int
    -- replace (Just _) False = Just 0
    replace (Just _) False = Nothing
    replace x _ = x

--Función para elegir una plantilla al azar
generate :: IO [[Bool]]
generate = do
    let plantillas = [plantilla_1, plantilla_2, plantilla_3, plantilla_4, plantilla_5]
    i <- randomRIO (0, length plantillas - 1)
    return (plantillas !! i)

main1 :: IO Board
main1 = do
  solution <- generateRandomSudoku
  plantilla <- generate  

  case solution of
      Just board -> do
        let puzzle = replaceWithZeros board plantilla
        return puzzle
