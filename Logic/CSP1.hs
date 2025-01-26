-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- -- import Logic (isValidMove)
-- import Data.List (sortBy, intersect, (\\))
-- import Data.Maybe (catMaybes)
-- import System.Random (randomRIO)

-- -- Encuentra las celdas vacías en el tablero
-- findEmptyCells :: Board -> [(Int, Int)]
-- findEmptyCells board =
--   [(r, c) | r <- [0..8], c <- [0..8], board !! r !! c == Nothing]

-- -- Calcula los valores válidos para una celda específica (intersección fila, columna, bloque)
-- validValues :: Board -> (Int, Int) -> [Int]
-- validValues board (row, col) =
--   let allValues = [1..9]
--       rowValues = getRow row board
--       colValues = getCol col board
--       blockValues = getBox (row, col) board
--   in allValues \\ (rowValues ++ colValues ++ blockValues)

-- -- Heurística para encontrar la celda más prometedora
-- findBestCell :: Board -> Maybe ((Int, Int), [Int])
-- findBestCell board =
--   let emptyCells = findEmptyCells board
--       cellDomains = [(cell, validValues board cell) | cell <- emptyCells]
--       -- Ordenar celdas por tamaño de dominio (MRV), filas y columnas más completas
--       rankedCells = sortBy (compareCells board) cellDomains
--   in case rankedCells of
--        [] -> Nothing
--        (cell, domain):_ -> Just (cell, domain)

-- -- Comparador heurístico: Intersección de filas, columnas y bloques
-- compareCells :: Board -> ((Int, Int), [Int]) -> ((Int, Int), [Int]) -> Ordering
-- compareCells board ((r1, c1), d1) ((r2, c2), d2) =
--   let score1 = completionScore board r1 c1
--       score2 = completionScore board r2 c2
--   in compare score2 score1 <> compare (length d1) (length d2)

-- -- Puntuación por intersección de fila, columna y bloque
-- completionScore :: Board -> Int -> Int -> Int
-- completionScore board row col =
--   let rowScore = length (getRow row board)
--       colScore = length (getCol col board)
--       blockScore = length (getBox (row, col) board)
--   in rowScore + colScore + blockScore

-- -- Genera pasos válidos para resolver el Sudoku
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps initialState = solve (board initialState) []
--   where
--     solve :: Board -> [(Int, Int, Int)] -> IO [(Int, Int, Int)]
--     solve board steps =
--       case findBestCell board of
--         Nothing -> return steps -- Tablero completo o sin solución
--         Just ((row, col), possibleValues) -> do
--           shuffledValues <- shuffle possibleValues -- Mezcla los valores posibles
--           let validSteps = [(row, col, val) | val <- shuffledValues, isValidMove board val (row, col)]
--           case validSteps of
--             [] -> return steps -- No se encontraron movimientos válidos
--             (step:_) -> do
--               let (r, c, v) = step
--               let newBoard = updateBoard board (r, c) (Just v)
--               solve newBoard (steps ++ [step])


-- -- Actualiza el valor en una celda específica del tablero
-- updateBoard :: Board -> (Int, Int) -> Maybe Int -> Board
-- updateBoard board (row, col) val =
--   take row board ++
--   [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
--   drop (row + 1) board

-- -- Mezcla una lista de valores aleatoriamente
-- shuffle :: [a] -> IO [a]
-- shuffle [] = return []
-- shuffle xs = do
--   i <- randomRIO (0, length xs - 1)
--   let (before, x:after) = splitAt i xs
--   rest <- shuffle (before ++ after)
--   return (x : rest)

-- -- Helpers para obtener filas, columnas y bloques
-- getRow :: Int -> Board -> [Int]
-- getRow row board = catMaybes (board !! row)

-- getCol :: Int -> Board -> [Int]
-- getCol col board = catMaybes [board !! r !! col | r <- [0..8]]

-- getBox :: (Int, Int) -> Board -> [Int]
-- getBox (row, col) board =
--   let boxRow = (row `div` 3) * 3
--       boxCol = (col `div` 3) * 3
--   in catMaybes [board !! r !! c | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]

-- -- Verifica si un número puede colocarse en una celda
-- isValidMove :: Board -> Int -> (Int, Int) -> Bool
-- isValidMove board val (row, col) =
--   let rowValues = getRow row board
--       colValues = getCol col board
--       blockValues = getBox (row, col) board
--   in val `notElem` (rowValues ++ colValues ++ blockValues)


-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.List (sortBy,  intersect, (\\))
-- import Data.Maybe (catMaybes)
-- import Data.Function (on)
-- import System.Random (randomRIO)

-- -- Encuentra las celdas vacías en el tablero
-- findEmptyCells :: Board -> [(Int, Int)]
-- findEmptyCells board =
--   [(r, c) | r <- [0..8], c <- [0..8], board !! r !! c == Nothing]

-- -- Calcula los valores válidos para una celda específica
-- validValues :: Board -> (Int, Int) -> [Int]
-- validValues board (row, col) =
--   let allValues = [1..9]
--       rowValues = getRow row board
--       colValues = getCol col board
--       blockValues = getBox (row, col) board
--   in allValues \\ (rowValues ++ colValues ++ blockValues)

-- -- Heurística para encontrar la celda más prometedora
-- findBestCell :: Board -> Maybe ((Int, Int), [Int])
-- findBestCell board =
--   let emptyCells = findEmptyCells board
--       cellDomains = [(cell, validValues board cell) | cell <- emptyCells]
--       filteredDomains = filter (not . null . snd) cellDomains
--       rankedCells = sortBy (\(_, d1) (_, d2) -> compare (length d1) (length d2)) filteredDomains
--   in case rankedCells of
--        [] -> Nothing
--        (cell, domain):_ -> Just (cell, domain)

-- -- Genera los pasos para resolver el Sudoku (siempre devuelve un IO)
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps state =
--   case solve (board state) of
--     Just steps -> return steps -- Resolución exitosa del tablero actual
--     Nothing -> case solve (initialBoard state) of
--                  Just steps -> return steps -- Resolver el tablero inicial
--                  Nothing -> error "El tablero inicial es inválido, esto no debería ocurrir."

-- -- Resolver un tablero usando backtracking
-- solve :: Board -> Maybe [(Int, Int, Int)]
-- solve board = backtrack board []

-- -- Backtracking para completar el tablero
-- backtrack :: Board -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- backtrack board steps =
--   case findBestCell board of
--     Nothing -> Just steps -- Tablero completo
--     Just ((row, col), possibleValues) ->
--       tryValues board (row, col) possibleValues steps

-- -- Intenta colocar valores en una celda específica
-- tryValues :: Board -> (Int, Int) -> [Int] -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- tryValues _ _ [] _ = Nothing -- No hay valores posibles, backtrack
-- tryValues board (row, col) (val:vals) steps =
--   let newBoard = updateBoard board (row, col) (Just val)
--   in case backtrack newBoard ((row, col, val) : steps) of
--        Just result -> Just result
--        Nothing -> tryValues board (row, col) vals steps

-- -- Actualiza el valor en una celda específica del tablero
-- updateBoard :: Board -> (Int, Int) -> Maybe Int -> Board
-- updateBoard board (row, col) val =
--   take row board ++
--   [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
--   drop (row + 1) board

-- -- Helpers para obtener filas, columnas y bloques
-- getRow :: Int -> Board -> [Int]
-- getRow row board = catMaybes (board !! row)

-- getCol :: Int -> Board -> [Int]
-- getCol col board = catMaybes [board !! r !! col | r <- [0..8]]

-- getBox :: (Int, Int) -> Board -> [Int]
-- getBox (row, col) board =
--   let boxRow = (row `div` 3) * 3
--       boxCol = (col `div` 3) * 3
--   in catMaybes [board !! r !! c | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]







-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.List (minimumBy, delete, sortBy, (\\))
-- import Data.Maybe (mapMaybe, isJust)
-- import Control.Monad (guard)
-- -- import Data.List (sortBy,  intersect, (\\))


-- import Debug.Trace

-- traceSolve :: Show a => String -> a -> a
-- traceSolve msg x = trace (msg ++ show x) x



-- type Position = (Int, Int)
-- type Domain = [Int]
-- type Constraint = (Position, Position)

-- -- Método principal
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps state = 
--   case solveWithCSP (board state) of
--     Just steps -> return steps
--     Nothing    -> case solveWithCSP (initialBoard state) of
--                     Just steps -> return steps
--                     Nothing    -> error "El tablero inicial es inválido, esto no debería ocurrir."

-- -- Solución basada en CSP con técnicas de optimización
-- solveWithCSP :: Board -> Maybe [(Int, Int, Int)]
-- solveWithCSP board = solve initialDomains constraints []
--   where
--     emptyCells = [(r, c) | r <- [0..8], c <- [0..8], board !! r !! c == Nothing]
--     constraints = generateConstraints emptyCells
--     initialDomains = initializeDomains board emptyCells

-- -- Inicializa dominios para cada celda vacía
-- initializeDomains :: Board -> [Position] -> [(Position, Domain)]
-- initializeDomains board positions =
--   [(pos, validValues board pos) | pos <- positions]

-- -- Genera restricciones binarias (todos deben ser diferentes)
-- generateConstraints :: [Position] -> [Constraint]
-- generateConstraints positions = 
--   [(p1, p2) | p1 <- positions, p2 <- positions, p1 /= p2, sharesUnit p1 p2]

-- -- Verifica si dos celdas comparten unidad (fila, columna o bloque)
-- sharesUnit :: Position -> Position -> Bool
-- sharesUnit (r1, c1) (r2, c2) =
--   r1 == r2 || c1 == c2 || (r1 `div` 3 == r2 `div` 3 && c1 `div` 3 == c2 `div` 3)

-- -- Verifica si un valor es válido en una posición
-- validValues :: Board -> Position -> [Int]
-- validValues board (r, c) =
--   let rowValues = getRow board r
--       colValues = getCol board c
--       boxValues = getBox board (r, c)
--   in [1..9] \\ (rowValues ++ colValues ++ boxValues)

-- -- -- Propagación de restricciones con AC-3
-- -- ac3 :: [(Position, Domain)] -> [Constraint] -> Maybe [(Position, Domain)]
-- -- ac3 domains [] = Just domains
-- -- ac3 domains (constraint:constraints) =
-- --   case revise domains constraint of
-- --     Nothing -> Nothing -- Inconsistencia encontrada
-- --     Just newDomains ->
-- --       let newConstraints = [(p1, p2) | (p1, p2) <- constraints, p2 /= snd constraint]
-- --       in ac3 newDomains newConstraints


-- ac3 :: [(Position, Domain)] -> [Constraint] -> Maybe [(Position, Domain)]
-- ac3 domains [] = Just domains
-- ac3 domains ((xi, xj):constraints) =
--   case revise domains (xi, xj) of
--     Nothing -> Nothing -- Inconsistencia encontrada
--     Just newDomains ->
--       -- Añade solo las restricciones afectadas
--       let newConstraints = [(xk, xi) | xk <- neighbors xi, xk /= xj]
--       in ac3 newDomains (constraints ++ newConstraints)
--   where
--     neighbors pos = [p2 | (p1, p2) <- constraints, p1 == pos]



-- -- Aplica revisión de arcos (revise)
-- revise :: [(Position, Domain)] -> Constraint -> Maybe [(Position, Domain)]
-- revise domains (xi, xj) =
--   case lookup xi domains of
--     Nothing -> Just domains
--     Just di ->
--       case lookup xj domains of
--         Nothing -> Just domains
--         Just dj ->
--           let newDi = filter (\x -> any (\y -> x /= y) dj) di
--           in if null newDi
--              then Nothing
--              else Just (updateDomain domains xi newDi)

-- -- Actualiza un dominio en la lista
-- updateDomain :: [(Position, Domain)] -> Position -> Domain -> [(Position, Domain)]
-- updateDomain domains pos newDomain =
--   (pos, newDomain) : filter (\(p, _) -> p /= pos) domains

-- -- -- Resuelve el Sudoku utilizando MRV y LCV
-- -- solve :: [(Position, Domain)] -> [Constraint] -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- -- solve [] _ steps = Just steps -- Tablero resuelto
-- -- solve domains constraints steps = do
-- --   let (pos, domain) = selectMRV domains
-- --   let orderedValues = orderLCV domain pos domains constraints
-- --   tryValues pos orderedValues domains constraints steps


-- solve :: [(Position, Domain)] -> [Constraint] -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- solve [] _ steps = Just steps -- Tablero resuelto
-- solve domains constraints steps = do
--   let (pos, domain) = selectMRV domains
--   let orderedValues = orderLCV domain pos domains constraints
--   let _ = traceSolve ("Trying cell " ++ show pos ++ " with domain: " ++ show domain) ()
--   tryValues pos orderedValues domains constraints steps





-- -- Selecciona la celda con MRV
-- selectMRV :: [(Position, Domain)] -> (Position, Domain)
-- selectMRV domains = minimumBy (\(_, d1) (_, d2) -> compare (length d1) (length d2)) domains

-- -- Ordena valores según LCV
-- orderLCV :: Domain -> Position -> [(Position, Domain)] -> [Constraint] -> [Int]
-- orderLCV domain pos domains constraints =
--   let affected = [p2 | (p1, p2) <- constraints, p1 == pos]
--       conflicts val = length [() | p <- affected, elem val (lookupDomain domains p)]
--   in sortBy (\v1 v2 -> compare (conflicts v1) (conflicts v2)) domain

-- -- Intenta asignar valores a una celda
-- tryValues :: Position -> [Int] -> [(Position, Domain)] -> [Constraint] -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- tryValues _ [] _ _ _ = Nothing
-- tryValues pos (val:vals) domains constraints steps = do
--   let newDomains = updateDomain domains pos [val]
--   case ac3 newDomains constraints of
--     Nothing -> tryValues pos vals domains constraints steps
--     Just consistentDomains -> solve consistentDomains constraints ((fst pos, snd pos, val) : steps)

-- -- Helpers
-- lookupDomain :: [(Position, Domain)] -> Position -> Domain
-- lookupDomain domains pos = maybe [] id (lookup pos domains)

-- getRow :: Board -> Int -> [Int]
-- getRow board r = mapMaybe id (board !! r)

-- getCol :: Board -> Int -> [Int]
-- getCol board c = mapMaybe id [row !! c | row <- board]

-- getBox :: Board -> Position -> [Int]
-- getBox board (r, c) =
--   let boxRow = (r `div` 3) * 3
--       boxCol = (c `div` 3) * 3
--   in mapMaybe id [board !! (boxRow + dr) !! (boxCol + dc) | dr <- [0..2], dc <- [0..2]]




-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.List (minimumBy, delete, sortBy, (\\))
-- import Data.Maybe (mapMaybe, isJust, fromMaybe)
-- import Control.Monad (guard)



-- -- -- import Data.List (sortBy,  intersect, (\\))



-- type Position = (Int, Int)
-- type Domain = [Int]
-- type Constraint = (Position, Position)

-- -- Método principal
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps state = do
--   let initialDomains = initializeDomains (board state)
--   case solveWithCSP initialDomains of
--     Just steps -> return steps
--     Nothing -> do
--       let fallbackDomains = initializeDomains (initialBoard state)
--       case solveWithCSP fallbackDomains of
--         Just steps -> return steps
--         Nothing -> error "El tablero inicial es inválido, lo cual no debería ocurrir."

-- -- Inicializa dominios para cada celda vacía
-- initializeDomains :: Board -> [(Position, Domain)]
-- initializeDomains board =
--   [((r, c), validValues board (r, c)) | r <- [0..8], c <- [0..8], board !! r !! c == Nothing]

-- -- Calcula los valores válidos para una celda específica
-- validValues :: Board -> Position -> [Int]
-- validValues board (r, c) =
--   let rowValues = getRow board r
--       colValues = getCol board c
--       blockValues = getBox board (r, c)
--   in [1..9] \\ (rowValues ++ colValues ++ blockValues)

-- -- Solución basada en CSP con técnicas de optimización
-- solveWithCSP :: [(Position, Domain)] -> Maybe [(Int, Int, Int)]
-- solveWithCSP domains =
--   case ac3 domains constraints of
--     Nothing -> Nothing
--     Just consistentDomains -> backtrack consistentDomains constraints []
--   where
--     constraints = generateConstraints (map fst domains)

-- -- Genera restricciones binarias (todos deben ser diferentes)
-- generateConstraints :: [Position] -> [Constraint]
-- generateConstraints positions =
--   [(p1, p2) | p1 <- positions, p2 <- positions, p1 /= p2, sharesUnit p1 p2]

-- -- Verifica si dos celdas comparten unidad (fila, columna o bloque)
-- sharesUnit :: Position -> Position -> Bool
-- sharesUnit (r1, c1) (r2, c2) =
--   r1 == r2 || c1 == c2 || (r1 `div` 3 == r2 `div` 3 && c1 `div` 3 == c2 `div` 3)

-- -- Propagación de restricciones con AC-3
-- ac3 :: [(Position, Domain)] -> [Constraint] -> Maybe [(Position, Domain)]
-- ac3 domains [] = Just domains
-- ac3 domains (constraint:constraints) =
--   case revise domains constraint of
--     Nothing -> Nothing -- Inconsistencia encontrada
--     Just newDomains ->
--       let newConstraints = [(p, xi) | (xi, p) <- constraints, p /= snd constraint]
--       in ac3 newDomains (constraints ++ newConstraints)

-- -- Aplica revisión de arcos (revise)
-- revise :: [(Position, Domain)] -> Constraint -> Maybe [(Position, Domain)]
-- revise domains (xi, xj) =
--   case lookup xi domains of
--     Nothing -> Just domains
--     Just di ->
--       case lookup xj domains of
--         Nothing -> Just domains
--         Just dj ->
--           let newDi = filter (\x -> any (\y -> x /= y) dj) di
--           in if null newDi
--              then Nothing
--              else Just (updateDomain domains xi newDi)

-- -- Actualiza un dominio en la lista
-- updateDomain :: [(Position, Domain)] -> Position -> Domain -> [(Position, Domain)]
-- updateDomain domains pos newDomain =
--   (pos, newDomain) : filter (\(p, _) -> p /= pos) domains

-- -- Resuelve el Sudoku utilizando MRV y LCV
-- backtrack :: [(Position, Domain)] -> [Constraint] -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- backtrack [] _ steps = Just steps -- Tablero resuelto
-- backtrack domains constraints steps = do
--   let (pos, domain) = selectMRV domains
--   let orderedValues = orderLCV domain pos domains constraints
--   tryValues pos orderedValues domains constraints steps

-- -- Selecciona la celda con MRV
-- selectMRV :: [(Position, Domain)] -> (Position, Domain)
-- selectMRV domains = minimumBy (\(_, d1) (_, d2) -> compare (length d1) (length d2)) domains

-- -- Ordena valores según LCV
-- orderLCV :: Domain -> Position -> [(Position, Domain)] -> [Constraint] -> [Int]
-- orderLCV domain pos domains constraints =
--   let affected = [p2 | (p1, p2) <- constraints, p1 == pos]
--       conflicts val = length [() | p <- affected, elem val (lookupDomain domains p)]
--   in sortBy (\v1 v2 -> compare (conflicts v1) (conflicts v2)) domain

-- -- Intenta asignar valores a una celda
-- tryValues :: Position -> [Int] -> [(Position, Domain)] -> [Constraint] -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- tryValues _ [] _ _ _ = Nothing
-- tryValues pos (val:vals) domains constraints steps = do
--   let newDomains = updateDomain domains pos [val]
--   case ac3 newDomains constraints of
--     Nothing -> tryValues pos vals domains constraints steps
--     Just consistentDomains -> backtrack consistentDomains constraints ((fst pos, snd pos, val) : steps)

-- -- Helpers para obtener filas, columnas y bloques
-- lookupDomain :: [(Position, Domain)] -> Position -> Domain
-- lookupDomain domains pos = fromMaybe [] (lookup pos domains)

-- getRow :: Board -> Int -> [Int]
-- getRow board r = mapMaybe id (board !! r)

-- getCol :: Board -> Int -> [Int]
-- getCol board c = mapMaybe id [row !! c | row <- board]

-- getBox :: Board -> Position -> [Int]
-- getBox board (r, c) =
--   let boxRow = (r `div` 3) * 3
--       boxCol = (c `div` 3) * 3
--   in mapMaybe id [board !! (boxRow + dr) !! (boxCol + dc) | dr <- [0..2], dc <- [0..2]]



-- {-# LANGUAGE LambdaCase #-}

-- module Logic.CSP (solveSudoku, generateSolutionSteps) where

-- import Data.Bits
-- import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Common (GameState(..), Board)
-- import Control.Monad
-- import Control.Applicative (Alternative(..))



-- type Cell = Word -- Celda representada como un Word

-- allBitsSet :: Word
-- allBitsSet = 1022 -- Representa los bits para 1..9

-- -- Función principal
-- generateSolutionSteps :: Board -> Maybe [(Int, Int, Int)]
-- generateSolutionSteps board = solveSudokuSteps (boardToCells board) []

-- solveSudoku :: Board -> Maybe Board
-- solveSudoku board = fmap cellsToBoard (solveCells (boardToCells board))

-- -- Convierte un `Board` (tu representación) a una lista de celdas
-- boardToCells :: Board -> [Cell]
-- boardToCells board =
--     [cellToWord (board !! r !! c) | r <- [0..8], c <- [0..8]]
--   where
--     cellToWord Nothing  = makePossible allBitsSet
--     cellToWord (Just n) = makeFixed (bit n)

-- -- Convierte una lista de celdas de vuelta a un `Board`
-- cellsToBoard :: [Cell] -> Board
-- cellsToBoard cells =
--     [[wordToCell (cells !! (r * 9 + c)) | c <- [0..8]] | r <- [0..8]]
--   where
--     wordToCell cell
--       | isFixed cell = Just (countTrailingZeros cell)
--       | otherwise    = Nothing

-- -- Marca una celda como posible o fija
-- makePossible, makeFixed :: Word -> Cell
-- makePossible = (`setBit` 15)
-- makeFixed = (`clearBit` 15)

-- isFixed :: Cell -> Bool
-- isFixed = not . flip testBit 15

-- -- Reemplaza una celda en la lista
-- replaceCell :: Int -> Cell -> [Cell] -> [Cell]
-- replaceCell i newCell cells = take i cells ++ [newCell] ++ drop (i + 1) cells

-- -- Resolver Sudoku con pasos
-- solveSudokuSteps :: [Cell] -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- solveSudokuSteps cells steps
--     | isGridInvalid cells = Nothing
--     | isGridFilled cells  = Just steps
--     | otherwise           =
--         let (grid1, grid2) = nextGrids cells
--             (ix, val) = findFirstChange cells grid1
--             step = (ix `div` 9, ix `mod` 9, val)
--         in solveSudokuSteps grid1 (step : steps) <|> solveSudokuSteps grid2 steps

-- -- Resolver sin pasos
-- solveCells :: [Cell] -> Maybe [Cell]
-- solveCells cells
--     | isGridInvalid cells = Nothing
--     | isGridFilled cells  = Just cells
--     | otherwise           =
--         let (grid1, grid2) = nextGrids cells
--         in solveCells grid1 <|> solveCells grid2

-- -- Verifica si el tablero está lleno
-- isGridFilled :: [Cell] -> Bool
-- isGridFilled = all isFixed

-- -- Verifica si el tablero es inválido
-- isGridInvalid :: [Cell] -> Bool
-- isGridInvalid cells = any hasDuplicates (rows ++ cols ++ subGrids)
--   where
--     rows = chunksOf 9 cells
--     cols = transpose rows
--     subGrids = [concat [take 3 (drop (c * 3) (rows !! r)) | r <- [sr..sr+2]]
--                | sr <- [0, 3, 6], c <- [0, 1, 2]]
--     hasDuplicates group =
--         let fixed = filter isFixed group
--         in length fixed /= length (nub fixed)

-- -- Divide un tablero en dos posibles caminos
-- nextGrids :: [Cell] -> ([Cell], [Cell])
-- nextGrids cells =
--     let (ix, cell) = head [(i, c) | (i, c) <- zip [0..] cells, not (isFixed c)]
--         first = setBit (clearBit cell (countTrailingZeros cell)) (countTrailingZeros cell + 1)
--     in (replaceCell ix first cells, replaceCell ix (clearBit cell (countTrailingZeros cell)) cells)

-- -- Encuentra el primer cambio entre dos listas
-- findFirstChange :: [Cell] -> [Cell] -> (Int, Int)
-- findFirstChange old new = head [(i, countTrailingZeros newCell) | (i, (oldCell, newCell)) <- zip [0..] (zip old new), oldCell /= newCell]

-- -- Divide una lista en chunks
-- chunksOf :: Int -> [a] -> [[a]]
-- chunksOf n [] = []
-- chunksOf n xs = take n xs : chunksOf n (drop n xs)
































-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.Maybe (isNothing, fromJust, catMaybes)
-- import Data.List (nub, (\\))
-- import Debug.Trace (trace)


-- -- Genera los pasos para resolver un Sudoku
-- -- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- -- generateSolutionSteps gameState = do
-- --   let currentBoard = board gameState
-- --       initial = initialBoard gameState
-- --   case solveBoard currentBoard of
-- --     Just solved -> return (generateSteps currentBoard solved)
-- --     Nothing -> case solveBoard initial of
-- --       Just solvedInitial -> return (generateSteps currentBoard solvedInitial)
-- --       Nothing -> error "No solution exists for the initial board, which should not happen."


-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps gameState = do
--   let currentBoard = board gameState
--       initial = initialBoard gameState
--   putStrLn "Starting solver..."
--   case solveBoard currentBoard of
--     Just solved -> do
--       putStrLn "Solver succeeded on the current board."
--       return (generateSteps currentBoard solved)
--     Nothing -> do
--       putStrLn "Solver failed on the current board. Trying initial board..."
--       case solveBoard initial of
--         Just solvedInitial -> do
--           putStrLn "Solver succeeded on the initial board."
--           return (generateSteps currentBoard solvedInitial)
--         Nothing -> do
--           putStrLn "Solver could not find a solution. This should not happen with the initial board."
--           return []





-- -- Genera los pasos dados un tablero inicial y el resuelto
-- generateSteps :: Board -> Board -> [(Int, Int, Int)]
-- generateSteps initial solved =
--   [ (r, c, fromJust v)
--   | (r, row) <- zip [0..] solved
--   , (c, v) <- zip [0..] row
--   , isNothing (initial !! r !! c)  -- Solo las celdas vacías del inicial
--   ]

-- -- Soluciona un Sudoku dado un tablero
-- -- solveBoard :: Board -> Maybe Board
-- -- solveBoard board
-- --   | not (isValidBoard board) = Nothing  -- Si el tablero es inválido, no se puede resolver
-- --   | otherwise = solveHelper board

-- solveBoard :: Board -> Maybe Board
-- solveBoard board = solveHelper board
--   -- | not (isValidBoard board) = error "Board is invalid!"  -- Depuración
--   -- | otherwise = solveHelper board




-- -- Valida si un tablero es válido
-- -- isValidBoard :: Board -> Bool
-- -- isValidBoard board = all (isValidCell board) [(r, c) | r <- [0..8], c <- [0..8]]

-- -- Valida una celda en un tablero
-- -- isValidCell :: Board -> (Int, Int) -> Bool
-- -- isValidCell board (row, col) =
-- --   case board !! row !! col of
-- --     Nothing -> True  -- Las celdas vacías son válidas
-- --     Just n -> n `notElem` (getRow board row ++ getCol board col ++ getBox board (row, col))


-- isValidCell :: Board -> (Int, Int) -> Bool
-- isValidCell board (row, col) =
--   case board !! row !! col of
--     Nothing -> True
--     Just n ->
--       let rowVals = getRow board row
--           colVals = getCol board col
--           boxVals = getBox board (row, col)
--       in n `notElem` rowVals && n `notElem` colVals && n `notElem` boxVals






-- -- Resuelve el Sudoku recursivamente
-- -- solveHelper :: Board -> Maybe Board
-- -- solveHelper board
-- --   | isComplete board = Just board  -- Si el tablero está completo, devolverlo
-- --   | otherwise =
-- --       let emptyCell = findEmpty board
-- --       in case emptyCell of
-- --            Nothing -> Nothing
-- --            Just (row, col) ->
-- --              let candidates = getCandidates board (row, col)
-- --              in tryCandidates board (row, col) candidates


-- solveHelper :: Board -> Maybe Board
-- solveHelper board
--   | isComplete board = Just board
--   | otherwise =
--       let emptyCell = findEmpty board
--       in case emptyCell of
--            Nothing -> Nothing
--            Just (row, col) ->
--              let candidates = getCandidates board (row, col)
--              in trace ("Trying cell: " ++ show (row, col) ++ ", candidates: " ++ show candidates)
--                 tryCandidates board (row, col) candidates







-- -- Encuentra la primera celda vacía
-- findEmpty :: Board -> Maybe (Int, Int)
-- findEmpty board =
--   case [(r, c) | r <- [0..8], c <- [0..8], isNothing (board !! r !! c)] of
--     [] -> Nothing
--     (x:_) -> Just x

-- -- Obtiene los números posibles para una celda
-- getCandidates :: Board -> (Int, Int) -> [Int]
-- getCandidates board (row, col) =
--   [1..9] \\ (getRow board row ++ getCol board col ++ getBox board (row, col))

-- -- Intenta cada candidato para una celda
-- tryCandidates :: Board -> (Int, Int) -> [Int] -> Maybe Board
-- tryCandidates _ _ [] = Nothing  -- No hay candidatos, no se puede continuar
-- tryCandidates board (row, col) (n:ns) =
--   let newBoard = updateBoard board (row, col) (Just n)
--   in case solveHelper newBoard of
--        Just solved -> Just solved
--        Nothing -> tryCandidates board (row, col) ns

-- -- Actualiza el tablero con un nuevo valor en una celda
-- updateBoard :: Board -> (Int, Int) -> Maybe Int -> Board
-- updateBoard board (row, col) val =
--   take row board ++
--   [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
--   drop (row + 1) board

-- -- Verifica si un tablero está completo
-- isComplete :: Board -> Bool
-- isComplete = all (all (not . isNothing))

-- -- Obtiene una fila del tablero
-- getRow :: Board -> Int -> [Int]
-- getRow board row = catMaybes (board !! row)

-- -- Obtiene una columna del tablero
-- getCol :: Board -> Int -> [Int]
-- getCol board col = catMaybes [row !! col | row <- board]

-- -- Obtiene la caja 3x3 que contiene una celda
-- getBox :: Board -> (Int, Int) -> [Int]
-- getBox board (row, col) =
--   let boxRow = (row `div` 3) * 3
--       boxCol = (col `div` 3) * 3
--   in catMaybes [board !! r !! c | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]




-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.Maybe (isNothing, fromJust, catMaybes)
-- import Data.List (nub, (\\), minimumBy)
-- import Debug.Trace (trace)

-- -- Genera los pasos para resolver un Sudoku
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps gameState = do
--   let currentBoard = board gameState
--   putStrLn "Starting solver..."
--   case solveBoard currentBoard of
--     Just solved -> do
--       putStrLn "Solver succeeded on the current board."
--       return (generateSteps currentBoard solved)
--     Nothing -> do
--       putStrLn "Solver could not find a solution."
--       return []

-- -- Genera los pasos dados un tablero inicial y el resuelto
-- generateSteps :: Board -> Board -> [(Int, Int, Int)]
-- generateSteps initial solved =
--   [ (r, c, fromJust v)
--   | (r, row) <- zip [0..] solved
--   , (c, v) <- zip [0..] row
--   , isNothing (initial !! r !! c)
--   ]

-- -- Soluciona un Sudoku dado un tablero
-- solveBoard :: Board -> Maybe Board
-- solveBoard board = solveHelper board

-- -- Resuelve el Sudoku con heurísticas y backtracking mejorado
-- solveHelper :: Board -> Maybe Board
-- solveHelper board
--   | isComplete board = Just board
--   | otherwise =
--       let emptyCell = selectCell board
--       in case emptyCell of
--            Nothing -> Nothing
--            Just (row, col) ->
--              let candidates = getCandidates board (row, col)
--              in trace ("Trying cell: " ++ show (row, col) ++ ", candidates: " ++ show candidates)
--                 tryCandidates board (row, col) candidates

-- -- Selecciona la mejor celda para explorar (MRV + Degree Heuristic)
-- selectCell :: Board -> Maybe (Int, Int)
-- selectCell board =
--   let emptyCells = [(r, c) | r <- [0..8], c <- [0..8], isNothing (board !! r !! c)]
--   in if null emptyCells
--         then Nothing
--         else Just $ minimumBy compareCells emptyCells
--   where
--     compareCells a b =
--       let lenA = length (getCandidates board a)
--           lenB = length (getCandidates board b)
--       in compare lenA lenB

-- -- Obtiene los números posibles para una celda
-- getCandidates :: Board -> (Int, Int) -> [Int]
-- getCandidates board (row, col) =
--   [1..9] \\ (getRow board row ++ getCol board col ++ getBox board (row, col))

-- -- Intenta cada candidato para una celda
-- tryCandidates :: Board -> (Int, Int) -> [Int] -> Maybe Board
-- tryCandidates _ _ [] = Nothing
-- tryCandidates board (row, col) (n:ns) =
--   let newBoard = updateBoard board (row, col) (Just n)
--   in case solveHelper newBoard of
--        Just solved -> Just solved
--        Nothing -> tryCandidates board (row, col) ns

-- -- Actualiza el tablero con un nuevo valor en una celda
-- updateBoard :: Board -> (Int, Int) -> Maybe Int -> Board
-- updateBoard board (row, col) val =
--   take row board ++
--   [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
--   drop (row + 1) board

-- -- Verifica si un tablero está completo
-- isComplete :: Board -> Bool
-- isComplete = all (all (not . isNothing))

-- -- Obtiene una fila del tablero
-- getRow :: Board -> Int -> [Int]
-- getRow board row = catMaybes (board !! row)

-- -- Obtiene una columna del tablero
-- getCol :: Board -> Int -> [Int]
-- getCol board col = catMaybes [row !! col | row <- board]

-- -- Obtiene la caja 3x3 que contiene una celda
-- getBox :: Board -> (Int, Int) -> [Int]
-- getBox board (row, col) =
--   let boxRow = (row `div` 3) * 3
--       boxCol = (col `div` 3) * 3
--   in catMaybes [board !! r !! c | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]


-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.Maybe (isNothing, fromJust, catMaybes)
-- import Data.List (nub, (\\), sortOn, sort)
-- import Debug.Trace (trace)

-- -- Genera los pasos para resolver un Sudoku
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps gameState = do
--   let currentBoard = board gameState
--   putStrLn "Starting solver..."
--   case solveBoard currentBoard of
--     Just solved -> do
--       putStrLn "Solver succeeded on the current board."
--       return (generateSteps currentBoard solved)
--     Nothing -> do
--       putStrLn "Solver could not find a solution."
--       return []

-- -- Genera los pasos dados un tablero inicial y el resuelto
-- generateSteps :: Board -> Board -> [(Int, Int, Int)]
-- generateSteps initial solved =
--   [ (r, c, fromJust v)
--   | (r, row) <- zip [0..] solved
--   , (c, v) <- zip [0..] row
--   , isNothing (initial !! r !! c)
--   ]

-- -- Resuelve un Sudoku completo
-- solveBoard :: Board -> Maybe Board
-- solveBoard board =
--   case solveHelper board of
--     Just solved -> if validateSolution solved then Just solved else Nothing
--     Nothing -> Nothing

-- -- Valida el tablero completo
-- validateSolution :: Board -> Bool
-- validateSolution board =
--   all validGroup [getRow board r | r <- [0..8]] &&
--   all validGroup [getCol board c | c <- [0..8]] &&
--   all validGroup [getBox board (r, c) | r <- [0, 3, 6], c <- [0, 3, 6]]
--   where
--     validGroup group = sort (catMaybes group) == [1..9]

-- -- Backtracking con heurísticas
-- solveHelper :: Board -> Maybe Board
-- solveHelper board
--   | isComplete board = Just board
--   | otherwise =
--       let emptyCell = selectCell board
--       in case emptyCell of
--            Nothing -> Nothing
--            Just (row, col) ->
--              let candidates = getCandidates board (row, col)
--              in trace ("Trying cell: " ++ show (row, col) ++ ", candidates: " ++ show candidates)
--                 tryCandidates board (row, col) candidates

-- -- Selección de celdas usando heurísticas MRV
-- selectCell :: Board -> Maybe (Int, Int)
-- selectCell board =
--   let emptyCells = [(r, c) | r <- [0..8], c <- [0..8], isNothing (board !! r !! c)]
--       sortedCells = sortOn (\cell -> length (getCandidates board cell)) emptyCells
--   in if null sortedCells then Nothing else Just (head sortedCells)

-- -- Obtiene los candidatos válidos para una celda
-- getCandidates :: Board -> (Int, Int) -> [Int]
-- getCandidates board (row, col) =
--   [1..9] \\ ( catMaybes (getRow board row ++ getCol board col ++ getBox board (row, col)))

-- -- Intenta cada candidato en una celda
-- tryCandidates :: Board -> (Int, Int) -> [Int] -> Maybe Board
-- tryCandidates _ _ [] = Nothing
-- tryCandidates board (row, col) (n:ns) =
--   let newBoard = updateBoard board (row, col) (Just n)
--   in case solveHelper newBoard of
--        Just solved -> Just solved
--        Nothing -> tryCandidates board (row, col) ns

-- -- Actualiza una celda en el tablero
-- updateBoard :: Board -> (Int, Int) -> Maybe Int -> Board
-- updateBoard board (row, col) val =
--   take row board ++
--   [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
--   drop (row + 1) board

-- -- Comprueba si el tablero está completo
-- isComplete :: Board -> Bool
-- isComplete = all (all (not . isNothing))

-- -- Obtiene una fila del tablero
-- getRow :: Board -> Int -> [Maybe Int]
-- getRow board row = board !! row

-- -- Obtiene una columna del tablero
-- getCol :: Board -> Int -> [Maybe Int]
-- getCol board col = [row !! col | row <- board]

-- -- Obtiene la caja 3x3 que contiene una celda
-- getBox :: Board -> (Int, Int) -> [Maybe Int]
-- getBox board (row, col) =
--   let boxRow = (row `div` 3) * 3
--       boxCol = (col `div` 3) * 3
--   in [board !! r !! c | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]





-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.Maybe (isNothing, fromJust, catMaybes)
-- import Data.List (nub, (\\), sort, sortOn)
-- import Debug.Trace (trace)

-- -- Genera los pasos para resolver un Sudoku
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps gameState = do
--   let currentBoard = board gameState
--   putStrLn "Starting solver..."
--   case solveBoard currentBoard of
--     Just solved -> do
--       putStrLn "Solver succeeded on the current board."
--       return (generateSteps currentBoard solved)
--     Nothing -> do
--       putStrLn "Solver could not find a solution."
--       return []

-- -- Genera los pasos dados un tablero inicial y el resuelto
-- generateSteps :: Board -> Board -> [(Int, Int, Int)]
-- generateSteps initial solved =
--   [ (r, c, fromJust v)
--   | (r, row) <- zip [0..] solved
--   , (c, v) <- zip [0..] row
--   , isNothing (initial !! r !! c)
--   ]

-- -- Resuelve un Sudoku completo
-- solveBoard :: Board -> Maybe Board
-- solveBoard board =
--   case solveHelper board of
--     Just solved -> if validateSolution solved then Just solved else Nothing
--     Nothing -> Nothing

-- -- Valida el tablero completo
-- validateSolution :: Board -> Bool
-- validateSolution board =
--   all validGroup [getRow board r | r <- [0..8]] &&
--   all validGroup [getCol board c | c <- [0..8]] &&
--   all validGroup [getBox board (r, c) | r <- [0, 3, 6], c <- [0, 3, 6]]
--   where
--     validGroup group = sort (catMaybes group) == [1..9]

-- -- Resuelve el Sudoku con backtracking y forward checking
-- solveHelper :: Board -> Maybe Board
-- solveHelper board
--   | isComplete board = Just board
--   | otherwise =
--       let emptyCell = selectCell board
--       in case emptyCell of
--            Nothing -> Nothing
--            Just (row, col) ->
--              let candidates = getCandidates board (row, col)
--              in tryCandidates board (row, col) candidates

-- -- Selección de celdas usando heurísticas MRV
-- selectCell :: Board -> Maybe (Int, Int)
-- selectCell board =
--   let emptyCells = [(r, c) | r <- [0..8], c <- [0..8], isNothing (board !! r !! c)]
--       sortedCells = sortOn (\cell -> length (getCandidates board cell)) emptyCells
--   in if null sortedCells then Nothing else Just (head sortedCells)

-- -- Obtiene los candidatos válidos para una celda
-- getCandidates :: Board -> (Int, Int) -> [Int]
-- getCandidates board (row, col) =
--   [1..9] \\ (catMaybes (getRow board row) ++ catMaybes (getCol board col) ++ catMaybes (getBox board (row, col)))

-- -- Intenta cada candidato en una celda con retroceso completo
-- tryCandidates :: Board -> (Int, Int) -> [Int] -> Maybe Board
-- tryCandidates _ _ [] = Nothing
-- tryCandidates board (row, col) (n:ns) =
--   let newBoard = updateBoard board (row, col) (Just n)
--   in if isValidPartial newBoard
--        then case solveHelper newBoard of
--               Just solved -> Just solved
--               Nothing -> tryCandidates board (row, col) ns
--        else tryCandidates board (row, col) ns

-- -- Verifica si el tablero parcial es válido
-- isValidPartial :: Board -> Bool
-- isValidPartial board =
--   all (noDuplicates . catMaybes) [getRow board r | r <- [0..8]] &&
--   all (noDuplicates . catMaybes) [getCol board c | c <- [0..8]] &&
--   all (noDuplicates . catMaybes) [getBox board (r, c) | r <- [0, 3, 6], c <- [0, 3, 6]]
--   where
--     noDuplicates xs = length xs == length (nub xs)

-- -- Actualiza una celda en el tablero
-- updateBoard :: Board -> (Int, Int) -> Maybe Int -> Board
-- updateBoard board (row, col) val =
--   take row board ++
--   [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
--   drop (row + 1) board

-- -- Comprueba si el tablero está completo
-- isComplete :: Board -> Bool
-- isComplete = all (all (not . isNothing))

-- -- Obtiene una fila del tablero
-- getRow :: Board -> Int -> [Maybe Int]
-- getRow board row = board !! row

-- -- Obtiene una columna del tablero
-- getCol :: Board -> Int -> [Maybe Int]
-- getCol board col = [row !! col | row <- board]

-- -- Obtiene la caja 3x3 que contiene una celda
-- getBox :: Board -> (Int, Int) -> [Maybe Int]
-- getBox board (row, col) =
--   let boxRow = (row `div` 3) * 3
--       boxCol = (col `div` 3) * 3
--   in [board !! r !! c | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]




-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.Maybe (isNothing, fromJust, catMaybes)
-- import Data.List (nub, (\\), sort, sortOn)
-- import Debug.Trace (trace)

-- -- Genera los pasos para resolver un Sudoku
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps gameState = do
--   let currentBoard = board gameState
--   putStrLn "Starting solver..."
--   case solveBoard currentBoard of
--     Just solved -> do
--       putStrLn "Solver succeeded on the current board."
--       return (generateSteps currentBoard solved)
--     Nothing -> do
--       putStrLn "Solver could not find a solution."
--       return []

-- -- Genera los pasos dados un tablero inicial y el resuelto
-- generateSteps :: Board -> Board -> [(Int, Int, Int)]
-- generateSteps initial solved =
--   [ (r, c, fromJust v)
--   | (r, row) <- zip [0..] solved
--   , (c, v) <- zip [0..] row
--   , isNothing (initial !! r !! c)
--   ]

-- -- Resuelve un Sudoku completo
-- solveBoard :: Board -> Maybe Board
-- solveBoard board =
--   case solveHelper board of
--     Just solved -> if validateSolution solved then Just solved else Nothing
--     Nothing -> Nothing

-- -- Valida el tablero completo
-- validateSolution :: Board -> Bool
-- validateSolution board =
--   all validGroup [getRow board r | r <- [0..8]] &&
--   all validGroup [getCol board c | c <- [0..8]] &&
--   all validGroup [getBox board (r, c) | r <- [0, 3, 6], c <- [0, 3, 6]]
--   where
--     validGroup group = sort (catMaybes group) == [1..9]

-- -- Resuelve el Sudoku con backtracking y validación dinámica
-- solveHelper :: Board -> Maybe Board
-- solveHelper board
--   | isComplete board = Just board  -- Tablero completo
--   | otherwise =
--       let emptyCell = selectCell board
--       in case emptyCell of
--            Nothing -> Nothing  -- No hay celdas vacías
--            Just (row, col) ->
--              let candidates = getCandidates board (row, col)
--              in tryCandidates board (row, col) candidates

-- -- Selección de celdas usando heurísticas MRV
-- selectCell :: Board -> Maybe (Int, Int)
-- selectCell board =
--   let emptyCells = [(r, c) | r <- [0..8], c <- [0..8], isNothing (board !! r !! c)]
--       sortedCells = sortOn (\cell -> length (getCandidates board cell)) emptyCells
--   in if null sortedCells then Nothing else Just (head sortedCells)

-- -- Obtiene los candidatos válidos para una celda
-- getCandidates :: Board -> (Int, Int) -> [Int]
-- getCandidates board (row, col) =
--   [1..9] \\ (catMaybes (getRow board row) ++ catMaybes (getCol board col) ++ catMaybes (getBox board (row, col)))

-- -- Intenta cada candidato en una celda con validación y retroceso completo
-- tryCandidates :: Board -> (Int, Int) -> [Int] -> Maybe Board
-- tryCandidates _ _ [] = Nothing  -- No hay más candidatos
-- tryCandidates board (row, col) (n:ns) =
--   let newBoard = updateBoard board (row, col) (Just n)
--   in if isValidPartial newBoard
--        then case solveHelper newBoard of
--               Just solved -> Just solved
--               Nothing -> tryCandidates board (row, col) ns
--        else tryCandidates board (row, col) ns

-- -- Verifica si el tablero parcial es válido
-- isValidPartial :: Board -> Bool
-- isValidPartial board =
--   all (noDuplicates . catMaybes) [getRow board r | r <- [0..8]] &&
--   all (noDuplicates . catMaybes) [getCol board c | c <- [0..8]] &&
--   all (noDuplicates . catMaybes) [getBox board (r, c) | r <- [0, 3, 6], c <- [0, 3, 6]]
--   where
--     noDuplicates xs = length xs == length (nub xs)

-- -- Actualiza una celda en el tablero
-- updateBoard :: Board -> (Int, Int) -> Maybe Int -> Board
-- updateBoard board (row, col) val =
--   take row board ++
--   [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
--   drop (row + 1) board

-- -- Comprueba si el tablero está completo
-- isComplete :: Board -> Bool
-- isComplete = all (all (not . isNothing))

-- -- Obtiene una fila del tablero
-- getRow :: Board -> Int -> [Maybe Int]
-- getRow board row = board !! row

-- -- Obtiene una columna del tablero
-- getCol :: Board -> Int -> [Maybe Int]
-- getCol board col = [row !! col | row <- board]

-- -- Obtiene la caja 3x3 que contiene una celda
-- getBox :: Board -> (Int, Int) -> [Maybe Int]
-- getBox board (row, col) =
--   let boxRow = (row `div` 3) * 3
--       boxCol = (col `div` 3) * 3
--   in [board !! r !! c | r <- [boxRow..boxRow+2], c <- [boxCol..boxCol+2]]









