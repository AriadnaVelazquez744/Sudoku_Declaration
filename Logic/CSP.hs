-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.List (minimumBy)
-- import Data.Ord (comparing)
-- import Data.Maybe (isJust)

-- -- Representa el estado enriquecido de una celda.
-- data CSPCell = CSPStatic Int Int Int       -- Valor inicial fijo (número, fila, columna).
--              | CSPFixed Int Int Int        -- Valor asignado (número, fila, columna).
--              | CSPPossibles [Int] Int Int  -- Valores posibles (lista, fila, columna).
--   deriving (Show, Eq)

-- -- Representa el tablero CSP enriquecido.
-- type CSPBoard = [[CSPCell]]

-- -- Punto de entrada principal.
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps gameState = do
--     let initial = parseCSPBoard (initialBoard gameState)
--     let initialPositions = getInitialPositions (initialBoard gameState)
--     putStrLn "Tablero inicial convertido a CSP:"
--     print initial
--     result <- solveCSP initial [] []
--     case result of
--         Just solution -> do
--             let filteredSolution = filterSolution solution initialPositions
--             putStrLn "Solución encontrada:"
--             print filteredSolution
--             return filteredSolution
--         Nothing -> do
--             putStrLn "No se encontró una solución."
--             return []

-- -- Convierte un tablero inicial en un CSPBoard.
-- parseCSPBoard :: Board -> CSPBoard
-- parseCSPBoard board =
--     [[parseCell r c (board !! r !! c) | c <- [0..8]] | r <- [0..8]]
--   where
--     parseCell :: Int -> Int -> Maybe Int -> CSPCell
--     parseCell r c (Just n) = CSPStatic n r c
--     parseCell r c Nothing  = CSPPossibles [1..9] r c

-- -- Obtiene las posiciones iniciales (celdas estáticas).
-- getInitialPositions :: Board -> [(Int, Int)]
-- getInitialPositions board =
--     [(r, c) | r <- [0..8], c <- [0..8], isJust (board !! r !! c)]

-- -- Actualiza las posibilidades en todo el tablero.
-- updatePossibilities :: CSPBoard -> CSPBoard
-- updatePossibilities board =
--   [ [ updateCell r c board | c <- [0..8] ] | r <- [0..8] ]
--   where
--     updateCell :: Int -> Int -> CSPBoard -> CSPCell
--     updateCell r c b = case b !! r !! c of
--         CSPStatic n r' c'     -> CSPStatic n r' c'  -- No cambia.
--         CSPFixed n r' c'      -> CSPFixed n r' c'   -- Ya resuelta, no cambia.
--         CSPPossibles _ r' c'  -> CSPPossibles (filterByRules r c b) r' c'

-- -- Filtra los valores posibles de una celda según las reglas.
-- filterByRules :: Int -> Int -> CSPBoard -> [Int]
-- filterByRules r c board =
--   let rowVals = getRow r board
--       colVals = getCol c board
--       blockVals = getBlock r c board
--       usedVals = rowVals ++ colVals ++ blockVals
--   in [x | x <- [1..9], x `notElem` usedVals]

-- -- Obtiene los valores fijos de la fila.
-- getRow :: Int -> CSPBoard -> [Int]
-- getRow r board = [ n | CSPStatic n _ _ <- board !! r ] ++
--                  [ n | CSPFixed n _ _  <- board !! r ]

-- -- Obtiene los valores fijos de la columna.
-- getCol :: Int -> CSPBoard -> [Int]
-- getCol c board = [ n | row <- board, CSPStatic n _ _ <- [row !! c] ] ++
--                  [ n | row <- board, CSPFixed n _ _  <- [row !! c] ]

-- -- Obtiene los valores fijos del bloque 3x3.
-- getBlock :: Int -> Int -> CSPBoard -> [Int]
-- getBlock r c board =
--   let startRow = (r `div` 3) * 3
--       startCol = (c `div` 3) * 3
--   in [ n | i <- [0..2], j <- [0..2],
--            CSPStatic n _ _ <- [board !! (startRow + i) !! (startCol + j)] ] ++
--      [ n | i <- [0..2], j <- [0..2],
--            CSPFixed n _ _  <- [board !! (startRow + i) !! (startCol + j)] ]

-- -- Selecciona la siguiente celda a resolver.
-- selectNextCell :: CSPBoard -> Maybe (Int, Int, [Int])
-- selectNextCell board = do
--   let candidates = [ (r, c, possibles)
--                  | (r, row) <- zip [0..] board,
--                    (c, CSPPossibles possibles _ _) <- zip [0..] row ]
--   if null candidates
--     then Nothing
--     else Just $ minimumBy (\(_, _, ps1) (_, _, ps2) -> compare (length ps1) (length ps2)) candidates

-- -- Resuelve el CSP usando backtracking.
-- solveCSP :: CSPBoard -> [(Int, Int, Int)] -> [CSPBoard] -> IO (Maybe [(Int, Int, Int)])
-- solveCSP board solution history = do
--   if isComplete board
--     then do
--       putStrLn "¡Tablero completo!"
--       return (Just solution)
--     else case selectNextCell board of
--       Nothing -> do
--         putStrLn "No se encontraron celdas para resolver."
--         return Nothing
--       Just (r, c, possibles) -> tryPossibilities r c possibles
--   where
--     tryPossibilities :: Int -> Int -> [Int] -> IO (Maybe [(Int, Int, Int)])
--     tryPossibilities _ _ [] = do
--       putStrLn "Retrocediendo..."
--       return Nothing
--     tryPossibilities r c (val:vals) = do
--       putStrLn $ "Intentando asignar " ++ show val ++ " a la celda (" ++ show r ++ ", " ++ show c ++ ")"
--       let newBoard = applyAssignment board r c val
--       let newHistory = board : history
--       result <- solveCSP (updatePossibilities newBoard) ((r, c, val) : solution) newHistory
--       case result of
--         Just sol -> return (Just sol)
--         Nothing  -> do
--           putStrLn $ "Falló asignación de " ++ show val ++ " a la celda (" ++ show r ++ ", " ++ show c ++ ")"
--           solveCSP (head newHistory) solution (tail newHistory)

-- -- Verifica si el tablero está completo.
-- isComplete :: CSPBoard -> Bool
-- isComplete = all (all isFixed)
--   where
--     isFixed (CSPStatic _ _ _) = True
--     isFixed (CSPFixed _ _ _)  = True
--     isFixed _                 = False

-- -- Asigna un valor fijo a una celda.
-- applyAssignment :: CSPBoard -> Int -> Int -> Int -> CSPBoard
-- applyAssignment board r c val =
--     [[updateCell r' c' cell | (c', cell) <- zip [0..] row] | (r', row) <- zip [0..] board]
--   where
--     updateCell r' c' cell
--       | r' == r && c' == c = case cell of
--           CSPPossibles _ _ _ -> CSPFixed val r' c'
--           _ -> cell
--       | otherwise = cell

-- -- Filtra las celdas estáticas de la solución final.
-- filterSolution :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int)]
-- filterSolution solution initialPositions =
--     [step | step@(r, c, _) <- solution, (r, c) `notElem` initialPositions]


module Logic.CSP where

import Common (GameState(..), Board)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)

-- type Board = [[Maybe Int]]
type Domain = [Int]
type CSP = [[Domain]] -- Cada celda tiene un dominio asociado

initializeCSP :: Board -> CSP
initializeCSP board = 
    [ [ if cell == Nothing then [1..9] else [fromJust cell] | cell <- row ] 
    | row <- board 
    ]
  where
    fromJust (Just x) = x
    fromJust _ = error "Unexpected Nothing"

ac3 :: CSP -> CSP
ac3 csp = foldl propagate csp allConstraints
  where
    allConstraints = [(row, col) | row <- [0..8], col <- [0..8]]

    propagate :: CSP -> (Int, Int) -> CSP
    propagate csp (row, col) =
        if length (csp !! row !! col) == 1 -- Si es un valor fijo
        then eliminate csp (row, col) (head (csp !! row !! col))
        else csp

    eliminate :: CSP -> (Int, Int) -> Int -> CSP
    eliminate csp (row, col) value =
        foldl update csp (affectedCells row col)
      where
        affectedCells r c =
            -- Filtra las celdas en la fila, columna y subcuadro
            [(r', c') | r' <- [0..8], c' <- [0..8], 
                        (r' == r || c' == c || inSameSubGrid (r, c) (r', c')) &&
                        (r', c') /= (r, c)]

        inSameSubGrid (r1, c1) (r2, c2) =
            (r1 `div` 3 == r2 `div` 3) && (c1 `div` 3 == c2 `div` 3)

        update :: CSP -> (Int, Int) -> CSP
        update csp (r, c) =
            let currentDomain = csp !! r !! c
            in if value `elem` currentDomain
                then replaceAt csp r c (filter (/= value) currentDomain)
                else csp

        replaceAt :: [[a]] -> Int -> Int -> a -> [[a]]
        replaceAt board r c val =
            take r board ++
            [take c (board !! r) ++ [val] ++ drop (c + 1) (board !! r)] ++
            drop (r + 1) board

replaceAt :: [[a]] -> Int -> Int -> a -> [[a]]
replaceAt matrix row col newVal =
    take row matrix ++
    [take col (matrix !! row) ++ [newVal] ++ drop (col + 1) (matrix !! row)] ++
    drop (row + 1) matrix

solveCSP :: CSP -> Maybe Board
solveCSP csp
    | isSolved csp = Just (cspToBoard csp)
    | otherwise = case selectUnassigned csp of
        Nothing -> Nothing -- No hay más variables por asignar
        Just (row, col) -> tryValues csp row col (csp !! row !! col)
  where
    isSolved = all (all ((== 1) . length)) -- Si todas las celdas tienen un solo valor

    selectUnassigned :: CSP -> Maybe (Int, Int)
    selectUnassigned csp =
        let candidates = [(row, col) | row <- [0..8], col <- [0..8], length (csp !! row !! col) > 1]
        in if null candidates then Nothing else Just (head candidates)

    tryValues :: CSP -> Int -> Int -> [Int] -> Maybe Board
    tryValues csp row col [] = Nothing -- Sin valores posibles
    tryValues csp row col (v:vs) =
        let newCSP = assignValue csp row col v
        in case solveCSP (ac3 newCSP) of
            Just solution -> Just solution
            Nothing -> tryValues csp row col vs

    assignValue :: CSP -> Int -> Int -> Int -> CSP
    assignValue csp row col value =
        replaceAt csp row col [value]

    cspToBoard :: CSP -> Board
    cspToBoard csp = map (map toMaybe) csp
      where
        toMaybe [v] = Just v
        toMaybe _ = Nothing

solveBoardWithCSP :: Board -> Maybe Board
solveBoardWithCSP board =
    solveCSP (ac3 (initializeCSP board))

-- -- Representa el estado enriquecido de una celda.
-- data CSPCell = CSPStatic Int Int Int       -- Valor inicial fijo (número, fila, columna).
--              | CSPFixed Int Int Int        -- Valor asignado (número, fila, columna).
--              | CSPPossibles [Int] Int Int  -- Valores posibles (lista, fila, columna).
--   deriving (Show, Eq)

-- -- Representa el tablero CSP enriquecido.
-- type CSPBoard = [[CSPCell]]

-- -- Punto de entrada principal.
-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps gameState = do
--     let initial = parseCSPBoard (initialBoard gameState)
--     let initialPositions = getInitialPositions (initialBoard gameState)
--     putStrLn "Tablero inicial convertido a CSP:"
--     print initial
--     result <- solveCSP initial [] []
--     case result of
--         Just solution -> do
--             -- let filteredSolution = filterSolution solution initialPositions
--             if isValidSolution initial solution
--                 then do
--                     putStrLn "Solución encontrada:"
--                     print solution
--                     return solution
--                 else do
--                     putStrLn "La solución no es válida."
--                     return []
--         Nothing -> do
--             putStrLn "No se encontró una solución."
--             return []

-- -- Convierte un tablero inicial en un CSPBoard.
-- parseCSPBoard :: Board -> CSPBoard
-- parseCSPBoard board =
--     [[parseCell r c (board !! r !! c) | c <- [0..8]] | r <- [0..8]]
--   where
--     parseCell :: Int -> Int -> Maybe Int -> CSPCell
--     parseCell r c (Just n) = CSPStatic n r c
--     parseCell r c Nothing  = CSPPossibles [1..9] r c

-- -- Obtiene las posiciones iniciales (celdas estáticas).
-- getInitialPositions :: Board -> [(Int, Int)]
-- getInitialPositions board =
--     [(r, c) | r <- [0..8], c <- [0..8], isJust (board !! r !! c)]

-- -- Actualiza las posibilidades en todo el tablero.
-- updatePossibilities :: CSPBoard -> CSPBoard
-- updatePossibilities board =
--   [ [ updateCell r c board | c <- [0..8] ] | r <- [0..8] ]
--   where
--     updateCell :: Int -> Int -> CSPBoard -> CSPCell
--     updateCell r c b = case b !! r !! c of
--         CSPStatic n r' c'     -> CSPStatic n r' c'  -- No cambia.
--         CSPFixed n r' c'      -> CSPFixed n r' c'   -- Ya resuelta, no cambia.
--         CSPPossibles _ r' c'  -> CSPPossibles (filterByRules r c b) r' c'

-- -- Filtra los valores posibles de una celda según las reglas.
-- filterByRules :: Int -> Int -> CSPBoard -> [Int]
-- filterByRules r c board =
--   let rowVals = getRow r board
--       colVals = getCol c board
--       blockVals = getBlock r c board
--       usedVals = rowVals ++ colVals ++ blockVals
--   in [x | x <- [1..9], x `notElem` usedVals]

-- -- Obtiene los valores fijos de la fila.
-- getRow :: Int -> CSPBoard -> [Int]
-- getRow r board = [ n | CSPStatic n _ _ <- board !! r ] ++
--                  [ n | CSPFixed n _ _  <- board !! r ]

-- -- Obtiene los valores fijos de la columna.
-- getCol :: Int -> CSPBoard -> [Int]
-- getCol c board = [ n | row <- board, CSPStatic n _ _ <- [row !! c] ] ++
--                  [ n | row <- board, CSPFixed n _ _  <- [row !! c] ]

-- -- Obtiene los valores fijos del bloque 3x3.
-- getBlock :: Int -> Int -> CSPBoard -> [Int]
-- getBlock r c board =
--   let startRow = (r `div` 3) * 3
--       startCol = (c `div` 3) * 3
--   in [ n | i <- [0..2], j <- [0..2],
--            CSPStatic n _ _ <- [board !! (startRow + i) !! (startCol + j)] ] ++
--      [ n | i <- [0..2], j <- [0..2],
--            CSPFixed n _ _  <- [board !! (startRow + i) !! (startCol + j)] ]

-- -- Selecciona la siguiente celda a resolver.
-- selectNextCell :: CSPBoard -> Maybe (Int, Int, [Int])
-- selectNextCell board = do
--   let candidates = [ (r, c, possibles)
--                  | (r, row) <- zip [0..] board,
--                    (c, CSPPossibles possibles _ _) <- zip [0..] row ]
--   if null candidates
--     then Nothing
--     else Just $ minimumBy (\(_, _, ps1) (_, _, ps2) -> compare (length ps1) (length ps2)) candidates

-- -- Resuelve el CSP usando backtracking.
-- solveCSP :: CSPBoard -> [(Int, Int, Int)] -> [CSPBoard] -> IO (Maybe [(Int, Int, Int)])
-- solveCSP board solution history = do
--   if isComplete board
--     then do
--       putStrLn "¡Tablero completo!"
--       return (Just solution)
--     else case selectNextCell board of
--       Nothing -> do
--         putStrLn "No se encontraron celdas para resolver."
--         return Nothing
--       Just (r, c, possibles) -> tryPossibilities r c possibles history
--   where
--     tryPossibilities :: Int -> Int -> [Int] -> [CSPBoard] -> IO (Maybe [(Int, Int, Int)])
--     tryPossibilities r c [] history = do
--       putStrLn "Retrocediendo..."
--       return Nothing
--     tryPossibilities r c (val:vals) history = do
--       putStrLn $ "Intentando asignar " ++ show val ++ " a la celda (" ++ show r ++ ", " ++ show c ++ ")"
--       let newBoard = applyAssignment board r c val
--       result <- solveCSP (updatePossibilities newBoard) ((r, c, val) : solution) (board : history)
--       case result of
--         Just sol -> return (Just sol)
--         Nothing  -> do
--           putStrLn $ "Falló asignación de " ++ show val ++ " a la celda (" ++ show r ++ ", " ++ show c ++ ")"
--           tryPossibilities r c vals history

-- -- Verifica si el tablero está completo.
-- isComplete :: CSPBoard -> Bool
-- isComplete = all (all isFixed)
--   where
--     isFixed (CSPStatic _ _ _) = True
--     isFixed (CSPFixed _ _ _)  = True
--     isFixed _                 = False


-- -- Asigna un valor fijo a una celda.
-- applyAssignment :: CSPBoard -> Int -> Int -> Int -> CSPBoard
-- applyAssignment board r c val =
--     [[updateCell r' c' cell | (c', cell) <- zip [0..] row] | (r', row) <- zip [0..] board]
--   where
--     updateCell r' c' cell
--       | r' == r && c' == c = case cell of
--           CSPPossibles _ _ _ -> CSPFixed val r' c'
--           _ -> cell
--       | otherwise = cell

-- -- Filtra las celdas estáticas de la solución final.
-- filterSolution :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int)]
-- filterSolution solution initialPositions =
--     [step | step@(r, c, _) <- solution, (r, c) `notElem` initialPositions]



-- -- Verifica si una solución es válida de acuerdo con las reglas del Sudoku.
-- isValidSolution :: CSPBoard -> [(Int, Int, Int)] -> Bool
-- isValidSolution board solution =
--   noEmptyCells finalBoard &&
--   all isValidGroup allGroups
--   where
--     -- Combina el tablero inicial y la solución CSP para generar el tablero final.
--     finalBoard = applySolution board solution

--     -- Verifica que no existan celdas vacías.
--     noEmptyCells :: [[Int]] -> Bool
--     noEmptyCells = all (all (/= 0))

--     -- Verifica si todos los valores en un grupo (fila, columna o bloque) son únicos.
--     isValidGroup :: [Int] -> Bool
--     isValidGroup group = length group == length (unique group)

--     -- Obtiene todos los grupos (filas, columnas y bloques) del tablero final.
--     allGroups :: [[Int]]
--     allGroups = getAllRows finalBoard ++ getAllCols finalBoard ++ getAllBlocks finalBoard

--     -- Elimina duplicados en una lista.
--     unique :: [Int] -> [Int]
--     unique = foldr (\x seen -> if x `elem` seen then seen else x : seen) []


-- -- Combina el tablero inicial con la solución para generar el tablero final.
-- applySolution :: CSPBoard -> [(Int, Int, Int)] -> [[Int]]
-- applySolution board solution =
--   [[resolveCell r c | c <- [0..8]] | r <- [0..8]]
--   where
--     -- Convierte la solución a un formato compatible con lookup.
--     solutionMap = convertSolution solution

--     -- Resuelve el valor final de una celda, combinando el tablero inicial y la solución CSP.
--     resolveCell :: Int -> Int -> Int
--     resolveCell r c =
--       case (board !! r !! c, lookup (r, c) solutionMap) of
--         (CSPStatic n _ _, Nothing) -> n       -- Mantener las celdas estáticas.
--         (CSPPossibles _ _ _, Just val) -> val -- Usar valores del CSP para celdas resueltas.
--         (CSPStatic _ _ _, Just _)  -> error $ "Las celdas estáticas no deben sobrescribirse: (" ++ show r ++ ", " ++ show c ++ ")"
--         (CSPPossibles _ _ _, Nothing) -> error $ "Celda sin resolver: (" ++ show r ++ ", " ++ show c ++ ")"
--         _ -> error $ "Estado inesperado en la celda: (" ++ show r ++ ", " ++ show c ++ ")"

-- -- Convierte [(Int, Int, Int)] en [((Int, Int), Int)].
-- convertSolution :: [(Int, Int, Int)] -> [((Int, Int), Int)]
-- convertSolution = map (\(r, c, val) -> ((r, c), val))



-- -- Obtiene todas las filas del tablero.
-- getAllRows :: [[Int]] -> [[Int]]
-- getAllRows = id

-- -- Obtiene todas las columnas del tablero.
-- getAllCols :: [[Int]] -> [[Int]]
-- getAllCols board = [[board !! r !! c | r <- [0..8]] | c <- [0..8]]

-- -- Obtiene todos los bloques 3x3 del tablero.
-- getAllBlocks :: [[Int]] -> [[Int]]
-- getAllBlocks board =
--   [ [ board !! (rBase + rOffset) !! (cBase + cOffset)
--     | rOffset <- [0..2], cOffset <- [0..2] ]
--   | rBase <- [0,3,6], cBase <- [0,3,6] ]
