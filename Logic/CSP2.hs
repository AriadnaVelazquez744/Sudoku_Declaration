-- module Logic.CSP (generateSolutionSteps) where

-- import Common (GameState(..), Board)
-- import Data.List (minimumBy)
-- import Data.Ord (comparing)
-- import Data.Maybe (isJust)

-- -- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- -- generateSolutionSteps gameState = do
-- --     let initial = parseCSPBoard (initialBoard gameState)
-- --     putStrLn "Tablero inicial convertido a CSP:"
-- --     print initial
-- --     case solveCSP initial [] of
-- --         Just solution -> do
-- --             putStrLn "Solución encontrada:"
-- --             print solution
-- --             return solution
-- --         Nothing -> do
-- --             putStrLn "No se encontró una solución."
-- --             return []

-- -- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- -- generateSolutionSteps gameState = do
-- --     let initial = parseCSPBoard (initialBoard gameState)
-- --     let initialPositions = getInitialPositions (initialBoard gameState)
-- --     putStrLn "Tablero inicial convertido a CSP:"
-- --     print initial
-- --     case solveCSP initial [] of
-- --         Just solution -> do
-- --             let filteredSolution = filter (\(r, c, _) -> (r, c) `notElem` initialPositions) solution
-- --             putStrLn "Solución encontrada:"
-- --             print filteredSolution
-- --             return filteredSolution
-- --         Nothing -> do
-- --             putStrLn "No se encontró una solución."
-- --             return []

-- generateSolutionSteps :: GameState -> IO [(Int, Int, Int)]
-- generateSolutionSteps gameState = do
--     let initial = parseCSPBoard (initialBoard gameState)
--     let initialPositions = getInitialPositions (initialBoard gameState)
--     putStrLn "Tablero inicial convertido a CSP:"
--     print initial
--     case solveCSP initial [] of
--         Just solution -> do
--             let filteredSolution = filter (\(r, c, _) -> (r, c) `notElem` initialPositions) solution
--             putStrLn "Solución encontrada:"
--             print filteredSolution
--             return filteredSolution
--         Nothing -> do
--             putStrLn "No se encontró una solución."
--             return []




-- -- Representa el estado enriquecido de una celda.
-- -- data CSPCell = CSPStatic Int           -- Valor fijo del tablero inicial.
-- --              | CSPFixed Int            -- Valor resuelto en la solución.
-- --              | CSPPossibles [Int]      -- Lista de valores posibles (1-9).
-- --   deriving (Show, Eq)

-- data CSPCell = CSPStatic Int Int Int  -- valor, fila, columna
--              | CSPFixed Int Int Int   -- valor, fila, columna
--              | CSPPossibles [Int] Int Int  -- posibles valores, fila, columna
--   deriving (Show, Eq)



-- -- Representa el tablero CSP enriquecido.
-- type CSPBoard = [[CSPCell]]

-- -- Convierte un tablero inicial en un CSPBoard.
-- -- parseCSPBoard :: Board -> CSPBoard
-- -- parseCSPBoard = map (map parseCell)
-- --   where
-- --     parseCell :: Maybe Int -> CSPCell
-- --     parseCell (Just n) = CSPStatic n
-- --     parseCell Nothing  = CSPPossibles [1..9]

-- parseCSPBoard :: Board -> CSPBoard
-- parseCSPBoard board = 
--     [[parseCell r c (board !! r !! c) | c <- [0..8]] | r <- [0..8]]
--   where
--     parseCell :: Int -> Int -> Maybe Int -> CSPCell
--     parseCell r c (Just n) = CSPStatic n r c
--     parseCell r c Nothing  = CSPPossibles [1..9] r c



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
--         CSPStatic n r' c'     -> CSPStatic n r' c'
--         CSPFixed n r' c'      -> CSPFixed n r' c'
--         CSPPossibles _ r' c'  -> CSPPossibles (filterByRules r c b) r' c'

-- -- Filtra los valores posibles de una celda según las reglas.
-- filterByRules :: Int -> Int -> CSPBoard -> [Int]
-- filterByRules r c board =
--   let rowVals = getRow r board
--       colVals = getCol c board
--       blockVals = getBlock r c board
--       usedVals = rowVals ++ colVals ++ blockVals
--   in [x | x <- [1..9], x `notElem` usedVals]

-- -- -- Obtiene los valores fijos de la fila.
-- -- getRow :: Int -> CSPBoard -> [Int]
-- -- getRow r board = [ n | CSPStatic n <- board !! r ] ++
-- --                  [ n | CSPFixed n  <- board !! r ]

-- -- -- Obtiene los valores fijos de la columna.
-- -- getCol :: Int -> CSPBoard -> [Int]
-- -- getCol c board = [ n | row <- board, CSPStatic n <- [row !! c] ] ++
-- --                  [ n | row <- board, CSPFixed n  <- [row !! c] ]

-- -- -- Obtiene los valores fijos del bloque 3x3.
-- -- getBlock :: Int -> Int -> CSPBoard -> [Int]
-- -- getBlock r c board =
-- --   let startRow = (r `div` 3) * 3
-- --       startCol = (c `div` 3) * 3
-- --   in [ n | i <- [0..2], j <- [0..2],
-- --            CSPStatic n <- [board !! (startRow + i) !! (startCol + j)] ] ++
-- --      [ n | i <- [0..2], j <- [0..2],
-- --            CSPFixed n  <- [board !! (startRow + i) !! (startCol + j)] ]

-- getRow r board = [ n | CSPStatic n _ _ <- board !! r ] ++
--                  [ n | CSPFixed n _ _  <- board !! r ]

-- getCol c board = [ n | row <- board, CSPStatic n _ _ <- [row !! c] ] ++
--                  [ n | row <- board, CSPFixed n _ _  <- [row !! c] ]

-- getBlock r c board =
--   let startRow = (r `div` 3) * 3
--       startCol = (c `div` 3) * 3
--   in [ n | i <- [0..2], j <- [0..2],
--            CSPStatic n _ _ <- [board !! (startRow + i) !! (startCol + j)] ] ++
--      [ n | i <- [0..2], j <- [0..2],
--            CSPFixed n _ _  <- [board !! (startRow + i) !! (startCol + j)] ]


-- -- Selecciona la siguiente celda a resolver.
-- -- selectNextCell :: CSPBoard -> Maybe (Int, Int, [Int])
-- -- selectNextCell board = do
-- --   let candidates = [ (r, c, ps)
-- --                    | r <- [0..8], c <- [0..8],
-- --                      CSPPossibles ps <- [board !! r !! c] ]
-- --   if null candidates
-- --     then Nothing
-- --     else Just $ minimumBy compareCells candidates
-- --   where
-- --     compareCells (_, _, ps1) (_, _, ps2) = compare (length ps1) (length ps2)


-- -- selectNextCell :: CSPBoard -> Maybe (Int, Int, [Int])
-- -- selectNextCell board = do
-- --   let candidates = [ (r, c, ps)
-- --                    | r <- [0..8], c <- [0..8],
-- --                      case board !! r !! c of
-- --                        CSPPossibles ps -> True
-- --                        _ -> False,
-- --                      let CSPPossibles ps = board !! r !! c ]
-- --   if null candidates
-- --     then Nothing
-- --     else Just $ minimumBy compareCells candidates
-- --   where
-- --     compareCells (_, _, ps1) (_, _, ps2) = compare (length ps1) (length ps2)


-- -- selectNextCell :: CSPBoard -> Maybe (Int, Int, [Int])
-- -- selectNextCell board = do
-- --   let candidates = [ (r, c, ps)
-- --                    | (r, row) <- zip [0..] board,
-- --                      (c, cell) <- zip [0..] row,
-- --                      case cell of
-- --                        CSPPossibles ps r' c' -> [(r', c', ps)]
-- --                        _ -> [] ]
-- --   if null candidates
-- --     then Nothing
-- --     else Just $ minimumBy (\(_, _, ps1) (_, _, ps2) -> compare (length ps1) (length ps2)) (concat candidates)

-- -- selectNextCell :: CSPBoard -> Maybe (Int, Int, [Int])
-- -- selectNextCell board = do
-- --   let candidates = [ (r, c, possibles)
-- --                    | (r, row) <- zip [0..] board,
-- --                      (c, CSPPossibles possibles _ _) <- zip [0..] row ]
-- --   if null candidates
-- --     then Nothing
-- --     else Just $ minimumBy (\(_, _, ps1) (_, _, ps2) -> compare (length ps1) (length ps2)) candidates


-- selectNextCell :: CSPBoard -> Maybe (Int, Int, [Int])
-- selectNextCell board = do
--   let candidates = [ (r, c, possibles)
--                  | (r, row) <- zip [0..] board,
--                    (c, CSPPossibles possibles _ _) <- zip [0..] row ]

--   if null candidates
--     then Nothing
--     else Just $ minimumBy (\(_, _, ps1) (_, _, ps2) -> compare (length ps1) (length ps2)) candidates


-- -- Resuelve el CSP usando backtracking.
-- -- solveCSP :: CSPBoard -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- -- solveCSP board solution
-- --   | isComplete board = Just solution
-- --   | otherwise = do
-- --       case selectNextCell board of
-- --         Nothing -> Nothing
-- --         Just (r, c, possibles) -> do
-- --           foldr (tryAssign r c) Nothing possibles
-- --   where
-- --     tryAssign :: Int -> Int -> Int -> Maybe [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- --     tryAssign r c val acc = case acc of
-- --       Just s -> Just s
-- --       Nothing -> do
-- --         let newBoard = updatePossibilities $ applyAssignment board (r, c, val)
-- --         let newSolution = solution ++ [(r, c, val)]
-- --         solveCSP newBoard newSolution


-- -- solveCSP :: CSPBoard -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- -- solveCSP board solution =
-- --   let initialSolution = [(r, c, n) | r <- [0..8], c <- [0..8], CSPStatic n <- [board !! r !! c]]
-- --       fullSolution = solution ++ initialSolution
-- --   in if isComplete board
-- --      then Just fullSolution
-- --      else case selectNextCell board of
-- --        Nothing -> Nothing
-- --        Just (r, c, possibles) -> foldr (tryAssign r c fullSolution) Nothing possibles
-- --   where
-- --     tryAssign :: Int -> Int -> [(Int, Int, Int)] -> Int -> Maybe [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- --     tryAssign r c fullSolution val acc = case acc of
-- --       Just s -> Just s
-- --       Nothing ->
-- --         case board !! r !! c of
-- --           CSPStatic _ -> Nothing  -- No intentar asignar a celdas estáticas
-- --           _ -> do
-- --             let newBoard = updatePossibilities $ applyAssignment board (r, c, val)
-- --             let newSolution = fullSolution ++ [(r, c, val)]
-- --             solveCSP newBoard newSolution

-- -- solveCSP :: CSPBoard -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- -- solveCSP board solution =
-- --   if isComplete board
-- --   then Just solution
-- --   else case selectNextCell board of
-- --     Nothing -> Nothing
-- --     Just (r, c, possibles) -> foldr (tryAssign r c) Nothing possibles
-- --   where
-- --     tryAssign :: Int -> Int -> Int -> Maybe [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- --     tryAssign r c val acc = case acc of
-- --       Just s -> Just s
-- --       Nothing ->
-- --         case board !! r !! c of
-- --           CSPStatic _ -> Nothing  -- No intentar asignar a celdas estáticas
-- --           CSPFixed _ -> Nothing   -- No intentar asignar a celdas ya resueltas
-- --           CSPPossibles _ -> do
-- --             let newBoard = updatePossibilities $ applyAssignment board (r, c, val)
-- --             let newSolution = solution ++ [(r, c, val)]
-- --             solveCSP newBoard newSolution


-- -- solveCSP :: CSPBoard -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- -- solveCSP board solution =
-- --   if isComplete board
-- --   then Just solution
-- --   else case selectNextCell board of
-- --     Nothing -> Nothing
-- --     Just (r, c, possibles) -> foldr (tryAssign r c) Nothing possibles
-- --   where
-- --     tryAssign :: Int -> Int -> Int -> Maybe [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- --     tryAssign r c val acc = case acc of
-- --       Just s -> Just s
-- --       Nothing -> do
-- --         let newBoard = updatePossibilities $ applyAssignment board r c val
-- --         let newSolution = solution ++ [(r, c, val)]
-- --         solveCSP newBoard newSolution

-- -- solveCSP :: CSPBoard -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- -- solveCSP board solution =
-- --   if isComplete board
-- --   then Just solution
-- --   else case selectNextCell board of
-- --     Nothing -> Nothing
-- --     Just (r, c, possibles) -> foldr (tryAssign r c) Nothing possibles
-- --   where
-- --     tryAssign :: Int -> Int -> Int -> Maybe [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- --     tryAssign r c val acc = case acc of
-- --       Just s -> Just s
-- --       Nothing -> 
-- --         case board !! r !! c of
-- --           CSPPossibles _ _ _ -> do
-- --             let newBoard = updatePossibilities $ applyAssignment board r c val
-- --             let newSolution = solution ++ [(r, c, val)]
-- --             solveCSP newBoard newSolution
-- --           _ -> Nothing  -- No intentar asignar a celdas que no son CSPPossibles

-- solveCSP :: CSPBoard -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
-- solveCSP board solution =
--   if isComplete board
--   then Just solution
--   else case selectNextCell board of
--     Nothing -> Nothing
--     Just (r, c, possibles) -> foldr (tryAssign r c) Nothing possibles
--   where
--     tryAssign :: Int -> Int -> Int -> Maybe [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
--     tryAssign r c val acc = case acc of
--       Just s -> Just s
--       Nothing -> do
--         let newBoard = updatePossibilities $ applyAssignment board r c val
--         let newSolution = solution ++ [(r, c, val)]
--         solveCSP newBoard newSolution


-- -- Verifica si el tablero está completo.
-- -- isComplete :: CSPBoard -> Bool
-- -- isComplete = all (all isFixed)
-- --   where
-- --     isFixed (CSPStatic _) = True
-- --     isFixed (CSPFixed _)  = True
-- --     isFixed _             = False

-- isComplete :: CSPBoard -> Bool
-- isComplete = all (all isFixed)
--   where
--     isFixed (CSPStatic _ _ _) = True
--     isFixed (CSPFixed _ _ _) = True
--     isFixed _ = False




-- -- Asigna un valor fijo a una celda.
-- -- applyAssignment :: CSPBoard -> (Int, Int, Int) -> CSPBoard
-- -- applyAssignment board (r, c, val) =
-- --   [ [ if i == r && j == c
-- --         then CSPFixed val
-- --         else updateCell (board !! i !! j)
-- --     | j <- [0..8] ]
-- --   | i <- [0..8] ]
-- --   where
-- --     updateCell cell = case cell of
-- --       CSPPossibles ps -> CSPPossibles (filter (/= val) ps)
-- --       _ -> cell

-- -- applyAssignment :: CSPBoard -> (Int, Int, Int) -> CSPBoard
-- -- applyAssignment board (r, c, val) =
-- --     [[ if i == r && j == c
-- --         then case board !! i !! j of
-- --             CSPStatic n -> CSPStatic n  -- Mantiene el valor estático
-- --             CSPPossibles _ -> CSPFixed val
-- --             x -> x  -- Mantiene cualquier otro valor (incluyendo CSPFixed)
-- --         else board !! i !! j
-- --       | j <- [0..8] ]
-- --     | i <- [0..8] ]


-- -- applyAssignment :: CSPBoard -> (Int, Int, Int) -> CSPBoard
-- -- applyAssignment board (r, c, val) =
-- --     [[ if i == r && j == c
-- --         then case board !! i !! j of
-- --             CSPPossibles _ -> CSPFixed val
-- --             x -> x  -- Mantiene cualquier otro valor (incluyendo CSPStatic y CSPFixed)
-- --         else board !! i !! j
-- --       | j <- [0..8] ]
-- --     | i <- [0..8] ]

-- -- applyAssignment :: CSPBoard -> Int -> Int -> Int -> CSPBoard
-- -- applyAssignment board r c val =
-- --     [[updateCell r' c' cell | (c', cell) <- zip [0..] row] | (r', row) <- zip [0..] board]
-- --   where
-- --     updateCell r' c' cell
-- --       | r' == r && c' == c = CSPFixed val r c
-- --       | otherwise = cell

-- -- applyAssignment :: CSPBoard -> Int -> Int -> Int -> CSPBoard
-- -- applyAssignment board r c val =
-- --     [[updateCell r' c' cell | (c', cell) <- zip [0..] row] | (r', row) <- zip [0..] board]
-- --   where
-- --     updateCell r' c' cell
-- --       | r' == r && c' == c = case cell of
-- --           CSPPossibles _ _ _ -> CSPFixed val r c
-- --           _ -> cell
-- --       | otherwise = cell

-- applyAssignment :: CSPBoard -> Int -> Int -> Int -> CSPBoard
-- applyAssignment board r c val =
--     [[updateCell r' c' cell | (c', cell) <- zip [0..] row] | (r', row) <- zip [0..] board]
--   where
--     updateCell r' c' cell
--       | r' == r && c' == c = case cell of
--           CSPPossibles _ _ _ -> CSPFixed val r' c'
--           _ -> cell
--       | otherwise = cell


 