
module Logic.CSP where

import Common.Types (GameState(..), Board)
import Data.List (minimumBy,sortBy)
import Data.Ord (comparing)
import Data.Maybe (isJust,listToMaybe)
import System.Random (randomRIO)
import Control.Applicative ((<|>))


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
        in if null candidates
            then Nothing
            else Just (minimumBy (comparing (\(r, c) -> length (csp !! r !! c))) candidates)


    tryValues :: CSP -> Int -> Int -> [Int] -> Maybe Board
    tryValues csp row col values =
        let orderedValues = sortBy (comparing (constraints csp row col)) values
        in tryValuesOrdered csp row col orderedValues
        where
        tryValuesOrdered :: CSP -> Int -> Int -> [Int] -> Maybe Board
        tryValuesOrdered _ _ _ [] = Nothing
        tryValuesOrdered csp row col (v:vs) =
            case assignValue csp row col v of
                Nothing -> tryValuesOrdered csp row col vs -- Si falla, intenta el siguiente valor
                Just newCSP ->
                    let propagatedCSP = ac3 newCSP
                    in if not (isCSPValid propagatedCSP)
                        then tryValuesOrdered csp row col vs -- Retrocede si el CSP es inválido
                        else solveCSP propagatedCSP <|> tryValuesOrdered csp row col vs

        constraints :: CSP -> Int -> Int -> Int -> Int
        constraints csp row col value =
            length [() | (r, c) <- affectedCells row col,
                        value `elem` (csp !! r !! c)]

        affectedCells r c =
            [(r', c') | r' <- [0..8], c' <- [0..8],
                        (r' == r || c' == c || inSameSubGrid (r, c) (r', c')) &&
                        (r', c') /= (r, c)]


    assignValue :: CSP -> Int -> Int -> Int -> Maybe CSP
    assignValue csp row col value =
        let updatedCSP = replaceAt csp row col [value]
        in if any (any null) updatedCSP
            then Nothing -- Devuelve Nothing si algún dominio queda vacío
            else Just updatedCSP



    cspToBoard :: CSP -> Board
    cspToBoard csp = map (map toMaybe) csp
      where
        toMaybe [v] = Just v
        toMaybe _ = Nothing

solveBoardWithCSP :: Board -> Maybe Board
solveBoardWithCSP board =
    solveCSP (ac3Dynamic (nodeConsistency (initializeCSP board)))

nodeConsistency :: CSP -> CSP
nodeConsistency csp =
    foldl propagateNode csp allCells
  where
    allCells = [(row, col) | row <- [0..8], col <- [0..8]]

    propagateNode :: CSP -> (Int, Int) -> CSP
    propagateNode csp (row, col)
        | length (csp !! row !! col) == 1 = -- Si ya está asignado
            eliminate csp (row, col) (head (csp !! row !! col))
        | otherwise = csp

ac3Dynamic :: CSP -> CSP
ac3Dynamic csp = ac3' csp initialQueue
  where
    initialQueue = uniqueConstraints [(r, c, r', c') | r <- [0..8], c <- [0..8],
                                                       r' <- [0..8], c' <- [0..8],
                                                       (r /= r' || c /= c') &&
                                                       (r == r' || c == c' || inSameSubGrid (r, c) (r', c'))]

    ac3' csp [] = csp
    ac3' csp ((r1, c1, r2, c2):queue) =
        let (newCSP, changed) = revise csp (r1, c1) (r2, c2)
        in if changed
            then ac3' newCSP (uniqueConstraints (queue ++ [(r, c, r1, c1) | (r, c) <- neighbors (r1, c1), (r, c) /= (r2, c2)]))
            else ac3' csp queue

    uniqueConstraints = foldr (\x acc -> if x `elem` acc then acc else x : acc) []


    revise :: CSP -> (Int, Int) -> (Int, Int) -> (CSP, Bool)
    revise csp (r1, c1) (r2, c2) =
        let d1 = csp !! r1 !! c1
            d2 = csp !! r2 !! c2
            revisedDomain = filter (\x -> any (/= x) d2) d1
        in if revisedDomain /= d1
            then (replaceAt csp r1 c1 revisedDomain, True)
            else (csp, False)


    neighbors (r, c) =
        [(r', c') | r' <- [0..8], c' <- [0..8],
                    (r' == r || c' == c || inSameSubGrid (r, c) (r', c')) &&
                    (r', c') /= (r, c)]

eliminate :: CSP -> (Int, Int) -> Int -> CSP
eliminate csp (row, col) value =
    foldl update csp (affectedCells row col)
  where
    affectedCells r c =
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

inSameSubGrid (r1, c1) (r2, c2) =
        (r1 `div` 3 == r2 `div` 3) && (c1 `div` 3 == c2 `div` 3)

isCSPValid :: CSP -> Bool
isCSPValid = all (all (not . null)) -- Verifica que no haya dominios vacíos