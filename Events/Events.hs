module Events.Events where

import Common 
import Graphics.Gloss.Interface.Pure.Game
import Events.EventHelpers
import Logic.CSP (solveBoardWithCSP)
import System.IO.Unsafe (unsafePerformIO)
import Logic.Logic (generateNewBoard, isBoardComplete, isWinCondition, isValidMove)


handleEvent :: Event -> GameState -> GameState
handleEvent event state = case screen state of
    StartScreen   -> handleStartScreen event state
    GameScreen    -> handleGameScreen event state
    CreditsScreen -> handleCreditsScreen event state
    RulesScreen   -> handleRulesScreen event state
    EndScreen     -> handleEndScreen event state

handleStartScreen :: Event -> GameState -> GameState
handleStartScreen (EventKey (MouseButton LeftButton) Down _ (mx, my)) state
    | isButtonClicked mx my (0) (-280)   = state { screen = GameScreen }
    | isButtonClicked mx my (-500) (-250) = state { screen = CreditsScreen }
    | isButtonClicked mx my (450) (-250) = state { screen = RulesScreen }
    | otherwise = state
handleStartScreen _ state = state

handleGameScreen :: Event -> GameState -> GameState
handleGameScreen (EventKey (MouseButton LeftButton) Down _ (mx, my)) state
    | isButtonClicked mx my (-575) (300) = state { screen = StartScreen } -- Botón "Volver a Inicio"
    | isButtonClicked mx my (-575) (200) = unsafePerformIO $ generateNewGame state                
    | isButtonClicked mx my (-575) (100) = resetGame state              
    | isButtonClicked mx my (-575) (0)  = solveSudoku state  -- Botón Autosolver
    | -- Verifica si el clic es en una celda del tablero
      isCellClicked mx my = selectCell mx my state
    -- Si no, maneja clics generales en el tablero
    | otherwise = handleBoardClick mx my state

-- Navegación con teclas de flecha
handleGameScreen (EventKey (SpecialKey KeyRight) Down _ _) state =
    state { selectedCell = moveSelection (1, 0) (selectedCell state) }
handleGameScreen (EventKey (SpecialKey KeyLeft) Down _ _) state =
    state { selectedCell = moveSelection (-1, 0) (selectedCell state) }
handleGameScreen (EventKey (SpecialKey KeyUp) Down _ _) state =
    state { selectedCell = moveSelection (0, -1) (selectedCell state) }
handleGameScreen (EventKey (SpecialKey KeyDown) Down _ _) state =
    state { selectedCell = moveSelection (0, 1) (selectedCell state) }

-- Insertar un valor o eliminar el valor de la celda seleccionada
handleGameScreen (EventKey (Char c) Down _ _) state
    | c >= '1' && c <= '9' = checkWinCondition $ insertValue (read [c]) state
    | c == '\b'            = deleteValue state           
    | otherwise            = state          

-- Ignorar otros eventos
handleGameScreen _ state = state

handleCreditsScreen :: Event -> GameState -> GameState
handleCreditsScreen (EventKey (MouseButton LeftButton) Down _ (mx, my)) state
    | isButtonClicked mx my (0) (-290) = state { screen = StartScreen }
    | otherwise = state
handleCreditsScreen _ state = state

handleRulesScreen :: Event -> GameState -> GameState
handleRulesScreen (EventKey (MouseButton LeftButton) Down _ (mx, my)) state
    | isButtonClicked mx my (0) (-290) = state { screen = StartScreen }
    | otherwise = state
handleRulesScreen _ state = state

handleEndScreen :: Event -> GameState -> GameState
handleEndScreen (EventKey (MouseButton LeftButton) Down _ (mx, my)) state
    | isButtonClicked mx my (-570) (-150) = unsafePerformIO $ generateNewGame state { screen = StartScreen }
    | isButtonClicked mx my (-570) (150) = unsafePerformIO $ generateNewGame state { screen = GameScreen }
    | otherwise = state
handleEndScreen _ state = state

checkWinCondition :: GameState -> GameState
checkWinCondition state =
    if isWinCondition (board state)
        then state { screen = EndScreen, isWin = True }
        else state

solveSudoku :: GameState -> GameState
solveSudoku state =
    case solveBoardWithCSP (board state) of
        Just solvedBoard -> state { board = solvedBoard, message = "Sudoku automatically resolved." }
        Nothing          -> state { message = "Solution not found for this board." }



