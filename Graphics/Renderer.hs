module Graphics.Renderer where

import Graphics.Gloss
import Common
import Graphics.Board (drawBoard)
import Graphics.UI (drawButton)

-- Renderiza la pantalla según el estado actual.
render :: GameState -> Picture
render state = case screen state of
  StartScreen   -> drawStartScreen
  GameScreen    -> drawGameScreen state
  CreditsScreen -> drawCreditsScreen
  RulesScreen   -> drawRulesScreen
  EndScreen     -> drawEndScreen state

-- Pantalla de inicio
drawStartScreen :: Picture
drawStartScreen = pictures
  [ translate (-200) 200 $ scale 0.3 0.3 $ color blue $ text "Sudoku"
  , drawButton (-100) 50 "Jugar"
  , drawButton (-100) (-50) "Créditos"
  , drawButton (-100) (-150) "Reglas"
  ]

-- Pantalla del juego
drawGameScreen :: GameState -> Picture
drawGameScreen state = pictures
  [ drawBoard (board state) (initialBoard state) (selectedCell state) -- Tablero
  , drawButton (-250) 200 "Inicio"                                   -- Botones
  , drawButton (-250) 150 "Nuevo Juego"
  , drawButton (-250) 100 "Reiniciar"
  , translate (-200) (-250) $ scale 0.1 0.1 $ color red $ text (message state) -- Mensaje
  ]

-- Pantalla de créditos
drawCreditsScreen :: Picture
drawCreditsScreen = pictures
  [ translate (-200) 200 $ scale 0.2 0.2 $ color blue $ text "Créditos"
  , translate (-150) 100 $ scale 0.1 0.1 $ text "Creado por: Juan, Maria, y Ana"
  , drawButton (-100) (-100) "Volver"
  ]

-- Pantalla de reglas
drawRulesScreen :: Picture
drawRulesScreen = pictures
  [ translate (-200) 200 $ scale 0.2 0.2 $ color blue $ text "Reglas"
  , translate (-250) 100 $ scale 0.1 0.1 $ text "Completa el tablero sin repetir números por fila, columna y subcuadrante."
  , drawButton (-100) (-100) "Volver"
  ]

-- Pantalla de fin del juego
drawEndScreen :: GameState -> Picture
drawEndScreen state = pictures
  [ translate (-200) 200 $ scale 0.3 0.3 $ color green $ text "¡Ganaste!"
  , drawButton (-100) 50 "Volver a Inicio"
  , drawButton (-100) (-50) "Nuevo Juego"
  ]
