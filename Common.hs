module Common where

import Graphics.Gloss

-- Representa las diferentes pantallas del juego.
data Screen
    = StartScreen
    | GameScreen
    | CreditsScreen
    | RulesScreen
    | EndScreen
  deriving (Eq)

-- Representa el estado del juego completo.
data GameState = GameState
  { 
    screen :: Screen           -- Pantalla actual.
  , board :: Board             -- Tablero actual.
  , initialBoard :: Board      -- Tablero inicial generado.
  , selectedCell :: (Int, Int) -- Celda seleccionada.
  , message :: String          -- Mensaje para el usuario.
  , isWin :: Bool              -- Indicador de victoria.
  , images :: [(String, Picture)] -- Lista de imágenes cargadas
  }

-- Matriz del tablero, donde Nothing es una celda vacía.
type Board = [[Maybe Int]]

-- Tamaño de la celda en píxeles.
cellSize :: Float
cellSize = 85.0
