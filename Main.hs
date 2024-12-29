module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Common
import Logic
import Events
import Graphics

-- Función principal
main :: IO ()
main = play
  (InWindow "Sudoku" (500, 500) (100, 100)) -- Configuración de la ventana
  white                                    -- Fondo blanco
  30                                       -- FPS
  initialState                             -- Estado inicial del juego
  render                                   -- Función de renderizado
  handleEvent                              -- Manejo de eventos
  update                                   -- Actualización del estado (sin usar aquí)
