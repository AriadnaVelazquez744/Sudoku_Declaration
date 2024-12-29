module Graphics.UI where

import Graphics.Gloss

-- Dibuja un botón interactivo.
drawButton :: Float -> Float -> String -> Picture
drawButton x y label = pictures
  [ translate x y $ color (light blue) $ rectangleSolid 200 50
  , translate (x - 40) (y - 10) $ scale 0.1 0.1 $ color black $ text label
  ]

-- Verifica si un botón fue clicado.
isButtonClicked :: Float -> Float -> Float -> Float -> Bool
isButtonClicked mx my bx by =
  mx >= bx - 100 && mx <= bx + 100 && my >= by - 25 && my <= by + 25
