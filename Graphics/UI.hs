module Graphics.UI where

import Graphics.Gloss

-- Configuración general para botones
data ButtonConfig = ButtonConfig
  { 
    buttonWidth  :: Float
  , buttonHeight :: Float
  , buttonRadius :: Float
  , buttonBaseColor :: Color
  , buttonHighlightColor :: Color
  , buttonTextColor :: Color
  , buttonTextScale :: Float
  }

-- Dibuja un botón con texto centrado
drawButton :: ButtonConfig -> Float -> Float -> String -> Bool -> Picture
drawButton cfg x y label isHighlighted =
    let baseColor = if isHighlighted 
                    then buttonHighlightColor cfg
                    else buttonBaseColor cfg
        textScale = buttonTextScale cfg
        avgCharWidth = 65 -- Ancho promedio de un carácter a escala base
        textWidth = fromIntegral (length label) * avgCharWidth * textScale
        textXOffset = -textWidth / 2-- Centrar texto horizontalmente
        textYOffset = -(buttonHeight cfg) / 4 -- Centrar texto verticalmente
    in translate x y $
       pictures
          [ 
            translate 5 (-5) $
            color (dark baseColor) $
            roundedRectangle (buttonWidth cfg) (buttonHeight cfg) (buttonRadius cfg)
          , color baseColor $
            roundedRectangle (buttonWidth cfg - 10) (buttonHeight cfg - 10) (buttonRadius cfg)
          , translate textXOffset textYOffset $
            styledText textScale (buttonTextColor cfg) label
          ]

-- Botón ovalado con bordes suaves
roundedRectangle :: Float -> Float -> Float -> Picture
roundedRectangle width height radius =
    let halfWidth = width / 2
        halfHeight = height / 2
    in pictures
         [ translate (-halfWidth + radius) (-halfHeight + radius) $ circleSolid radius
         , translate (halfWidth - radius) (-halfHeight + radius) $ circleSolid radius
         , translate (-halfWidth + radius) (halfHeight - radius) $ circleSolid radius
         , translate (halfWidth - radius) (halfHeight - radius) $ circleSolid radius
         , rectangleSolid (width - 2 * radius) height
         , rectangleSolid width (height - 2 * radius)
         ]

-- Dibuja texto grueso e intenso
styledText :: Float -> Color -> String -> Picture
styledText scaleSize textColor txt =
    let offset = 0.8 -- Ajuste del grosor simulado
        baseText = scale scaleSize scaleSize $ text txt
    in pictures
         [ translate dx dy $ color textColor baseText
         | dx <- [-offset, 0, offset]
         , dy <- [-offset, 0, offset]
         ]

-- Verifica si un botón fue clicado.
isButtonClicked :: Float -> Float -> Float -> Float -> Bool
isButtonClicked mx my bx by =
    mx >= bx - 100 && mx <= bx + 100 && my >= by - 25 && my <= by + 25
