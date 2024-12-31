module Graphics.Renderer where

import Graphics.Gloss
import Common
import Graphics.Board (drawBoardWithGrid)
import Graphics.UI (drawButton, styledText, ButtonConfig(..))

-- Renderiza la pantalla según el estado actual.
render :: GameState -> Picture
render state = case screen state of
    StartScreen   -> drawStartScreen state
    GameScreen    -> drawGameScreen state
    -- CreditsScreen -> drawCreditsScreen
    -- RulesScreen   -> drawRulesScreen
    -- EndScreen     -> drawEndScreen state

lightLila = makeColorI 230 210 255 255
lighterLila = makeColorI 245 235 255 255 -- Tonalidad más clara de violeta
    
-- Pantalla de inicio
drawStartScreen :: GameState -> Picture
drawStartScreen state =
    let title = getImage "startTitle" (images state)
    in pictures
      [ 
        color lighterLila $ rectangleSolid 2500 1500, -- Fondo de color personalizado
        renderImage title (0) 150 2.0 2.0, -- Imagen centrada y escalada
        drawButton startButtonConfig (-500) (-200) "Creditos" False,
        drawButton startPlayButtonConfig (0) (-250) "Jugar" False,
        drawButton startButtonConfig (450) (-200) "Reglas" False
      ]
    
--Pantalla de juego
drawGameScreen :: GameState -> Picture
drawGameScreen state = 
    pictures
      [ 
        color lighterLila $ rectangleSolid 2500 1500,
        drawBoardWithGrid --parámetros para dibujar el tablero enmarcado
            (board state)
            (initialBoard state)
            (selectedCell state)
            (-6.5 * cellSize)   -- offsetX del tablero
            (7.40 * cellSize) -- offsetY del tablero
            cellSize,
        gameMessage (message state),
        -- pictures [drawButton gameButtonConfig x y label False | (x, y, label) <- buttonPositions]
        drawButton gameButtonConfig (-575) (150) "Inicio" False,
        drawButton gameButtonConfig (-575) (50) "Nuevo Juego" False,
        drawButton gameButtonConfig (-575) (-50) "Reiniciar" False
      ]

-- Dibuja un mensaje con estilo
drawMessage :: Float -> Float -> Float -> Color -> String -> Picture
drawMessage x y scaleSize color message =
  translate x y $ styledText scaleSize color message

-- Usar esta función para colocar un mensaje en la pantalla de juego
gameMessage :: String -> Picture
gameMessage msg = drawMessage (-750) (-250) 0.3 red msg

getImage :: String -> [(String, Picture)] -> Picture
getImage name imgs = case lookup name imgs of
    Just img -> img
    Nothing  -> blank -- Imagen predeterminada si no se encuentra
    
renderImage :: Picture -> Float -> Float -> Float -> Float -> Picture
renderImage img x y scaleX scaleY =
    translate x y $ scale scaleX scaleY img
    
-- Configuración de botones para la pantalla de inicio
startButtonConfig :: ButtonConfig
startButtonConfig = ButtonConfig
  { buttonWidth = 200
  , buttonHeight = 70
  , buttonRadius = 20
  , buttonBaseColor = lightLila
  , buttonHighlightColor = makeColorI 200 180 255 255
  , buttonTextColor = black
  , buttonTextScale = 0.32
  }

startPlayButtonConfig :: ButtonConfig
startPlayButtonConfig = ButtonConfig
  { 
    buttonWidth = 250
  , buttonHeight = 85
  , buttonRadius = 22
  , buttonBaseColor = lightLila
  , buttonHighlightColor = makeColorI 200 180 255 255
  , buttonTextColor = black
  , buttonTextScale = 0.38
  }

-- Configuración específica para los botones de la pantalla de juego
gameButtonConfig :: ButtonConfig
gameButtonConfig = ButtonConfig
  { buttonWidth = 250
  , buttonHeight = 70
  , buttonRadius = 25
  , buttonBaseColor = makeColorI 180 160 250 255
  , buttonHighlightColor = makeColorI 200 180 255 255
  , buttonTextColor = black
  , buttonTextScale = 0.25
  }






-- Pantalla de créditos
-- drawCreditsScreen :: Picture
-- drawCreditsScreen = pictures
--   [ 
--     translate (-200) 200 $ scale 0.2 0.2 $ color blue $ text "Créditos",
--     translate (-150) 100 $ scale 0.1 0.1 $ text "Creado por: Juan, Maria, y Ana",
--     drawButton (-100) (-100) "Volver"
--   ]

-- Pantalla de reglas
-- drawRulesScreen :: Picture
-- drawRulesScreen = pictures
--   [ 
--     translate (-200) 200 $ scale 0.2 0.2 $ color blue $ text "Reglas",
--     translate (-250) 100 $ scale 0.1 0.1 $ text "Completa el tablero sin repetir números por fila, columna y subcuadrante.",
--     drawButton (-100) (-100) "Volver"
--   ]

-- Pantalla de fin del juego
-- drawEndScreen :: GameState -> Picture
-- drawEndScreen state = pictures
--   [  
--     translate (-200) 200 $ scale 0.3 0.3 $ color green $ text "¡Ganaste!",
--     drawButton (-100) 50 "Volver a Inicio",
--     drawButton (-100) (-50) "Nuevo Juego"
--   ]
