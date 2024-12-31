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
    CreditsScreen -> drawCreditsScreen state
    -- RulesScreen   -> drawRulesScreen
    EndScreen     -> drawEndScreen state

lightLila = makeColorI 230 210 255 255
lighterLila = makeColorI 245 235 255 255
darkViolet = makeColorI 25 0 68 255
    
-- Pantalla de inicio
drawStartScreen :: GameState -> Picture
drawStartScreen state =
    let title = getImage "startTitle" (images state)
    in pictures
      [ 
        renderImage title (0) 130 2.45 2.45, -- Imagen centrada y escalada
        drawButton startButtonConfig (-500) (-250) " Credits " False,
        drawButton startPlayButtonConfig (0) (-280) " Play " False,
        drawButton startButtonConfig (450) (-250) " Rules " False
      ]
    
--Pantalla de juego
drawGameScreen :: GameState -> Picture
drawGameScreen state = 
    pictures
      [ 
        drawBoardWithGrid --parámetros para dibujar el tablero enmarcado
            (board state)
            (initialBoard state)
            (selectedCell state)
            (-6.5 * cellSize)   -- offsetX del tablero
            (7.40 * cellSize) -- offsetY del tablero
            cellSize,
        gameMessage (message state),
        -- pictures [drawButton gameButtonConfig x y label False | (x, y, label) <- buttonPositions]
        drawButton gameButtonConfig (-575) (150) "Back" False,
        drawButton gameButtonConfig (-575) (50) "New Game  " False,
        drawButton gameButtonConfig (-575) (-50) "Clear Board" False
      ]

-- Pantalla de fin del juego
drawEndScreen :: GameState -> Picture
drawEndScreen state =
    let win = getImage "winTitle" (images state)
    in pictures
      [  
        color darkViolet $ rectangleSolid 2500 1500, -- Fondo de color personalizado
        renderImage win (150) 0 0.9 0.9, -- Imagen centrada y escalada
        drawButton endButtonConfig (-570) (-150) "Go to Start" False,
        drawButton endButtonConfig (-570) (150) "New Game  " False
      ]

-- Pantalla de créditos
drawCreditsScreen :: GameState -> Picture
drawCreditsScreen state = 
    let logo = getImage "schoolLogo" (images state)
    in pictures
      [ 
        renderImage logo (550) (-40) 1.45 1.35, -- Imagen centrada y escalada
        translate (-200) 230 $ styledText 1.0 1.0 borde "Credits",
        translate (-750) 130 $ styledText 0.20 0.5 black "Created by MATCOM's third year students as part of the Declarative Programming subject.",
        translate (-700) 30 $ styledText 0.20 0.6 black "The authors are: ",
        translate (-700) (-20) $ styledText 0.18 0.5 black "Ariadna Velazquez Rey  C311 ",
        translate (-700) (-60) $ styledText 0.18 0.5 black "Lia Stephany Lopez Rozales  C312 ",
        translate (-700) (-100) $ styledText 0.18 0.5 black "Raidel Miguel Cabellud Lizaso   C311 ",

        drawButton gameButtonConfig (0) (-290) "Back" False
      ]


-- Dibuja un mensaje con estilo
drawMessage :: Float -> Float -> Float -> Color -> String -> Picture
drawMessage x y scaleSize color message =
    translate x y $ styledText scaleSize 0.8 color message

-- Usar esta función para colocar un mensaje en la pantalla de juego
gameMessage :: String -> Picture
gameMessage msg = drawMessage (-750) (-250) 0.3 red msg

creditsMessage :: String -> Picture
creditsMessage msg = drawMessage (-750) (-250) 0.3 darkViolet msg

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

borde = makeColorI 116 66 201 255

endButtonConfig :: ButtonConfig
endButtonConfig = ButtonConfig
  { 
    buttonWidth = 350
  , buttonHeight = 85
  , buttonRadius = 22
  , buttonBaseColor = lighterLila
  , buttonHighlightColor = borde
  , buttonTextColor = black
  , buttonTextScale = 0.36
  }
  






-- Pantalla de reglas
-- drawRulesScreen :: Picture
-- drawRulesScreen = pictures
--   [ 
--     translate (-200) 200 $ scale 0.2 0.2 $ color blue $ text "Reglas",
--     translate (-250) 100 $ scale 0.1 0.1 $ text "Completa el tablero sin repetir números por fila, columna y subcuadrante.",
--     drawButton (-100) (-100) "Volver"
--   ]

