module Views.Renderer where

import Graphics.Gloss
import Common
import Views.Board (drawBoardWithGrid)
import Views.UI (drawButton, styledText, ButtonConfig(..), splitMessage)

-- Renderiza la pantalla según el estado actual.
render :: GameState -> Picture
render state = case screen state of
    StartScreen   -> drawStartScreen state
    GameScreen    -> drawGameScreen state
    CreditsScreen -> drawCreditsScreen state
    RulesScreen   -> drawRulesScreen state
    EndScreen     -> drawEndScreen state

lightLila = makeColorI 230 210 255 255
lighterLila = makeColorI 245 235 255 255
darkViolet = makeColorI 25 0 68 255
borde = makeColorI 116 66 201 255
    
-- Pantalla de inicio
drawStartScreen :: GameState -> Picture
drawStartScreen state =
    let title = getImage "startTitle" (images state)
    in pictures
      [ 
        renderImage title (0) 130 2.45 2.45, -- Imagen centrada y escalada
        drawButton startButtonConfig (-500) (-250) " Credits " False,
        drawButton startPlayButtonConfig (0) (-280) " Play " False,     --los espacios son para poder centrar la imagen
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
        drawButton gameButtonConfig (-575) (300) "Back" False,
        drawButton gameButtonConfig (-575) (200) "New Game  " False,
        drawButton gameButtonConfig (-575) (100) "Clear Board" False,
        drawButton gameButtonConfig (-575) (0) " AutoSolve " False,
        drawButton gameButtonConfig (-575) (-100) "  Continue..." False
      ]

-- Pantalla de fin del juego
drawEndScreen :: GameState -> Picture
drawEndScreen state =
    let win = getImage "winTitle" (images state)
    in pictures
      [  
        color darkViolet $ rectangleSolid 2500 1500, -- Fondo de color personalizado
        renderImage win (150) (0) 0.9 0.9, -- Imagen centrada y escalada
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
        translate (-700) (-60) $ styledText 0.18 0.5 black "Lia Stephanie Lopez Rosales  C312 ",
        translate (-700) (-100) $ styledText 0.18 0.5 black "Raidel Miguel Cabellud Lizaso   C311 ",
        drawButton gameButtonConfig (0) (-290) "Back" False
      ]

-- Pantalla de reglas
drawRulesScreen :: GameState -> Picture
drawRulesScreen state = 
    let elements = getImage "sudokuElements" (images state)
        rulesText = 
          [ 
            "1. Fill the empty block with a single number from 1 to 9.",
            "2. Numbers cannot be repeated in the same row.",
            "3. Numbers cannot be repeated in the same column.",
            "4. Numbers cannot be repeated in the same square.",
            "5. Each Sudoku puzzle has a unique solution.",
            "6. Have fun and challenge your mind!"
          ]
    in pictures
      [ 
        renderImage elements (500) (0) 1.0 1.0, -- Imagen centrada y escalada
        translate (-150) 250 $ styledText 1.0 1.0 borde "Rules",
        renderTextBlock (-650, 100) 50 black 0.20 0.5 rulesText, -- Bloque de texto
        drawButton gameButtonConfig (0) (-290) "Back" False
      ]

-- Dibuja un mensaje con estilo
drawMessage :: Float -> Float -> Float -> Color -> String -> Picture
drawMessage x y scaleSize color message =
    translate x y $ styledText scaleSize 0.8 color message

-- Usar esta función para colocar un mensaje en la pantalla de juego
gameMessage :: String -> Picture
gameMessage msg = 
    let maxWidth = 300  -- Ancho máximo permitido para el mensaje
        charWidth = 10  -- Ancho aproximado de cada carácter
        lines = splitMessage maxWidth charWidth msg
        lineHeight = 30 -- Espacio entre líneas
        xInicial = -795 -- Posición horizontal inicial
        anchoDisponible = 350 -- Ancho del espacio disponible
    in pictures $ zipWith (\i line -> 
            let anchoTexto = fromIntegral (length line) * charWidth
                xCentered = xInicial + (anchoDisponible - anchoTexto) / 2
            in drawMessage xCentered (-300 - i * lineHeight) 0.22 red line
        ) [0..] lines

creditsMessage :: String -> Picture
creditsMessage msg = drawMessage (-750) (-250) 0.3 darkViolet msg

getImage :: String -> [(String, Picture)] -> Picture
getImage name imgs = case lookup name imgs of
    Just img -> img
    Nothing  -> blank -- Imagen predeterminada si no se encuentra
    
renderImage :: Picture -> Float -> Float -> Float -> Float -> Picture
renderImage img x y scaleX scaleY =
    translate x y $ scale scaleX scaleY img

-- Función para renderizar varias líneas de texto
renderTextBlock :: (Float, Float) -> Float -> Color -> Float -> Float -> [String] -> Picture
renderTextBlock (startX, startY) lineSpacing color scaleSize scaleThickness lines =
    pictures $ zipWith (\i line -> translate startX (startY - i * lineSpacing) $ styledText scaleSize scaleThickness color line) [0..] lines
    
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
