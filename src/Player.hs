{-# LANGUAGE TemplateHaskell, Arrows #-}

module Player hiding (playerDirection,playerPosition,playerImage,inputRotation,inputAcceleration) where

import Base.GraphicsManager
import Base.InputHandler
import Base.Geometry
import qualified SDL
import Control.Lens
import Linear
import Linear.Affine

data Player = Player 
    { 
        _bounding  :: Rectangle
      , _direction :: Double
    }

makeLenses ''Player

player :: Wire (Timed NominalDiffTime ()) () IO [SDL.Scancode] Player
player = proc keysDown -> do
    rect <- playerPosition -< keysDown
    direction <- playerDirection -< keysDown
    returnA -< Player rect direction

playerDirection = integral 0 . inputRotation

playerPosition :: Wire (Timed NominalDiffTime ()) () IO [SDL.Scancode] Rectangle
playerPosition = proc keysDown -> do
    thrust <- inputAcceleration -< keysDown
    direction <- playerDirection -< keysDown
    let accel = (thrust * cos direction,thrust * sin direction)
    (x,y) <- integral (320,240) . integral (0,0) -< accel
    returnA -< wrap (R (P (V2 x y)) (V2 64 64))

playerImage r = loadImage r "data/Asteroids_Spaceship.bmp"

renderPlayer r p = do
    img <- playerImage r
    drawImage r img (p^.bounding^.pos) (p^.direction)

inputRotation =      pi . when (keyDown SDL.ScancodeRight)
                <|> -pi . when (keyDown SDL.ScancodeLeft)
                <|>  0


inputAcceleration  =     100 . when (keyDown SDL.ScancodeUp)
                     <|> 0
