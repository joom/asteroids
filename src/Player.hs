{-# LANGUAGE TemplateHaskell #-}

module Player where

import Base.GraphicsManager
import Base.InputHandler
import Base.Geometry
import qualified Asteroid as A
import qualified SDL
import Control.Monad.State
import Control.Lens
import Control.Monad(when)
import Data.Fixed(mod')
import Linear
import Linear.Affine

data Player = Player 
    { 
        _bounding  :: Rectangle
      , _velocity  :: V2 Double
      , _direction :: Double
      , _alive     :: Bool
      , _image     :: IO SDL.Texture
    }

makeLenses ''Player

playerSize = V2 64 64

initialize :: SDL.Renderer -> Point V2 Int -> Player
initialize r pos = Player (R pos playerSize) 
     (V2 0 0) 0 True (loadImage r "data/Asteroids_Spaceship.bmp")

update :: KeyboardState -> [A.Asteroid] -> Player -> Player
update kS asts = execState (updateS kS asts)

updateS :: KeyboardState -> [A.Asteroid] -> State Player ()
updateS kS asts = do
    direction -= if isDown kS SDL.ScancodeLeft then 1.0 / 9.0 else 0
    direction += if isDown kS SDL.ScancodeRight then 1.0 / 9.0 else 0
    direction %= (`mod'` (2*pi))
    dir <- use direction
    when (isDown kS SDL.ScancodeUp) $ do
        velocity._x += cos dir
        velocity._y += sin dir
    modify move
    bounding %= wrap

draw :: SDL.Renderer -> Player -> IO ()
draw r p = do
    img <- p^.image
    drawImage r img (p^.bounding^.pos) (p^.direction)

move :: Player -> Player
move p = bounding.pos.lensP +~ fmap floor (p^.velocity) $ p
