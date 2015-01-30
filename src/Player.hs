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
import Base.GameComponent
import Control.Applicative ((<$>))

data Player = Player 
    { 
        _bounding  :: Rectangle
      , _velocity  :: V2 Double
      , _direction :: Double
      , _alive     :: Bool
      , _image     :: IO SDL.Texture
      , _lastShot  :: Int
    }

makeLenses ''Player

player :: Player -> GameComponent Player
player p = GameComponent
         {
           _update = (\kS _ -> player $ execState ( do
	      direction -= if isDown kS SDL.ScancodeLeft then 1.0 / 9.0 else 0
	      direction += if isDown kS SDL.ScancodeRight then 1.0 / 9.0 else 0
	      direction %= (`mod'` (2*pi))
	      dir <- use direction
	      when (isDown kS SDL.ScancodeUp) $ do
		  velocity._x += cos dir
		  velocity._y += sin dir
	      velocity %= clamp 10.0
	      bounding.pos.lensP += (floor <$> p^.velocity)
	      bounding %= wrap) p )
         , _draw   = (\r -> do
             img <- p^.image
             drawImage r img (p^.bounding^.pos) (p^.direction))
         , _value  = p
         }


playerSize = V2 64 64

defaultPlayer :: SDL.Renderer -> GameComponent Player
defaultPlayer r = player $ Player (R (P $ V2 320 240) playerSize)
     (V2 0 0) 0 True (loadImage r "data/Asteroids_Spaceship.bmp") 0

