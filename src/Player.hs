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

data Player = Player { _bounding :: Shape, _velocity :: (Double,Double), _direction :: Double, _alive :: Bool, _image :: IO SDL.Texture }
makeLenses ''Player

playerHeight = 20
playerWidth = 20

initialize :: SDL.Renderer -> (Int,Int) -> Player
initialize r (x,y) = Player (Rectangle x y playerWidth playerHeight) (0,2) 0 True (loadImage r "data/Asteroids_Spaceship.bmp")

update :: KeyboardState -> [A.Asteroid] -> Player -> Player
update kS asts = execState (updateS kS asts)

updateS :: KeyboardState -> [A.Asteroid] -> State Player ()
updateS kS asts = do
    direction -= if isDown kS SDL.ScancodeLeft then 1.0 / 9.0 else 0
    direction += if isDown kS SDL.ScancodeRight then 1.0 / 9.0 else 0
    direction %= (`mod'` (2*pi))
    dir <- use direction
    when (isDown kS SDL.ScancodeUp) $ do
        velocity._1 += cos dir
        velocity._2 += sin dir
    modify move
    modify wrap

draw :: SDL.Renderer -> Player -> IO ()
draw r (Player (Rectangle x y _ _)  _ dir _ image) = do
    img <- image
    drawImage r img (x, y) dir

move :: Player -> Player
move p = (bounding.y +~ floor (p^.velocity._2)) . (bounding.x +~ floor (p^.velocity._1)) $ p

wrap :: Player -> Player
wrap = (bounding.y %~ (`mod` 480)) . (bounding.x %~ (`mod` 640))

