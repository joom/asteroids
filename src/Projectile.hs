{-# LANGUAGE TemplateHaskell #-}

module Projectile where
import Base.GraphicsManager(drawCircle)
import qualified SDL
import Base.Geometry
import Linear
import Linear.Affine
import Control.Lens
import Control.Monad.State

data Projectile = Projectile {
                  _bounding :: Rectangle
                , _vel :: V2 Int
                , _life :: Int
                } deriving (Eq)

makeLenses ''Projectile

update :: Projectile -> Projectile
update = execState updateS

updateS :: State Projectile ()
updateS = do
    velocity <- use vel
    bounding.pos.lensP += velocity
    bounding %= wrap
    life -= 1

draw :: SDL.Renderer -> Projectile -> IO ()
draw r p = drawCircle r $ p^.bounding^.pos

