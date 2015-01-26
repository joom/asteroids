{-# LANGUAGE TemplateHaskell #-}

module Projectile where
import Base.GraphicsManager(drawCircle)
import qualified SDL
import Base.Geometry
import Control.Lens

data Projectile = Projectile {
                  _bounding :: Shape
                , _vel :: (Int,Int)
                }

makeLenses ''Projectile

update :: Projectile -> Projectile
update p@(Projectile r@(Rectangle x y _ _) (dx,dy)) = p { _bounding = newR }
    where
        newR = r { _rectX=x+dx, _rectY=y+dy }

draw :: SDL.Renderer -> Projectile -> IO ()
draw r (Projectile (Rectangle x y _ _) _) = drawCircle r (x, y)

offScreen :: Projectile -> Bool
offScreen (Projectile (Rectangle x y _ _) _) = x < 0 || x > 640 || y < 0 || y > 480
