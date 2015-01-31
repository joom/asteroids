{-# LANGUAGE TemplateHaskell #-}

module Projectile where
import Base.GraphicsManager(drawCircle)
import Base.Geometry
import Linear
import Linear.Affine
import Control.Lens
import FRP.Netwire

data Projectile = Projectile {
                  _bounding :: Rectangle
                } deriving (Eq)

makeLenses ''Projectile

projectile :: Point V2 Int -> V2 Int -> Wire s e m a b
projectile pt v = for 60 . projectilePosition pt v

projectilePosition (P (V2 x y)) (V2 dx dy) = proc _ -> do
    pos <- integral (x,y) . (pure (dx,dy)) -< ()
    returnA -< wrap $ R (P (uncurry V2 $ pos)) (V2 1 1)

renderProjectile r p = drawCircle r (p^.bounding^.pos)
