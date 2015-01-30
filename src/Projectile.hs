{-# LANGUAGE TemplateHaskell #-}

module Projectile where
import Base.GraphicsManager(drawCircle)
import Base.GameComponent
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

projectile :: Projectile -> GameComponent Projectile
projectile p = GameComponent
             {
               _update = (\_ _ -> projectile $ execState ( do
                   bounding.pos.lensP += p^.vel
                   bounding %= wrap
                   life -= 1) p)
             , _draw   = (\r -> drawCircle r $ p^.bounding^.pos)
             , _value  = p
             }
newProjectile :: Point V2 Int -> V2 Int -> GameComponent Projectile
newProjectile pt v = projectile $ Projectile (R pt (V2 1 1)) v 100
