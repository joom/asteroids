{-# LANGUAGE TemplateHaskell #-}

module Asteroid where

import Base.GraphicsManager (drawRect)
import Base.Geometry 
import SDL (Renderer)
import Control.Lens
import Linear
import Linear.Affine

data Asteroid =	Asteroid {
		  _bounding :: Rectangle,
                  _level :: Int
	      } deriving (Eq)

makeLenses ''Asteroid

renderAsteroid :: Renderer -> Asteroid -> IO ()
renderAsteroid r a = drawRect r (a^.bounding) (V4 255 255 255 255)

asteroid :: Point V2 Int -> V2 Int -> Int -> Wire (Timed NominalDiffTime ()) () IO a Asteroid
asteroid (P (V2 x y)) (V2 dx dy) lv = proc _ -> do
    pos <- integral (x,y) . integral (dx,dy) -< ()
    let sz = floor . (2**) . fromIntegral . (+3) $ lv
    let rect = R (P (uncurry V2 $ pos)) (V2 sz sz)
    returnA -< Asteroid (wrap rect) lv

