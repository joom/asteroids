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
		  _vel :: V2 Int,
                  _level :: Int
	      } deriving (Eq)

makeLenses ''Asteroid

levelSize :: Int -> Int
levelSize = floor . (2**) . fromIntegral . (+3)

asteroidInitialize :: Point V2 Int -> V2 Int -> Int -> Asteroid
asteroidInitialize pos vel lvl  =  Asteroid (R pos size) vel lvl
    where
      size = V2 (levelSize lvl) (levelSize lvl)

update :: Asteroid -> Asteroid
update a = (bounding.size .~ V2 newSize newSize) . move $ a
    where
      newSize = levelSize (a^.level)
draw :: Renderer -> Asteroid -> IO ()
draw r a = drawRect r (a^.bounding) (V4 255 255 255 255)

move :: Asteroid -> Asteroid
move a = (bounding %~ wrap) . (bounding.pos.lensP +~ a^.vel) $ a

