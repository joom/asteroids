{-# LANGUAGE TemplateHaskell #-}

module Platform where

import Base.GraphicsManager (drawRect)
import Base.Geometry (Shape(..),collides,x,y)
import Graphics.UI.SDL (Surface)
import Control.Lens

data Asteroid =	Asteroid {
		  _bounding :: Shape,
		  _vel :: (Int,Int),
                  _level :: Int
	      }

vel :: Lens' Platform (Int,Int)
vel = lens _vel (\shape v -> shape { _vel = v })

bounding :: Lens' Platform Shape
bounding = lens _bounding (\shape v -> shape { _bounding = v })

level :: Lens' Asteroid Int
level = lens _level (\shape v -> shape { _level = v })

levelSize :: Int -> Int
levelSize = (*5)

asteroidInitialize :: (Int,Int) -> (Int,Int) -> Int -> Asteroid
asteroidInitialize (x,y) vel lvl  =  Asteroid (Rectangle x y size size) vel
    where
      size = levelSize lvl

update :: Asteroid -> Asteroid
update = move

draw :: Surface -> Asteroid -> IO ()
draw screen (Asteroid (Rectangle x y w h) _ _) = drawRect screen (x,y) w h (0,0,0)

move :: Asteroid -> Asteroid
move mp@(Asteroid (Rectangle x y w h) (dx,dy) _) = (bounding .~ newRect) $ mp
    where
        newRect = ( x .~ (mp^.bounding.x + dx) `mod` 640 ) . ( y .~ (mp^.bounding.y + dy) `mod` 480 ) $ mp^.bounding
        
