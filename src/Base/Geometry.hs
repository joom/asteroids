{-# LANGUAGE TemplateHaskell #-}

module Base.Geometry where

import Linear
import Linear.Affine
import Control.Lens
import qualified SDL

data Rectangle = R { _pos :: Point V2 Int, _size :: V2 Int  } deriving (Eq,Show)
makeLenses ''Rectangle

rToSDL :: (Num a) => Rectangle -> SDL.Rectangle a
rToSDL (R p s) = fmap fromIntegral (SDL.Rectangle p s)

hasIntersection :: Rectangle -> Rectangle -> Bool
hasIntersection (R (P (V2 x y)) (V2 w h)) (R (P (V2 x1 y1)) (V2 w1 h1)) = inRange x w x1 w1 && inRange y h y1 h1
	where
		inRange p1 l1 p2 l2 = (p1 <= p2) && (p2 <= p1 + l1) || (p2 <= p1) && (p1 <= p2 +l2)

wrap :: Rectangle -> Rectangle
wrap r = pos.lensP %~ (`modH` V2 640 480) $ r
    where
        v@(V2 x y) `modH` v'@(V2 x' y')
          | x + (r^.size^._x) < 0  && y + (r^.size^._y) < 0 = v `modT` v'
          | x + (r^.size^._x) < 0 = V2 ((v `modT` v')^._x) y
          | y + (r^.size^._y) < 0 = V2 x ((v `modT` v')^._y)
          | x > x' && y > y' = v `modT` v'
          | x > x' = V2 ((v `modT` v')^._x) y
          | y > y' = V2 x ((v `modT` v')^._y)
          | otherwise = v
        (V2 x y) `modT` (V2 x' y') = V2 (x `mod` x') (y `mod` y')


