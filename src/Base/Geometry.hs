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

wrap :: Rectangle -> Rectangle
wrap = pos.lensP %~ (`modT` (V2 640 480))
    where
        (V2 x y) `modT` (V2 x' y') = V2 (x `mod` x') (y `mod` y')

