{-# LANGUAGE TemplateHaskell #-}

module Base.GameComponent where

import SDL (Renderer)
import Base.InputHandler (KeyboardState)
import Control.Lens

data GameComponent m = GameComponent
                     {
                       _update :: KeyboardState -> Int -> GameComponent m
                     , _draw   :: Renderer -> IO ()
                     , _value  :: m
                     }

makeLenses ''GameComponent
