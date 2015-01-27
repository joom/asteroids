module Base.GraphicsManager where

import qualified SDL
import Linear
import Linear.Affine (Point(P),lensP)
import Base.Geometry
import Data.Word(Word8)
import Control.Monad(liftM)
import Data.Text(pack)
import Control.Lens
import Foreign.C.Types (CInt)

type Width = Int
type Height = Int
type Title = String

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

loadImage :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadImage renderer path = do
  bmp <- SDL.loadBMP path
  fmt <- SDL.surfaceFormat bmp
  key <- SDL.mapRGB fmt (V3 0 0 0)
  SDL.setColorKey bmp $ Just key
  SDL.createTextureFromSurface renderer bmp

drawImage :: SDL.Renderer -> SDL.Texture -> Point V2 Int -> Double -> IO ()
drawImage r t p d = mapM_ (\i -> drawTexture r t (p+i) d) offsets
    where
      offsets = [ P $ V2 x y | x <- [-640,0,640 ], y <- [-480, 0, 480 ] ]

drawTexture :: SDL.Renderer -> SDL.Texture -> Point V2 Int -> Double -> IO ()
drawTexture r tex pos rads = do
  ti <- SDL.queryTexture tex
  let dims = V2 (SDL.textureWidth ti) (SDL.textureHeight ti)
  let degrees = realToFrac $ 360.0 * (rads/(2*pi))
  SDL.renderCopyEx r tex Nothing
      (Just $ SDL.Rectangle (fmap fromIntegral pos) dims)
      degrees Nothing (V2 False False)
  

drawCircle :: SDL.Renderer -> Point V2 Int -> IO ()
drawCircle r pos = do
    SDL.setRenderDrawColor r (V4 255 255 255 255)
    SDL.renderDrawPoint r (fmap fromIntegral pos)

{-
loadFont :: String -> Int -> IO SDL.Font
loadFont = SDL.openFont

drawText :: SDL.Surface -> SDL.Font -> String -> Pos -> (Int,Int,Int) -> IO Bool
drawText screen font text pos (r,g,b) = do
    surface <- SDL.renderTextSolid font text (SDL.Color (fromIntegral r) (fromIntegral g) (fromIntegral b))
    drawImage screen surface pos
-}

initialize w h title = do
    window <- SDL.createWindow (pack title) (SDL.defaultWindow { SDL.windowSize = V2 w h })
    SDL.createRenderer window (-1) SDL.defaultRenderer

drawRect :: SDL.Renderer -> Rectangle -> V4 Int -> IO ()
drawRect r rect color = do
    SDL.setRenderDrawColor r (fmap fromIntegral color)
    SDL.renderFillRect r (Just . rToSDL $ rect)

begin :: SDL.Renderer -> IO ()
begin r = do
    SDL.setRenderDrawColor r (V4 0 0 0 255)
    SDL.renderClear r

end :: SDL.Renderer -> IO ()
end = SDL.renderPresent
