module Base.GraphicsManager where

import qualified SDL as SDL
import qualified SDL.Video as SDL
import Linear as SDL
import Linear.Affine (Point(P))
import Data.Word(Word8)
import Control.Monad(liftM)
import Data.Text(pack)

type Width = Int
type Height = Int
type Title = String
type Pos = (Int,Int)

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

loadImage :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadImage renderer path = do
  bmp <- SDL.loadBMP path
  SDL.createTextureFromSurface renderer bmp

drawImage :: SDL.Renderer -> SDL.Texture -> Pos -> Double -> IO ()
drawImage renderer tex (x,y) rads = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
  let degrees = realToFrac $ 360.0 * (rads/(2*pi))
  SDL.renderCopyEx renderer tex Nothing (Just $ SDL.Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))) degrees Nothing (V2 False False)
{-
loadFont :: String -> Int -> IO SDL.Font
loadFont = SDL.openFont

drawText :: SDL.Surface -> SDL.Font -> String -> Pos -> (Int,Int,Int) -> IO Bool
drawText screen font text pos (r,g,b) = do
    surface <- SDL.renderTextSolid font text (SDL.Color (fromIntegral r) (fromIntegral g) (fromIntegral b))
    drawImage screen surface pos
-}
initialize :: Width -> Height -> Title -> IO SDL.Renderer
initialize w h title = do
    window <- SDL.createWindow (pack title) (SDL.defaultWindow { SDL.windowSize = V2 (fromIntegral w) (fromIntegral h) })
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    return renderer

type Color = (Int,Int,Int)
drawRect :: SDL.Renderer -> Pos -> Width -> Height -> Color -> IO ()
drawRect renderer (x,y) w h (r,g,b) = do
    SDL.setRenderDrawColor renderer  (V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral 255))
    let rect = SDL.Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))
    SDL.renderFillRect renderer (Just rect)
    return ()

begin :: SDL.Renderer -> IO ()
begin = SDL.renderClear

end :: SDL.Renderer -> IO ()
end = SDL.renderPresent
