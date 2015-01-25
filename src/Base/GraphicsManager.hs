module Base.GraphicsManager where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL
import qualified Graphics.UI.SDL.TTF as SDL
import Data.Word(Word8)
import Control.Monad(liftM)

type Width = Int
type Height = Int
type Title = String
type Pos = (Int,Int)

screenWidth  = 640
screenHeight = 480
screenBpp    = 32

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO SDL.Surface
loadImage filename colorKey = SDL.load filename >>= SDL.displayFormat >>= setColorKey' colorKey

setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = (SDL.mapRGB . SDL.surfaceGetPixelFormat) surface r g b >>= SDL.setColorKey surface [SDL.SrcColorKey] >> return surface

drawImage :: SDL.Surface -> SDL.Surface -> Pos -> IO Bool
drawImage screen img (x,y) = SDL.blitSurface img Nothing screen offset
    where
        offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }

loadFont :: String -> Int -> IO SDL.Font
loadFont = SDL.openFont

drawText :: SDL.Surface -> SDL.Font -> String -> Pos -> (Int,Int,Int) -> IO Bool
drawText screen font text pos (r,g,b) = do
    surface <- SDL.renderTextSolid font text (SDL.Color (fromIntegral r) (fromIntegral g) (fromIntegral b))
    drawImage screen surface pos

initialize :: Width -> Height -> Title -> IO SDL.Surface
initialize w h title = do
    screen <- SDL.setVideoMode w h screenBpp [SDL.SWSurface]
    SDL.setCaption title []
    return screen

type Color = (Int,Int,Int)
drawRect :: SDL.Surface -> Pos -> Width -> Height -> Color -> IO ()
drawRect screen (x,y) w h (r,g,b) = do
    color <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen (fromIntegral r) (fromIntegral g) (fromIntegral b)
    SDL.fillRect screen (Just (SDL.Rect x y w h)) color
    return ()

begin :: SDL.Surface -> IO ()
begin screen = do
    color <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0xff 0xff 0xff
    SDL.fillRect screen Nothing color
    return ()

end :: SDL.Surface -> IO ()
end screen = do
    SDL.flip screen
