{-# LANGUAGE Arrows #-}

import qualified SDL
import FRP.Netwire
import Control.Wire
import Prelude hiding ((.), id, until)
import Data.Text (pack)
import Linear
import Linear.Affine

instance (Num a, Num b) => Num (a,b) where
  (a,b) + (c,d) = (a+c,b+d)
  (a,b) * (c,d) = (a*c,b*d)
  abs (a,b) = (abs a,abs b)
  signum (a,b) = (signum a, signum b)
  fromInteger a = (fromInteger a,fromInteger a)
  negate (a,b) = (negate a, negate b)

instance (Fractional a, Fractional b) => Fractional (a,b) where
  (a,b) / (c,d) = (a/c,b/d)
  recip (a,b)   = (recip a,recip b)
  fromRational a = (fromRational a, fromRational a)

keyDown :: SDL.Scancode -> [SDL.Scancode] -> Bool
keyDown = elem

keyUp :: SDL.Scancode -> [SDL.Scancode] -> Bool
keyUp = notElem

press :: SDL.Scancode -> [SDL.Scancode] -> [SDL.Scancode]
press sym [] = [sym]
press sym (x:xs) = if x == sym then (x:xs) else x : press sym xs

release :: SDL.Scancode -> [SDL.Scancode] -> [SDL.Scancode]
release _ [] = []
release sym (x:xs) = if x == sym then xs else x : release sym xs

handleEvents :: [SDL.Scancode] -> IO ([SDL.Scancode],Bool)
handleEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        Nothing -> return (keysDown,False)
        Just ev -> case SDL.eventPayload ev of
           SDL.QuitEvent -> return (keysDown,True)
           SDL.KeyboardEvent _ SDL.KeyDown _ False (SDL.Keysym sym _ _) ->
             handleEvents $ press sym keysDown
           SDL.KeyboardEvent _ SDL.KeyUp _ False (SDL.Keysym sym _ _) ->
             handleEvents $ release sym keysDown
           _ -> handleEvents keysDown

fly :: Wire (Timed NominalDiffTime ()) () IO [SDL.Scancode] (Double, Double)
fly = proc keysDown -> do
    thrust <- inputAcceleration -< keysDown
    direction <- integral 0 . inputRotation -< keysDown
    let accel = (thrust * cos direction,thrust * sin direction)
    pos <- integral (320,240) . integral (0,0) -< accel
    returnA -< wrap pos

wrap :: (Double,Double) -> (Double,Double)
wrap (x,y) = (fromIntegral $ (floor x)`mod`640,fromIntegral $ (floor y)`mod`480)

inputRotation =      pi . when (keyDown SDL.ScancodeRight)
                <|> -pi . when (keyDown SDL.ScancodeLeft)
                <|>  0


inputAcceleration  =     100 . when (keyDown SDL.ScancodeUp)
                     <|> 0
main = do
 -- setup SDL shit
 SDL.initialize [SDL.InitEverything]
 window <- SDL.createWindow (pack "W") (SDL.defaultWindow { SDL.windowSize = V2 640 480 })
 rend <- SDL.createRenderer window (-1) SDL.defaultRenderer
 myLoop rend [] clockSession_ fly

myLoop :: SDL.Renderer -> [SDL.Scancode] -> Session IO (Timed NominalDiffTime ()) -> Wire (Timed NominalDiffTime ()) () IO [SDL.Scancode] (Double,Double) -> IO ()
myLoop rend keysDown s w = do
  (keysDown',q) <- handleEvents keysDown
  (v,s') <- stepSession s
  (r,w') <- stepWire w v (Right keysDown')
  case r of
    Right (x,y) -> do
      SDL.setRenderDrawColor rend (V4 0 0 0 255)
      SDL.renderClear rend
      SDL.setRenderDrawColor rend (V4 255 255 255 255)
      SDL.renderFillRect rend (Just $ SDL.Rectangle (P (V2 (floor x) (floor y))) (V2 10 10))
      SDL.renderPresent rend
    _ -> return ()
  if not q then myLoop rend keysDown' s' w' else return ()
