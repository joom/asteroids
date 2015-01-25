module Base.InputHandler(KeyboardState, KeyState, initialize, update, isDown, isUp, isPressed, isReleased, putLastKeyboardState) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Utilities as SDL.Utilities
import Control.Monad.State(liftIO)
data KeyState = UP | DOWN | PRESSED | RELEASED deriving (Prelude.Enum, Eq, Ord, Bounded, Show)
--data Key = KEY_LEFT | KEY_RIGHT | KEY_DOWN | KEY_UP deriving (Enum)
type KeyboardState = [(SDL.SDLKey,KeyState)]

isDown :: KeyboardState -> SDL.SDLKey -> Bool
isDown kb key = (key,DOWN) `elem` kb

isUp :: KeyboardState -> SDL.SDLKey -> Bool
isUp kb key = (key,UP) `elem` kb

isPressed :: KeyboardState -> SDL.SDLKey -> Bool
isPressed kb key = (key,PRESSED) `elem` kb

isReleased :: KeyboardState -> SDL.SDLKey -> Bool
isReleased kb key = (key,RELEASED) `elem` kb

initialKeyboardState :: KeyboardState
initialKeyboardState = [(k,UP) | k <- SDL.Utilities.enumFromTo SDL.SDLK_FIRST SDL.SDLK_LAST]

set :: KeyboardState -> SDL.SDLKey -> KeyState -> KeyboardState
set [] _ _ = []
set ((key,val):ks) k state = if key == k then ((key,state):ks) else (key,val) : (set ks k state)

modKeyboardState :: KeyboardState -> SDL.Event -> KeyboardState
modKeyboardState ks (SDL.KeyDown (SDL.Keysym k _ _)) = set ks k PRESSED
modKeyboardState ks (SDL.KeyUp (SDL.Keysym k _ _)) = set ks k RELEASED
modKeyboardState ks _ = ks

putLastKeyboardState :: KeyboardState -> KeyboardState
putLastKeyboardState [] = []
putLastKeyboardState (x:xs) = (modKey x) : (putLastKeyboardState xs)
    where
        modKey :: (SDL.SDLKey,KeyState) -> (SDL.SDLKey,KeyState)
        modKey (k,RELEASED) = (k,UP)
        modKey (k,PRESSED) = (k,DOWN)
        modKey k = k        

initialize :: IO (Bool,KeyboardState)
initialize = do
    SDL.enableKeyRepeat 0 0
    update initialKeyboardState

update :: KeyboardState -> IO (Bool,KeyboardState)
update ks = do
    event <- SDL.pollEvent
    case event of
        SDL.Quit -> return (True,ks)
        SDL.NoEvent -> return (False,ks)
        _       ->  do
            update (modKeyboardState ks event)
