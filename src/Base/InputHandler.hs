module Base.InputHandler(KeyboardState, KeyState, initialize, update, isDown, isUp, isPressed, isReleased, putLastKeyboardState) where

import qualified SDL
import qualified SDL.Input as SDL
import qualified SDL.Input.Keyboard as SDL
import qualified SDL.Input.Keyboard.Codes as SDL
import Control.Monad.State(liftIO)
data KeyState = UP | DOWN | PRESSED | RELEASED deriving (Prelude.Enum, Eq, Ord, Bounded, Show)
--data Key = KEY_LEFT | KEY_RIGHT | KEY_DOWN | KEY_UP deriving (Enum)
type KeyboardState = [(SDL.Scancode,KeyState)]

isDown :: KeyboardState -> SDL.Scancode -> Bool
isDown kb key = (key,DOWN) `elem` kb

isUp :: KeyboardState -> SDL.Scancode -> Bool
isUp kb key = (key,UP) `elem` kb

isPressed :: KeyboardState -> SDL.Scancode -> Bool
isPressed kb key = (key,PRESSED) `elem` kb

isReleased :: KeyboardState -> SDL.Scancode -> Bool
isReleased kb key = (key,RELEASED) `elem` kb

initialKeyboardState :: KeyboardState
initialKeyboardState = []

set :: KeyboardState -> SDL.Scancode -> KeyState -> KeyboardState
set [] k state = [(k,state)]
set ((key,val):ks) k state = if key == k then (key,state) : ks else (key,val) : set ks k state

modKeyboardState :: KeyboardState -> SDL.KeyMotion -> SDL.Keysym -> KeyboardState
modKeyboardState ks SDL.KeyDown (SDL.Keysym k _ _) = set ks k PRESSED
modKeyboardState ks SDL.KeyUp (SDL.Keysym k _ _) = set ks k RELEASED

putLastKeyboardState :: KeyboardState -> KeyboardState
putLastKeyboardState [] = []
putLastKeyboardState (x:xs) = modKey x : putLastKeyboardState xs
    where
        modKey :: (SDL.Scancode,KeyState) -> (SDL.Scancode,KeyState)
        modKey (k,RELEASED) = (k,UP)
        modKey (k,PRESSED) = (k,DOWN)
        modKey k = k        

initialize :: IO (Bool,KeyboardState)
initialize = update initialKeyboardState

update :: KeyboardState -> IO (Bool,KeyboardState)
update ks = do
    event <- SDL.pollEvent
    case event of
        Nothing -> return (False,ks)
        Just ev -> case SDL.eventPayload ev of
           SDL.QuitEvent -> return (True,ks)
           SDL.KeyboardEvent _ motion _ False sym -> update (modKeyboardState ks motion sym)
           _ -> update ks
