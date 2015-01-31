module Base.InputHandler(keyDown, keyUp, handleEvents) where

import qualified SDL

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

handleEvents :: (MonadIO m) => [SDL.Scancode] -> m (Bool,[SDL.Scancode])
handleEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        Nothing -> return (False,keysDown)
        Just ev -> case SDL.eventPayload ev of
           SDL.QuitEvent -> return (True,keysDown)
           SDL.KeyboardEvent _ SDL.KeyDown _ False (SDL.Keysym sym _ _) ->
             handleEvents $ press sym keysDown
           SDL.KeyboardEvent _ SDL.KeyUp _ False (SDL.Keysym sym _ _) ->
             handleEvents $ release sym keysDown
           _ -> handleEvents keysDown

