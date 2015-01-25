module Base.AudioManager where
import Graphics.UI.SDL.Mixer as SDL.Mixer

type Id = Int

initialize :: IO ()
initialize = SDL.Mixer.openAudio SDL.Mixer.defaultFrequency SDL.Mixer.AudioU16Sys 1 1024

playSound :: String -> IO Id
playSound file = SDL.Mixer.loadWAV file >>= \x -> SDL.Mixer.playChannel (-1) x 0

stopSound :: Id -> IO ()
stopSound soundID = SDL.Mixer.haltChannel (-1)

playMusic :: SDL.Mixer.Music -> IO ()
playMusic mus = SDL.Mixer.playMusic mus (-1)

stopMusic :: IO ()
stopMusic = SDL.Mixer.haltMusic

--update :: 
