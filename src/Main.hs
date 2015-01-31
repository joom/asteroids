{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Main where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Player as P
import qualified Level as L
import qualified Asteroid as A
import qualified Base.GraphicsManager as G
import Base.InputHandler
import qualified Base.Geometry as Geo
import Control.Lens
import Control.Applicative ((<$>))
import qualified SDL
import Timer

screenBpp    = 32

data AppData = AppData {
    _level :: Wire s e m [SDL.Scancode] L.Level,
    _keyboardState :: [SDL.Scancode],
    _lastTicks :: Int,
    _timer :: Timer,
    _session :: Session t s
}

makeLenses ''AppData

data AppConfig = AppConfig {
    screen    :: SDL.Renderer
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- G.initialize 640 480 "Asteroids"
    level <- L.initialize screen 0
    fps <- start defaultTimer
    return (AppConfig screen, AppData level [] 0 fps) 

loop :: AppEnv ()
loop = do
    -- Update Code

    (quit,keysDown) <- use keyboardState >>= handleEvents
    (v,s') <- use session >>= stepSession
    session .= s'

    lvl <- use level
    (r,w') <- stepWire lvl v (Right keysDown)
    level .= w'
    case r of
        Right (lvl',won) -> do
	    screen    <- screen <$> ask

	    -- Drawing Code
	    lTicks <- use lastTicks
	    liftIO $ do
		G.begin screen
		L.renderLevel screen lvl'
		G.end screen

		-- Ensure framerate
		when (ticks - lTicks < msecsPerFrame) $ 
		    SDL.delay . fromIntegral $ msecsPerFrame - ticks + lTicks
	    lastTicks .= ticks

	    when won $ do
		lastTicks .= 0
		use timer >>= liftIO . start >>= (timer .=)
		liftIO (L.initialize screen ((nL^.L.lconfig^.L.lId) + 1)) >>= (level .=)
    unless quit loop
 where
    framesPerSecond = 20
    msecsPerFrame    = 1000 `div` framesPerSecond

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main = do
    SDL.initialize [SDL.InitEverything]
    (env, state) <- initEnv
    runLoop env state
