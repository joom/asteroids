{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Word
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Player as P
import qualified Level as L
import qualified Asteroid as A
import qualified Base.GraphicsManager as G
import qualified Base.InputHandler as IH
import qualified Base.Geometry as Geo
import qualified SDL
import Timer
import Config

screenBpp    = 32


data AppData = AppData {
    level :: L.Level,
    keyboardState :: IH.KeyboardState,
    fps :: Timer
}

data AppConfig = AppConfig {
    screen    :: SDL.Renderer
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

getScreen :: MonadReader AppConfig m => m SDL.Renderer
getScreen = liftM screen ask

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = liftM fps get >>= act >>= putFPS

putLevel :: MonadState AppData m => L.Level -> m ()
putLevel l = modify $ \s -> s { level = l }

putKeyboardState :: MonadState AppData m => IH.KeyboardState -> m ()
putKeyboardState t = modify $ \s -> s { keyboardState = t }

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- G.initialize height width "Best Game Ever"
    (_,keys) <- IH.initialize
    level <- L.initialize screen 0
    fps <- start defaultTimer
    return (AppConfig screen, AppData level keys fps) 

loop :: AppEnv ()
loop = do
    modifyFPSM $ liftIO . start

    -- Update Code

    keyboardState <- liftM keyboardState get
    (quit,keyState) <- liftIO $ IH.update keyboardState
    putKeyboardState keyState

    fps <- liftM fps get
    level <- liftM level get
    let (won,nL) = L.update keyState level
    screen    <- getScreen
    if won then liftIO (L.initialize screen ((L.lId . L.lC $ nL) + 1)) >>= putLevel else putLevel nL

    putKeyboardState $ IH.putLastKeyboardState keyState

    -- Drawing Code

    liftIO $ do
        G.begin screen
        L.draw screen nL
        G.end screen

        -- Ensure framerate

        ticks <- getTimerTicks fps
        when (ticks < secsPerFrame) $ 
            SDL.delay $ secsPerFrame - ticks

    unless quit loop
 where
    framesPerSecond = 20
    secsPerFrame    = 1000 `div` framesPerSecond

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main = do
    SDL.initialize [SDL.InitEverything]
    (env, state) <- initEnv
    runLoop env state
