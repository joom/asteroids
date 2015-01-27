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
import qualified Base.InputHandler as IH
import qualified Base.Geometry as Geo
import Control.Lens
import Control.Applicative ((<$>))
import qualified SDL
import Timer

screenBpp    = 32


data AppData = AppData {
    _level :: L.Level,
    _keyboardState :: IH.KeyboardState,
    _lastTicks :: Int,
    _timer :: Timer
}

makeLenses ''AppData

data AppConfig = AppConfig {
    screen    :: SDL.Renderer
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

initEnv :: IO (AppConfig, AppData)
initEnv = do    
    screen <- G.initialize 640 480 "Best Game Ever"
    (_,keys) <- IH.initialize
    level <- L.initialize screen 0
    fps <- start defaultTimer
    return (AppConfig screen, AppData level keys 0 fps) 

loop :: AppEnv ()
loop = do
    -- Update Code

    (quit,keyState) <- use keyboardState >>= liftIO . IH.update

    ticks <- use timer >>= fmap fromIntegral . liftIO . getTimerTicks
    (won,nL) <- L.update keyState ticks <$> use level
    screen    <- screen <$> ask

    keyboardState .= IH.putLastKeyboardState keyState

    -- Drawing Code
    lTicks <- use lastTicks
    liftIO $ do
        G.begin screen
        L.draw screen nL
        G.end screen

        -- Ensure framerate
        when (ticks - lTicks < msecsPerFrame) $ 
            SDL.delay . fromIntegral $ msecsPerFrame - ticks + lTicks
    lastTicks .= ticks

    if won
      then do
        lastTicks .= 0
        use timer >>= liftIO . start >>= (timer .=)
        liftIO (L.initialize screen ((nL^.L.lconfig^.L.lId) + 1)) >>= (level .=)
      else
        level .= nL
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
