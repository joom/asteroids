{-# LANGUAGE FlexibleContexts #-}
module Level where

import Control.Monad
import Control.Monad.State
import qualified Player as P
import qualified Platform as Platform
import qualified Switch as Switch
import qualified Base.InputHandler as IH
import qualified Base.AudioManager as AM
import qualified Base.GraphicsManager as G
import qualified Base.Camera as C
import qualified Base.Geometry as Geo
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as SDL
import Config
import Timer
import Foreign(touchForeignPtr)
import Control.Lens((^.))

data LevelConfig = LevelConfig {
    lId :: Int,
    numAsteroids :: Int,
    currMusic :: SDL.Music
}

data LevelData = LevelData {
    asteroids :: [A.Asteroid],
    player :: P.Player,
}

data Level = Level {
    lC :: LevelConfig,
    lD :: LevelData
}

initialize :: Int -> IO Level
initialize 0 = do
    m <- SDL.loadMUS "../assets/music/Aalborn_Pulse.mp3"
    let l = Level (LevelConfig 0 10 m) (LevelData [] (P.initialize sPos))
    AM.playMusic m
    return l
    where
        sPos = (0,200)

update :: IH.KeyboardState -> Level -> (Bool,Level)
update kS l@(Level lC lev) = (won,l { lD=nLD })
    where
        (won,nLD) = runState (updateS kS lC) lev

updateS :: IH.KeyboardState -> LevelConfig -> State LevelData Bool
updateS kS lC = do
            obs <- gets $ (map A.update) . asteroids
            modify $ \s -> s { asteroids=obs }
            let aObs = map A._bounding obs
            pObs <- gets $ P._bounding player
            currP <- gets player
            lev <- get
            let newPlayer =  P.update (sObs ++ mObs ++ (skip n pObs)) obs ((players lev)!!n)
            modify $ \t -> t { players=newPlayer, asteroids=obs }
            return $ (length obs) > 0

draw :: SDL.Surface -> Level -> IO ()
draw screen (Level lC lev) = do
    -- last will force evaluation, and keep type IO ()
    sequence $ map (A.draw screen (asteroids lC)
    sequence $ map (P.draw screen (players lev)
    G.drawRect screen (0,0) 640 480 (50,100,50)
    -- Need to tell the GC to not free the music (SDL should really do this)
    touchForeignPtr . currMusic $ lC
    return ()

-- Helper function

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 (x:xs) = xs
skip n (x:xs) = x : (skip (n-1) xs)
