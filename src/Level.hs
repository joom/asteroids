{-# LANGUAGE FlexibleContexts #-}
module Level where

import Control.Monad
import Control.Monad.State
import qualified Player as P
import qualified Asteroid as A
import qualified Base.InputHandler as IH
import qualified Base.GraphicsManager as G
import qualified Base.Geometry as Geo
import qualified SDL as SDL
import SDL.Audio as SDL
import Config
import Timer
import Foreign(touchForeignPtr)
import Control.Lens((^.))

data LevelConfig = LevelConfig {
    lId :: Int,
    numAsteroids :: Int--,
    --currMusic :: SDL.Music
}

data LevelData = LevelData {
    asteroids :: [A.Asteroid],
    player :: P.Player
}

data Level = Level {
    lC :: LevelConfig,
    lD :: LevelData
}

initialize :: SDL.Renderer -> Int -> IO Level
initialize r 0 = do
    --m <- SDL.loadMUS "../assets/music/Aalborn_Pulse.mp3"
    return $ Level (LevelConfig 0 10 {-m-}) (LevelData [] (P.initialize r sPos))
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
            currP <- gets player
            lev <- get
            let newPlayer =  P.update kS obs currP
            modify $ \t -> t { player=newPlayer, asteroids=obs }
            return $ (length obs) > 0

draw :: SDL.Renderer -> Level -> IO ()
draw r (Level lC lev) = do
    -- last will force evaluation, and keep type IO ()
    sequence $ map (A.draw r) (asteroids lev)
--    sequence $ map (P.draw r (players lev))
    G.drawRect r (0,0) 640 480 (50,100,50)
    P.draw r (player lev)
    -- Need to tell the GC to not free the music (SDL should really do this)
--    touchForeignPtr . currMusic $ lC
    return ()

-- Helper function

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 (x:xs) = xs
skip n (x:xs) = x : (skip (n-1) xs)
