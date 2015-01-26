{-# LANGUAGE FlexibleContexts #-}
module Level where

import Control.Monad
import Control.Monad.State
import qualified Player as P
import qualified Asteroid as A
import qualified Projectile as Pr
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
    projectiles :: [Pr.Projectile],
    player :: P.Player
}

data Level = Level {
    lC :: LevelConfig,
    lD :: LevelData
}

initialize :: SDL.Renderer -> Int -> IO Level
initialize r 0 = do
    --m <- SDL.loadMUS "../assets/music/Aalborn_Pulse.mp3"
    return $ Level (LevelConfig 0 10 {-m-}) (LevelData [] [] (P.initialize r sPos))
    where
        sPos = (0,200)

update :: IH.KeyboardState -> Level -> (Bool,Level)
update kS l@(Level lC lev) = (won,l { lD=nLD })
    where
        (won,nLD) = runState (updateS kS lC) lev

newProjectile :: P.Player -> Pr.Projectile
newProjectile (P.Player (Geo.Rectangle x y w h) (dx,dy) dir _ _) = Pr.Projectile (Geo.Rectangle x' y' 2 2) (dx',dy')
    where
        (x',y') = (oX+dx',oY+dy')
        dx' = floor $ ((cos dir) * 5.0)
        dy' = floor $ ((sin dir) * 5.0)
        oX = (x + w + 10) + (floor $ ((cos dir) * (fromIntegral w) / 2))
        oY = (y + h + 10) + (floor $ ((sin dir) * (fromIntegral h) / 2))

updateS :: IH.KeyboardState -> LevelConfig -> State LevelData Bool
updateS kS lC = do
            obs <- gets $ (map A.update) . asteroids
            projs <- gets $ (map Pr.update) . projectiles
            currP <- gets player
            let aObs = map A._bounding obs
            lev <- get
            let newPlayer =  P.update kS obs currP
            let projs' = if IH.isDown kS SDL.ScancodeSpace then (newProjectile newPlayer) : projs  else projs
            modify $ \t -> t { player=newPlayer, asteroids=obs, projectiles=projs' }
            return $ (length obs) > 0

draw :: SDL.Renderer -> Level -> IO ()
draw r (Level lC lev) = do
    -- last will force evaluation, and keep type IO ()
    sequence $ map (A.draw r) (asteroids lev)
--    sequence $ map (P.draw r (players lev))
    sequence $ map (Pr.draw r) (projectiles lev)
    P.draw r (player lev)
    -- Need to tell the GC to not free the music (SDL should really do this)
--    touchForeignPtr . currMusic $ lC
    return ()

-- Helper function

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 (x:xs) = xs
skip n (x:xs) = x : (skip (n-1) xs)
