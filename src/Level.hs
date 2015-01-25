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
    startPos :: [(Int,Int)],
    goal :: Geo.Shape,
    staticObstacles :: [Platform.Platform],
    holdSwitches :: [Switch.Switch LevelData],
    currMusic :: SDL.Music
}

data LevelData = LevelData {
    movingObstacles :: [Platform.Platform],
    camera :: C.FixedCamera,
    players :: [P.Player],
    curPlayer :: Int,
    levelHistory :: [LevelData]
}

data Level = Level {
    lC :: LevelConfig,
    lD :: LevelData
}

initialize :: Int -> IO Level
initialize 0 = do
    m <- SDL.loadMUS "../assets/music/Aalborn_Pulse.mp3"
    let l = Level (LevelConfig 0 sPos (Geo.Rectangle 350 240 20 20) platforms [(Switch.Hold (Geo.Rectangle 250 290 30 10) id (\l@(LevelData _ fc@(C.FixedCamera (x,y) _ _) _ _ _) -> l { camera=(fc { C.corner=(x+1,y) } ) }))] m) (LevelData mPlatforms (C.FixedCamera (0,0) height width) (map P.initialize sPos) 0 [])
    AM.playMusic m
    return l
    where
        sPos = [(0,200), (100,200)]
        mPlatforms = [(Platform.moveableInitialize (10,200) (100,250) 30 10 5 (0,0,0) )]
        platforms = [(Platform.Platform (Geo.Rectangle 0 300 400 30) (0,0,0)),(Platform.Platform (Geo.Rectangle 450 290 300 30) (0,0,0)), (Platform.Platform (Geo.Rectangle 300 260 95 10) (0,0,0)),(Platform.Platform (Geo.Rectangle (-10) 10 300 30) (0,0,0))]

{-update :: IH.KeyboardState -> Level -> (Bool,Level)
update kS l@(Level lC lev) = if IH.isDown kS SDL.SDLK_LSHIFT then (False,l { lD=(head (levelHistory lev)) }) else (not . (elem False) . (map (Geo.collides (goal lC))) $ pObs, l { lD=nL })
    where
        nL = map (Switch.update nL' newPlayers) (holdSwitches lC)
        nL' = lev { players=newPlayers, levelHistory=(lev:(levelHistory lev)), curPlayer=playInd, movingObstacles=newObstacles }
        playInd = max 0 (min pI ((length (players lev)) - 1))
        pI = (curPlayer lev) + (if IH.isDown kS SDL.SDLK_q then (-1) else 0) + (if IH.isDown kS SDL.SDLK_e then 1 else 0) 
        sObs = (map Platform.bounding $ staticObstacles lC)
        pObs = (map P.bounding $ players lev)
        -- lengthy way to exclude players from hitting themselves
        newPlayers = [ P.update (if n == playInd then kS else []) (sObs ++ mObs ++ (skip n pObs)) newObstacles ((players lev)!!n) | n <- [0..((length . players $ lev) - 1)] ]
        mObs = (map Platform.mBounding $ newObstacles)
        newObstacles = map Platform.update (movingObstacles lev)
-}

update :: IH.KeyboardState -> Level -> (Bool,Level)
update kS l@(Level lC lev) = (won,l { lD=nLD })
    where
        (won,nLD) = runState (updateS kS lC) lev

updateS :: IH.KeyboardState -> LevelConfig -> State LevelData Bool
updateS kS lC = do
    prev <- gets levelHistory
    if ((IH.isDown kS SDL.SDLK_LSHIFT) && (not . null $ prev))
        then do
            put . head $ prev
            return False
        else do
            obs <- gets $ (map Platform.update) . movingObstacles
            modify $ \s -> s { movingObstacles=obs }
            let mObs = map Platform._bounding obs
            let sObs = (map Platform._bounding) . staticObstacles $ lC
            pObs <- gets $ (map P._bounding) . players
            pls <- gets players
            currP <- gets curPlayer
            let playInd = max 0 (min (currP + (if IH.isDown kS SDL.SDLK_q then (-1) else 0) + (if IH.isDown kS SDL.SDLK_e then 1 else 0)) ((length pls) - 1))
            lev <- get
            let newPlayers = [ P.update (if n == playInd then kS else []) (sObs ++ mObs ++ (skip n pObs)) obs ((players lev)!!n) | n <- [0..((length . players $ lev) - 1)] ]
            modify $ \t -> t { players=newPlayers, levelHistory=(t:(levelHistory t)), curPlayer=playInd, movingObstacles=obs }
            modify $ \t -> swUpdate t (holdSwitches lC)
            return $ not . (elem False) . (map (Geo.collides (goal lC))) $ pObs
    

swUpdate :: LevelData -> [Switch.Switch LevelData] -> LevelData
swUpdate x [] = x
swUpdate lev (x:xs) = swUpdate (snd $ Switch.update lev (players lev) x) xs

draw :: SDL.Surface -> Level -> IO ()
draw screen (Level lC lev) = do
    -- last will force evaluation, and keep type IO ()
    sequence $ map (Platform.draw screen (camera lev)) (staticObstacles lC)
    sequence $ map (P.draw screen (camera lev)) (players lev)
    sequence $ map (Platform.draw screen (camera lev)) (movingObstacles lev)
    sequence $ map (Switch.draw screen (camera lev)) (holdSwitches lC)
    let camRect = C.shapeToScreen (camera lev) (goal lC) 640 480
    case camRect of
    	Just r -> G.drawRect screen (r^.Geo.x,r^.Geo.y) (r^.Geo.width) (r^.Geo.height) (50,100,50)
    	Nothing -> return ()
    -- Need to tell the GC to not free the music (SDL should really do this)
    touchForeignPtr . currMusic $ lC
    return ()

-- Helper function

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 (x:xs) = xs
skip n (x:xs) = x : (skip (n-1) xs)
