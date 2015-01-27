{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Level where

import Control.Monad
import Control.Monad.State
import qualified Player as P
import qualified Asteroid as A
import qualified Projectile as Pr
import qualified Base.InputHandler as IH
import qualified Base.GraphicsManager as G
import Base.Geometry
import Linear
import Linear.Affine
import qualified SDL
import SDL.Audio as SDL
import Timer
import Foreign(touchForeignPtr)
import Control.Lens hiding (Level)
import System.Random(randomR,mkStdGen,StdGen)

data LevelConfig = LevelConfig {
    lId :: Int,
    numAsteroids :: Int,
    time :: Int
    --currMusic :: SDL.Music
}

data LevelData = LevelData {
    _asteroids :: [A.Asteroid],
    _projectiles :: [Pr.Projectile],
    _player :: P.Player,
    _asteroidsSpawned :: Int
}

makeLenses ''LevelData

data Level = Level {
    lC :: LevelConfig,
    lD :: LevelData
}

initialize :: SDL.Renderer -> Int -> IO Level
initialize r 0 = -- do
    --m <- SDL.loadMUS "../assets/music/Aalborn_Pulse.mp3"
    return $ Level (LevelConfig 0 10 10000{-m-}) (LevelData [] [] (P.initialize r sPos) 0)
    where
        sPos = P $ V2 0 200

update :: IH.KeyboardState -> Int -> Level -> (Bool,Level)
update kS ticks l@(Level lC lev) =  (False, l { lD=nLD }) --(won,l { lD=nLD })
    where
        (won,nLD) = runState (updateS kS ticks lC) lev

newProjectile :: P.Player -> Pr.Projectile
newProjectile p = Pr.Projectile (R pt (V2 1 1)) v' 100
    where
        v' =  fmap (floor . (*10.0)) (V2 (cos $ p^.P.direction) (sin $ p^.P.direction))
        pt = P $ offset + v' + (p^.P.bounding^.pos^.lensP)
        offset = fmap (`quot` 2) $ p^.P.bounding^.size

makeAsteroid :: Int -> A.Asteroid
makeAsteroid n = A.asteroidInitialize coords vel 3
    where
        P vel = fmap floor . fmap (*speed) . normalize . fmap (fromIntegral :: Int -> Double) $ coords - (P (V2 320 240))
        coords = P $ if vert == 0 then V2 0 y else V2 x 0
        (x,r) = randomR (0,640) (mkStdGen n) :: (Int,StdGen)
        (y,r1) = randomR (0,480) r :: (Int,StdGen)
        (vert,r2) = randomR (0,1) r1 :: (Int,StdGen)
        (speed,_) = randomR (5.0,15.0) r2 :: (Double,StdGen)

updateS :: IH.KeyboardState -> Int -> LevelConfig -> State LevelData Bool
updateS kS elapsedTicks lC = do
            asteroids %= map A.update
            let ticksPerAst = (time lC) `quot` (numAsteroids lC)
            let astsSpawnedByNow = elapsedTicks `quot` ticksPerAst
            spawned <- use asteroidsSpawned
            when (all (>spawned) [astsSpawnedByNow,numAsteroids lC]) $ do
                asteroidsSpawned += 1
                asteroids %= (:) (makeAsteroid elapsedTicks)
            asts <- use asteroids
            player %= P.update kS asts 
            user <- use player
            projectiles %= map Pr.update
            -- get the intersection of asteroids and projectiles
            projs <- use projectiles
            let projB = map Pr._bounding projs
            asteroids %= filter (\a -> none (hasIntersection (a^.A.bounding)) projB) 
            projectiles %= filter (\p -> p^.Pr.life>0)
            when (IH.isDown kS SDL.ScancodeSpace) $
                projectiles %= (:) (newProjectile user)
            return . not . null $ asts

draw :: SDL.Renderer -> Level -> IO ()
draw r (Level lC lev) = do
    -- last will force evaluation, and keep type IO ()
    mapM_ (A.draw r) (lev^.asteroids)
--    sequence $ map (P.draw r (players lev))
    mapM_ (Pr.draw r) (lev^.projectiles)
    P.draw r (lev^.player)
    -- Need to tell the GC to not free the music (SDL should really do this)
--    touchForeignPtr . currMusic $ lC
    return ()

-- Helper function

skip :: Int -> [a] -> [a]
skip n = drop (n+2)
