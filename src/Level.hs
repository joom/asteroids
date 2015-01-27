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
import Control.Monad.Random

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
    _asteroidsSpawned :: Int,
    _score :: Int
}

makeLenses ''LevelData

data Level = Level {
    lC :: LevelConfig,
    lD :: LevelData
}

initialize :: SDL.Renderer -> Int -> IO Level
initialize r _ = -- do
    --m <- SDL.loadMUS "../assets/music/Aalborn_Pulse.mp3"
    return $ Level (LevelConfig 0 10 10000{-m-}) (LevelData [] [] (P.initialize r sPos) 0 0)
    where
        sPos = P $ V2 0 200

update :: IH.KeyboardState -> Int -> Level -> (Bool,Level)
update kS ticks l@(Level lC lev) =  (won,l { lD=nLD })
    where
        (won,nLD) = runState (updateS kS ticks lC) lev

newProjectile :: P.Player -> Pr.Projectile
newProjectile p = Pr.Projectile (R pt (V2 1 1)) v' 100
    where
        v' =  fmap (floor . (*10.0)) (V2 (cos $ p^.P.direction) (sin $ p^.P.direction))
        pt = P $ offset + v' + (p^.P.bounding^.pos^.lensP)
        offset = fmap (`quot` 2) $ p^.P.bounding^.size

makeAsteroid :: Int -> A.Asteroid
makeAsteroid n = evalRand makeAsteroidS (mkStdGen n)

makeAsteroidS :: Rand A.Asteroid
makeAsteroidS = do
        x <- inRange (0,640)
        y <- inRange (0,480)
        vert <- inRange (False,True)
        let coords = P $ if vert then V2 0 y else V2 x 0
        speed <- inRange (5.0,15.0)
        let P vel = fmap (floor . (*speed)) . normalize . fmap i2d $ coords - P (V2 320 240)
        return $ A.asteroidInitialize coords vel 3
            where
               i2d :: Int -> Double
               i2d = fromIntegral

updateS :: IH.KeyboardState -> Int -> LevelConfig -> State LevelData Bool
updateS kS elapsedTicks lC = do
            asteroids %= map A.update
            let ticksPerAst = time lC `quot` numAsteroids lC
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
            let intersection = [ (a,p) | a <- asts, p <- projs,
                                 (a^.A.bounding) `hasIntersection` (p^.Pr.bounding) ]
            asteroids %= filter (not . (`elem` map fst intersection))
            projectiles %= filter (not . (`elem` map snd intersection)) 
            projectiles %= filter (\p -> p^.Pr.life>0)
            score += length intersection
            when (IH.isDown kS SDL.ScancodeSpace) $
                projectiles %= (:) (newProjectile user)
            return $ spawned == numAsteroids lC  && null asts

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
