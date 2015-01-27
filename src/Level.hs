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
import Control.Applicative ((<$>))

data LevelConfig = LevelConfig {
    _lId :: Int,
    _numAsteroids :: Int,
    _time :: Int
    --currMusic :: SDL.Music
}

makeLenses ''LevelConfig

data LevelData = LevelData {
    _asteroids :: [A.Asteroid],
    _projectiles :: [Pr.Projectile],
    _player :: P.Player,
    _asteroidsSpawned :: Int,
    _score :: Int
}

makeLenses ''LevelData

defaultLevelData :: SDL.Renderer -> LevelData
defaultLevelData r = LevelData [] [] (P.defaultPlayer r) 0 0

data Level = Level {
    _lconfig :: LevelConfig,
    _ldata :: LevelData
}

makeLenses ''Level

initialize :: SDL.Renderer -> Int -> IO Level
initialize r _ = -- do
    --m <- SDL.loadMUS "../assets/music/Aalborn_Pulse.mp3"
    return $ Level (LevelConfig 0 10 10000{-m-}) (defaultLevelData r)

update :: IH.KeyboardState -> Int -> Level -> (Bool,Level)
update kS ticks l@(Level lC lev) =  (won,ldata .~ nLD $ l)
    where
        (won,nLD) = runState (updateS kS ticks lC) lev

newProjectile :: P.Player -> Pr.Projectile
newProjectile p = Pr.Projectile (R pt (V2 1 1)) v' 100
    where
        v' =  (floor . (*10.0)) <$> V2 (cos $ p^.P.direction) (sin $ p^.P.direction)
        pt = P $ offset + v' + (p^.P.bounding^.pos^.lensP)
        offset = (`quot` 2) <$>  p^.P.bounding^.size

makeAsteroid :: Int -> A.Asteroid
makeAsteroid n = evalRand makeAsteroidS (mkStdGen n)

makeAsteroidS :: Rand A.Asteroid
makeAsteroidS = do
        x <- inRange (0,640)
        y <- inRange (0,480)
        vert <- inRange (False,True)
        let coords = P $ if vert then V2 0 y else V2 x 0
        speed <- inRange (5.0,15.0)
        let P vel = (floor . (*speed)) <$> normalize (fmap i2d $ coords - P (V2 320 240))
        return $ A.asteroidInitialize coords vel 3
            where
               i2d :: Int -> Double
               i2d = fromIntegral

hitAsteroids projs = foldr (\a (acc,pcc) ->
              case firstCollision a projs of
                Just p ->
                  if a^.A.level > 1
                    then
                      let copyA = (A.level -~ 1) a in
                      let randA = (A.vel .~ V2 5 5) copyA in
                      (randA : (copyA : acc), p : pcc)
                    else (acc,p : pcc)
                Nothing -> (a : acc, pcc)) ([],[])

hitProjectiles badProjs = foldr (\p acc -> if p^.Pr.life == 0 ||  any (==p) badProjs
                then acc
                else p : acc) []

firstCollision :: A.Asteroid -> [Pr.Projectile] -> Maybe Pr.Projectile
firstCollision _ [] = Nothing
firstCollision a (p:ps)
     | a `collides` p = Just p
     | otherwise = firstCollision a ps

collides :: A.Asteroid -> Pr.Projectile -> Bool
collides a p = (a^.A.bounding) `hasIntersection` (p^.Pr.bounding)

playerLives :: P.Player -> [A.Asteroid] -> Bool
playerLives p = none (((p^.P.bounding)`hasIntersection`).A._bounding)

updateS :: IH.KeyboardState -> Int -> LevelConfig -> State LevelData Bool
updateS kS elapsedTicks lC = do
            asteroids %= map A.update
            let astsSpawnedByNow = elapsedTicks * (lC^.numAsteroids) `quot` lC^.time
            spawned <- use asteroidsSpawned
            when (all (>spawned) [astsSpawnedByNow,lC^.numAsteroids]) $ do
                asteroidsSpawned += 1
                asteroids %= (:) (makeAsteroid elapsedTicks)
            asts <- use asteroids
            player %= P.update kS asts 
            user <- use player
            player.P.alive .= playerLives  user asts
            projectiles %= map Pr.update
            -- get the intersection of asteroids and projectiles
            projs <- use projectiles
            let (newAsts,badProjs) = hitAsteroids projs asts
            asteroids .= newAsts
            projectiles %= hitProjectiles badProjs
            numAsts <- length <$> (use asteroids)
            score += length asts - numAsts
            canShoot <- (>250) . (elapsedTicks-) . P._lastShot <$> use player
            when ((IH.isDown kS SDL.ScancodeSpace) &&
                   canShoot ) $ do
                    player.P.lastShot .= elapsedTicks
                    projectiles %= (:) (newProjectile user)
            return $ spawned == (lC^.numAsteroids)  && null asts

draw :: SDL.Renderer -> Level -> IO ()
draw r (Level lC lev) = do
    mapM_ (A.draw r) (lev^.asteroids)
--    sequence $ map (P.draw r (players lev))
    mapM_ (Pr.draw r) (lev^.projectiles)
    P.draw r (lev^.player)
    -- Need to tell the GC to not free the music (SDL should really do this)
--    touchForeignPtr . currMusic $ lC
    return ()

