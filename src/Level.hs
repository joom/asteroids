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
import Base.GameComponent

data LevelConfig = LevelConfig {
    _lId :: Int,
    _numAsteroids :: Int,
    _time :: Int
    --currMusic :: SDL.Music
}

makeLenses ''LevelConfig

data LevelData = LevelData {
    _asteroids :: [GameComponent A.Asteroid],
    _projectiles :: [GameComponent Pr.Projectile],
    _player :: GameComponent P.Player,
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

makeAsteroid :: Int -> GameComponent A.Asteroid
makeAsteroid n = evalRand makeAsteroidS (mkStdGen n)

makeAsteroidS :: Rand (GameComponent A.Asteroid)
makeAsteroidS = do
        x <- inRange (0,640)
        y <- inRange (0,480)
        vert <- inRange (False,True)
        let coords = P $ if vert then V2 0 y else V2 x 0
        speed <- inRange (2.0,5.0)
        let P vel = (floor . (*speed)) <$> normalize (fmap i2d $ coords - P (V2 320 240))
        return $ A.defaultAsteroid coords vel 3
            where
               i2d :: Int -> Double
               i2d = fromIntegral

hitAsteroids :: [GameComponent Pr.Projectile] -> [GameComponent A.Asteroid] -> ([GameComponent A.Asteroid], [GameComponent Pr.Projectile])
hitAsteroids projsG = foldr (\ga (acc,pcc) ->
              let projs = map _value projsG in
              let a = ga^.value in
              case firstCollision a projs of
                Just p ->
                  if a^.A.level > 1
                    then
                      let copyA = (A.level -~ 1) a in
                      let randA = A.asteroid $ (A.vel .~ -copyA^.A.vel) copyA in
                      (randA : ((A.asteroid copyA) : acc), (Pr.projectile p) : pcc)
                    else (acc,(Pr.projectile p) : pcc)
                Nothing -> (ga : acc, pcc)) ([],[])


newProjectile :: GameComponent P.Player -> GameComponent Pr.Projectile
newProjectile p = Pr.newProjectile pt v'
    where
        v' =  (floor . (*10.0)) <$> V2 (cos $ p^.value^.P.direction) (sin $ p^.value^.P.direction)
        pt = P $ offset + v' + (p^.value^.P.bounding^.pos^.lensP)
        offset = (`quot` 2) <$>  p^.value^.P.bounding^.size



hitProjectiles :: [GameComponent Pr.Projectile] -> [GameComponent Pr.Projectile] -> [GameComponent Pr.Projectile]
hitProjectiles badProjsG =
    let badProjs = map _value badProjsG in
    foldr (\p acc -> if p^.value^.Pr.life == 0 ||  p^.value `elem` badProjs
      then acc
      else p : acc) []

firstCollision :: A.Asteroid -> [Pr.Projectile] -> Maybe Pr.Projectile
firstCollision _ [] = Nothing
firstCollision a (p:ps)
     | a `collides` p = Just p
     | otherwise = firstCollision a ps

collides :: A.Asteroid -> Pr.Projectile -> Bool
collides a p = (a^.A.bounding) `hasIntersection` (p^.Pr.bounding)

playerLives :: GameComponent P.Player -> [GameComponent A.Asteroid] -> Bool
playerLives p = none (((p^.value^.P.bounding)`hasIntersection`).A._bounding._value)

updateS :: IH.KeyboardState -> Int -> LevelConfig -> State LevelData Bool
updateS kS elapsedTicks lC = do
            asteroids %= map (\a -> _update a kS elapsedTicks)
            let astsSpawnedByNow = elapsedTicks * (lC^.numAsteroids) `quot` lC^.time
            spawned <- use asteroidsSpawned
            when (all (>spawned) [astsSpawnedByNow,lC^.numAsteroids]) $ do
                asteroidsSpawned += 1
                asteroids %= (:) (makeAsteroid elapsedTicks)
            asts <- use asteroids
            player %= (\p -> _update p kS elapsedTicks)
            user <- use player
            player.value.P.alive .= playerLives  user asts
            projectiles %= map (\p -> _update p kS elapsedTicks)
            -- get the intersection of asteroids and projectiles
            projs <- use projectiles
            let (newAsts,badProjs) = hitAsteroids projs asts
            asteroids .= newAsts
            projectiles %= hitProjectiles badProjs
            numAsts <- length <$> use asteroids
            score += length asts - numAsts
            canShoot <- (>250) . (elapsedTicks-) . P._lastShot . _value <$> use player
            when (IH.isDown kS SDL.ScancodeSpace &&
                   canShoot ) $ do
                    player.value.P.lastShot .= elapsedTicks
                    projectiles %= (:) (newProjectile user)
            return $ spawned == (lC^.numAsteroids)  && null asts

draw :: SDL.Renderer -> Level -> IO ()
draw r (Level lC lev) = do
    mapM_ (\a -> _draw a r) (lev^.asteroids)
--    sequence $ map (P.draw r (players lev))
    mapM_ (\p -> _draw p r) (lev^.projectiles)
    _draw (lev^.player) r
    -- Need to tell the GC to not free the music (SDL should really do this)
--    touchForeignPtr . currMusic $ lC
    return ()

