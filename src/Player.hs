{-# LANGUAGE TemplateHaskell #-}

module Player where

import Base.GraphicsManager
import Base.InputHandler
import Base.Geometry
import qualified Base.Camera as C
import qualified Platform as P
import Graphics.UI.SDL (SDLKey(..), Surface)
import Control.Monad.State
import Control.Lens

data Player = Player { _bounding :: Shape, _velocity :: (Int,Int), _alive :: Bool, _image :: IO Surface }
$( makeLenses ''Player )

playerHeight = 20
playerWidth = 20

initialize :: (Int,Int) -> Player
initialize (x,y) = Player (Rectangle x y playerWidth playerHeight) (0,2) True (loadImage "dot.bmp" Nothing)

update :: KeyboardState -> [Shape] -> [P.Platform] -> Player -> Player
update kS rects mp p = snd $ runState (updateS kS rects mp) p

updateS :: KeyboardState -> [Shape] -> [P.Platform] -> State Player ()
updateS kS rects mp = do
    x <- use $ bounding.x
    y <- use $ bounding.y
    velocity._1 %= (\dx -> if strafe == 0 then ((if dx > 0 then floor else ceiling) $ ((fromIntegral dx)*0.9)) else (if dx*strafe > 0 then max (min (dx+strafe) 5) (-5) else strafe))
    modify moveV
    curP <- get
    let (nP,oG) = (moveObjs mp) . (checkCollisionV rects y) $ curP
    put $ (checkCollisionH rects x) . moveH $ nP
    if oG && (isPressed kS SDLK_UP) then velocity._2 %= (\dy -> (min 0 dy) - 10 ) else return ()
    where
        strafe = (if isDown kS SDLK_LEFT then -1 else 0) + (if isDown kS SDLK_RIGHT then 1 else 0)

draw :: Surface -> C.FixedCamera -> Player -> IO ()
draw screen cam (Player (Rectangle x y _ _)  _ _ image) = do
    img <- image
    let pt = C.shapeToScreen cam (Point x y) 640 480
    case pt of
        Just (Point x' y') -> do
            drawImage screen img (x' , y')
        Nothing -> return True
    return ()

moveH :: Player -> Player
moveH p = bounding.x +~ p^.velocity._1 $ p

moveV :: Player -> Player
moveV p = (bounding.y +~ p^.velocity._2) . (velocity._2 +~ 2) $ p

checkCollisionH :: [Shape] -> Int -> Player -> Player
checkCollisionH [] _ p = p
checkCollisionH (r:rs) origX p@(Player _ (dx,dy) a i)
    | r `collides` (x .~ origX $ p^.bounding) = if r^.x - origX > 0
        then if (p^.bounding.x + p^.velocity._1) < (r^.x - p^.bounding.width - 1) then ((bounding.x .~ (r^.x - p^.bounding.width - 1)) $ p) else p
        else if (p^.bounding.x + p^.velocity._1) > (r^.x - p^.bounding.width - 1) then ((bounding.x .~ (r^.x + r^.width + 1)) $ p) else p
    | p^.velocity._1 == 0 = p
    | p^.velocity._1 > 0 = if r `collides` ((x .~ origX) . (width .~ (p^.bounding.x - origX + p^.bounding.width)) $ p^.bounding)
        then bounding.x .~ (r^.x - p^.bounding.width - 1) $ p
        else checkCollisionH rs origX p
    | otherwise = if r `collides` (width .~ (origX - p^.bounding.x) $ p^.bounding)
        then (bounding.x .~ r^.x + r^.width + 1) $ p
        else checkCollisionH rs origX p

checkCollisionV :: [Shape] -> Int -> Player -> (Player,Bool)
checkCollisionV [] _ p = (p,False)
checkCollisionV (r:rs) origY p@(Player _ (dx,dy) a i)
    | r `collides` (y .~ origY $ p^.bounding) = if r^.y - origY < 0
        then if (p^.bounding.y+p^.velocity._2) < (r^.y)+(r^.height)+1 then ((bounding.y .~ (r^.y + r^.height + 1)) . (velocity._2 .~ 2) $ p, False) else (p,False)
        else if (p^.bounding.y+p^.velocity._2) > (r^.y)-(r^.height)-1 then ((bounding.y .~ (r^.y - p^.bounding.height - 1)) . (velocity._2 .~ 2) $ p, True) else (p,False)
    | p^.velocity._2 == 0 = (p, False)
    | p^.velocity._2 > 0 = if r `collides` ((y .~ origY) . (height .~ ((p^.bounding.y) - origY + (p^.bounding.height))) $ p^.bounding)
        then ((bounding.y .~ r^.y - p^.bounding.height - 1) . (velocity._2 .~ 2) $ p, True)
        else checkCollisionV rs origY p
    | otherwise = if r `collides` (height .~ (origY - (p^.bounding.y)) $ p^.bounding)
        then ((bounding.y .~ r^.y + r^.height + 1) . (velocity._2 .~ 2) $ p, False)
        else checkCollisionV rs origY p

moveObjs :: [P.Platform] -> (Player,Bool) -> (Player,Bool)
moveObjs ((P.MoveablePlatform _ _ r (dx',dy') _ fwd):rs) (p, b)
    | r `collides` (y +~ 3 $ p^.bounding) = ((bounding.x +~ dx) . (velocity._2 .~ dy) $ p,True)
    | otherwise = moveObjs rs (p,b)
        where
            dx = if fwd then dx' else dx'*(-1)
            dy = if fwd then dy' else dy'*(-1)
moveObjs _ p = p
