module Base.Geometry where

import Control.Lens

data Shape = Rectangle { _rectX :: Int, _rectY :: Int, _rectW :: Int, _rectH :: Int } | Line { _start :: (Int,Int), _end :: (Int,Int) }
	| Point { _ptX :: Int, _ptY :: Int } deriving (Eq,Show)


x :: Lens' Shape Int
x = lens getX setX
    where
        getX (Rectangle x _ _ _) = x
        getX (Point x _) = x
        getX (Line (x,_) _) = x
        setX (Rectangle x y z w) x' = Rectangle x' y z w
        setX (Point x y) x' = Point x' y
        setX (Line (x,y) e) x' = Line (x',y) e


y :: Lens' Shape Int
y = lens getY setY
	where
		getY (Rectangle _ y _ _) = y
		getY (Point _ y) = y
		getY (Line (_,y) _) = y
		setY (Rectangle x y z w) y' = Rectangle x y' z w
		setY (Point x y) y' = Point x y'

width :: Lens' Shape Int
width = lens getW setW
	where
		getW (Rectangle _ _ w _) = w
		getW (Line (x1,_) (x2,_)) = abs(x1-x2)
		getW (Point _ _) = 1
		setW (Rectangle x y w h) w' = Rectangle x y w' h
		setW (Line s e) _ = Line s e
		setW (Point x y) _ = Point x y

height :: Lens' Shape Int
height = lens getH setH
	where
		getH (Rectangle _ _ _ h) = h
		getH (Line (_,y1) (_,y2)) = abs(y1-y2)
		getH (Point _ _) = 1
		setH (Rectangle x y w h) h' = Rectangle x y w h'
		setH (Line s e) _ = Line s e
		setH (Point x y) _ = Point x y

collides :: Shape -> Shape -> Bool
collides (Rectangle x y w h) (Rectangle x1 y1 w1 h1) = inRange x w x1 w1 && inRange y h y1 h1
	where
		inRange p1 l1 p2 l2 = (p1 <= p2) && (p2 <= p1 + l1) || (p2 <= p1) && (p1 <= p2 +l2)
collides (Point x y) (Point x1 y1) = (x == x1) && (y == y1)
collides (Rectangle x y w h) (Point x1 y1) = inRange x1 x w && inRange y1 y h
	where
		inRange p1 p2 l2 = (p2 <= p1) && (p1 <= p2 + l2)
collides p@(Point _ _) r@(Rectangle{}) = collides r p
collides l1@(Line (x',y') (x1',y1')) l2@(Line (x2',y2') (x3',y3'))
  | l1 == l2 = True
  | crossDir == 0 = False
  | otherwise = (0 <= t) && (t <= 1) && (0 <= u) && (u <= 1)
	where
		crossDir = dx*dy1 - dx1*dy
		t = ((x2-x)*dy1 - (y2-y)*dx1) /  (dx*dy1 - dx1*dy)
		u = ((x2-x)*dy - (y2-y)*dx) /  (dx*dy1 - dx1*dy)
		dx = x1 - x
		dy = y1 - y
		dx1 = x3 - x2
		dy1 = y3 - y2
		x = fromIntegral x'
		x1 = fromIntegral x1'
		x2 = fromIntegral x2'
		x3 = fromIntegral x3'
		y = fromIntegral y'
		y1 = fromIntegral y1'
		y2 = fromIntegral y2'
		y3 = fromIntegral y3'
collides (Point x' y') (Line (lx',ly') (lx1',ly1')) = abs((y - ly1) - ((ly1 - ly)/(lx1 - lx)) * (x - lx1)) < 0.1
	where
		x = fromIntegral x'
		y = fromIntegral y'
		lx = fromIntegral lx'
		ly = fromIntegral ly'
		lx1 = fromIntegral lx1'
		ly1 = fromIntegral ly1'
collides l@(Line _ _) p@(Point _ _) = collides p l
collides (Rectangle x y w h) l@(Line _ _) =
        collides (Line (x,y) (x+w,y)) l ||
	collides (Line (x,y) (x,y+h)) l ||
	collides (Line (x+w,y) (x+w,y+h)) l ||
	collides (Line (x,y+h) (x+w,y+h)) l
collides l@(Line _ _) r@(Rectangle{}) = collides r l
