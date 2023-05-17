import Control.Monad
import Distribution.Compat.Graph (neighbors)
import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sort, tails)
import Control.Monad (when)

type Rng32 = Word32


-- Random
chunksOf n [] = []
chunksOf n xs = a : chunksOf n b where
  (a, b) = splitAt n xs

xorshift32 :: Rng32 -> Rng32
xorshift32 a = d where
  b = a `xor` (a `shiftL` 13)
  c = b `xor` (b `shiftR` 17)
  d = c `xor` (c `shiftL` 5)

randint :: (Int, Int) -> Rng32 -> (Int, Rng32)
randint (nmin, nmax) gen = (val, nxt) where
    nxt = xorshift32 gen
    val = nmin + (fromIntegral nxt) `mod` (nmax + 1 - nmin)

randints :: (Int, Int) -> Rng32 -> [Int]
randints range gen =
    val : randints range nxt
    where (val, nxt) = randint range gen

getRng32 :: IO Rng32
getRng32 = do
    now <- getPOSIXTime
    return (round (now * 1000) :: Rng32)








class (Show a) => Actor a where
    move :: a -> a

data Arena a = Arena { actors :: [a]
                     } deriving (Show)

tick :: (Actor a) => Arena a -> Arena a
tick (Arena actors) = Arena (map move actors)

operateArena :: (Actor a) => Arena a -> IO ()
operateArena a = do
    print a
    line <- getLine
    when (null line) $ operateArena (tick a)

----

maxX = 320
maxY = 240

data BasicActor = Ball { x :: Int
                       , y :: Int
                       , dx :: Int
                       , dy :: Int
                       , w :: Int
                       , h :: Int
                       }
                | Ghost { x :: Int
                        , y :: Int
                        , rnd :: Rng32
                        , w :: Int
                        , h :: Int
                        } 
                |  Wall { wx :: Int
                        , wy :: Int
                        , w :: Int
                        , h :: Int
                        } deriving (Show)

moveX :: BasicActor -> BasicActor
moveX (Ball x y dx dy w h)
    | 0 <= x + dx && x + dx < maxX = Ball (x + dx) y dx dy w h
    | otherwise                    = Ball (x - dx) y (-dx) dy w h
moveX (Ghost x y rnd w h) = Ghost x' y rnd' w h
    where (d, rnd') = randint (-1,1) rnd 
          x' = (x + 5 * d) `mod` maxX
moveX (Wall wx wy w h) = Wall wx wy w h

moveY :: BasicActor -> BasicActor
moveY (Ball x y dx dy w h)
    | 0 <= y + dy && y + dy < maxY = Ball x (y + dy) dx dy w h
    | otherwise                    = Ball x (y - dy) dx (-dy) w h
moveY (Ghost x y rnd w h) = Ghost x y' rnd' w h
    where (d, rnd') = randint (-1,1) rnd 
          y' = (y + 5 * d) `mod` maxY
moveY (Wall wx wy w h) = Wall wx wy w h

instance Actor BasicActor where
    move = moveX . moveY 

----
allPairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

collisionCheck :: BasicActor -> BasicActor -> Bool
collisionCheck (Ball x1 y1 dx dy w1 h1) (Wall x2 y2 w2 h2) = y2 < y1 + h1 && y1 < y2 + h2 && x2 < x1 + w1 && x1 < x2 + w2
            
collision :: BasicActor -> BasicActor -> BasicActor
collision (Ball x1 y1 dx dy w1 h1) (Wall x2 y2 w2 h2)
    | collisionCheck (Ball x1 y1 dx dy w1 h1) (Wall x2 y2 w2 h2) = move $ Ball x1 y1 (updateHorizontalSpeed x1 dx x2) (updateVerticalSpeed y1 dy y2) w1 h1
    | otherwise = Ball x1 y1 dx dy w1 h1

-- Aggiordno dx,dy in base a dove collido
updateHorizontalSpeed x1 dx other_x
    | x1 > other_x = -dx
    | otherwise =  dx

updateVerticalSpeed y1 dy other_y
    | y1 > other_y = -dy
    | otherwise = dy


-- instance Actor Wall where
--     move = id    -- move w = w
        
----

main = do
    rnd <- getRng32
    operateArena (Arena [Ball 200 100 5 5 10 10, Ghost 100 100 rnd 10 10, Wall 50 50 20 20])
    -- try to add a Wall to the actors
