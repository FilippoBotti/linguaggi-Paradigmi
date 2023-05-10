import Control.Monad
import Distribution.Compat.Graph (neighbors)
import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sort)
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
                       }
                | Ghost { x :: Int
                        , y :: Int
                        , rnd :: Rng32
                        } deriving (Show)

moveX :: BasicActor -> BasicActor
moveX (Ball x y dx dy)
    | 0 <= x + dx && x + dx < maxX = Ball (x + dx) y dx dy
    | otherwise                    = Ball (x - dx) y (-dx) dy
moveX (Ghost x y rnd) = Ghost x' y rnd'
    where (d, rnd') = randint (-1,1) rnd
          x' = (x + 5 * d) `mod` maxX

moveY :: BasicActor -> BasicActor
moveY (Ball x y dx dy)
    | 0 <= y + dy && y + dy < maxY = Ball x (y + dy) dx dy
    | otherwise                    = Ball x (y - dy) dx (-dy)
moveY (Ghost x y rnd) = Ghost x y' rnd'
    where (d, rnd') = randint (-1,1) rnd
          y' = (y + 5 * d) `mod` maxY

instance Actor BasicActor where
    move = moveX . moveY 

----

data Wall = Wall { wx :: Int
                 , wy :: Int
                 } deriving (Show)

instance Actor Wall where
    move = id    -- move w = w
        
----

main = do
    rnd <- getRng32
    operateArena (Arena [Ball 200 100 5 5, Ghost 100 100 rnd])
    -- try to add a Wall to the actors
