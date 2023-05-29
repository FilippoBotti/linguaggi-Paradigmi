{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Control.Monad
import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sort)
type Rng32 = Word32

-- RANDOM
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
-- /RANDOM



class (Show a) => Actor a where
    move :: String -> [a] -> a -> [a]
    rect :: a -> (Int, Int, Int, Int)  -- (x, y, w, h)

data Arena a = Arena { actors :: [a]
                     } deriving (Show)

tick :: (Actor a) => Arena a -> String -> Arena a
tick (Arena actors) keys = Arena $ concat (map (move keys actors) actors)

operateArena :: (Actor a) => Arena a -> IO ()
operateArena arena = do
    print arena
    line <- getLine
    when (line /= "q") $ operateArena (tick arena line)

checkCollision :: (Actor a) => a -> a -> Bool
checkCollision a1 a2 = (rect a1) /= (rect a2) && y2 < y1+h1 && y1 < y2+h2 && x2 < x1+w1 && x1 < x2+w2
    where
        (x1, y1, w1, h1) = rect a1
        (x2, y2, w2, h2) = rect a2

maxX = 320
maxY = 240
actorW = 20
actorH = 20

data BasicActor = Ball { x :: Int, y :: Int, dx :: Int, dy :: Int }
                | Ghost { x :: Int, y :: Int, rnd :: Rng32}
                | Turtle { x :: Int, y :: Int, dead :: Bool} deriving (Show)

instance Eq BasicActor where
    (Ball x1 y1 _ _) == (Ball x2 y2 _ _) = x1 == x2 && y1 == y2
    (Ghost x1 y1 _) == (Ghost x2 y2 _) = x1 == x2 && y1 == y2
    (Turtle x1 y1 _) == (Turtle x2 y2 _) = x1 == x2 && y1 == y2
    _ == _ = False

collide :: BasicActor -> BasicActor -> BasicActor

collide (Ball x y dx dy) (Ball x2 y2 _ _) = Ball x y (update_direction x x2 dx) (update_direction y y2 dy) -- TODO


collide (Ball x y dx dy) (Turtle x2 y2 dead) 
    | dead = Ball x y (update_direction x x2 dx) (update_direction y y2 dy) -- TODO
    | otherwise = Ball x y dx dy

collide (Turtle x y _) (Ball _ _ _ _) = Turtle x y True

collide a _ = a

update_direction :: Int -> Int -> Int -> Int
update_direction self_dir other_dir dx  
    | other_dir < self_dir = dx
    | otherwise = -dx


moveX :: BasicActor -> BasicActor
moveX (Ball x y dx dy)
    | 0 <= x + dx && x + dx < maxX = Ball (x + dx) y dx dy
    | otherwise                    = Ball (x - dx) y (-dx) dy
moveX (Ghost x y rnd) = Ghost x' y rnd'
    where (d, rnd') = randint (-1,1) rnd
          x' = (x + 5 * d) `mod` maxX
moveX (Turtle x y dead)
    | 0 <= x + 5 && x + 5 < maxX = Turtle (x+5) y dead
    | otherwise                    = Turtle x y dead

moveY :: BasicActor -> BasicActor
moveY (Ball x y dx dy)
    | 0 <= y + dy && y + dy < maxY = Ball x (y + dy) dx dy
    | otherwise                    = Ball x (y - dy) dx (-dy)
moveY (Ghost x y rnd) = Ghost x y' rnd'
    where (d, rnd') = randint (-1,1) rnd
          y' = (y + 5 * d) `mod` maxY
moveY (Turtle x y dead)
    | 0 <= y + 5 && y + 5 < maxY =Turtle x (y+5) dead
    | otherwise                    = Turtle x y dead

instance Actor BasicActor where
    rect (Ball x y _ _) = (x, y, actorW, actorH)
    rect (Ghost x y _) = (x, y, actorW, actorH)
    rect (Turtle x y _) = (x, y, actorW, actorH)
    move keys actors (Ball x y dx dy) = 
        let actor_and_collision = zip actors $ map (checkCollision $ (moveX . moveY) (Ball x y dx dy)) actors
            actor_collision = get_obj_in_collision actor_and_collision
            object_after_all_collision = foldl collide ((moveX . moveY) (Ball x y dx dy)) actor_collision
            in [object_after_all_collision]
    move keys actors (Ghost x y rnd) =         
        let actor_and_collision = zip actors $ map (checkCollision $ (moveX . moveY) (Ghost x y rnd)) actors
            actor_collision = get_obj_in_collision actor_and_collision
            object_after_all_collision = foldl collide ((moveX . moveY) (Ghost x y rnd)) actor_collision
            in [object_after_all_collision]
    move keys actors (Turtle x y dead) = 
        let actor_and_collision = zip actors $ map (checkCollision $ (moveX . moveY) (Turtle x y dead)) actors
            actor_collision = get_obj_in_collision actor_and_collision
            object_after_all_collision = foldl collide ((moveX . moveY) (Turtle x y dead)) actor_collision
            in [object_after_all_collision]

-- update_element :: Eq a => [a] -> a -> a -> [a]
-- update_element [] _ _ = []
-- update_element (x:xs) old new
--   | x == old = new : xs
--   | otherwise = x : update_element xs old new

get_obj_in_collision :: [(BasicActor, Bool)] -> [BasicActor]
get_obj_in_collision xs = [x | (x, flag) <- xs, flag]

main = do
    rnd <- getRng32
    operateArena (Arena [Ball 200 100 5 5, Ball 230 120 (-5) (-5), Ghost 100 100 rnd, Turtle 160 120 False])
