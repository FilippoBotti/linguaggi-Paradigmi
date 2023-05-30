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

-- collisione tra due Ball: a seconda della direzione della palla la modifico
collide (Ball x y dx dy) (Ball x2 y2 _ _) = Ball x y (update_direction x x2 dx) (update_direction y y2 dy) -- TODO

-- collisione tra palla e tartaruga: verifico che la tartaruga sia viva e modifico la direzione della palla
collide (Ball x y dx dy) (Turtle x2 y2 dead) 
    | dead = Ball x y (update_direction x x2 dx) (update_direction y y2 dy) -- TODO
    | otherwise = Ball x y dx dy

-- collisione tra tartaruga e palla: la tartaruga muore 
collide (Turtle x y _) (Ball _ _ _ _) = Turtle x y True

collide a _ = a

-- modifico la direzione della palla nel caso di una collisione: se la palla arriva da destra (self_dir > other_dir) allora proseguirà verso destra
-- se la palla arriva da sinistra allora proseguirà verso sinistra, stesso ragionamento se viene dall'alto e dal basso.
-- N.B è una traduzione del bounce.py, si sfrutta quindi il valore assoluto di dx (o dy a seconda dei casi) come speed.
update_direction :: Int -> Int -> Int -> Int
update_direction self_dir other_dir speed  
    | other_dir < self_dir = abs speed
    | otherwise = - (abs speed)

-- movimenti orizzontali: si verifica che la palla rimanga nell'arena (self.x>0 && self.x +self.dx +self.w < arena_w)
moveX :: String -> BasicActor  -> BasicActor
moveX keys (Ball x y dx dy) 
    | 0 <= x + dx && x + dx + actorW < maxX = Ball (x + dx) y dx dy
    | otherwise                    = Ball (x - dx) y (-dx) dy
-- movimento random orizzontale del fantasma
moveX keys (Ghost x y rnd) = Ghost x' y rnd'
    where (d, rnd') = randint (-1,1) rnd
          x' = (x + 5 * d) `mod` maxX
-- movimento orizzontale della tartaruga: se viene premuto "d" ci si muove verso destra di 5, se viene premuto "a" verso sinistra di 5 sempre
-- nelle dimensioni dell'arena
moveX "d" (Turtle x y dead) 
    | 0 <= x + 5 && x + 5 + actorW < maxX = Turtle (x+5) y dead
    | otherwise                    = Turtle x y dead
moveX "a" (Turtle x y dead) 
    | 0 <= x - 5 && x - 5 + actorW < maxX = Turtle (x-5) y dead
    | otherwise                    = Turtle x y dead
moveX _ (Turtle x y dead) = Turtle x y dead

-- movimenti verticali, stesse logiche degli orizzontali
moveY :: String -> BasicActor  -> BasicActor
moveY keys (Ball x y dx dy)
    | 0 <= y + dy && y + dy + actorH < maxY = Ball x (y + dy) dx dy
    | otherwise                    = Ball x (y - dy) dx (-dy)
moveY keys (Ghost x y rnd) = Ghost x y' rnd'
    where (d, rnd') = randint (-1,1) rnd
          y' = (y + 5 * d) `mod` maxY
moveY "w" (Turtle x y dead) 
    | 0 <= y + 5 && y + 5 < maxY = Turtle x (y+5) dead
    | otherwise                    = Turtle x y dead
moveY "s" (Turtle x y dead) 
    | 0 <= y - 5 && y - 5 < maxY = Turtle x (y-5) dead
    | otherwise                    = Turtle x y dead
moveY _ (Turtle x y dead) = Turtle x y dead

instance Actor BasicActor where
    rect (Ball x y _ _) = (x, y, actorW, actorH)
    rect (Ghost x y _) = (x, y, actorW, actorH)
    rect (Turtle x y _) = (x, y, actorW, actorH)
    move keys actors (Ball x y dx dy) = 
        -- codice commentato: prima movimento, successivamente verifica delle collisioni (idea abbandonata, ma funzionante)
        -- let update_actors = (update_element actors (Ball x y dx dy) $ (moveX keys . moveY keys) (Ball x y dx dy))
        --     actor_and_collision = filter (checkCollision $ (moveX keys . moveY keys) (Ball x y dx dy)) update_actors
        --     object_after_all_collision = foldl collide ((moveX keys . moveY keys) (Ball x y dx dy)) actor_and_collision
        --     in [object_after_all_collision]
        
        -- codice: prima si verificano le collisioni, poi si muove (come codice bounce.py)
        -- si filtra dalla lista di attori quelli che sono in collisione con la palla
        -- attraverso una fold left si sommano tutti gli effetti degli oggetti in collisione con la palla (sulla palla stessa)
        -- si muove quindi con le funzioni move combinate la palla ottenuta dalla "cascata" di collisioni
        let actor_and_collision = filter (checkCollision $ Ball x y dx dy) actors
            object_after_all_collision = foldl collide (Ball x y dx dy) actor_and_collision
            in [(moveX keys . moveY keys) object_after_all_collision]

        -- si genera un random tra 0 e 10 e si richiama una funzione che restituisce una palla e il fantasma mosso con le move
        -- se il numero random è pari a 0, altrimenti restituisce solo il fantasma mosso con le move
    move keys actors (Ghost x y rnd) = 
        let random_tuple = randint (0,10) rnd
            update_actors = spawn_new_ball (fst random_tuple) (Ghost x y rnd) keys
            in update_actors
        
        -- si filtrano gli oggetti in collisione con la tartaruga e si appliano gli effetti delle collisioni (come per la ball)
        -- si richiama una funzione che verifica se la tartaruga è morta e nel caso restituisce una lista vuota, altrimenti
        -- restituisce la tartaruga spostata con la move
    move keys actors (Turtle x y dead) = 
            let actor_and_collision = filter (checkCollision $ Turtle x y dead) actors
                object_after_all_collision = foldl collide (Turtle x y dead) actor_and_collision
                in remove_dead_turtle keys object_after_all_collision

-- funzione non utilizzata, aggiornava un attore nella lista, utile per la move commentata
-- update_element :: Eq a => [a] -> a -> a -> [a]
-- update_element [] _ _ = []
-- update_element (x:xs) old new
--   | x == old = new : xs
--   | otherwise = x : update_element xs old new

-- funzione che verifica se una tartaruga è morta e la rimuove, altrimenti muove la tartaruga
remove_dead_turtle keys (Turtle x y dead)
    | dead = []
    | otherwise = [(moveX keys . moveY keys) (Turtle x y dead)]

-- funzione che muove il fantasma e spawna una Ball se il numero random è pari a 0
spawn_new_ball random_value (Ghost x y rnd) keys
    | random_value == 0 = [Ball (min 300 x) (min 220 y) 5 5, (moveX keys . moveY keys) (Ghost x y rnd)]
    | otherwise = [(moveX keys . moveY keys) (Ghost x y rnd)]

main = do
    rnd <- getRng32
    operateArena (Arena [Ball 200 100 5 5, Ball 230 120 (-5) (-5), Ghost 100 100 rnd, Turtle 160 120 False])
