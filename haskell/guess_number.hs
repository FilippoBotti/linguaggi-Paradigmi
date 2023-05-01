import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sort)
import Control.Monad (when)

type Rng32 = Word32


main = do
    generatore <- getRng32
    let number = fst (randint (1,90) generatore)
    askForNumber number 10



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



askForNumber :: Int -> Int -> IO ()
askForNumber selected_num 0 = putStrLn "hai finito i tentativi"
askForNumber selected_num tentativi = do
    putStrLn "Which number (1-90) am I thinking of?"
    guess <- getLine
    when (not $ null guess) $
        if guess == show selected_num
        then putStrLn "You are correct!"
        else do
            if ((read guess) < selected_num)
            then putStrLn $ "Hai scelto un numero troppo piccolo, tentativi rimasti: " ++ show tentativi 
            else do
                 putStrLn $ "Hai scelto un numero troppo grande, tentativi rimasti:" ++ show tentativi
            askForNumber selected_num (decrementa_tentativi tentativi)

decrementa_tentativi tent = tent -1