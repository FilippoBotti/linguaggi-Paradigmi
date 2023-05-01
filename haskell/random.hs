import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sort)
type Rng32 = Word32

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

-- una singola giocata


a_list = randints (1,6) 1234
b_list = randints (1,6) 50000

a_plays = reverse (sort (take 9 a_list))
b_plays = reverse (sort (take 9 b_list))


cmp (first, second) = first  - second


comparazioni = zip (take 3 a_plays) (take 3 b_plays)

-- i risultati indicano quante volte a batte b su tre giocate
risultati = length (filter  (>0) (map cmp comparazioni))