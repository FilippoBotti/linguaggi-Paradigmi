import Distribution.Compat.Graph (neighbors)
import Data.Word (Word32)
import Data.Bits (xor, shiftL, shiftR)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sort)
import Control.Monad (when)
import Data.Array

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






-- Esercizio con array

-- Funzione che prende un array ed effettua una mossa cleanup
-- Per prima cosa calcoliamo le coordinate corrispondenti all'indice,
-- successivamente calcoliamo i 4 vicini e li andiamo a modificare negando il rispettivo valore
cleanUp :: (Int, Int) -> Array Int Bool -> Int -> Array Int Bool


cleanUp (cols,rows) matrix index =
    let (y,x) = divMod index cols
        upperNeig = x-1 + y*cols
        downNeig = x+1 + y*cols
        leftNeig = x + (y+1)*cols
        rightNeig = x + (y-1)*cols
        result = matrix // [(i, not $ matrix ! i) |  i <- [0..rows*cols-1], i == upperNeig || i==leftNeig || i==rightNeig || i==downNeig]
        in result



main = do
    putStrLn "Quante colonne?"
    cols_input <- getLine
    putStrLn "Quante righe?"
    rows_input <- getLine
    putStrLn "Quante mosse random?"
    mosse_random_input <- getLine
    when ((not $ null cols_input) && (not $ null rows_input) && (not $ null mosse_random_input)) $
        do
            let cols = read cols_input
            let rows = read rows_input
            let mosse_random = read mosse_random_input
            let matrix = array (0, cols*rows-1) [(i, False) | i <- [0..cols*rows-1]]
            gen <- getRng32
            shuffleMatch matrix (cols,rows) 0 gen mosse_random (-1)


checkWin matrix = not $ (False `elem` matrix)

-- Funzione che effettua un numero di mosse random scelto dall'utente
-- le mosse non possono essere ripetute
-- se dopo le mosse random il gioco è terminato (potrebbe capitare), allora viene effettuata un'ultima 
-- mossa random per permettere al gioco di riportarsi in uno stato "non vinto" e permettere all'utente di giocare
-- Dopo le mosse random inizia il gioco dell'utente
shuffleMatch matrix (cols,rows) mosse_random rand_generator max_mosse previous_mossa = do
    print matrix
    if mosse_random < max_mosse
        then do
            let rand = randint (0,length matrix - 1) rand_generator
            let randomIndex = fst rand
            let gen = snd rand
            if (previous_mossa /= randomIndex)
                then do
                    print $ "Mossa numero: " ++ show (mosse_random+1) ++ " Indice random: " ++ show randomIndex 
                    let result = cleanUp (cols,rows) matrix randomIndex
                    shuffleMatch result (cols,rows) (mosse_random+1) gen max_mosse randomIndex
            else do
                shuffleMatch matrix (cols,rows) (mosse_random) gen max_mosse previous_mossa
    else do
        if checkWin matrix
           then shuffleMatch matrix (cols,rows) (max_mosse-1) rand_generator max_mosse previous_mossa
        else do
            print "Fine shuffle, inizio gioco utente"
            mossaUtente matrix (cols,rows) 0 

-- Funzione che permette all'utente di giocare in base alla cella inserita come input
-- inoltre viene richiamato il controllo per verificare che l'utente abbia finito
-- La cella selezionata dall'utente deve essere nel range corretto di valori, altrimenti
-- viene richiesta un'altra cella. 
-- Se l'utente vuole terminare il gioco deve inserire -1
mossaUtente matrix (cols,rows) mosse_utente   = do
    print matrix
    if checkWin matrix
        then do
            print "Hai vinto!" 
            print matrix
    else do
        putStrLn "Which cell? (-1 per uscire)"
        index <- getLine
        when (not $ null index) $
            if read index == -1 
                then do
                    print matrix
                    print "Fine gioco"
            else do
            if read index < 0 || read index >= length matrix
                then do
                    print "Cella fuori range [0-length(matrix)-1]"
            else do
                print $ "Mossa numero: " ++ show (mosse_utente+1) ++ " Cella scelta " ++ index 
                let result = cleanUp (cols,rows) matrix (read index)
                mossaUtente result (cols,rows) (mosse_utente+1) 

