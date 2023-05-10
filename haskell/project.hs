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



-- Esercizio con liste

-- Funzione che per prima cosa calcola gli indici della cella selezionata
-- coupledList: effettua una zip per ottenere una liste di tuple (posizione,valore)
-- Result: effettua una map che restituisce una lista del tipo [bool], dove
--          gli elementi i cui indici sono i "vicini" della cella selezionata saranno posti a true,
--          mentre tutti gli altri a false
-- NewList: effettua una zip tra la funzione result e la matrice di gioco, ottenendo una lista
--          del tipo (bool,bool) dove il primo elemento indica se la cella è "vicina" alla cella scelta,
--          mentre il secondo elemento indica il valore della cella nella matrice di gioco
-- res: effettua una map che data la tupla (bool,bool) sopra descritta (newList) restituisce una
--      lista di [bool] i cui valori sono quelli della matrice di gioco originale INVERTITI per le posizioni
--      "vicine" alla cella selezionata, mentre sono i valori originali nelle altre posizioni

cleanUp :: (Int, Int) -> [Bool] -> Int -> [Bool]

cleanUp (cols,rows) matrix index =
    let (y,x) = divMod index cols
        coupledList = zip [0..(length matrix)] matrix
        result = map (isAdjacent (y,x) cols . fst) coupledList
        newList = zip result matrix
        res = map changeResult newList
        in res

-- Funzione che data una tupla di booleani verifica se il primo bool è True e nel caso
-- restituisce il secondo elemento invertito, altrimenti restituisce il secondo elemento così com'è
changeResult (x,y)
    | id x = not y
    | otherwise = y

-- Funzione che verifica se un indice è "vicino" o meno ad una cella nella matrice
isAdjacent (y,x) cols index =
    let (y1,x1) = divMod index cols
        dy = abs (y-y1)
        dx = abs (x-x1)
        result = (dx + dy) == 1
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
            let matrix = replicate (cols*rows) False
            gen <- getRng32
            shuffleMatch matrix (cols,rows) 0 gen mosse_random (-1)

-- Funzione che effettua un numero di mosse random scelto dall'utente
-- le mosse non possono essere ripetute
-- se dopo le mosse random il gioco è terminato (potrebbe capitare), allora viene effettuata un'ultima 
-- mossa random per permettere al gioco di riportarsi in uno stato "non vinto" e permettere all'utente di giocare
-- Dopo le mosse random inizia il gioco dell'utente
shuffleMatch matrix (cols,rows) mosse_random rand_generator max_mosse previous_mossa = do
    if mosse_random < max_mosse
        then do
            print $ chunksOf rows matrix
            let rand = randint (0,length matrix - 1) rand_generator
            let randomIndex = fst rand
            let gen = snd rand
            if (previous_mossa /= randomIndex)
                then do
                    print $ "Mossa numero: " ++ show (mosse_random+1) ++ " Indice random: " ++ show randomIndex 
                    let result = cleanUp (cols,rows) matrix randomIndex
                    shuffleMatch result (cols,rows) (mosse_random+1) gen max_mosse randomIndex
            else do
                shuffleMatch matrix (cols,rows) (mosse_random+1) gen max_mosse previous_mossa
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
    print $ chunksOf rows matrix
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
                    print $ chunksOf rows matrix
                    print "Fine gioco"
            else if read index < 0 || read index >= length matrix
                then do
                    print "Cella fuori range [0-length(matrix)-1]"
            else do
                print $ "Mossa numero: " ++ show (mosse_utente+1) ++ " Cella scelta " ++ index 
                let result = cleanUp (cols,rows) matrix (read index)
                mossaUtente result (cols,rows) (mosse_utente+1) 

checkWin matrix = not $ (True `elem` matrix)