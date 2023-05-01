-- BOTTI FILIPPO MATR. 333653
import System.IO (readFile, writeFile)
import Data.Char (toUpper)
import Data.List (transpose)

parseInt = read :: String -> Int

-- Main utilizzato per debuggare il codice passo passo, sfruttando i risultati delle funzioni. Codice altamente impuro. Utilizzare solo per verifica
-- main = do
--     contents <- readFile "skyscrapers-3x3.txt"
--     putStr contents
    -- print "Parsing del file in lista di interi:"
    -- print (transpose (parseFileToListOfInteger contents))
    -- print "Elementi su cui verificare i vincoli dall'alto e dal basso:"
    -- print (getOnlyElementsToCheck (transpose (parseFileToListOfInteger contents)))
    -- print "Tuple dall'alto (vincolo,skyscraper) con vincolo diverso da zero:"
    -- print (getOnlyLeftPositiveConstraints (getOnlyLeftConstraints (transpose (parseFileToListOfInteger contents))) (getTettiFromLeft (getOnlyElementsToCheck ((transpose (parseFileToListOfInteger contents))))))
    -- print "Tuple dal basso (vincolo,skyscraper) con vincolo diverso da zero:"
    -- print (getOnlyRightPositiveConstraints (getOnlyRightConstraints (transpose (parseFileToListOfInteger contents))) (getTettiFromRight (getOnlyElementsToCheck (transpose (parseFileToListOfInteger contents)))))
    -- print "Elementi su cui verificare i vincoli da sinistra e da destra:"
    -- print (getOnlyElementsToCheck (parseFileToListOfInteger contents))
    -- print "Tuple da sinistra (vincolo,skyscraper) con vincolo diverso da zero:"
    -- print (getOnlyLeftPositiveConstraints (getOnlyLeftConstraints (parseFileToListOfInteger contents)) (getTettiFromLeft (getOnlyElementsToCheck (parseFileToListOfInteger contents))))
    -- print "Tuple da destra (vincolo,skyscraper) con vincolo diverso da zero:"
    -- print (getOnlyRightPositiveConstraints (getOnlyRightConstraints (parseFileToListOfInteger contents)) (getTettiFromRight (getOnlyElementsToCheck (parseFileToListOfInteger contents))))

-- Main effettivo
main = do
    contents <- readFile "skyscrapers-3x3.txt"
    putStr contents
    if (checkHorizontalConstraints contents == 0) && (checkVerticalConstraints contents == 0)
        then return "Skyscraper corretto."
    else return "Skyscraper non corretto."


-- Funzione che prende una stringa in input e resituisce una matrice di interi sotto la forma lista di liste: [[Int]]
parseFileToListOfInteger :: String -> [[Int]]
parseFileToListOfInteger input =
    let righeInLista = lines input
        numeriInRiga = map words righeInLista
        listeInteri = map (map parseInt) numeriInRiga
    in listeInteri

-- Funzione che data una matrice restituisce la prima colonna. Utilizzata per i vincoli di sinistra 
-- e (passando la matrice trasposta come argomento) i vincoli dall'alto
getOnlyLeftConstraints :: [[Int]] -> [Int]
getOnlyLeftConstraints listOfIntegers = concat $ drop 1 . init $ map (take 1) listOfIntegers

-- Funzione che data una matrice restituisce l'utlima colonna. Utilizzata per i vincoli di destra
-- e (passando la matrice trasposta come argomento) i vincoli dalbasso
getOnlyRightConstraints :: [[Int]] -> [Int]
getOnlyRightConstraints listOfIntegers = concat $ drop 1 . init $ map (take 1 . reverse) listOfIntegers

-- Funzione che data una matrice restituisce restituisce una matrice contenente i soli elementi centrali.
-- Usata per eliminare i vincoli e ottenere solo le celle su cui applicare l'algoritmo skyscrapers
getOnlyElementsToCheck :: [[Int]] -> [[Int]]
getOnlyElementsToCheck listOfIntegers = drop 1 . init $ map (drop 1 . init) listOfIntegers

-- Funzione che applica l'algoritmo skyscraper su una matrice e restituisce un valore per ogni riga, corrispondente
-- al numero delle volte in cui cambia il massimo (da sinistra) sulla rispettiva riga. Utilizzata anche
-- per la verifica dall'alto
getTettiFromLeft :: [[Int]] -> [Int]
getTettiFromLeft = map skyscrapers

-- Funzione duale della precedente ma applicata da destra. Utilizzata anche per la verifica dal basso
getTettiFromRight :: [[Int]] -> [Int]
getTettiFromRight = map (skyscrapers . reverse)

-- Funzione che effettua una zip tra due liste di interi e restituisce come valore una lista di tuple 
-- il cui primo elemento è sempre maggiore di zero. Utilizzata per filtrare solo i risultati
-- dell'algoritmo skyscraper solo per i vincoli maggiori di zero
getOnlyPositiveConstraints :: [Int] -> [Int] -> [(Int,Int)]
getOnlyPositiveConstraints elements tetti = filter ((>0) . fst) $ zip elements tetti

-- Funzione che verifica se il numero di volte in cui cambia il massimo (algoritmo skyscraper) è
-- uguale al vincolo imposto. Restituisce una lista di tutti 0 se i vincoli sono tutti rispettati
checkConstraints :: [(Int,Int)] -> [Int]
checkConstraints constraints = filter (/=0) (map (\(x,y) -> x-y) constraints)

-- Funzione che verifica i vincoli laterali di una matrice. Restituisce 0 se ogni vincolo è rispettato
-- mentre resituisce un numero diverso da zero se vi è almeno un vincolo non rispettato
checkHorizontalConstraints :: String -> Int
checkHorizontalConstraints input =
    let listeInteri = parseFileToListOfInteger input
        firstElements = getOnlyLeftConstraints listeInteri
        lastElements = getOnlyRightConstraints listeInteri
        elemToCheck = getOnlyElementsToCheck listeInteri
        tettiFromLeft = getTettiFromLeft elemToCheck
        tettiFromRight = getTettiFromRight elemToCheck
        leftConstraints = getOnlyPositiveConstraints firstElements tettiFromLeft
        rightConstraints = getOnlyPositiveConstraints lastElements tettiFromRight
        leftResult = checkConstraints leftConstraints
        rightResult = checkConstraints rightConstraints
        in length leftResult + length rightResult

-- Funzione che verifica i vincoli verticali di una matrice. Restituisce 0 se ogni vincolo è rispettato
-- mentre resituisce un numero diverso da zero se vi è almeno un vincolo non rispettato
checkVerticalConstraints :: String -> Int
checkVerticalConstraints input =
    let listeInteri = transpose (parseFileToListOfInteger input)
        firstElements = getOnlyLeftConstraints listeInteri
        lastElements = getOnlyRightConstraints listeInteri
        elemToCheck = getOnlyElementsToCheck listeInteri
        tettiFromLeft = getTettiFromLeft elemToCheck
        tettiFromRight = getTettiFromRight elemToCheck
        leftConstraints = getOnlyPositiveConstraints firstElements tettiFromLeft
        rightConstraints = getOnlyPositiveConstraints lastElements tettiFromRight
        leftResult = checkConstraints leftConstraints
        rightResult = checkConstraints rightConstraints
        in length leftResult + length rightResult


-- Funzione che applica l'algoritmo skyscraper
skyscrapers' [] (max,counter) = counter
skyscrapers' (x:xs) (max, counter)
    | max >=  x = skyscrapers' xs (max, counter)
    | otherwise = skyscrapers' xs (x, counter +1)

skyscrapers :: [Int] -> Int
skyscrapers list = skyscrapers' list (0, 0)

