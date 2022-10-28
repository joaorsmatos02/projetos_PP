module T4_Module (cesar, fromCesar, vigenere, getVigenere, decipherSubts, decipherSubtsBack) where
import Data.Char (ord, isUpper, toUpper, toLower)

lower :: [Char]
lower = ['a'..'z']

upper :: [Char]
upper = ['A'..'Z']

------------------------------------------------------------
-- Cesar
------------------------------------------------------------

cesar :: String -> Int -> String
cesar [] _ = ""
cesar (x:xs) n
    | null xs && x `elem` lower ++ upper = if x `elem` lower then [lower !! mod (n + ord x - 97) 26] else [upper !! mod (n + ord x - 65) 26]
    | null xs = [x]
    | x `elem` lower = lower !! mod (n + ord x - 97) 26 : cesar xs n
    | x `elem` upper = upper !! mod (n + ord x - 65) 26 : cesar xs n
    | otherwise = x : cesar xs n

fromCesar :: String -> Int -> String
fromCesar [] _ = ""
fromCesar (x:xs) n
    | x `elem` lower = if mod (ord x - 97 - n) 26 < 0 then (lower !! (26 + mod (ord x - 97 - n) 26)) : fromCesar xs n else lower !! mod (ord x - 97 - n) 26 : fromCesar xs n
    | x `elem` upper = if mod (ord x - 65 - n) 26 < 0 then (upper !! (26 + mod (ord x - 65 - n) 26)) : fromCesar xs n else upper !! mod (ord x - 65 - n) 26 : fromCesar xs n
    | otherwise = x : fromCesar xs n

------------------------------------------------------------
-- Vigenere
------------------------------------------------------------

vigenere :: String -> String -> Int -> String
vigenere [] [] _ = ""
vigenere [] (_:_) _ = ""
vigenere (x:xs) xy w
    | null xs && x `elem` lower ++ upper = if x `elem` lower then [lower !! mod ((p !! w) + ord x - 97) 26] else [upper !! mod ((p !! w) + ord x - 65) 26]
    | null xs = [x]
    | x `elem` lower = lower !! mod ((p !! w) + ord x - 97) 26 : vigenere xs xy (mod (w+1) (length p))
    | x `elem` upper = upper !! mod ((p !! w) + ord x - 65) 26 : vigenere xs xy (mod (w+1) (length p))
    | otherwise = x : vigenere xs xy w
        where p = map (\n -> mod (ord n - 65) 26) xy

getVigenere :: String -> String -> Int -> String
getVigenere [] [] _ = ""
getVigenere [] (_:_) _ = ""
getVigenere (x:xs) xy w
    | null xs && x `elem` lower ++ upper = if x `elem` lower then [lower !! mod (ord x - 97 - (p !! w)) 26] else [upper !! mod (ord x - 65 - (p !! w)) 26]
    | null xs = [x]
    | x `elem` lower = lower !! abs(mod (ord x - 97 - (p !! w)) 26) : getVigenere xs xy (mod (w+1) (length p))
    | x `elem` upper = upper !! abs(mod (ord x - 65 - (p !! w)) 26) : getVigenere xs xy (mod (w+1) (length p))
    | otherwise = x : getVigenere xs xy w
        where p = map (\n -> mod (ord n - 65) 26) xy

------------------------------------------------------------
-- Subts
------------------------------------------------------------

sameRemover :: [Char] -> [Char]
sameRemover [] = []
sameRemover (x:xs)
                    |x `elem` xs = x : sameRemover(scr x xs)
                    |otherwise = x : sameRemover xs
                    where scr = sameCRemover


sameCRemover :: Char -> [Char] -> [Char]
sameCRemover _ [] = []
sameCRemover x (y:ys)
                    | x == y = sameCRemover x ys
                    | otherwise = y : sameCRemover x ys

alphabet :: [Char] -> [Char]
alphabet x = sameRemover (x ++ ['A'..'Z'])

-- 1º Char -> Frase 
-- 2º Char -> novo Abecedario

decipherSubts :: [Char] -> [Char] -> [Char]
decipherSubts [] _ = []
decipherSubts (x:xs) ys
                        | x `notElem` (['a' .. 'z'] ++ ['A' .. 'Z']) = x : decipherSubts xs ys
                        | isUpper x = toUpper (findEqualSnd (findEqualFst (toLower x) alBet) yCounter) : decipherSubts xs ys
                        | otherwise = findEqualSnd (findEqualFst x alBet) yCounter  : decipherSubts xs ys
                            where yCounter = zip (turnLower (alphabet ys)) [1..];


decipherSubtsBack :: [Char] -> [Char] -> [Char]
decipherSubtsBack [] _ = []
decipherSubtsBack (x:xs) ys
                        | x `notElem` (['a' .. 'z'] ++ ['A' .. 'Z']) = x : decipherSubtsBack xs ys
                        | isUpper x = toUpper (findEqualSnd (findEqualFst (toLower x) yCounter) alBet) : decipherSubtsBack xs ys
                        | otherwise = findEqualSnd (findEqualFst x yCounter) alBet  : decipherSubtsBack xs ys
                            where yCounter = zip (turnLower (alphabet ys)) [1..];


findEqualFst :: Char -> [(Char,Int)] -> Int
findEqualFst x (y:ys) = if x == fst y then snd y else findEqualFst x ys

findEqualSnd :: Int -> [(Char,Int)] -> Char
findEqualSnd x (y:ys) = if x == snd y then fst y else findEqualSnd x ys

alBet :: [(Char, Int)]
alBet = zip ['a'..'z'][1..]

turnLower :: [Char] -> [Char]
turnLower [] = []
turnLower xs = map toLower xs

turnUpper :: [Char] -> [Char]
turnUpper [] = []
turnUpper xs = map toUpper xs

