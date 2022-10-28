module T1_fc56292 where

-- A

checksum :: [Int] -> [Int]
checksum [] = [0]
checksum x = 10 - mod (sum (zipWith (*) x [1..length x])) 10 : x

-- B

vogais :: [Char]
vogais = ['a', 'e', 'i', 'o', 'u']

consoantes :: [Char]
consoantes = [x | x <- ['a'..'z'], x `notElem` vogais]

segunda :: [String]
segunda = [a : [b] | a <- consoantes, b <- vogais]

terceira :: [String]
terceira = [a : [b] | a <- vogais, b <- consoantes]

palavras :: [String]
palavras = [a : b ++ c |
   a <- vogais,
   b <- segunda,
   a /= head b,
   c <- terceira,
   last b /= head c]