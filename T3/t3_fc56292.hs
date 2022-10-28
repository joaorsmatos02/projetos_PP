module Expressoes (
    Expr(),
    constante,
    variavel,
    (|+|),
    (|*|),
    avalia
) where

import Data.Text (replace, pack, unpack)

data Expr = Expressao String

instance Eq Expr where
    (Expressao x) == (Expressao y) = avalia [] (Expressao x) == avalia [] (Expressao y)

instance Show Expr where
    show (Expressao x) = x

constante :: Int -> Expr
constante x = Expressao $ show x

variavel :: String -> Expr
variavel = Expressao

(|+|) :: Expr -> Expr -> Expr
(|+|) x y = Expressao $ "(" ++ show x ++ " + " ++ show y  ++ ")"

(|*|) :: Expr -> Expr -> Expr
(|*|) x y = Expressao $ "(" ++ show x ++ " * " ++ show y  ++ ")"

avalia :: [(String,Int)] -> Expr -> Int
avalia [] (Expressao x) = avaliaAux $ preparaString(substituiZero x)
avalia x (Expressao y) = avaliaAux $ preparaString(substitui x y)

avaliaAux :: [String] -> Int
avaliaAux x 
    | null x = 0
    | length x == 1 = read (avaliaParen (head x)) :: Int
    | otherwise = if proximo == -1 then read (head x) :: Int else avaliaAux $ preparaString (concat (substituiLista x (avaliaParen (x !! proximo)) proximo))
        where proximo = escolheProximo x 0

substituiLista :: [String] -> String -> Int -> [String]
substituiLista x y z = take (max 0 z) x ++ y : drop (min (z + 1) (length x)) x

escolheProximo :: [String] -> Int -> Int
escolheProximo x y
    | length x == 1 = if length (words (head x)) == 3 then y else -1
    | otherwise = if length (words (head x)) == 3 then y else escolheProximo (tail x) (y+1)

divide :: (Char -> Bool) -> String -> [String]
divide x y 
    |xs ==  "" = []
    |otherwise = a : divide x b
        where (a, b) = break x xs
              xs = dropWhile x y

preparaString :: String -> [String]
preparaString = divide (\n -> n == '(' || n == ')')

avaliaParen :: String -> String
avaliaParen xs
    | length x == 1 = head x
    | x !! 1 == "+" = show $ (read (head x) :: Int) + (read (last x) :: Int)
    | otherwise = show $ (read (head x) :: Int) * (read (last x) :: Int)
         where x = words xs

substitui :: [(String,Int)] -> String -> String
substitui x y
    | null x || null y = []
    | length x == 1 = substituiZero aux
    | otherwise = substitui (tail x) aux
        where aux = unpack $ replace (pack (fst(head x))) (pack (show (snd (head x)))) (pack y)

substituiZero :: String -> String
substituiZero x
    | null x = []
    | null (tail x) = if head x `notElem` lista then ['0'] else x
    | otherwise = if head x `notElem` lista then '0' : substituiZero (tail x) else head x : substituiZero (tail x)
        where lista = ['1'..'9'] ++ ['*', '+', ' ', '(', ')']