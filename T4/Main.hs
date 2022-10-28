import T4_Module
import QuickCheck
import System.Directory.Internal.Prelude
import Control.Monad 
import Test.QuickCheck 

------------------------------------------------------------

ifStatement :: String -> String -> String -> String -> String
ifStatement a b c d
                    | a == "cesar" && b == "enc" = cesar d (read c)
                    | a == "cesar" && b == "dec" = fromCesar d (read c)
                    | a == "vigenere" && b == "enc" = vigenere d c 0
                    | a == "vigenere" && b == "dec" = getVigenere d c 0
                    | a == "substitui" && b == "enc" = decipherSubts d c
                    | a == "substitui" && b == "dec" = decipherSubtsBack d c
                    | otherwise = "Utilização: \n Main -t -- Corre os testes \n Main método direçao chave -- Executa as cifras"

------------------------------------------------------------
-- main
-----------------------------------------------------------

main :: IO ()
main = do
commands <- getArgs

if length commands == 1 && head commands == "-t"
    then do 
        quickCheck prop_subst
        quickCheck prop_vigenere 
        quickCheck prop_cesar 
        quickCheck prop_cesar_mn
        quickCheck prop_vigenere_subs
        quickCheck prop_vigenere_cesar
        quickCheck prop_subs_cesar
    else if length commands == 3
         then do 
             input <- getContents
             putStrLn (ifStatement (head commands) (commands!!1) (commands!!2) input)
          else
             putStrLn "Utilização: \n Main -t -- Corre os testes \n Main método direçao chave -- Executa as cifras"
           
-- Amar pelos Dois