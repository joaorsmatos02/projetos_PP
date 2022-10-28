module T2_fc56292 where

votosCandidato :: [(String, (String, String, String))] -> String -> Int
votosCandidato a b
        | length a == 1 = votosPais (snd(head a)) b
        | otherwise = votosPais (snd(head a)) b + votosCandidato (tail a) b

votosPais :: (String, String, String) -> String -> Int
votosPais (a,b,c) d
        | a == d = 3
        | b == d = 2
        | c == d = 1
        | otherwise = 0
 
vencedorEleicao :: [(String,(String,String,String))] -> String
vencedorEleicao a = fst (head (filter ((==maximum (map snd pares)).snd) pares))
                where pares = votosPar a (map fst a)

votosPar :: [(String,(String,String,String))] -> [String] -> [(String, Int)]
votosPar a b
        | length b == 1 = [(head b, votosCandidato a (head b))]
        | otherwise = (head b, votosCandidato a (head b)) : votosPar a (tail b)