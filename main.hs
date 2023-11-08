
-- funções auxiliares

stringToInt :: String -> Integer
stringToInt s = read s

-- conta casos

conta [] (z, m ,v) = (z, m, v)

conta (vizinho : resto) (z,m,v) = do
    if vizinho == "Z" then (conta resto (z+1, m, v))
    else if vizinho == "M" then (conta resto (z, m+1, v))
    else (conta resto (z, m, v+1))

-- função para cada critério

criterio :: String -> (Integer, Integer, Integer) -> [Char]

criterio no (z, m, v) = if no == "M" && v == 3 then "V"
                        else if no == "V" && z >= 1 then "Z"
                        else if no == "V" && v < 2 then "M"
                        else if no == "V" && v > 3 then "M"
                        else if no == "Z" && v == 0 then "M"
                        else no


-- aplicaCriterio no vizinhos = criterio (no (count vizinhos))

-- função que cria vetor de n nil

crianil l 0 = l

crianil l n = do
    crianil ("nil" : l) (n-1)

-- pegar vizinhos em uma lista

sublista _ 0 = []

sublista [] _ = []

sublista (viz: resto) x = (viz: (sublista resto (x-1)))

removeSegundo [a, b] = [a]

removeSegundo [a, b, c] = [a, c]

removeX [] _ = []

removeX (y: resto) x = if x == y then (removeX resto x) else (y: (removeX resto x))

vizinhos [l1, l2, l3] = removeX ((sublista l1 3) ++ (removeSegundo (sublista l2 3)) ++ (sublista l3 3)) "nil"

-- Funções para gerar nova matriz

mudaLinha :: [String] -> ([String], [String], [String]) -> [String]

mudaLinha [] _ = []

mudaLinha (l: resto) ((viz1: resto1), (viz2: resto2), (viz3: resto3)) = 
    ((criterio l (conta ((vizinhos [(viz1: resto1), (viz2: resto2), (viz3: resto3)])) (0, 0, 0))):
    mudaLinha resto (resto1, resto2, resto3))

mudaMatriz :: [[String]] -> [String] -> [[String]]

mudaMatriz [l1, l2] l3 = [mudaLinha l1 (("nil": l3), ("nil": l1), ("nil": l2))]

mudaMatriz (l1: (l2: restoListas)) l3 =
    ((mudaLinha l1 (("nil": l3), ("nil": l1), ("nil": l2))):
    mudaMatriz (l2: restoListas) l1)

-- Iteração base do programa

itera matriz _ _ 0 = (matriz, 0)

itera matriz matrizanterior dim n = 
    if matrizanterior == matriz 
        then (matriz, n)
        else (itera (mudaMatriz (matriz ++ [(crianil [] dim)]) (crianil [] dim)) matriz  dim (n-1))

-- Interface

quebraLinha :: String -> [String]

quebraLinha [] = []

quebraLinha (l:resto) = if l == ' ' then quebraLinha resto
                        else [[l]] ++ (quebraLinha resto)

lerLinha :: Integer -> IO [String]

lerLinha n = do    
    print ("Escreva a linha " ++ (show n) ++ ":")
    palavra <- getLine
    let linha = quebraLinha palavra
    return linha

printaResultado :: ([[String]], Integer) -> Integer -> IO()

printaResultado (matriz, i) n = do
    print matriz
    if i == 0 then do
        putStrLn ("Após " ++ show (n-i) ++ " iterações, o sistema ainda não estabilizou.")
    else do
        putStrLn ("Estabilizou após "++ (show (n-i)) ++ " iterações.")

leMatrizEExecuta :: Integer -> Integer -> Integer -> [[String]] -> IO()

leMatrizEExecuta dim n i matriz = do
    if dim == i then do
        printaResultado (itera matriz [] dim n) n
    else do
        linha <- (lerLinha i)
        leMatrizEExecuta dim n (i+1) (matriz++[linha])
    

main :: IO()

main = do 
    putStrLn "Digite a dimensão da matriz: "
    m <- getLine
    let dim = stringToInt m
    putStrLn "Digite a quantidade máxima de iterações: "
    nString <- getLine
    let n = stringToInt nString
    leMatrizEExecuta dim n 0 []