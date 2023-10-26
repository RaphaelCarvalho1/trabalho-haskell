
stringToInt :: String -> Integer
stringToInt s = read s

-- função para cada critério



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

main :: IO()

iteracao2 [] _ = print ""

iteracao2 (l: resto) [(viz1: resto1), (viz2: resto2), (viz3: resto3)] = do 
    print (vizinhos [(viz1: resto1), (viz2: resto2), (viz3: resto3)])
    iteracao2 resto [resto1, resto2, resto3]

iteracao [l1, l2] l3 = do
    iteracao2 l1 [("nil": l3), ("nil": l1), ("nil": l2)]

iteracao (l1: (l2: restoListas)) l3 = do
    iteracao2 l1 [("nil": l3), ("nil": l1), ("nil": l2)]
    iteracao (l2: restoListas) l1

main = do
    n <- getLine
    let h = stringToInt n
    (iteracao [["V", "Z", "M", "M"], 
                ["M", "M", "M", "Z"], 
                ["Z", "V", "Z", "V"], 
                ["M", "V", "M", "Z"],
                (crianil [] h)] (crianil [] h))