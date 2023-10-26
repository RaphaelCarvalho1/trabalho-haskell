
-- pegar vizinhos em uma lista

-- função para cada critério

-- função que cria vetor de n nil

main :: IO()

iteracao2 [] _ = print ""

iteracao2 (l: resto) [(viz1: resto1), (viz2: resto2), (viz3: resto3)] = do 
    print [(viz1: resto1), (viz2: resto2), (viz3: resto3)]
    iteracao2 resto [resto1, resto2, resto3]

iteracao [l1, l2] l3 = do
    iteracao2 l1 [("nil": l3), ("nil": l1), ("nil": l2)]

iteracao (l1: (l2: restoListas)) l3 = do
    iteracao2 l1 [("nil": l3), ("nil": l1), ("nil": l2)]
    iteracao (l2: restoListas) l1

main = do
    (iteracao [["V", "Z", "M", "M"], 
                ["M", "M", "M", "Z"], 
                ["Z", "V", "Z", "V"], 
                ["M", "V", "M", "Z"],
                ["nil", "nil", "nil", "nil"]] ["nil", "nil", "nil", "nil"])
    -- name <- getLine
    -- putStrLn (name ++ ", nome de otario")
    -- print 12