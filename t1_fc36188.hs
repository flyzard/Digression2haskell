{--
Princípios de Programação
Trabalho para casa 1

João Felizardo - fc36188
--}

-- Obtem um ponto equidistante dos dados três pontos
triangulacao :: (Floating a) => ((a, a), (a, a), (a, a)) -> (a, a)
triangulacao ((x1, y1), (x2, y2), (x3, y3)) = (((x1 + x2 + x3) / 3), ((y1 + y2 + y3) / 3))


-- Obtem a distância entre dois pontos
distancia :: (Floating t) => (t, t) -> (t, t) -> t
distancia (x1 , y1) (x2 , y2) = sqrt (x*x + y*y)
    where
      x = x1 - x2
      y = y1 - y2

-- Obtem a distância de um ponto à origem do referencial (0,0)
distanciaOrigem :: (Floating a) => [(a, a)] -> [a]
distanciaOrigem ((a,b):xs) = distancia (a, b) (0, 0) : distanciaOrigem xs
distanciaOrigem _ = []

-- Recebendo um par de pares refrentes a dois pontos, calcula o ponto sucessivo
pointSucc :: (Floating a) => ((a, a), (a, a)) -> (a, a)
pointSucc (y, z) = (c + (c-a), d + (d-b))
        where
            a = fst y
            b = snd y
            c = fst z
            d = snd z

-- Obtem os ultimos elementos de uma lista de tupulos
takeLastTwo :: (Floating a) => [(a, a)] -> ((a, a), (a, a))
takeLastTwo [] = error "No takeLastTwo for empty lists!"
takeLastTwo (x:[]) = error "No takeLastTwo for one element lists!"
takeLastTwo xs = (l!!1, l!!0)
    where
        l = reverse xs

-- Com base nos ultimos dois elementos da lista de tupulos, calcula o proximo ponto
proximoPonto :: (Floating a) => [(a, a)] -> (a, a)
proximoPonto [] = error "No proximoPonto for empty lists!"
proximoPonto (x:[]) = error "No proximoPonto for one element lists!"
proximoPonto xs = pointSucc (takeLastTwo xs)
