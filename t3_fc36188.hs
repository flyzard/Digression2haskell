{--
Princípios de Programação
Trabalho para casa 3

João Felizardo - fc36188
--} 

-- Calcula a distância entre dois pontos
distanceBetweenTwoPoints :: (Ord a, Floating a) => (a, a) -> (a, a) -> a
distanceBetweenTwoPoints (x1, y1) (x2, y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- Calcula a distância total de uma lista de pontos
distancia :: (Ord a, Floating a) => [(a, a)] -> a
distancia [] = 0
distancia (x:[]) = 0
distancia (x:s:xs) = (distanceBetweenTwoPoints x s) + distancia (s:xs)

minimaDistanciaA :: (Ord a, Floating a) => (a, a) -> [(a, a)] -> a
minimaDistanciaA _ [] = 0
minimaDistanciaA p xs = foldl1 (\x y -> min x y) $ map (distanceBetweenTwoPoints p) xs -- min (distancia (p:x:[])) (minimaDistanciaA p xs)

-- Verifica se um determinado ponto p está à distância de pelo menos d de todos os pontos da lista xs
isAtMinDistance :: (Ord a, Floating a) => a -> [(a, a)] -> (a, a) -> Bool
isAtMinDistance d ys p = (minimaDistanciaA p ys) >= d

-- Evita pontos de xs que estejam a menos de d distancia dos pontos de ys
evitaPontos :: (Floating a, Ord a) => a -> [(a, a)] -> [(a, a)] -> [(a, a)]
evitaPontos _ ys [] = []
evitaPontos _ [] xs = xs
evitaPontos 0 _ xs = xs
evitaPontos d ys xs = filter (isAtMinDistance d ys) xs --[x | x<-xs, (minimaDistanciaA x ys) >= d]
