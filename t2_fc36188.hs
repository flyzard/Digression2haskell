{--
Princípios de Programação
Trabalho para casa 2

João Felizardo - fc36188
--} 

-- Calcula a distância entre dois pontos
distance :: (Ord a, Floating a) => (a, a) -> (a, a) -> a
distance (x1 , y1) (x2 , y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- Calcula a distância total de uma lista de pontos
distancia :: (Ord a, Floating a) => [(a, a)] -> a
distancia [] = 0
distancia (x:[]) = 0
distancia (x:s:xs) = distance s x + distancia (s:xs)

-- Dado duas listas de pontos e um ponto inicial, retorna um persurso que que começa no
-- ponto inicial, seguido de todos os pontos das listas, mantendo a ordem relativa a
-- cada lista, de modo a ter o percurso mais curto possível.
fundePercursos :: (Floating a, Ord a) => [(a, a)] -> [(a, a)] -> (a, a) -> [(a, a)]
fundePercursos xs [] t = t:xs
fundePercursos [] ys t = t:ys
fundePercursos (x:xs) (y:ys) t 
    | (distance x t) > (distance y t) = t:(fundePercursos (x:xs) ys y)
    | otherwise = t:(fundePercursos xs (y:ys) x)

-- Dado uma lista de pontos e um novo ponto p, devolve a distancia mais 
-- curta em que p se encontra entre o dois pontos consecutivos.
getShortestDistance :: (Ord a, Floating a) => [(a, a)] -> (a, a) -> a
getShortestDistance (xs:[]) _ = 0
getShortestDistance (x:s:[]) p= distancia (x:p:s:[])
getShortestDistance (x:s:xs) p = min (distancia (x:p:s:[])) (getShortestDistance (s:xs) p)

-- Recebe uma lista l e um ponto p, e insere p na lista l, na posição 
-- em que a distância total é a minimiza possível.
adicionaParagem :: (Floating a, Ord a) => [(a, a)] -> (a, a) -> [(a, a)]
adicionaParagem [] p = p:[]
adicionaParagem (x:[]) p = p:x:[]
adicionaParagem (x:s:[]) p = x:p:s:[]
adicionaParagem all@(x:s:xs) p 
    | distancia (x:p:s:[]) == (getShortestDistance all p) = x:p:s:xs
    | otherwise = x:(adicionaParagem (s:xs) p)

