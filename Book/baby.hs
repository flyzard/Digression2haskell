-- Exercicios


doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
    then x
    else x*2

boomBangs xs = [ if x < 10  then "BOOM!" else "BANG!" | x <- xs, odd x]


circumference' :: Double -> Double
circumference' r = 2 * pi * r 


lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"


factorial :: (Integral a) => a -> a 
factorial 0 = 1  
factorial n = n * factorial (n - 1)

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  


tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  


bmiTell :: (RealFloat a) => a -> String
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!" 


-- bmiTell :: (RealFloat a) => a -> a -> String  
-- bmiTell weight height  
--     | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
--     | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
--     | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
--     | otherwise                 = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b 


-- bmiTell :: (RealFloat a) => a -> a -> String  
-- bmiTell weight height  
--     | bmi <= skinny = "You're underweight, you emo, you!"  
--     | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
--     | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
--     | otherwise     = "You're a whale, congratulations!"  
--     where bmi = weight / height ^ 2
--         skinny = 18.5  
--         normal = 25.0  
--         fat = 30.0


head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  


cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  


maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 

-- equidistantes ::                       

-- triangulacao :: (Float t) => ((t, t), (t, t), (t, t)) -> (a,a)
-- triangulacao ((x1, y1), (x2, y2), (x3, y3)) = 3.14

distance :: (Ord a, Floating a) => (a, a) -> (a, a) -> a
distance (x1 , y1) (x2 , y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)


triangulacao :: (Floating a) => ((a, a), (a, a), (a, a)) -> (a, a)
triangulacao ((x1, y1), (x2, y2), (x3, y3)) = (((x1 + x2 + x3) / 3), ((y1 + y2 + y3) / 3))

distanciaOrigem :: (Ord a, Floating a) => [(a, a)] -> [a]
-- distanciaOrigem xs = [dis | (x, y) <- xs, let dis = distance (x, y)]
distanciaOrigem ((a,b):xs) = distance (a, b) (0, 0) : distanciaOrigem xs
distanciaOrigem _ = []

pointSucc :: (Floating a) => ((a, a), (a, a)) -> (a, a)
pointSucc (y, z) = (c + (c-a), d + (d-b))
        where
            a = fst y
            b = snd y
            c = fst z
            d = snd z

takeLastTwo :: (Floating a) => [(a, a)] -> ((a, a), (a, a))
takeLastTwo [] = error "No takeLastTwo for empty lists!"
takeLastTwo (x:[]) = error "No takeLastTwo for one element lists!"
takeLastTwo xs = (l!!1, l!!0)
    where
        l = reverse xs

proximoPonto :: (Floating a) => [(a, a)] -> (a, a)
proximoPonto xs = pointSucc (takeLastTwo xs)

-- Trabalho 2
distancia :: (Ord a, Floating a) => [(a, a)] -> a
distancia [] = 0
distancia (x:[]) = 0
distancia (x:xs) = distance x (head xs) + distancia(xs)

fundePercursos :: (Floating a, Ord a) => [(a, a)] -> [(a, a)] -> (a, a) -> [(a, a)]
fundePercursos xs [] t = t:xs
fundePercursos [] ys t = t:ys
fundePercursos (x:xs) (y:ys) t 
    | (distance x t) > (distance y t) = t:(fundePercursos (x:xs) ys y)
    | otherwise = t:(fundePercursos xs (y:ys) x)


-- adicionaParagem :: (Floating a, Ord a) => [(a, a)] -> (a, a) -> [(a, a)]
-- adicionaParagem (x:[]) p = p:x
-- adicionaParagem (x:xs) p = 
--     let previousStops = adicionaParagem [a | a <- xs, (distance x t) > (distance y t)]
--         postStops = adicionaParagem [a | a <- xs, ]
--     in previousStops ++ [p] ++ postStops
-- adicionaParagem _ _ = error "No adicionaParagem for empty list or non tuple"