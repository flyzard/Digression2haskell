-- Sum elements
sum' :: Num a => [a] -> a
sum' = foldl (+) 0

replicate':: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x
    | n == 0 = []
    | otherwise = x : replicate' (n-1) x

maximo :: Ord a => [a] -> a
maximo [] = error "maximum of empty list"
maximo [x] = x
-- maximo (x:xs)= max x (maximo xs)
maximo (x:xs)
    | x > maximo xs = x
    | otherwise = maximo xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

substitui :: Eq a => a -> a -> [a] -> [a]
substitui _ _ [] = []
substitui a b (x:xs)
    | x == a = b : substitui a b xs
    | otherwise = x : substitui a b xs

altera :: Ord a => [a] -> a -> a -> [a]
altera [] _ _ = []
altera (x:xs) a b
    | x < b = a : l
    | otherwise = x : l
    where l = altera xs a b

multiplos :: [Int] -> Int -> [Int]
multiplos [] _ = []
multiplos (x:xs) a
    | mod x a == 0 = x : l
    | otherwise = l
    where l = multiplos xs a

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

potencias :: Integer -> [Integer] -> [Integer]
potencias _ [] = []
potencias a (x:xs) = (a ^ x) : potencias a xs

posicoes :: [Int] -> Int -> [Int]
posicoes xs a = posInc xs a 0 where
    posInc [] _ _ = []
    posInc (x:xs) a p
        | mod x a == 0 = p : posInc xs a (p + 1)
        | otherwise = posInc xs a (p + 1)

frase :: Int -> [(Int, String)] -> String
frase _ [] = ""
frase p ((a,b):xs)
    | p == a = b ++ frase p xs
    | otherwise = frase p xs

trocaPares :: [a] -> [a]
trocaPares [] = []
trocaPares [x] = [x]
trocaPares (x:s:xs) = s : x : trocaPares xs

fusao :: (Ord a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)]
fusao [] xs = xs
fusao xs [] = xs
fusao ax@((a, b):xs) ay@((c, d):ys)
    | c < a = (c, d) : fusao ax ys
    | otherwise = (a, b) : fusao xs ay

repBinaria :: Int -> String
repBinaria 0 = "0"
repBinaria 1 = "1"
repBinaria x
    | mod x 2 == 0 = f ++ "0"
    | otherwise = f ++ "1"
    where f = repBinaria $ div x 2

odioso :: Int -> Bool
odioso 0 = False
odioso 1 = True
odioso n
    | mod n 2 == 1 = not $ odioso next
    | otherwise = odioso next
    where next = quot n 2

nBaseX :: Int -> Int -> String
nBaseX 0 _ = ""
nBaseX 1 _ = "1"
nBaseX n x 
    | m >= 10 = f ++ [toEnum (m + 55) :: Char]
    | otherwise = f ++ show m
    where f = nBaseX (quot n x) x
          m = mod n x

insert :: Ord a =>  a -> [a] -> [a]
insert n [] = [n]
insert n all@(x:xs)
    | x > n = n : all
    | otherwise = x : insert n xs

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = let smaller = insertSort [y | y <- xs, y < x]
                        bigger = insertSort [y | y <- xs, y > x]
                    in smaller ++ [x] ++ bigger

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort = foldr insert []

randomList :: Int -> [Int]
randomList n = take n randomInfiniteList

randomInfiniteList :: [Int] 
randomInfiniteList = iterate f 1234
    where
        f x = (1343 * x + 997) `mod` 1001
