import Data.Char

-- Função que recebe 3 inteiros e devolve a sua soma
addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

addThreePositive :: Int -> Int -> Int -> Int
addThreePositive a b c = if (a > 0) && (b > 0) && (c > 0) then a + b + c else 0

shorterThan :: Int -> Int -> Int -> Bool
shorterThan a b c = abs(a-b) < c

addDigit :: Int -> Int -> Int
addDigit a b = read (show a ++ show b)

biggerThan10 :: [a] -> Bool
biggerThan10 xs = length xs > 10

notemptyList :: [a] -> Bool
notemptyList xs = not (null xs)

initTail :: String -> String
initTail xs = init (tail xs)

secondElem :: [a] -> a
secondElem (x:s:xs) = s

initLast :: [a] -> a
initLast xs = secondElem $ reverse xs

enesim :: [a] -> Int -> a
enesim xs pos = xs !! pos 

revertabFirst :: [a] -> [a]
revertabFirst (x:xs) = x:(reverse xs)

sumFistFive :: (Num a) => [a] -> a
-- sumFistFive xs = sum (take 5 xs)
sumFistFive xs = sumXElements xs 5

sumXElements :: (Num a) => [a] -> Int -> a
sumXElements xs n = sum $ take n xs

cenas :: Eq a => [a] -> [a] -> Bool
cenas x y = (notemptyList x) && (notemptyList y) && (length x == length y) && ((last x) == (last y))


prefix :: String -> String -> Bool
prefix [] [] = True
prefix xs [] = False
prefix [] ys = True
prefix xs ys = xs == take (length xs) ys

sufix :: String -> String -> Bool
sufix [] [] = True
sufix xs [] = False
sufix [] ys = True
sufix xs ys = prefix (reverse xs) (reverse ys)

prefixOrSuffix :: String -> String -> Bool
prefixOrSuffix [] [] = True
prefixOrSuffix xs [] = False
prefixOrSuffix [] ys = True
prefixOrSuffix xs ys = (prefix xs ys) || (sufix xs ys)


getNext :: (Floating a, Enum a)  => a -> a -> a -> a
getNext a b n = a + ((b - a) / n)

particao :: (Floating a, Enum a)  => a -> a -> a -> [a]
particao a z n = [a, (getNext a z n)..z]

-- 9
-- [x^2 | x <- [1..100]]

pitagoricos :: (Num a, Eq a, Enum a, Ord a) => a -> [(a, a, a)]
pitagoricos n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], ((a^2) + (b^2)) == (c^2), a <= b]

perfeitos :: (Num a, Integral a) => a -> [a]
perfeitos n = [x | x <- [1..n], x == ((sum [y | y <- [1..x], (mod x y) == 0]) - x)]

-- 12
-- take 6 [x^2 | x <- [1, 2..]]

produtoEscalar :: (Floating a) => [a] -> [a] -> a 
produtoEscalar (x:[]) (y:[]) = x * y
produtoEscalar (x:xs) (y:ys) = (x * y) + produtoEscalar xs ys

reproduz xs = concat [(replicate x x) | x <- xs]


-- [(x,y)| x <- [1,2,3], y <- [4,5,6]]

-- == 

-- [(x,y)| x <- [1,2,3], y <- [b | b <- [4, 5, 6]]]

pares :: Int -> [(Int, Int)]
pares n = [(i, j) | i <- [1..n], j <- [1..n], i /= j]

fromTo :: Int -> Int -> String -> String
fromTo b e s = [s !! c | c <- [b..e]]

tail' xs = fromTo ((length xs) -1) ((length xs) - 1) xs
init' xs = fromTo 0 0 xs

-- matId :: Int -> [[Int]]
-- matId n = 

-------  ex_02_tipos  -------

-------  ex_03_funcoes  -------
firstParElement :: (a, a) -> a
firstParElement (a, b) = a

trocados :: (a, a) -> (a, a)
trocados (a, b) = (b, a) 

firstFromTriplet :: (a, a, a) -> a
firstFromTriplet (a, b, c) = a

changeTwoFromTriplet :: (a, a, a) -> (a, a, a)
changeTwoFromTriplet (a, b, c) = (b,a,c)

secondFromList :: [a] -> a
secondFromList (x:s:xs) = s

secondFromPairList :: [(a, a)] -> a
secondFromPairList ((a, b):xs) = b


somaVec :: (Double,Double) -> (Double, Double) -> (Double,Double)
somaVec (a, b) (c, d) = ((a + c), (b + d))

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

hd1 (x:_)= x

hd2 :: [Int] -> Int
hd2 (x:_)= x

hd3 :: [a] -> a
hd3 (x:_)= x

whichQuadrant :: (Floating a, Ord a) => (a, a) -> Int
whichQuadrant (x, y) 
    | (x > 0) && (y > 0) = 1
    | (x > 0) && (y < 0) = 4
    | (x < 0) && (y < 0) = 3
    | (x < 0) && (y > 0) = 2
    | otherwise = 0

ordinalPrefix :: Int -> [Char]
ordinalPrefix x
    | mod x 10 == 2 = t ++ "nd"
    | mod x 10 == 1 = t ++ "st"
    | otherwise = t ++ "th"
    where t = (show x)

leetSpeak :: [Char] -> [Char]
leetSpeak s = let 
    r 'a' = '4'
    r 'i' = '1'
    r 't' = '7'
    r 'o' = '0'
    r 's' = '5'
    r 'e' = '3'
    r x = toUpper x
    in  map r s

safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

halve :: [a] -> ([a],[a])
halve xs = 
    let l1 = quot (length xs) 2
        l2 = (length xs) - l1
    in ((take l1 xs), reverse (take l2 (reverse xs)))


-- 13 Value and type
vnt :: [Int] -> [Char] -> [(Int, Char)]
vnt a b = zip xs ys
        where xs = tail a
              ys = init b

-- recursion exercises --
sum' :: Num a  => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

-- replicate' :: Int a => a -> [a]
-- replicate' 0 = []
-- replicate' n = 1:(replicate' (n - 1) x)


maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs)
    | x > y = x
    | otherwise =  y
    where y = maximo xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a [x] = a == x
elem' a (x:xs) = a == x || elem' a xs

substitui :: Eq a => a -> a -> [a] -> [a]
substitui _ _ [] = []
substitui a b (x:xs)
    | x == a = b:l
    | otherwise = x:l
    where l = substitui a b xs

altera :: Ord a => [a] -> a -> a -> [a]
altera [] _ _ = []
altera (x:xs) b a 
    | x < b = a:(altera xs b a)
    | otherwise = x:(altera xs b a)

multiplos :: [Int] -> Int -> [Int]
multiplos [] _ = []
multiplos (x:xs) n
    | mod x n == 0 = x:l
    | otherwise = l
    where l = multiplos xs n

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = [] 
zip' _ [] = [] 
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

-- potencias :: Integer -> [Integer] -> [Floating]
-- potencias _ [] = []
-- potencias n (x:xs) = (n ** x):(potencias n xs)

-- posicoes [1,3,6,2,5,15,3,5,7,18] 3 -> [1,2,5,6,9]
posicoes :: [Int] -> Int -> [Int]
posicoes list n = f list 0 where 
    f [] _ = []
    f (x:xs) ix 
        | mod x n == 0 = ix : f xs (ix + 1)
        | otherwise = f xs (ix + 1)

-- frase 3 [(3,"As "),(1,"Sete ") (3,"armas "), (5,"Amor "), (3,"e os "), (1,"anos "), (3,"baroes ")] -> "As armas e os baroes "
frase :: Int -> [(Int,String)]-> String
frase _ [] = ""
frase n ((a,b):xs)
    | a == n = b ++ frase n xs
    | otherwise = frase n xs

trocaPares :: [Int] -> [Int]
trocaPares [] = []
trocaPares [x] = [x]
trocaPares (x:s:xs)
    | even s = s:x:(trocaPares xs)
    | otherwise = x:s:(trocaPares xs)


-- fusao [('b',8),('g',2),('m',6),('v',4)] [('a',3),('g',5),('m',2)] -> [('a',3),('b',8),('g',7),('m',8),('v',4)]
fusao :: (Ord a, Num b)=>[(a,b)] -> [(a,b)] -> [(a,b)]
fusao [] [] = []
fusao xs [] = xs
fusao [] xs = xs
fusao ((a,b):xs) ((c,d):ys) 
    | a == c = (a, (b+d)): fusao xs ys
    | a > c = (c, d): fusao ((a,b):xs) ys
    | otherwise = (a,b):fusao xs ((c,d):ys)

repBinaria :: Int -> String
repBinaria 0 = "0"
repBinaria 1 = "1"
repBinaria n = repBinaria (quot n 2) ++ show (mod n 2)

odioso :: Int -> Bool
odioso 0 = False
odioso 1 = True
odioso n
    | mod n 2 == 1 = not $ odioso next
    | otherwise = odioso next
    where next = quot n 2

-- nBaseX :: Int -> Int -> String
-- nBaseX 0 _ = '0'
-- nBaseX 1 _ = '1'
-- nBaseX n x = nBaseX (quot n x) ++ show (mod n x)


insert :: Ord a =>a-> [a] -> [a]
insert n [] = [n]
insert n [x]
    | x > n = n:x:[]
    | otherwise = x:n:[]

insert n (x:s:[])
    | n < x = n:x:s:[]
    | n < s = x:n:s:[]
    | otherwise = x:s:n:[]

insert n (x:s:xs)
    | (x < n) && (s > n) = (x:n:s:xs)
    | otherwise = x:(insert n (s:xs))


-- insertSort :: Ord a => a -> [a] -> [a]
-- insertSort n [] = [n]
-- insertSort n xs = 
--     let smallerSorted = insertSort n [a | a <- xs, a <= n]  
--         biggerSorted = insertSort n [a | a <- xs, a > n]  
--     in  smallerSorted ++[n]++ biggerSorted  

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:yx)
    | x < y = x:merge xs (y:yx)
    | otherwise = y:merge (x:xs) yx

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:xs) =
    let smaller = mergeSort [a | a <- xs, a <= x]
        bigger = mergeSort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

-- To be able toj create lists to test
randomList :: Int -> [Int]
randomList n = take n randomInfiniteList
randomInfiniteList :: [Int]
randomInfiniteList = iterate f 1234
    where
        f x = (1343 * x + 997) `mod` 1001

-------------- hight order functions --------------
