-- (*2) multiplies a given number by 2
-- (>0) Checks if a given number is greater than 0
-- (1/) divids 1 by a given number
-- (/2) divides a given number by 2
-- (+1) adds 1 to a given number
-- (++"/n") adds a new line to a given string

add1 (x, y)= x + y
add2 x y = x + y
add3 x = (x+)

successor:: Int -> Int
successor = add3 1

-- successor:: Int -> Int
-- successor = add2 1

-- successor:: Int -> Int
-- successor x = add1 (1, x)

zipWith' :: (a->b->c)-> [a] -> [b] -> [c]
zipWith' _ [] _ = []  
zipWith' _ _ [] = [] 
-- zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f xs ys = [f x y | x <- xs, y <- ys]

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

dropWhile' :: (a -> Bool)-> [a] -> [a]
dropWhile' f (x:xs)
    | f x = takeWhile' f xs
    | otherwise = x : takeWhile' f xs

dropUntil' :: (a -> Bool)-> [a] -> [a]
dropUntil' f (x:xs)
    | f x = x : xs
    | otherwise = dropUntil' f xs

total :: (Int -> Int)-> Int -> Int
total f 0 = f 0
-- total f x = f x + total f (x-1)
-- total f x = sum [f i | i <- [0..x]]
total f x = sum $ map f [0..x]

aplica :: [a -> a] -> [a] -> [a]
aplica [] xs = xs
aplica _ [] = []
-- aplica (f:fs) xs = aplica fs $ map f xs
-- aplica fs xs = foldr (\f acc -> map f acc) xs fs
aplica fs xs = foldl (\acc f-> map f acc) xs fs

-- \x -> x + 1 :: NUm a => a -> a
-- (\x -> x + 1)6 :: Num a => a
-- \x -> x > 0 :: (Ord a, Num a) => a -> Bool
-- \x y -> x + y :: Num a => a -> a -> a
-- (\x y -> x + y) 7 :: Num a => a -> a
-- (\x y -> x + y) 7 3 :: Num a => a
-- \x -> (\y -> x + y) :: Num a => a -> a -> a
-- (\f x -> f (f x))(\y -> y + 1) :: Num a => a -> a
-- \f x -> f (f x) :: (t -> t) -> t -> t

mult = \x y z -> x * y * z

isNonBlank :: Char -> Bool
isNonBlank = \ x -> not $ elem x [' ','\t','\n']


curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b


-- foldr (\y z -> y * 3 + z) 0 [1..4] :: (Num a, Enum a) => a
-- foldr (\x y -> if x > 0 then x + y else y) 0 [4,-3,2,-1] :: (Ord a, Num a) => a
-- foldr (\x y -> x ^ 2 + y) 0 [2..5] :: (Num a, Enum a) => a
-- foldr (*)1 [-3..(-1)] :: (Num a, Enum a) => a
-- foldr (\x s -> if x == 'z' then x:s else s) [] "Oz alunoz dze PzPz" :: [Char]

sum' :: Num a => [a] -> a
sum' = foldl1 (+)

length' :: [a] -> Int
length' = foldr (\_ acc -> acc + 1) 0

map' :: (a -> a) -> [a] -> [a]
map' f = foldr (\x acc -> f x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x:acc else acc) []

binary2decimal :: (Num a) => [a] -> a -- foldl ((+).(2*)) 0
binary2decimal = foldl (\acc x -> acc * 2 + x) 0

indexOf :: [Int] -> Int -> Int
-- indexOf xs n = length xs - foldl (\acc x -> if x == n || acc > 0 then acc + 1 else acc) 0 xs
indexOf xs n = length $ takeWhile (/=n) xs

poly :: Int -> [Int] -> Int
poly x = sum . zipWith (\a b -> b * (x ^ a)) [0..] . reverse

selectApply :: (a -> b) -> (a -> Bool) -> [a] -> [b]
selectApply f b = map f . filter b

histograma :: Eq a => [a] -> [(a, Int)]
histograma [] = []
histograma all@(x:xs) = (x, l) : histograma fs
    where fs = filter (/=x) xs
          l = length all - length fs

-- countOnList :: Eq a => a -> [a] -> Int
-- countOnList n [] = 0
-- countOnList n (x:xs)
--     | x == n = 1 + f
--     | otherwise = f
--     where f = countOnList n xs

-- removeAllFromList :: Eq a => a -> [a] -> [a]
-- removeAllFromList _ [] = []
-- removeAllFromList n (x:xs)
--     | x /= n = x : f
--     | otherwise = f
--     where f = removeAllFromList n xs

gz :: [[Int]] -> [[Bool]]
gz = map (map (> 0))

-- iter :: (a -> b) -> Num -> (a -> b)
-- iter f 0 = 0
-- iter f n = iter f (n-1)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = concat . map box
    where
    box x = [x | p x]


sumlen :: [Int] -> (Int, Int)
sumlen xs = (foldr (+) 0 xs, length xs)
