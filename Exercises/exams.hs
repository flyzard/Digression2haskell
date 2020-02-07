import Data.Char  
import Control.Monad 

maximo:: (Int -> Int) -> (Int, Int)-> Int
maximo f (x, y) = m $ map' f [x..y]
    where 
        m [x] = x
        m (x:xs)
            | x > m xs = x
            | otherwise = m xs

map' :: (a->b) -> [a] -> [b]
map' f = foldl (\acc x -> f x : acc) []

vc :: Int -> Bool
vc i = let rnl = toDigits i
           s = foldl (\acc (x,y) -> x * y + acc) 0 $ zip (tail rnl) [2..9]
           r = mod s 11
       in length rnl == 9 && (r == 0 || head rnl == 11 - r)
       where toDigits 0 = []
             toDigits x = let (d, m) = x `divMod` 10
                          in m : toDigits d


data Cor = Branco | Azul | Vermelho | Verde deriving (Show, Read)
data Matriosca = M Cor (Maybe Matriosca) deriving Show

-- paraLista :: Maybe Matriosca -> [Cor]
-- paraLista Nothing = []
-- paraLista (Just (M c n)) = c : paraLista n

paraLista :: Matriosca -> [Cor]
paraLista (M c n) = c : rest n
        where rest :: Maybe Matriosca -> [Cor]
              rest Nothing = []
              rest (Just (M c n)) = c : rest n

-- Nao percebo o enunciado disto "gerada pelo par Azul == Vermelho" ???
instance Eq Cor where
    Azul == Azul = True
    Vermelho == Vermelho = True
    Branco == Branco = True
    Verde == Verde = True
    _ == _ = False

numCor :: Cor -> Maybe Matriosca -> Int
numCor c Nothing = 0
numCor cor (Just (M c n))
    | cor == c = 1 + f
    | otherwise = f
        where f = numCor cor n

numMatrioscas :: Matriosca -> Int
numMatrioscas (M c Nothing) = 1
numMatrioscas (M c (Just m)) = 1 + numMatrioscas m

paraMatriosca :: [Cor] -> Matriosca
paraMatriosca [x] = M x Nothing
paraMatriosca (x:xs) = M x (Just $ paraMatriosca xs)

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

getColors :: [Cor] -> IO ()
getColors c = do 
                line <- getLine
                if line `elem` ["Azul", "Branco", "Verde", "Vermelho"]
                    then getColors $ (read line :: Cor) : c
                else 
                    print $ show $ paraMatriosca $ reverse c

main = getColors []
-- ??? How to demonstrate equality




-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
multiplosEntre :: Int -> Int -> Int -> Int
multiplosEntre a b c
    | mod a c == 0 = div (b - a) c
    | otherwise = multiplosEntre a (b - 1) c

intercalar :: [a] -> [[a]] -> [a]
intercalar _ [] = []
intercalar xs (y:ss) = y ++ xs ++ intercalar xs ss

transposta :: [[a]] -> [[a]]
transposta [] = []
transposta ([]:xss) = transposta xss
transposta xss = [h | (h:_) <- xss] : transposta [t | (_:t) <- xss]

data Html = Div [Html] | Texto String | Negrito String deriving (Show, Eq)

profundidade :: Html -> Int
profundidade (Texto _) = 1
profundidade (Negrito _) = 1
profundidade (Div []) = 1
profundidade (Div a) = 1 + maximum (map profundidade a)

realcar :: String -> Html -> Html
realcar s html