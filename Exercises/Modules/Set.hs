module Modules.Set
(
    Set,
    empty,
    null
    -- singleton,
    -- member,
    -- insert
    -- fromList,
    -- filter,
    -- remove,
    -- union,
    -- intersection,
    -- difference,
    -- size,
    -- partition
) where

data Set a =
  Nil |
  Cons a (Set a) deriving Eq

empty :: Set a
empty = Nil

null' :: (Eq a) => Set a -> Bool
null' a = a == Nil

singleton :: [a] -> Set a
singleton [] = Nil
singleton (x:s:xs)
    | 

-- member :: a -> Set -> Bool
-- member _ [] = False
-- member a (x:xs)
--     | a == x = True
--     | otherwise = member a xs


-- insert :: a -> Set -> Set
-- insert a [] = [a]
-- insert a (x:xs)
--     | x > a = a : x : xs
--     | otherwise = x : insert a xs
-- fromList,
-- filter,
-- remove,
-- union,
-- intersection,
-- difference,
-- size,
-- partition






