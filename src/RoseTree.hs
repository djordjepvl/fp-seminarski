module RoseTree where

data Rose a = Node a [Rose a]

size :: Rose a -> Int
size (Node _ children) = 1 + sum (map size children)

height :: Rose a -> Int
height (Node _ []) = 0
height (Node _ children) = 1 + maximum (map height children)

leaves :: Rose a -> [a]
leaves (Node x []) = [x]
leaves (Node _ children) = concat (map leaves children)

elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node x _ ) = [x]
elemsOnDepth n (Node _ children) = concat (map (elemsOnDepth (n-1)) children)

instance Functor Rose where
    fmap f (Node x children) = Node (f x) $ map (fmap f) children

foldRose :: (a -> b -> b) -> b -> Rose a -> b
foldRose f curr (Node x children) = f x (foldr (\p q -> foldRose f q p) curr children)