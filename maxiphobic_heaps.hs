{-# LANGUAGE DatatypeContexts #-}

data Ord a => Tree a = Null | Fork Int a (Tree a) (Tree a) deriving (Show, Eq)


isEmpty :: Ord a => Tree a -> Bool 
isEmpty Null = True
isEmpty Fork {} = False

minElem :: Ord a => Tree a -> a
minElem (Fork _ x _ _) = x

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork _ _ a b) = merge a b

insert :: Ord a => a -> Tree a -> Tree a
insert x = merge (Fork 1 x Null Null)

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null b = b
merge a Null = a
merge a b
    | minElem a < minElem b = join a b
    | otherwise = join b a

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork n x a b) c = Fork (n + size c) x aa (merge bb cc)
    where (aa, bb, cc) = orderBySize a b c

orderBySize :: Ord a => Tree a -> Tree a -> Tree a -> (Tree a, Tree a, Tree a)
orderBySize a b c
    | size a == biggest = (a, b, c)
    | size b == biggest = (b, a, c)
    | size c == biggest = (c, a, b)
    where biggest = size a `max` size b `max` size c

size :: Ord a => Tree a -> Int
size Null = 0
size (Fork n _ _ _) = n


main :: IO ()
main = print ans
    where
        ans = insert 4 
            $ insert 5 
            $ insert 3 
            $ insert 6 
            $ insert 2 
            $ insert 7 
            $ insert 1 Null
            