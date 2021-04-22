{-# LANGUAGE DatatypeContexts #-}

data Colour = Blue | Red deriving (Show, Eq)
data Ord a => Tree a = Null | Fork Colour a (Tree a) (Tree a) deriving (Show, Eq)


isEmpty :: Ord a => Tree a -> Bool 
isEmpty Null = True
isEmpty Fork {} = False

minElem :: Ord a => Tree a -> a
minElem (Fork _ x _ _) = x

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork _ _ a b) = merge a b

insert :: Ord a => a -> Tree a -> Tree a
insert x = merge (Fork Blue x Null Null)

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null b = b
merge a Null = a
merge a b 
    | minElem a < minElem b = join a b
    | otherwise = join b a

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork Blue x a b) c = Fork Red x (merge a c) b
join (Fork Red x a b) c = Fork Blue x a (merge b c)


main :: IO ()
main = do
    print ans1
    print ans2
    where
        ans1 = insert 2 
            $ insert 1 
            $ insert 4 
            $ insert 3 
            $ insert 6 
            $ insert 5 
            $ insert 8
            $ insert 7 
            $ insert 9 Null
        

        s0 = insert 9 
            $ insert 8 
            $ insert 7 Null
        s1 = insert 6 
            $ insert 5 Null
        s2 = merge s0 s1
        s3 = insert 4 
            $ insert 3 Null
        s4 = merge s2 s3
        s5 = insert 2 
            $ insert 1 Null
        ans2 = merge s4 s5
