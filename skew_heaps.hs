{-# LANGUAGE DatatypeContexts #-}

data Ord a => Tree a = Null | Fork a (Tree a) (Tree a) deriving (Show, Eq)


isEmpty :: Ord a => Tree a -> Bool
isEmpty Null = True
isEmpty Fork {} = False

minElem :: Ord a => Tree a -> a
minElem (Fork x _ _) = x

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork _ a b) = merge a b

insert :: Ord a => a -> Tree a -> Tree a
insert x = merge (Fork x Null Null)

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null b = b
merge a Null = a
merge a b
    | minElem a < minElem b = join a b
    | otherwise = join b a

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork x a b) c = Fork x b (merge a c)


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

        ans2 = insert 9 
            $ insert 2 
            $ insert 1 
            $ insert 4 
            $ insert 3 
            $ insert 6 
            $ insert 5 
            $ insert 7 
            $ insert 8 Null
