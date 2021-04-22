{-# LANGUAGE DatatypeContexts #-}

data Ord a => Tree a = Null | Fork a (Tree a) (Tree a) (Tree a) deriving (Show, Eq)


isEmpty :: Ord a => Tree a -> Bool
isEmpty Null = True
isEmpty Fork {} = False

minElem :: Ord a => Tree a -> a
minElem (Fork x _ _ _) = x

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork _ a b c) = merge a $ merge b c

insert :: Ord a => a -> Tree a -> Tree a
insert x = merge $ Fork x Null Null Null

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null b = b
merge a Null = a
merge a b
    | minElem a < minElem b = join a b
    | otherwise = join b a

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork x a b c) d = Fork x b c $ merge a d


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
