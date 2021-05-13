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


inserts :: (Ord a, Show a) => Tree a -> [a] -> Tree a
inserts = foldl (flip insert)


printTree :: (Ord a, Show a) => Tree a -> IO()
printTree = subPrintTree 1 []

subPrintTree :: (Ord a, Show a) => Int -> [Int] -> Tree a -> IO ()
subPrintTree _ _ Null = putStrLn "[]"
subPrintTree n nlist (Fork col x a b) = do
    print x
    let nlist1 = nlist ++ [n]

    makeEdge 1 n nlist1 (putStr "|  ") (putStr  "   ")
    putStrLn ""
    makeEdge 1 (n-1) nlist1 (putStr "|  ") (putStr  "   ")
    putStr "+- "
    subPrintTree (n+1) nlist1 a

    makeEdge 1 n nlist1 (putStr "|  ") (putStr  "   ")
    putStrLn ""
    makeEdge 1 (n-1) nlist1 (putStr "|  ") (putStr  "   ")
    putStr "`- "
    subPrintTree (n+1) nlist b

makeEdge :: Monad m => Int -> Int -> [Int] -> m a -> m a -> m ()
makeEdge i max [] act1 act2
    | i <= max  = do
        act2
        makeEdge (i+1) max [] act1 act2
    | otherwise = return ()
makeEdge i max list@(h:t) act1 act2
    | i > max   = return ()
    | i == h    = do
        act1
        makeEdge (i+1) max t act1 act2
    | otherwise = do
        act2
        makeEdge (i+1) max list act1 act2



main :: IO ()
main = do
    printTree ans1
    printTree ans2
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
