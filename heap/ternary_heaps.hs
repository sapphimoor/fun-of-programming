{-# LANGUAGE DatatypeContexts #-}

data (Ord a, Show a) => Tree a = Null | Fork a (Tree a) (Tree a) (Tree a) deriving (Show, Eq)


isEmpty :: (Ord a, Show a) => Tree a -> Bool
isEmpty Null = True
isEmpty Fork {} = False

minElem :: (Ord a, Show a) => Tree a -> a
minElem (Fork x _ _ _) = x

deleteMin :: (Ord a, Show a) => Tree a -> Tree a
deleteMin (Fork _ a b c) = merge a $ merge b c

insert :: (Ord a, Show a) => a -> Tree a -> Tree a
insert x = merge $ Fork x Null Null Null

merge :: (Ord a, Show a) => Tree a -> Tree a -> Tree a
merge Null b = b
merge a Null = a
merge a b
    | minElem a < minElem b = join a b
    | otherwise = join b a

join :: (Ord a, Show a) => Tree a -> Tree a -> Tree a
join (Fork x a b c) d = Fork x b c $ merge a d


inserts :: (Ord a, Show a) => Tree a -> [a] -> Tree a
inserts = foldl (flip insert)


printTree :: (Ord a, Show a) => Tree a -> IO()
printTree = subPrintTree 1 [] 

subPrintTree :: (Ord a, Show a) => Int -> [Int] -> Tree a -> IO ()
subPrintTree _ _ Null = putStrLn "[]"
subPrintTree n nlist (Fork x a b c) = do
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
    putStr "+- "
    subPrintTree (n+1) nlist1 b

    makeEdge 1 n nlist1 (putStr "|  ") (putStr  "   ")
    putStrLn ""
    makeEdge 1 (n-1) nlist1 (putStr "|  ") (putStr  "   ")
    putStr "`- "
    subPrintTree (n+1) nlist c

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
    printTree ans
    where
        ans = insert 13
            $ insert 12
            $ insert 11
            $ insert 10
            $ insert 9
            $ insert 8
            $ insert 7
            $ insert 6
            $ insert 5
            $ insert 4
            $ insert 3
            $ insert 2
            $ insert 1 Null
