import Data.Maybe
import Data.Char


-------------------------------------------------
-- DEFINE List

data List a = Nil | Cons a (List a) deriving Show


makeList :: [a] -> List a
makeList = foldr Cons Nil


wrap :: a -> List a
wrap x = Cons x Nil

nil :: List a -> Bool
nil Nil         = True
nil (Cons _ _)  = False

foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil            = f Nothing
foldL' f (Cons x xs)    = f $ Just (x, foldL' f xs)

foldL :: (a -> b -> b) -> b -> List a -> b
foldL f e Nil           = e
foldL f e (Cons x xs)   = f x $ foldL f e xs

{-
    h = foldL f e
<=>
    h xs = case xs of
        Nil         -> e
        Cons y ys   -> f y (h ys)
-}

isort :: Ord a => List a -> List a
isort = foldL insert Nil
    where
        insert :: Ord a => a -> List a -> List a
        insert y Nil    = wrap y
        insert y (Cons x xs)
            | y < x     = Cons y $ Cons x xs
            | otherwise = Cons x $ insert y xs


unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f u = case f u of
    Nothing     -> Nil
    Just (x, v) -> Cons x $ unfoldL' f v

unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f g b =
    if p b
        then Nil
        else Cons (f b) $ unfoldL p f g $ g b


minimumL :: Ord a => List a -> a
minimumL Nil            = error " **minimumL':  Unexpected \"Nothing\" is passed as argument."
minimumL (Cons x xs)    = foldL min x xs

deleteL :: Eq a => a -> List a -> List a
deleteL y Nil = Nil
deleteL y (Cons x xs)
    | y == x    = xs
    | otherwise = Cons x $ deleteL y xs

delmin :: Ord a => List a -> Maybe (a, List a)
delmin Nil  = Nothing
delmin xs   = Just (y, deleteL y xs)
    where
        y = minimumL xs

ssort :: Ord a => List a -> List a
ssort = unfoldL' delmin


bubble :: Ord a => List a -> Maybe (a, List a)
bubble = foldL step Nothing
    where
        step x Nothing  = Just (x, Nil)
        step x (Just (y, ys))
            | x < y     = Just (x, Cons y ys)
            | otherwise = Just (y, Cons x ys)

bsort :: Ord a => List a -> List a
bsort = unfoldL' bubble


fact = foldL (*) 1 . unfoldL (==0) id pred

{-
id :: a -> a
id x = x

pred :: Enum a => a -> a
pred = subtract 1
-}

hyloL' f e p g h = foldL f e . unfoldL p g h

hyloL f e p g h b = if p b then e else f (g b) $ hyloL f e p g h $ h b

fact' :: Integer -> Integer
fact' = hyloL (*) 1 (==0) id pred

{-
fact' n = hyloL (*) 1 (==0) id pred n
        = if n==0 then 1 else (*) (id n) (hyloL (*) 1 (==0) id pred $ pred n)
        = if n==0 then 1 else n * fact' $ n-1
-}


-------------------------------------------------
-- ex3.1

{-
Assumptions are as follow:
    foldL f e Nil           = e
    foldL f e (Cons x xs)   = f x $ foldL f e xs
    (h (f a b) = f' a (h b)) && (h e = e')
Consider the joined function:
    h . foldL f e
First, "foldL" is evaluated as h is strict function.
This function has 2 cases as follow:
    1. foldL f e Nil           = e
    2. foldL f e (Cons x xs)   = f x $ foldL f e xs
If the former is held, then h is passed "e" and subsequently executes the formula:
    h e                      (== e')
and otherwise,  h is passed "f x (foldL f e xs)" and subsequently executes the formula:
    h (f x (foldL f e xs))   (== f' x (h . (foldL f e xs)))
Now, we can obtain the next fomula:
    foldL f' e' xs  = case xs of
        Nil         -> e'
        Cons y ys   -> f' y $ foldL f' e' ys
Thus the proof is shown from the above.
    h . foldL f e = foldL f' e'
-}


-------------------------------------------------
-- ex3.2

mapL :: (a -> b) -> List a -> List b
mapL f Nil          = Nil
mapL f (Cons x xs)  = Cons (f x) $ mapL f xs

appendL :: List a -> List a -> List a
appendL Nil ys          = ys
appendL (Cons x xs) ys  = Cons x $ appendL xs ys

concatL :: List (List a) -> List a
concatL = foldL appendL Nil


test_mapL       = mapL (^2) $ makeList [1,2,3]
test_appendL    = appendL (makeList [1,2,3]) $ makeList [4,5,6]
test_concatL    = concatL $ makeList [makeList [1,2,3], makeList [4,5,6], makeList [7,8,9]]

-------------------------------------------------
-- ex3.3

{-
Asumptions are as follow:
    foldL f e Nil           = e
    foldL f e (Cons x xs)   = f x $ foldL f e xs
    mapL f Nil          = Nil
    mapL f (Cons x xs)  = Cons (f x) $ mapL f xs
Consider the joined function:
    foldL f e . map g
First, "map g" is evaluated and make the list with input (Cons x1 (Cons x2 (... Cons xn Nil))) such that:
    Cons (g x1) (Cons (g x2) (... Cons (g xn) Nil))
thus "foldL f e" is passed this list and then execute the formula such that:
    f (g x1) (f (g x2) (... f (g xn) e))
and this formula is equivalent with:
    (f.g) x1 ((f.g) x2 (... (f.g) xn e))
Therefore the proof is shown from the above.
    foldL f e . map g = foldL (f.g) e

<c.f.>
(.) :: (b -> c) -> (a -> b) -> a -> c
f   :: a -> b -> c
g   :: a -> b

(f.g) a c   = (((.) f g) a) c
            = (f (g a)) c
-}


-------------------------------------------------
-- ex3.4

insert1 :: Ord a => a -> List a -> (List a, List a)
insert1 y = foldL f (Nil, wrap y)
    where
        f x (xs, insertedXs)
            | y < x     = (Cons x xs, Cons y (Cons x xs))
            | otherwise = (Cons x xs, Cons x insertedXs)


isort1 :: Ord a => List a -> List a
isort1 = foldL ((snd .) . insert1) Nil

test_isort1 = isort1 $ makeList [6,9,3,1,8,4,2,7,5]


-------------------------------------------------
-- ex3.5

paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL f e Nil           = e
paraL f e (Cons x xs)   = f x (xs, paraL f e xs)

insert2 :: Ord a => a -> List a -> List a
insert2 y = paraL f $ wrap y
    where
        f x (xs, insertedXs)
            | y < x     = Cons y $ Cons x xs
            | otherwise = Cons x insertedXs


isort2 :: Ord a => List a -> List a
isort2 = foldL insert2 Nil

test_isort2 = isort2 $ makeList [6,9,3,1,8,4,2,7,5]


-------------------------------------------------
-- ex3.6

myUnfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
myUnfoldL p f g = unfoldL' $ \b -> if p b then Nothing else Just (f b, g b)

myUnfoldL' :: (b -> Maybe (a, b)) -> b -> List a
myUnfoldL' f = unfoldL (isNothing . f) g h
    where
        g u = case f u of
            Just (x, _) -> x
            Nothing     -> error " **myUnfoldL':  Unexpected \"Nothing\" is appeared after \"g u\"."
        h u = case f u of
            Just (_, v) -> v
            Nothing     -> error " **myUnfoldL':  Unexpected \"Nothing\" is appeared after \"h u\"."


-------------------------------------------------
-- ex3.7

{-
Asumptions are as follow:
    unfoldL p f g b = if p b then Nil else Cons (f b) $ unfoldL p f g $ g b
    (p . h = p') && (f . h = f') && (g . h = h . g')
Consider the joined function:
    unfoldL p f g . h
When we pass the argument b to this function, it becomes:
    (unfoldL p f g . h) b = unfoldL p f g $ h b
                          = if p (h b) then Nil else Cons (f $ h b) (unfoldL p f g $ g $ h b)
                          = if (p . h) b then Nil else Cons ((f . h) b) (unfoldL p f g $ (g . h) b)
                          = if p' b then Nil else Cons (f' b) (unfoldL p f g $ (h . g') b)
                          = if p' b then Nil else Cons (f' b) (unfoldL p f g $ h $ g' b)
                          = if p' b then Nil else Cons (f' b) $ (unfoldL p f g . h) $ g' b
Now, we can obtain the next formula:
(unfold p' f' g') b = if p' b then Nil else Cons (f' b) $ (unfoldL p' f' g') $ g' b
Therefore the proof is shown from the above.
    unfoldL p f g . h = unfoldL p' f' g'
-}


-------------------------------------------------
-- ex3.8

myFoldL' :: (Maybe (a, b) -> b) -> List a -> b
myFoldL' f = foldL (\x xs -> f $ Just (x, xs)) $ f Nothing
myFoldL :: (a -> b -> b) -> b -> List a -> b
myFoldL f e = foldL' f'
    where
        f' Nothing          = e
        f' (Just (x, xs))   = f x xs

{-
foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil            = f Nothing 
foldL' f (Cons x xs)    = f $ Just (x, foldL' f xs)

foldL :: (a -> b -> b) -> b -> List a -> b
foldL f e Nil           = e
foldL f e (Cons x xs)   = f x $ foldL f e xs
-}

-------------------------------------------------
-- ex3.9

foldLargs :: (a -> b -> b) -> b -> (Maybe (a, b) -> b)
foldLargs f e = f'
    where
        f' Nothing          = e
        f' (Just (x, xs))   = f x xs

unfoldLargs :: (b -> Bool) -> (b -> a) -> (b -> b) -> (b -> Maybe (a, b))
unfoldLargs p f g b = if p b then Nothing else Just (f b, g b)


-------------------------------------------------
-- ex3.10

myDeleteL :: Eq a => a -> List a -> List a
myDeleteL y = paraL (\x (xs, deletedXs) -> if y == x then xs else Cons x deletedXs) Nil


delmin' :: Ord a => List a -> Maybe (a, List a)
delmin' Nil = Nothing
delmin' xs  = Just (y, myDeleteL y xs)
    where
        y = minimumL xs
ssort2 :: List Integer -> List Integer
ssort2 = unfoldL' delmin'

test_ssort2 = ssort2 $ makeList [6,9,3,1,8,4,2,7,5]

{-
paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL f e Nil           = e
paraL f e (Cons x xs)   = f x (xs, paraL f e xs)
-}

-------------------------------------------------
-- ex3.11

myDelmin :: Ord a => List a -> Maybe (a, List a)
myDelmin Nil    = Nothing
myDelmin xs     = paraL f Nothing xs
    where
        z = minimumL xs
        f x (zs, Nothing)       = if z == x then Just (z, zs) else Nothing
        f x (zs, Just (y, ys))  = Just (y, Cons x ys)

ssort3 :: Ord a => List a -> List a
ssort3 = unfoldL' myDelmin
test_ssort3 = ssort3 $ makeList [6,9,3,1,8,4,2,7,5]


-------------------------------------------------
-- ex3.12

myBubble :: Ord a => List a -> Maybe (a, List a)
myBubble Nil    = Nothing
myBubble xs     = Just (z, zs)
    where
        step x Nil = wrap x
        step x (Cons y ys)
            | x < y     = Cons x $ Cons y ys
            | otherwise = Cons y $ Cons x ys
        Cons z zs = foldL step Nil xs


bsort1 :: Ord a => List a -> List a
bsort1 = unfoldL' myBubble

test_bsort1 = bsort1 $ makeList [6,9,3,1,8,4,2,7,5]


-------------------------------------------------
-- ex3.13

myInsert :: Ord a => a -> List a -> List a
myInsert x xs   = unfoldL' ins (xs, Just x)
    where
        ins ::Ord a => (List a, Maybe a) -> Maybe (a, (List a, Maybe a))
        ins (Nil, Nothing)          = Nothing
        ins (Nil, Just z)           = Just (z, (Nil, Nothing))
        ins (Cons y ys, Nothing)    = Just (y, (ys, Nothing))
        ins (Cons y ys, Just z)     = if y < z then Just (y, (ys, Just z)) else Just (z, (Cons y ys, Nothing))


myIsort :: Ord a => List a -> List a
myIsort = foldL myInsert Nil

test_myIsort = myIsort $ makeList [6,9,3,1,8,4,2,7,5]


-------------------------------------------------
-- ex3.14

apoL' :: (b -> Maybe (a, Either b (List a))) -> b -> List a
apoL' f u = case f u of
    Nothing     -> Nil
    Just (x, Left v)    -> Cons x $ apoL' f v
    Just (x, Right xs)  -> Cons x xs

myInsert' :: Ord a => a -> List a -> List a
myInsert' x xs = apoL' ins (xs, Just x)
    where
        ins (Nil, Nothing)        = Nothing
        ins (Nil, Just z)         = Just (z, Right Nil)
        ins (Cons y ys, Nothing)  = Just (y, Right ys)
        ins (Cons y ys, Just z)   = if y < z then Just (y, Left (ys, Just z)) else Just (z, Right (Cons y ys))


myIsort' :: Ord a => List a -> List a
myIsort' = foldL myInsert' Nil

test_myIsort' = myIsort' $ makeList [6,9,3,1,8,4,2,7,5]


-------------------------------------------------
-- ex3.15

reverseL :: List a -> List a
reverseL = foldL (flip appendL . wrap) Nil

string2decimal :: List Char -> Int
string2decimal = foldL (\n m -> 10 * m + digitToInt n) 0 . reverseL

decimal2binary :: Int -> List Bool
decimal2binary = reverseL . unfoldL (==0) ((==1) . flip mod 2) (`div` 2)

string2binary :: List Char -> List Bool 
string2binary = decimal2binary . string2decimal


test_string2binary = string2binary $ makeList "50"
