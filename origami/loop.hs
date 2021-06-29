import Data.Maybe

-------------------------------------------------
-- DEFINE Nat

data Nat = Zero | Succ Nat deriving (Show, Eq)

makeNat :: Integer -> Nat
makeNat n = if n==0 then Zero else Succ $ makeNat $ n - 1


foldN :: a -> (a -> a) -> Nat -> a
foldN z s Zero      = z
foldN z s (Succ n)  = s $ foldN z s n

iter :: Nat -> (a -> a) -> (a -> a)
iter n f x = foldN x f n


unfoldN' :: (a -> Maybe a) -> a -> Nat
unfoldN' f x = case f x of
    Nothing -> Zero
    Just y  -> Succ $ unfoldN' f y

unfoldN :: (a -> Bool) -> (a -> a) -> a -> Nat
unfoldN p f x = if p x then Zero else Succ $ unfoldN p f $ f x

{-
unfoldN computes the minimum n such that
    p $ iter n f x
is satisfied.
-}


untilN :: (a -> Bool) -> (a -> a) -> a -> a
untilN p f x = foldN x f $ unfoldN p f x

{-
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f $ f x
-}


-------------------------------------------------
-- ex3.16

foldN' :: (Maybe a -> a) -> Nat -> a
foldN' f Zero       = f Nothing
foldN' f (Succ n)   = f $ Just $ foldN' f n


myFoldN :: a -> (a -> a) -> Nat -> a
myFoldN z s = foldN' f
    where
        f Nothing   = z
        f (Just n)  = s n

myFoldN' :: (Maybe a -> a) -> Nat -> a
myFoldN' f = foldN (f Nothing) $ \n -> f $ Just n


-------------------------------------------------
-- ex3.17

{-
    h = foldN z s
<=>
    h n = case n of
        Zero    -> z
        Succ m  -> s (h m)
-}

{-
    h . foldN z s = foldN z' s'
<=
    h . s = s' . h && h Zero = z'    
-}


-------------------------------------------------
-- ex3.18

addN :: Nat -> Nat -> Nat
addN n = foldN n Succ

mulN :: Nat -> Nat -> Nat
mulN n = foldN Zero $ addN n

powN :: Nat -> Nat -> Nat
powN n = foldN (Succ Zero) $ mulN n


test_addN1 = addN (makeNat 3) $ makeNat 2
test_addN2 = addN (makeNat 3) $ makeNat 0
test_addN3 = addN (makeNat 0) $ makeNat 2
test_mulN1 = mulN (makeNat 3) $ makeNat 2
test_mulN2 = mulN (makeNat 3) $ makeNat 0
test_mulN3 = mulN (makeNat 0) $ makeNat 2
test_powN1 = powN (makeNat 3) $ makeNat 2
test_powN2 = powN (makeNat 3) $ makeNat 0
test_powN3 = powN (makeNat 0) $ makeNat 2


-------------------------------------------------
-- ex3.19

predN :: Nat -> Maybe Nat
predN Zero      = Nothing
predN (Succ n)  = Just n


predN' :: Nat -> Maybe Nat
predN' n = foldN (Just n) s $ Succ Zero
    where
        s Nothing           = Nothing
        s (Just Zero)       = Nothing
        s (Just (Succ m))   = Just m

predNN :: Nat -> Nat -> Maybe Nat
predNN n m = foldN (Just m) s n
    where
        s Nothing           = Nothing
        s (Just Zero)       = Nothing
        s (Just (Succ m))   = Just m
        

test_predN'1 = predN' $ makeNat 5
test_predN'2 = predN' $ makeNat 0
test_predNN1 = predNN (makeNat 5) $ makeNat 2
test_predNN2 = predNN (makeNat 5) $ makeNat 7

-------------------------------------------------
-- ex3.20

subN :: Nat -> Nat -> Maybe Nat
subN n m = predNN m n

eqN :: Nat -> Nat -> Bool
eqN = ((==Just Zero) .) . subN

lessN :: Nat -> Nat -> Bool
lessN = (isNothing .) . subN


test_subN1 = subN (makeNat 5) $ makeNat 2
test_subN2 = subN (makeNat 5) $ makeNat 7
test_eqN1 = eqN (makeNat 5) $ makeNat 2
test_eqN2 = eqN (makeNat 5) $ makeNat 5
test_eqN3 = eqN (makeNat 5) $ makeNat 7
test_lessN1 = lessN (makeNat 5) $ makeNat 2
test_lessN2 = lessN (makeNat 5) $ makeNat 5
test_lessN3 = lessN (makeNat 5) $ makeNat 7

-------------------------------------------------
-- ex3.21

myUnfoldN' :: (a -> Maybe a) -> a -> Nat
myUnfoldN' f = unfoldN (isNothing . f) g
    where
        g x = case f x of
            Just y  -> y
            Nothing -> error " **myUnfoldN':  Unexpected \"Nothing\" is appeared after \"g x\"."

myUnfoldN :: (a -> Bool) -> (a -> a) -> a -> Nat
myUnfoldN p f = unfoldN' $ \x -> if p x then Nothing else Just $ f x

{-
unfoldN' :: (a -> Maybe a) -> a -> Nat
unfoldN' f x = case f x of
    Nothing -> Zero
    Just y  -> Succ (unfoldN' f y)

unfoldN :: (a -> Bool) -> (a -> a) -> a -> Nat
unfoldN p f x = if p x then Zero else Succ $ unfoldN p f $ f x
-}


-------------------------------------------------
-- ex3.22

{-
    h = unfoldN p f
<=>
    h x = if p x then Zero else Succ $ h $ f x 
-}

{-
    unfoldN p f . h = unfoldN p' f'
<=
    p . h = p' && f . h = h . f'
-}


-------------------------------------------------
-- ex3.23

divN :: Nat -> Nat -> Nat
divN _ Zero = error " **divN:  Cannot divine by \"Zero\"."
divN n m    = unfoldN (`lessN` m) f n
    where
        f x = case subN x m of
            Nothing -> error " **divN:  Unexpected \"Nothing\" is appeared after \"f x\"."
            Just y  -> y 


test_divN1 = divN (makeNat 5) $ makeNat 3
test_divN2 = divN (makeNat 6) $ makeNat 3
test_divN3 = divN (makeNat 7) $ makeNat 3
test_divN4 = divN (makeNat 7) $ makeNat 0


-------------------------------------------------
-- ex3.24

isOne :: Nat -> Bool 
isOne = (==) $ Succ Zero

logN :: Nat -> Nat
logN = unfoldN isOne ( `divN` (Succ $ Succ Zero))

logBaseN :: Nat -> Nat -> Nat
logBaseN (Succ Zero)    = error " **logBaseN:  It isn't difined that the base of logarithm is \"Succ Zero\"." 
logBaseN n              = unfoldN isOne (`divN` n)


test_logN1 = logN $ makeNat 8
test_logN2 = logN $ makeNat 7
test_logN3 = logN $ makeNat 4
test_logN4 = logN $ makeNat 3
test_logBaseN1 = logBaseN (makeNat 2) $ makeNat 7
test_logBaseN2 = logBaseN (makeNat 2) $ powN (makeNat 2) $ makeNat 3
test_logBaseN3 = logBaseN (makeNat 0) $ makeNat 3
test_logBaseN4 = logBaseN (makeNat 1) $ makeNat 3


-------------------------------------------------
-- ex3.25

hyloN' :: (Maybe b -> b) -> (a -> Maybe a) -> a -> b
hyloN' f g = foldN' f . unfoldN' g

hyloN :: (Maybe b -> b) -> (a -> Maybe a) -> a -> b
hyloN f g x = case g x of
    Nothing -> f Nothing 
    Just y  -> f $ Just $ hyloN f g y

{-
unfoldN' :: (a -> Maybe a) -> a -> Nat
unfoldN' f x = case f x of
    Nothing -> Zero
    Just y  -> Succ (unfoldN' f y)

foldN' :: (Maybe a -> a) -> Nat -> a
foldN' f Zero       = f Nothing
foldN' f (Succ n)   = f $ Just (foldN' f n)
-}


-------------------------------------------------
-- ex3.26

untilN2 :: (a -> Bool) -> (a -> a) -> a -> a -> a
untilN2 p f x y = foldN x f $ unfoldN p f y

-- untilN p f x = untilN2 p f x x

{-
Assumptions are as follow:
    untilN2 p f x y = foldN x f $ unfoldN p f y
    foldN z s Zero      = z
    foldN z s (Succ n)  = s $ foldN z s n
    unfoldN p f x = if p x then Zero else Succ $ unfoldN p f $ f x
Consider the next formula:
    foldN x f $ unfoldN p f y
First, the formula
    unfoldN p f y
has 2 cases:
    if p y then Zero
           else Succ $ unfoldN p f $ f y
If "p y" then the function
    foldN x f
is passed "Zero" and subsequently becomes
    foldN x f Zero = x
else that is passed "Succ $ unfoldN p f $ f y" and subsequently becomes
    foldN x f (Succ $ unfoldN p f $ f y)
    = f (foldN x f $ unfoldN p f $ f y)
    = f (untilN2 p f x (f y))
    = f $ foldN x f $ unfoldN p f $ f y
Next, the formula
    unfoldN p f $ f y
has also 2 cases:
    if p (f y) then Zero
               else Succ $ unfoldN p f $ f $ f y
If "p (f y)" then the function
    foldN x f
is passed "Zero" and subsequently becomes
    foldN x f Zero = x
else that is passed "Succ $ unfoldN p f $ f $ f y" and subsequently becomes
    foldN x f (Succ $ unfoldN p f $ f $ f y)
    = f (foldN x f $ unfoldN p f $ f $ f y)
    = f (untilN2 p f x (f y))
    = f $ foldN x f $ unfoldN p f $ f $ f y
and now, at former f is passed "x" and becomes
    f x
otherwise f is passed "f $ foldN x f $ unfoldN p f $ f $ f y" and becomes
    f $ f $ foldN x f $ unfoldN p f $ f $ f y
so "f (untilN2 p f x (f y))" can be rewrited to the formula
    untilN2 p f (f x) (f y)
From the avobe, we can obtain the result
    untilN2 p f x y = if p y then x else untilN2 p f (f x) $ f y
-}

{-
untilN p f x = untilN2 p f x x
             = if p x then x else untilN2 p f (f x) (f x)
             = if p x then x else untilN p f $ f x
-}


-------------------------------------------------
-- ex3.27

{-
until p f x = if p x then x else until p f (f x)
            = if p x then x else $ if p (f x) then (f x) else until p f $ f $ f x

untilN p f x = foldN x f (unfoldN p f x)
             = if p x then foldN x f Zero else foldN x f (Succ $unfoldN p f $ f x)
             = if p x then x else f (foldN x f $ unfoldN p f $ f x)
             = if p x then x else f (if p (f x) then foldN x f Zero else foldN x f $ Succ $ unfoldN p f $ f $ f x)
             = if p x then x else f $ if p (f x) then x else f $ foldN x f $ unfoldN p f $ f $ f x
-}


-------------------------------------------------
-- ex3.28
