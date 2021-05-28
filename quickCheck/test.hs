import Test.QuickCheck
import Control.Monad

---------------------------------------------------------------------------------------------------
-- DEFINE functions

insert :: Integer -> [Integer] -> [Integer]
insert x [] = [x]
insert x (h:t)
    | x <= h    = x:h:t
    | otherwise = h : insert x t


exist :: Integer -> [Integer] -> Bool
exist x [] = False
exist x (h:t)
    | x == h    = True
    | otherwise = exist x t

exists :: [Integer] -> [Integer] -> Bool
exists [] xs = True
exists (h:t) xs
    | exist h xs    = exists t xs
    | otherwise     = False

hasSameMember :: [Integer] -> [Integer] -> Bool
hasSameMember x y = exists x y && exists y x


merge :: [Integer] -> [Integer] -> [Integer]
merge x [] = x
merge [] y = y
merge x@(h1:t1) y@(h2:t2)
    | h1 < h2   = h1 : merge t1 y
    | otherwise = h2 : merge x t2

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort (h1:h2:t) = merge (merge [h1] [h2]) (mergeSort t)



---------------------------------------------------------------------------------------------------
-- DEFINE properties

prop_plusAssociative :: Integer -> Integer -> Integer -> Bool
--prop_PlusAssociative:: Float -> Float -> Float -> Bool
prop_plusAssociative x y z = (x + y) + z == x + (y + z)


-- prop_insertOrdered :: Integer -> [Integer] -> Property
prop_insertOrdered x xs = ordered xs
                            ==> ordered (insert x xs)

-- prop_insertOrdered' :: Integer -> Property
prop_insertOrdered' x   = forAll orderedList'
                            $ \xs -> ordered (insert x xs)

-- prop_insertOrdered'' :: Integer -> Property
prop_insertOrdered'' x  = forAll orderedList''
                            $ \xs -> ordered (insert x xs)


-- prop_insertOrdered2 x xs = ordered xs
--                             ==> trivial (length xs <= 2)
--                                 $ ordered
--                                 $ insert x xs

prop_insertOrdered2 x   = forAll orderedList''
                            $ \xs -> trivial (length xs <= 2)
                                    $ ordered
                                    $ insert x xs

prop_insertOrdered2' x  = forAll orderedList''
                            $ \xs -> classify (null xs) "empty lists"
                                    $ classify (length xs == 1) "unit lists"
                                    $ ordered
                                    $ insert x xs

prop_insertOrdered2'' x = forAll orderedList''
                            $ \xs -> collect (length xs)
                                    $ ordered
                                    $ insert x xs


-- prop_insertExist :: Integer -> Property
-- prop_insertExist x xs   = ordered xs
--                             ==> exists (x:xs) (insert x xs) && exists (insert x xs) (x:xs)

prop_insertExist x xs   = ordered xs
                                ==> hasSameMember (x:xs) (insert x xs)

-- prop_insertExist' :: Integer -> [Integer] -> Property
prop_insertExist' x      = forAll orderedList''
                                $ \xs -> hasSameMember (x:xs) (insert x xs)


-- prop_mergeOrdered :: [Integer] -> [Integer] -> Property
prop_mergeOrdered x y   = ordered x && ordered y
                            ==> ordered
                                $ merge x y

prop_mergeOrdered'       = forAll orderedList''
                            $ \x -> forAll orderedList''
                            $ \y -> ordered
                                    $ merge x y

-- prop_mergeExist :: [Integer] -> [Integer] -> Property
-- prop_mergeExist x y     = ordered x && ordered y
--                             ==> exists (x ++ y) (merge x y) && exists (merge x y) (x ++ y)

prop_mergeExist x y     = ordered x && ordered y
                                ==> hasSameMember (x ++ y) (merge x y)

prop_mergeExist' x y    = forAll orderedList''
                            $ \x -> forAll orderedList''
                            $ \y -> hasSameMember (x ++ y) (merge x y)

-- prop_mergeSortOrdered :: [Integer] -> Bool
prop_mergeSortOrdered list  = ordered 
                            $ mergeSort list

-- prop_mergeSortExist :: [Integer] -> Bool
-- prop_mergeSortExist list = exists list (mergeSort list)  && exists (mergeSort list) list

prop_mergeSortExist list    = hasSameMember list (mergeSort list)


prop_max_le :: Integer -> Integer -> Bool
prop_max_le x y = x <= x `max` y



---------------------------------------------------------------------------------------------------
-- DEFINE functions FOR properties


ordered :: [Integer] -> Bool
ordered [] = True
ordered [_] = True
ordered (h1:h2:t)
    | h1 > h2   = False
    | otherwise = ordered (h2:t)


atLeast :: Integer -> Gen Integer
atLeast x = do  diff <- arbitrary
                return (x + abs diff)

orderedList' :: Gen [Integer]
orderedList' = do   x <- arbitrary
                    listsFrom x
    where
        listsFrom x = oneof [return [], do  y <- atLeast x
                                            liftM (x:) (listsFrom y)]

orderedList'' :: Gen [Integer]
orderedList'' = do  x <- arbitrary
                    listsFrom x
    where
        listsFrom x = frequency [(1, return []),
                                 (15, do    y <- atLeast x
                                            liftM (x:) (listsFrom y))]


-- trivial :: Testable prop => Bool -> prop -> Property
trivial p = classify p "trivial"

---------------------------------------------------------------------------------------------------
-- DEFINE Tree

data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- instance Arbitrary a => Arbitrary (Tree a)
--     where
--         arbitrary = frequency [(1, return Leaf),
--                                (3, liftM3 Branch arbitrary arbitrary arbitrary)]

instance Arbitrary a => Arbitrary (Tree a)
    where
        arbitrary = sized arbTree

arbTree 0 = return Leaf
arbTree n
    | n > 0 = frequency [(1, return Leaf),
                         (3, liftM3 Branch shrub arbitrary shrub)]
        where
            shrub = arbTree (n `div` 2)
