import Test.QuickCheck


-------------------------------------------------
-- DEFINE Queue

type Queue a = [a]


empty :: Queue a
empty = []

add :: a -> Queue a -> Queue a
add x q = q ++ [x]

isEmpty :: Queue a -> Bool
isEmpty = null

front :: Queue a -> a
front(x:q) = x

remove :: Queue a -> Queue a
remove(x:q) = q



-------------------------------------------------
-- DEFINE QueueI

type QueueI a = ([a], [a])


emptyI :: QueueI a
emptyI = ([], [])

addI :: a -> QueueI a -> QueueI a
addI x (f, b) = flipQ (f, x:b)

isEmptyI :: QueueI a -> Bool
isEmptyI (f, b) = null f

frontI :: QueueI a -> a
frontI (x:f, b) = x

removeI :: QueueI a -> QueueI a
removeI (x:f, b) = flipQ(f, b)

flipQ :: QueueI a -> QueueI a
flipQ([], b) = (reverse b, [])
flipQ q      = q




-------------------------------------------------
-- DEFINE properties

prop_abs_empty      = retrieve emptyI == empty
prop_abs_add x q    = invariant q
                        ==> retrieve (addI x q) == add x (retrieve q)
prop_abs_isEmpty q  = invariant q
                        ==> isEmptyI q == isEmpty (retrieve q)
prop_abs_front q    = invariant q && not (isEmptyI q)
                        ==> frontI q == front (retrieve q)
prop_abs_remove q   = invariant q && not (isEmptyI q)
                        ==> retrieve (removeI q) == remove (retrieve q)

prop_alg_isEmpty q      = invariant q
                            ==> isEmptyI q == (q == emptyI)
prop_alg_front_empty x  = frontI (addI x emptyI) == x
prop_alg_front_add x q  = invariant q && not (isEmptyI q)
                            ==> frontI (addI x q) == frontI q
prop_alg_remove_empty x = removeI (addI x emptyI) == emptyI
prop_alg_remove_add x q = invariant q && not (isEmptyI q)
                            ==> removeI (addI x q) `equiv` addI x (removeI q)

prop_add_equiv q q' x   = q `equiv` q'
                            ==> addI x q `equiv` addI x q'
prop_front_equiv q q'   = not (isEmptyI q) && q `equiv` q'
                            ==> frontI q == frontI q'
prop_isEmpty_equiv q q' = q `equiv` q'
                            ==> isEmptyI q == isEmptyI q'
prop_remove_equiv q q'  = not (isEmptyI q) && q `equiv` q'
                            ==> removeI q `equiv` removeI q'


-------------------------------------------------
-- DEFINE functions FOR properties

retrieve :: QueueI Integer -> Queue Integer
retrieve (f, b) = f ++ reverse b

invariant :: QueueI Integer -> Bool
invariant (f, b) = not (null f) || null b

q `equiv` q' =
    invariant q && invariant q' && retrieve q == retrieve q'