import Test.QuickCheck


-------------------------------------------------
-- DEFINE Buffer

type Buffer = (Int, String)


empty :: Buffer
empty = (0, "")

insert :: Char -> Buffer -> Buffer
insert c (i, s) = (i+1, take i s ++ [c] ++ drop i s)

delete :: Buffer -> Buffer
delete (i, s) = (i-1, take (i-1) s ++ drop i s)

left :: Buffer -> Buffer
left (i, s) = (i-1, s)

right :: Buffer -> Buffer
right (i, s) = (i+1, s)

atLeft :: Buffer -> Bool
atLeft (i, s) = i == 0

atRight :: Buffer -> Bool
atRight (i, s) = i == length s



-------------------------------------------------
-- DEFINE BufferI

type BufferI = (String, String)


emptyI :: BufferI
emptyI = ("", "")

insertI :: Char -> BufferI -> BufferI
insertI c (f, b) = (c:f, b)

deleteI :: BufferI -> BufferI
deleteI (c:f, b) = (f, b)

leftI :: BufferI -> BufferI
leftI (c:f, b) = (f, c:b)

rightI :: BufferI -> BufferI
rightI (f, c:b) = (c:f, b)

atLeftI :: BufferI -> Bool
atLeftI (f, _) = length f == 0

atRightI :: BufferI -> Bool
atRightI (_, b) = length b == 0



-------------------------------------------------
-- DEFINE properties

prop_abs_empty      = retrieve emptyI == empty
prop_abs_insert c b = retrieve (insertI c b) == insert c (retrieve b)
prop_abs_delete b   = not (atLeftI b) ==> retrieve (deleteI b) == delete (retrieve b)
prop_abs_left b     = not (atLeftI b) ==> retrieve (leftI b) == left (retrieve b)
prop_abs_right b    = not (atRightI b) ==> retrieve (rightI b) == right (retrieve b)
prop_abs_atLeft b   = atLeftI b == atLeft (retrieve b)
prop_abs_atRight b  = atRightI b == atRight (retrieve b)

prop_alg_insert_delete c b      = deleteI (insertI c b) == b
prop_alg_atLeft_add_left c b    = atLeftI (leftI (insertI c b)) == atLeftI b
prop_alg_atRight_add c b        = atRightI (insertI c b) == atRightI b

-------------------------------------------------
-- DEFINE functions FOR properties

retrieve :: BufferI -> Buffer
retrieve (f, b) = (length f, (reverse f) ++ b)
