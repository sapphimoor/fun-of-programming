import Test.QuickCheck
import Data.List
import Control.Monad

-------------------------------------------------
-- DEFINE propositional logic

newtype Name = Name String deriving (Eq, Show)

data Form = Var Name
            | Form :& Form
            | Not Form
    deriving (Eq, Show)

data Valuation = Val {falses :: [Name], trues :: [Name]} deriving (Eq, Show)


names :: Form -> [Name]
names (Var v)   = [v]
names (p :& q)  = names p `union` names q
names (Not p)   = names p


nothing :: Valuation
nothing = Val {falses = [], trues = []}

tableau :: Form -> Valuation -> [Valuation]
tableau (Var v) val
    | v `elem` trues val    = [val]
    | v `elem` falses val   = []
    | otherwise             = [val {trues = v : trues val}]
tableau (Not (Var v)) val
    | v `elem` trues val    = []
    | v `elem` falses val   = [val]
    | otherwise             = [val {falses = v : falses val}]
tableau (p :& q) val        = [val_pq | val_p <- tableau p val, val_pq <- tableau q val_p]
-- tableau (Not (p :& q)) val  = tableau (Not p) val ++ tableau (Not q) val
tableau (Not (p :& q)) val  = tableau (Not p) val ++ tableau (p :& Not q) val
-- tableau (Not (p :& q)) val  = tableau (Not p :& q) val ++ tableau (p :& Not q) val
tableau (Not (Not p)) val   = tableau p val

models :: Form -> [Valuation]
models p = tableau p nothing

(|=) :: Valuation -> Form -> Bool
val |= Var (Name v)
    | Name v `elem` trues  val   = True
    | Name v `elem` falses val   = False
    | otherwise             = error ("undefined variable " ++ v)
val |= (p :& q)             = (val |= p) && (val |= q)
val |= (Not p)              = not (val |= p)


-------------------------------------------------
-- DEFINE arbitrary

instance Arbitrary Name
    where
        arbitrary = sized
                    $ \n -> do  c <- elements (take (n+1) ['a'..'z'])
                                return (Name [c])

instance Arbitrary Form
    where
        arbitrary = sized arbForm

arbForm 0 = do  k <- choose (1, 26)
                c <- elements (take k ['a'..'z'])
                return (Var (Name [c]))
arbForm n
    | n > 0 = frequency [(1 , arbForm 0),
                         (10, liftM2 (:&) miniForm miniForm),
                         (5 , fmap Not (arbForm n))]
    where
        miniForm = arbForm (n `div` 3)

-- -- REFERENCE
-- instance Arbitrary a => Arbitrary (Tree a)
--     where
--         arbitrary = sized arbTree
-- 
-- arbTree 0 = return Leaf
-- arbTree n
--     | n > 0 = frequency [(1, return Leaf),
--                          (3, liftM3 Branch shrub arbitrary shrub)]
--         where
--             shrub = arbTree (n `div` 2)
-- 

valuationOver :: [Name] -> Gen Valuation
valuationOver ns =
    do  bs <- vector (length ns)
        return (Val {falses = [n | (n, False) <- ns `zip` bs],
                     trues  = [n | (n, True ) <- ns `zip` bs]})


-------------------------------------------------
-- DEFINE properties

prop_arbForm :: Form -> Property
prop_arbForm p = collect (count p) (isForm p)

-- prop_Sound :: Form -> Bool
-- prop_Sound p = all (\val -> val |= p) (models p)

prop_Sound :: Form -> Property
prop_Sound p = forAll (valuationOver (names p))
                    $ \ext -> all (\val -> (val `totalise` ext) |= p) (models p)


prop_Complete :: Form -> Property
prop_Complete p = forAll (valuationOver (names p))
                    $ \val -> val |= p
                        ==> any (`covers` val) (models p)

prop_Complete' :: Form -> Property
prop_Complete' p = forAll (valuationOver (names p))
                    $ \val -> val |= p
                        ==> trivial (length (names p) <= 2)
                            $ any (`covers` val) (models p)

prop_Complete'' :: Form -> Property
prop_Complete'' p = forAll (valuationOver (names p))
                    $ \val -> val |= p
                        ==> classify (0  <= len && len <= 4 ) "0-4."
                          $ classify (5  <= len && len <= 9 ) "5-9."
                          $ classify (10 <= len && len <= 14) "10-14."
                          $ classify (15 <= len && len <= 19) "15-19."
--                           $ classify (20 <= len             ) ">=20."
                          $ any (`covers` val) (models p)
    where
        len = length (names p)

prop_Complete_contraposition :: Form -> Property
prop_Complete_contraposition p = forAll (valuationOver (names p))
                                    $ \val -> not (any (`covers` val) (models p))
                                        ==> not (val |= p)


prop_Exclusive :: Form -> Bool
prop_Exclusive p =
    not (or [val `covers` val' | val <- ms, val' <- ms \\ [val]])
        where
            ms = models p


-------------------------------------------------
-- DEFINE functions FOR properties

totalise :: Valuation -> Valuation -> Valuation
val `totalise` ext = Val {falses = falses val ++ (falses ext \\ defined),
                          trues  = trues  val ++ (trues  ext \\ defined)}
    where
        defined = falses val ++ trues val

covers :: Valuation -> Valuation -> Bool
val `covers` val' = falses val `subset` falses val' &&
                    trues  val `subset` trues  val'
    where
        xs `subset` ys = all (`elem` ys) xs


trivial p = classify p "trivial"


isForm :: Form -> Bool
isForm form = case form of
    Var v   -> True
    p :& q  -> isForm p && isForm q
    Not p   -> isForm p

count :: Form -> Integer
count (Var v) = 1
count (Not p) = 1 + count p
count (p :& q) = count p + count q
