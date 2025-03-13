-- Pro připomenutí:
--
-- Definice nových datových typů pomocí 'data'.
--
--   data <jméno> <arg1> ... <argN> = <ctor1> | ... | <ctorN>

data Extended = MinusInf | PlusInf | Finite Integer
    deriving (Show, Eq)

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

data RoseTree a = Rose a [RoseTree a]
    deriving (Show, Eq)

-- Pokud chceme, aby nově definovaný datový typ patřil do nějaké typové třídy,
-- můžeme buď použít 'deriving' nebo explicitně definujeme instanci.

instance Ord Extended where
    compare MinusInf   MinusInf   = EQ
    compare MinusInf   _          = LT
    compare (Finite x) (Finite y) = compare x y
    compare PlusInf    PlusInf    = EQ
    compare _          PlusInf    = LT
    compare _          _          = GT

-- Nové typové třídy se vytvářejí pomocí 'class'.

class HasValue a where
    hasValue :: a -> Bool

-- > :t hasValue
-- hasValue :: (HasValue a) => a -> Bool
--
-- > :i Eq
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--
-- > :i Num
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--
-- Typová synonyma se vytvářejí pomocí 'type'.

type CharTree = Tree Char

-- Poznámka: 'newtype' je něco mezi 'data' a 'type'. Podobně jako 'data'
-- definuje nový typ, ale stejně jako 'type' nemá žádný efekt na runtime.
--
-- Omezení: pouze jeden konstruktor s jednou položkou.

newtype Z7 = Z7 { getZ7 :: Int }
    deriving (Eq, Show)

op1 :: (Int -> Int) -> Z7 -> Z7
op1 f (Z7 a) = Z7 $ f a `mod` 7

op2 :: (Int -> Int -> Int) -> Z7 -> Z7 -> Z7
op2 (?) (Z7 a) (Z7 b) = Z7 $ (a ? b) `mod` 7

instance Num Z7 where
    (+)    = op2 (+)
    (-)    = op2 (-)
    (*)    = op2 (*)
    negate = op1 negate
    abs    = id
    signum = op1 signum

    fromInteger = Z7 . fromInteger . (`mod` 7)

-- Stejně jako máme (datové) konstruktory pro vytváření hodnot, tak máme i
-- tzv. typové konstruktory, které vytvářejí konkrétní typy. S několika
-- typovými konstruktory jsme se už setkali: [] (seznamy), Maybe, Either, Tree
--
-- Tree sám o sobě není typ. Tree Int ale už ano. Tree tedy dostane jeden
-- argument a vytvoří (zkonstruuje) konkrétní typ.
--
-- Víceméně to odpovídá rozdílu mezi
--
--   Dictionary<Key, Value>
--   Dictionary
--
-- Druhý typ je v jistém smyslu neúplný.
--
-- Jak poznat, co je co? Nejprve zavedeme pojem 'kind'. Stejně jako hodnoty
-- mají typ, tak i typy mají svůj typ - kind.
--
-- Všechny základní typy mají kind '*'.
--
-- > :k Int
-- Int :: *
--
-- > :k Char
-- Char :: *
--
-- > :k [Int]
-- [Int] :: *
--
-- > :k Int -> Int
-- Int -> Int :: *
--
-- Typové konstruktory mají zajímavější kind.
--
-- > :k Tree
-- Tree :: * -> *
--
-- '* -> *' značí, že Tree potřebuje jeden konkrétní typ (jehož kind je '*')
-- a pak vyprodukuje konkrétní typ ('*'). A skutečně:
--
-- > :k Tree Int
-- Tree Int :: *
--
-- > :k Either
-- Either :: * -> * -> *
--
-- 'Either' potřebuje dva konkrétní typy než dostaneme konkrétní typ.
--
-- > :k Either Int
-- Either Int :: * -> *
--
-- > :k Either Int Char
-- Either Int Char :: *

data F a b = F (b a)

-- > :k F
-- F :: * -> (* -> *) -> *
--
-- K čemu je tohle dobré? Typové třídy se dají definovat i pro typové
-- konstruktory, tj. i pro věci, jejichž kind je různý od '*'.

class Collection c where
    toList :: c a -> [a]

    contains :: (Ord a) => a -> c a -> Bool
    contains a c = a `elem` toList c

-- 'c' má kind '* -> *', tj. instance můžeme definovat pro typové konstruktory,
-- které mají kind '* -> *' - [], Tree, Maybe, atp.
--
-- Collection obsahuje dvě funkce: toList a contains. contains má navíc
-- defaultní implementaci. Když definujeme instanci třídy Collection, contains
-- potom není nutné explicitně definovat. Nicméně explicitní implementace může
-- být užitečná, pokud je defaultní implementace neefektivní

instance Collection [] where
    toList = id

instance Collection Maybe where
    toList (Just x) = [x]
    toList _        = []

instance Collection Tree where
    toList Leaf = []
    toList (Node l x r) = toList l ++ [x] ++ toList r

    contains _ Leaf = False
    contains x (Node l y r)
        | x <  y    = contains x l
        | x == y    = True
        | otherwise = contains x r

-- Podívejme se na často používanou standardní typovou třídu, která je
-- definovaná pro typové konstruktory kindu '* -> *'.
--
-- > :i Functor
-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b
--   ...
--
-- Pokud za 'f' dáme '[]', dostaneme
--
--   fmap :: (a -> b) -> [] a -> [] b
--   fmap :: (a -> b) -> [a]  -> [b]
--
-- Tento typ už jsme dříve viděli: je to funkce 'map'. Typová třída 'Functor'
-- zobecňuje pojem mapování pro více než jen seznamy.
--
-- > :i Functor
-- ...
-- instance Functor [] -- Defined in `GHC.Base'
-- instance Functor Maybe -- Defined in `GHC.Base'
-- ...
--
-- > fmap show (Just 1)
-- Just "1"
--
-- > fmap show Nothing
-- Nothing

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Functor RoseTree where
    fmap f (Rose a r) = Rose (f a) (map (fmap f) r)

-- instance Functor (Either e) where
--     fmap _ (Left e)  = Left e
--     fmap f (Right a) = Right (f a)
--
-- Semigroup: množina s asociativní binární operací
-- Monoid: Semigroup s neutrálním prvkem
--
-- > :i Semigroup
-- class Semigroup a where
--     (<>) :: a -> a -> a
--     ...
--
-- > :i Monoid
-- class Monoid a where
--     mempty :: a
--     ...
--
-- instance Semigroup [a] where
--     (<>) = (++)
--
-- instance Monoid [a] where
--     mempty = []
--
-- Pro číselné typy známe dva monoidy: sčítání s nulou, násobení s jedničkou.

newtype Sum a = Sum { getSum :: a }
newtype Product a = Product { getProduct :: a }

instance (Num a) => Semigroup (Sum a) where
    Sum a <> Sum b = Sum (a + b)

instance (Num a) => Monoid (Sum a) where
    mempty = Sum 0

instance (Num a) => Semigroup (Product a) where
    Product a <> Product b = Product (a * b)

instance (Num a) => Monoid (Product a) where
    mempty = Product 1

-- Proč se bavíme o monoidech? Nedávno jsme si ukazovali funkce 'foldr' a
-- 'foldl'. Podobně jako 'Functor' zobecňuje 'map' (na 'fmap'), tak 'Foldable'
-- zobecňuje 'foldr' (na 'foldMap').
--
-- > :i Foldable
-- class Foldable (t :: * -> *) where
--   foldMap :: Monoid m => (a -> m) -> t a -> m

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

-- > let t = Node (Node Leaf 2 Leaf) 4 (Node Leaf 6 Leaf)
-- > foldr (+) 0 t
-- 12
--
-- > length t
-- 3
--
-- Pomocí 'foldMap' implementujte:

length' :: (Foldable t) => t a -> Int
length' = undefined

sum' :: (Foldable t, Num a) => t a -> a
sum' = undefined

product' :: (Foldable t, Num a) => t a -> a
product' = undefined

toList' :: (Foldable t) => t a -> [a]
toList' = undefined
