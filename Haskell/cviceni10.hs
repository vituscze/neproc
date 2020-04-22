-- 10. cvičení 2017-04-25
--
-- Nové datové typy se vytvářejí pomocí klíčového slova 'data'.
--
-- data <jméno> = <definice>
--
-- Jméno musí začínat velkým písmenem (typy, které začínají malým písmenem
-- jsou typové proměnné). Definice je pak výčet 'konstruktorů' - možných
-- hodnot datového typu - oddělený svislítkem '|'.
--
-- např. definice typu Bool by vypadala takto:
--
--   data Bool = False | True
--
-- Tohle jednoduše definuje Bool jako typ se dvěma konstantami - False a True.
--
-- Jednotlivé konstruktory mohou mít položky.

data Extended = PlusInfinity | MinusInfinity | Finite Integer

-- PlusInfinity a MinusInfinity jsou konstanty, Finite obsahuje jednu položku
-- typu Integer.
--
-- > :t Finite
-- Finite :: Integer -> Extended
--
-- > :t PlusInfinity
-- PlusInfinity :: Extended
--
-- Pokud dostaneme hodnotu typu Extended, můžeme ji rozebrat stejně jako
-- např. seznamy - pattern matching.

leq :: Extended -> Extended -> Bool
leq MinusInfinity _             = True
leq _             PlusInfinity  = True
leq (Finite x)    (Finite y)    = x <= y
leq _             _             = False

-- > PlusInfinity
-- <interactive>:2:1: error:
--     * No instance for (Show Extended) arising from a use of `print'
--     * In a stmt of an interactive GHCi command: print it
--
-- GHCi neví, jak zobrazit hodnotu PlusInfinity!
--
-- Potřebujeme implementovat Show pro typ Extended. Pro pár základních typových
-- tříd (Eq, Ord, Show, ...) to GHC umí udělat za nás.
--
-- data Extended = PlusInfinity | MinusInfinity | Finite Integer
--     deriving (Show)
--
-- data Point = Point Double Double
--   deriving (Show, Eq)  -- Ord nedává smysl
--
-- getX :: Point -> Double
-- getX (Point x _) = x
--
-- getY :: Point -> Double
-- getY (Point _ y) = y
--
-- Pokud máme datový typ, který má pouze jednen konstruktor, tak můžeme tyhle funkce
-- dostat zadarmo. Stačí tento typ definovat jako 'record'.

data Point = Point { getX :: Double, getY :: Double }
    deriving (Show, Eq)

-- > :t getX
-- getX :: Point -> Double
--
-- V předchozí definici jsme použili jméno Point dvakrát? Nedojde ke konfliktu?
-- Z kontextu se vždy dá jednoznačně určit, jestli se jedná o jméno typu
-- nebo o jméno konstruktoru.
--
-- Také můžeme definovat rekurzivní typy.

data IntTree = IntLeaf | IntNode Int IntTree IntTree
    deriving (Show, Eq)

insert :: Int -> IntTree -> IntTree
insert x IntLeaf = IntNode x IntLeaf IntLeaf
insert x (IntNode y l r)
    | x <  y    = IntNode y (insert x l) r
    | x == y    = IntNode x l r
    | otherwise = IntNode y l (insert x r)

-- Pokud definujeme strom takovýmto způsobem, budeme potřebovat kopii pro
-- každý datový typ. Řešení: datový typ parametrizujeme typovou proměnnou.

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

insert' :: (Ord a) => a -> Tree a -> Tree a
insert' x Leaf = Node x Leaf Leaf
insert' x (Node y l r)
    | x <  y    = Node y (insert' x l) r
    | x == y    = Node x l r
    | otherwise = Node y l (insert' x r)

-- Původní IntTree je tedy Tree Int (podobně jako [Int]).
--
-- > :t Leaf
-- Leaf :: Tree a
--
-- > :t Node
-- Node :: a -> Tree a -> Tree a -> Tree a
--
-- Všimněte si, že podobně jako [] vytváří Leaf strom, jehož prvky mají
-- libovolný typ.
--
-- Parametrů může být i více.
--
--   data Either a b = Left a | Right b
--
-- Either a b obsahuje buď hodnotu typu a nebo hodnotu typu b.
--
-- > :t Left
-- Left :: a -> Either a b
--
-- > :t Right
-- Right :: b -> Either a b
--
-- Kromě vlastních datových typů můžeme vytvářet synonyma pro již existující
-- typy. Jedná se pouze o syntaktickou zkratku, z hlediska kompilátoru jsou
-- synonymum a typ, který reprezentuje, shodné.
--
-- type String = [Char]

type CharTree = Tree Char

-- Některé typové třídy se dají odvodit automaticky pomocí deriving, ale někdy
-- nám defaultní instance nevyhovuje nebo odvodit nelze.

data BoolFn = BoolFn (Bool -> Bool)

-- Např. Eq automaticky odvodit nelze.
--
-- > :i Eq
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--
-- Vytvoříme tedy instanci manuálně:

instance Eq BoolFn where
    BoolFn f == BoolFn g =
        all (\x -> f x == g x) [False, True]

-- (/=) se pak automaticky dodefinuje podle (==), uvidíme níže.
--
-- instance Eq BoolFn říká, že definujeme instanci třídy Eq pro typ BoolFn.
-- Za where pak následují definice releventních hodnot (v našem případě
-- funkce (==)).

instance Show BoolFn where
    show (BoolFn f) = concat
        [ "BoolFn (let f True = "
        , show $ f True
        , "; f False = "
        , show $ f False
        , " in f)"
        ]

-- Jakmile implementujeme instanci třídy Eq pro náš typ, tak můžeme používat
-- i všechny ostatní funkce, které Eq používají, např. elem:
--
-- > BoolFn id `elem` [BoolFn not, BoolFn (not . not)]
-- True

data Optional a = Empty | Value a

-- instance Eq (Optional a) where
--     Empty   == Empty   = True
--     Value x == Value y = ???
--     _       == _       = False
--
-- Na místo ??? zjevně patří x == y, ale o typu a nic nevíme, speciálně nevíme,
-- jestli se vůbec dá porovnávat.
--
-- Kvůli tomu se do definice instancí dají přidat podmínky na vnitřní typy:

instance (Eq a) => Eq (Optional a) where
    Empty   == Empty   = True
    Value x == Value y = x == y
    _       == _       = False

-- Pokud se hodnoty typu a dají porovnávat, pak lze porovnávat i hodnoty typu
-- Optional a.
--
-- Můžeme také definovat nové typové třídy.

class HasValue a where
    hasValue :: a -> Bool

-- Pokud chce nějaký typ být součástí třídy HasValue, musí poskytnout
-- implementaci funkce hasValue. Např.

instance HasValue (Optional a) where
    hasValue (Value _) = True
    hasValue _         = False

instance HasValue [a] where
    hasValue = not . null

-- Jen pro připomenutí: typové třídy jsou Haskellské řešení tzv. "přetěžování",
-- tj. jedna funkce, která pracuje pro více typů.
