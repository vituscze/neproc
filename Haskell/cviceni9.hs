-- 9. cvičení 2017-04-18
--
-- Pro připomenutí:
--
-- Všechny funkce v Haskellu mají jeden argument. Pokud potřebujeme funkci
-- s více argumenty, použijeme Curryfikaci (vrátíme funkci, kde je první
-- argument zafixovaný; částečná aplikace).
--
-- > map (max 5) [1..10]
-- [5,5,5,5,5,6,7,8,9,10]

i :: a -> a
i x = x

-- > i length [1..5]
-- 5
--
-- Proč tohle funguje?


-- Poznámka ohledně whitespace. Jak Haskell pozná, že začínáte novou definici
-- nebo pokračujete v předchozí? Narozdíl od C# resp. Prologu nemá Haskell ';'
-- resp. '.'
--
-- Odpověd je odsazení. Pár klíčových slov uvozuje tzv. layout (where, let,
-- of, do). Haskell si pak zapamatuje odsazení následujícího kódu.
--
-- Pokud je odsazení další řádky větší, tak je to pokračování předchozí řádky.
-- Pokud je odsazení stejné, tak je to nová řádka (třeba v případě where
-- další lokální definice). Menší odsazení pak ukončuje layout.

test :: Int -> Int
test x =
 x + 2  -- v pořádku, pokračování předchozí řádky

test2 :: Int -> Int
test2 x = z
  where
    y = x * x +
        x       -- Pokračování předchozí řádky
    z = y * y   -- Nová definice

test3 = (+)     -- Konec where

-- Ukázali jsme si, že se funkce dají vracet jako návratové hodnoty a používat
-- jako argumenty. Funkce také můžeme ukládat do datových struktur.

fns :: (Ord a, Integral a) => [a -> a]
fns = [(+2), (*3), max 2, (`div` 2)]

applyArg :: a -> (a -> b) -> b
applyArg x f = f x

-- > map (applyArg 5) fns
-- [7,15,5,2]
--
-- Funkci applyArg použijeme pravděpodobně právě jednou. Můžeme dosáhnout
-- stejného výsledku bez toho, abychom zbytečně definovali extra funkce?
--
-- Odpovědí jsou lambda výrazy (možná taky znáte jako anonymní funkce).
-- Lambdy jsou vlastně něco jako funkční literály.
--
-- Syntax:
--
--   \ arg1 arg2 ... argN -> body
--
-- > :t \x y -> not x || y
-- \x y -> not x || y :: Bool -> Bool -> Bool
--
-- Za tělo funkce se považuje všechno vpravo od ->. Pokud rozsah těla lambdy
-- chceme omezit, stačí vhodně uzávorkovat.
--
-- Předchozí příklad můžeme napsat jako:
--
-- > map (\f -> f 5) fns
-- [7,15,5,2]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _ _      []     = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

-- > zipWith (+) [1..5] [10,20..50]
-- [11,22,33,44,55]
--
-- > zipWith (\a b -> floor (a^2 / b)) [1..5] [5..10]
-- [0,0,1,2,2]
--
-- Na argumentech můžeme provádět jednoduchý pattern matching, pokud
-- potřebujeme něco složitějšího, tak musíme použít case.
--
-- > map (\(a,b) -> a * b) [(4,2),(2,6),(0,10)]
-- [8,12,0]
--
-- Na funkce více argumentů se můžeme díky lambdám dívat takto:

f1 a b c = a * b + c

f2 = \a -> \b -> \c -> a * b + c

-- Tyto dvě definice jsou ekvivalentní.
--
--
-- Skládání datových struktur.
--
-- Většina funkcí, které pracují nad seznamy, má podobnou strukturu.
-- Případ pro prádzný seznam, případ pro neprádzný seznam s rekurzivním
-- voláním na zbytek seznamu.
--
-- Můžeme tohle zobecnit?

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f z []     = z
foldRight f z (x:xs) = f x (foldRight f z xs)

-- Standardní knihovna: foldr

sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

product' :: (Num a) => [a] -> a
product' = foldr (*) 1

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x r -> f x:r) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldr (\y r -> x == y || r) False

-- Idea:
--
--   (:)                                 f
--   / \                                / \
--  1  (:)       == foldr f z =>       1   f
--     / \                                / \
--    2  (:)                             2   f
--       / \                                / \
--      3   []                             3   z
--
--
-- Skládání druhým směrem.

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft f acc []     = acc
foldLeft f acc (x:xs) = foldLeft f (f acc x) xs

-- Standardní knihovna: foldl
--
-- Reprezentuje to, co známe z Prologu jako akumulátor.
--
--   (:)                                  f
--   / \                                 / \
--  1  (:)       == foldl f z =>        f   3
--     / \                             / \
--    2  (:)                          f   2
--       / \                         / \
--      3   []                      z   1

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

-- foldr1 a foldl1 jsou verze foldr a foldl, které pracují pouze na neprázdných
-- seznamech, počáteční hodnotou je první prvek seznamu.

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 max

-- Definované výše:
--   fns = [(+2), (*3), max 2, (`div` 2)]
--
-- > foldr (.) id fns 7
-- 11
--
--
-- Občas v kódu najdete operátor $, který je definovaný takto:
--
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
--
-- Nedělá nic zajímavého, ale má nízkou prioritu, takže se dá použít
-- pro odbourávání závorek.
--
-- > (max 2 . (*2) . (^2)) 5
-- 50
--
-- > max 2 . (*2) . (^2) $ 5
-- 50
--
--
-- Líné vyhodnocování
--
-- Vraťme se zpět k funkci elem'.
--
--   elem' :: (Eq a) => a -> [a] -> Bool
--   elem' x = foldr (\y r -> x == y || r) False
--
-- > elem' 2 [1..]
-- True
--
-- Proč jsme dostali True a výpočet se nezacyklil?
--
--   (1 == 2) || ((2 == 2) || ((3 == 2) ... ))
--
-- Short-circuit pro ||. Tak jak to známe v tradičních jazycích.
-- Ale...

or' True _ = True
or' _    x = x

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x = foldr (\y r -> or' (x == y) r) False

-- > elem'' 2 [1..]
-- True
--
-- Náš or' také umí short-circuit, jakto? Haskell vyhodnocuje jen to, co je
-- pro výpočet nezbytně nutné. Pokud zjistíme, že prvním argumentem funkce or'
-- je True, tak rovnou vracíme True a na druhý argument se ani nepodíváme.
--
-- Místo toho, abychom funkce volali na nějaký nekončný výpočet (např.
-- let x = x in x) a pak sledovali, jestli se výpočet zastaví nebo ne, můžeme
-- použít hodnotu undefined, která "shodí" program, pokud se ji někdo pokusí
-- vyhodnotit.
--
-- > undefined
-- *** Exception: Prelude.undefined
--
-- > True || undefined
-- True
--
-- > undefined || True
-- *** Exception: Prelude.undefined
--
-- > length [undefined, undefined, undefined]
-- 3
--
-- > head (1:undefined)
-- 1
--
-- Pár zajímavých definic:

ones :: [Integer]
ones = 1:ones

nats :: [Integer]
nats = 0:map (+1) nats

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

-- Dokonce můžeme implementovat vlastní if, který skutečně vyhodnotí pouze
-- jednu větev.

if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ b = b

-- > if' True 1 undefined
-- 1

-- Příklady na procvičení: test reflexivity, symetrie, tranzitivity
-- hledání tříd ekvivalence, reflexivní uzávěr, zobecněný kartézký součin,
-- skládání seznamu funkcí, hledání posloupnosti fcí maximalizujících výslednou
-- hodnotu
