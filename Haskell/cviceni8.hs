-- 8. cvičení 2017-04-11
--
-- Haskell je typovaný jazyk. Před kompilací/načtením se provede typová
-- kontrola a dále se pokračuje, pouze pokud je úspěšná.
--
-- Narozdíl od Pascalu, C, C++, C#, atp. se typy nemusejí explicitně psát
-- (kromě několika krajních případů), kompilátor si je odvodí sám.
--
-- Pro připomenutí:
-- :t <expr>  napíše typ výrazu expr
--
-- > :t 'x'
-- 'x' :: Char
--
-- > :t 1 < 2
-- 1 < 2 :: Bool
--
-- > :t ("abc", True)
-- ("abc", True) :: ([Char], Bool)
--
--
-- Funkce mají také typ
--
-- > :t not
-- not :: Bool -> Bool
--
-- Co funkce více argumentů?

pyth :: Double -> Double -> Double
pyth a b = sqrt (a * a + b * b)

-- Prozatím: všechno před poslední šipkou typy argumentů, za poslední šipkou typ
-- návratové hodnoty. Za chvíli uvidíme proč.
--
-- Pár dalších základních typů:
--
-- Int           - 64 (resp 32) bitové celé číslo
-- Integer       - celé číslo s libovolnou přesností
-- Float, Double - floating point
-- Char          - znak
-- [X]           - seznam prvků typu X
-- (X, Y)        - dvojice obsahující hodnotu typu X a hodnotu typu Y
-- X -> Y        - funkce s argumentem typu X a návratovou hodnotou typu Y
--
--
-- > :t head
-- head :: [a] -> a
--
-- Funkce head funguje pro seznamy prvků libovolného typu. 'a' je v tomto
-- případě proměnná, za kterou se pak dosadí konkrétní typ.
--
-- Jak poznat proměnnou od konkrétního typu? Proměnné vždy začínají malým
-- písmenem.
--
-- > :t fst
-- fst :: (a, b) -> a
--
-- Poznámka:
-- Každý operátor můžeme použít jako funkci tak, že jej zabalíme do kulatých
-- závorek.
--
-- a + b  je totéž co  (+) a b
--
-- Funkce lze použít jako operátor, když je dáme do 'backticků'
--
-- div a b  je totéž co  a `div` b
--
--
-- Jaký je typ +?
--
-- > :t (+)
-- (+) :: (Num a) => a -> a -> a
--
-- (+) nefunguje pro libovolný typ a! Funguje jen pro typy, které podporují
-- základní aritmetické operace.
--
-- Pomocí :i můžeme zjistit, jaké operace můžeme použít, pokud víme že typ a
-- je Num. A můžeme také zjistit, pro které typy jsou tyto operace definované.
--
-- > :i Num
-- class Num a where
-- ...
-- instance Num Int ...
-- ...
--
-- Další typové třídy:
-- Eq, test na rovnost/nerovnost: (==), (/=)
-- Ord, porovnávání: (<), (<=), atd
-- Show, převod na řetězce
-- Read, převod z řetězců
-- Enum, např [x..y]
--
-- Pattern matching

isZero :: Int -> Bool
isZero 0 = True
isZero x = False

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-- Pozor, hodně pomalé

-- Patterny se zkoušejí shora dolů. Pokud žádný neuspěje - runtime error

encode :: Char -> Char
encode 'a' = 'd'
encode 'b' = 'q'

-- > encode 'c'
-- *** Exception: ...: Non-exhaustive patterns in function encode
--
-- Další příklady:

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

first :: (a, b, c) -> a
first (x, _, _) = x

len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

-- [] matchuje prázdný seznam
-- (x:xs) matchuje neprázdný seznam s hlavou x a zbytkem xs
-- (x1:x2:xs) matchuje seznam délky alespoň 2, x1 je první prvek, x2 druhý
-- [x] matchuje seznam s právě jedním prvkem
-- [x,y]
-- atp.

-- Zpátky k fukčním typům. Proč vypadají typy funkcí více argumentů takto?
--
-- a -> b -> c
--
-- Důvod je jednoduchý: všechny funkce v Haskellu jsou pouze unární. Jak to
-- funguje? Podívejme se zpátky na funkci pyth. Jakmile aplikujeme pyth
-- na nějaké číslo x, dostaneme zpátky novou funkci typu Double -> Double, kde
-- první číslo je fixované (a jeho hodnota je x).

pyth3 :: Double -> Double
pyth3 = pyth 3.0

-- > pyth3 4.0
-- 5.0
--
-- > pyth3 10.0
-- 10.44030650891055
--
-- Tedy, a -> b -> c znamená:
--
-- a -> (b -> c)
-- ^    \______/
-- |        |
-- vstup    výstup
--

add :: Int -> Int -> Int
-- add x y = x + y
-- lépe
add = (+)

addTwo :: Int -> Int
-- addTwo x = add 2 x
-- lépe:
addTwo = add 2

-- nebo
-- addTwo = (+) 2
--
-- Syntaktická zkratka:
-- (+ 2) 3 --> 3 + 2
-- (2 +) 3 --> 2 + 3
-- (/ 2)
--
-- Funkce, které vracejí funkce jako návratové hodnoty jsou v Haskellu skoro
-- všude. Můžeme mít ale i funkce, jejichž argumenty jsou funkce!

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- > twice (+2) 4
-- 8
--
-- Pozor: a -> a -> a -> a  vs  (a -> a) -> a -> a

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x:map' f xs

-- Standardní knihovna: map
--
-- > map (^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) =
    if p x
    then x:filter' p xs
    else   filter' p xs

-- Standardní knihovna: filter
--
-- > filter odd [1..10]
-- [1,3,5,7,9]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) =
    if p x
    then x:takeWhile' p xs
    else []

-- Standardní knihovna: takeWhile
--
-- Další velice užitečná funkce je comp - skládání funkcí.

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

-- Standardní knihovna: (.)
--
-- > takeWhile (not . null) ["a", "b", "c", "", "d"]
-- ["a","b","c"]
--
--
-- Pattern guards

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' p (x:xs)
  | p x       = x:filter'' p xs
  | otherwise =   filter'' p xs

-- otherwise je jednoduše definováno jako True
-- Pokud se žádný guard nevyhodnotí na True, zkusí se další pattern.
--
-- Lokální definice

map'' :: (a -> b) -> [a] -> [b]
map'' f = go
  where
    go []     = []
    go (x:xs) = f x:go xs

-- Alternativně můžeme použít let-in, narozdíl od where je to výraz.

cubeArea :: Double -> Double
cubeArea x =
    let squareArea = x * x
    in  6 * squareArea

-- Stejně tak pro pattern matching existuje alternativa, která se dá použít
-- jako výraz.

check :: (Int -> [Int]) -> String
check g = "g 0 gives " ++ case g 0 of
    []    -> "empty list"
    (_:_) -> "non-empty list"

-- Tohle by bylo přehlednější s použitím lokální definice.

check' :: (Int -> [Int]) -> String
check' g = "g 0 gives " ++ msg (g 0)
  where
    msg []    = "empty list"
    msg (_:_) = "non-empty list"
