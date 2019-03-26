import Data.Char

-- 4. úloha
--
-- 1) Implementuje následující (tři) funkce:

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = undefined

-- on f g  aplikuje g na oba dva argumenty a pak aplikuje f.
--
-- > (max `on` abs) (-5) 4
-- 5

while :: (a -> Bool) -> (a -> a) -> a -> a
while = undefined

-- while c f  opakovaně aplikuje funkci f na svůj argument a to dokud platí
-- podmínka daná funkcí c.
--
-- > while (<100) (*2) 1
-- 128

pairwise :: (a -> a -> a) -> [a] -> [a]
pairwise = undefined

-- pairwise f  aplikuje funkci f na dva po sobě jdoucí prvky seznamu,
-- všechny výsledné hodnoty shromáždí do nového seznamu. Pokud má seznam lichou
-- délku, poslední prvek zůstává nezměněn.
--
-- > pairwise (+) [1..9]
-- [3,7,11,15,9]
--
--
-- 2) Implementujte mergesort a použijte ho pro case-insensitive třídění
-- řetězců.

mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeWith = undefined

sortWith  :: (a -> a -> Bool) -> [a] -> [a]
sortWith = undefined

-- Prvním argumentem je funkce, která provádí porovnávání.
--
-- > sortWith (<) [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- > sortWith (>) [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]
--
-- Pro case-insensitive třídění se vám může hodit funkce toLower :: Char -> Char
-- z modulu Data.Char. Abyste tuto funkci mohli použít, napište na začátek
-- souboru 'import Data.Char' (viz tohle zadání).

ciSort :: [String] -> [String]
ciSort = undefined

-- > ciSort ["Sort", "me"]
-- ["me","Sort"]

compAbs :: (Ord a, Num a) => a -> a -> Bool
compAbs x y = abs x < abs y

-- > sortWith compAbs [-10,-7..8]
-- [-1,2,-4,5,-7,8,-10]
--
--
-- BONUS)
--
-- Implementujte sortWith bez použití rekurze. Mohou se vám hodit funkce
-- probírané na cvičení.
--
-- mergeWith může být rekurzivně definovaný.
