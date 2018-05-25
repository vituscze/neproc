import Data.List (elemIndices)
import qualified Data.Map as Map
import Data.Map (Map)

-- 7. úloha
--
-- DEADLINE: 2017-06-06
--
-- Vyberte si jednu úlohu a vyřešte ji. Pokud odevzdáte obě, druhá se počítá
-- jako bonus.
--
-- V obou úlohách budete pracovat s knihovnou containers, která obsahuje
-- definici typu Map. Dokumentace je k nalezení zde:
--
--   https://hackage.haskell.org/package/containers
--
-- Konkrétně používáme modul Data.Map (viz import nahoře).
--
--   https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html
--
-- Pro hledání funkcí můžete taky zkusit Hoogle, který umí hledat nejen podle
-- jména, ale i podle typu.
--
--   https://www.haskell.org/hoogle/
--
-- 1) Orientovaný graf lze reprezentovat jako mapa mezi vrcholy a seznamy
-- jejich sousedů.

type Vertex = Int

newtype Graph = Graph (Map Vertex [Vertex])
    deriving (Show)

-- Např.:
--
--   0 ---> 1
--   ^      |
--   |      v
--   `----- 2
--
-- odpovídá:

example1 :: Graph
example1 = Graph $ Map.fromList [(0, [1]), (1, [2]), (2, [0])]

-- nebo
--
--          ,-------------.
--          |             v
--   0 ---> 1 ---> 2 ---> 3
--   |             ^
--   `-------------´

example2 :: Graph
example2 = Graph $ Map.fromList [(0, [1, 2]), (1, [2, 3]), (2, [3]), (3, [])]

-- Definujte funkci:

topSort :: Graph -> Maybe [Vertex]
topSort = undefined

-- topSort g najde nějaké topologické uspořádání vrcholů grafu g. Pokud žádné
-- neexistuje (tj. graf obsahuje cyklus), tak topSort g == Nothing.
--
-- Seznam [v1, v2, .. vn] je topologické uspořádání, pokud pro každou
-- orientovanou hranu (vi, vj) platí i < j.
--
-- > topSort example1
-- Nothing
--
-- > topSort example2
-- Just [0,1,2,3]
--
-- Pro testování můžete použít tuto funkci:

checkTopSort :: Graph -> [Vertex] -> Bool
checkTopSort (Graph g) top =
    Map.foldrWithKey (\v es r -> all (go v) es && r) True g
  where
    check [x] = x
    check _   = False

    go vi vj = check $ do
        i <- elemIndices vi top
        j <- elemIndices vj top
        return (i < j)

-- > checkTopSort example2 [0,1,2,3]
-- True
--
-- > checkTopSort example2 [1,2,0,3]
-- False
--
-- 2) Řídké vektory a matice můžeme také reprezentovat pomocí mapy mezi indexy
-- a hodnotami.

newtype Vector a = Vector (Map Int a)
    deriving (Show, Eq)

newtype Matrix a = Matrix (Map (Int, Int) a)
    deriving (Show, Eq)

-- Takto definovaný vektor (nebo matice) obsahuje pouze nenulové prvky, tento
-- invariant by měl platit pro každou funkci, kterou budete implementovat.
--
-- Např. jednotková matice velikosti 2x2:

i2 :: Matrix Double
i2 = Matrix $ Map.fromList [((1, 1), 1.0), ((2, 2), 1.0)]

example3 :: Matrix Double
example3 = Matrix $ Map.fromList [((1, 1), 2.0), ((1, 2), 1.0), ((2, 2), 1.0)]

-- Na konkrétním indexování (od nuly nebo od jedné) nezáleží.
--
-- Definujte:

scalar :: (Num a) => Vector a -> Vector a -> a
scalar = undefined

-- scalar je jednoduše skalární součin dvou vektorů.

plus :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
plus = undefined

-- plus je součet dvou matic.
--
-- > plus i2 i2
-- Matrix (Map.fromList [((1,1),2.0),((2,2),2.0)])

minus :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
minus = undefined

-- minus je rozdíl dvou matic.
--
-- > minus i2 i2
-- Matrix (fromList [])

transpose :: Matrix a -> Matrix a
transpose = undefined

-- transpose provede transpozici matice.
--
-- > transpose example3
-- Matrix (fromList [((1,1),2.0),((2,1),1.0),((2,2),1.0)])

mult :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
mult = undefined

-- A nakonec, mult je násobení matic.
--
-- > mult example3 example3
-- Matrix (fromList [((1,1),4.0),((1,2),3.0),((2,2),1.0)])
