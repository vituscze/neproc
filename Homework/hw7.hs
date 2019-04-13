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
-- Orientovaný graf lze reprezentovat jako mapa mezi vrcholem a seznamem hran, které
-- v tomto vrcholu začínají. Hranu pak stačí popsat pouze druhým vrcholem a váhou.

type Vertex = Int
type Weight = Double

newtype Graph = Graph (Map Vertex [(Vertex, Weight)])
    deriving (Show)

-- Např.:
--
--      1
--   0 --> 1
--   ^     | 1
-- 1 |     v
--   `---- 2
--
-- odpovídá:

example1 :: Graph
example1 = Graph $ Map.fromList [(0, [(1,1)]), (1, [(2,1)]), (2, [(0,1)])]

-- nebo
--               4
--         ,-----------.
--      1  |  3     5  v
--   0 --> 1 --> 2 --> 3
--   |           ^
--   `-----------´
--         2

example2 :: Graph
example2 = Graph $ Map.fromList [(0, [(1,1), (2,2)]), (1, [(2,3), (3,4)]), (2, [(3,5)]), (3, [])]

-- 1) Definujte funkci:

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
    Map.foldrWithKey (\v es r -> all (go v) (map fst es) && r) True g
  where
    check [x] = x
    check _   = False

    go vi vj = check $ do
        i <- elemIndices vi top
        j <- elemIndices vj top
        return (i < j)

-- >>> checkTopSort example2 [0,1,2,3]
-- True
--
-- >>> checkTopSort example2 [1,2,0,3]
-- False
--
-- 2) Implementujte Floyd-Warshallův algoritmus.

floydWarshall :: Graph -> Map (Vertex, Vertex) Weight
floydWarshall = undefined

-- Výsledkem je mapa, která pro každou dvojici vrcholů obsahuje
-- délku nejkratší cesty mezi nimi.
--
-- Předpokládejte, že vstupní graf neobsahuje negativní cykly
-- (tj. není je třeba explicitně ošetřovat).
--
-- Hint: Double obsahuje kladné nekonečno, což se vám může hodit
-- při implementaci. V Haskellu tuto hodnotu dostanete nejjednodušeji
-- jako 1 / 0.

-- >>> floydWarshall example1
-- fromList [((0,0),0.0),((0,1),1.0),((0,2),2.0),((1,0),2.0),((1,1),0.0),((1,2),1.0),((2,0),1.0),((2,1),2.0),((2,2),0.0)]
--
