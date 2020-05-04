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

data Edge = Edge { vertex :: Vertex, weight :: Double }
    deriving (Show)

newtype Graph = Graph (Map Vertex [Edge])
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
example1 = Graph $ Map.fromList [(0,[Edge 1 1]),(1,[Edge 2 1]),(2,[Edge 0 1])]

-- nebo
--               4
--         ,-----------.
--      1  |  3     5  v
--   0 --> 1 --> 2 --> 3
--   |           ^
--   `-----------´
--         2

example2 :: Graph
example2 = Graph $ Map.fromList [(0,[Edge 1 1,Edge 2 2]),(1,[Edge 2 3,Edge 3 4]),(2,[Edge 3 5]),(3,[])]

-- 1) Definujte funkci:

topoSort :: Graph -> Maybe [Vertex]
topoSort = undefined

-- topoSort g najde nějaké topologické uspořádání vrcholů grafu g. Pokud žádné
-- neexistuje (tj. graf obsahuje cyklus), tak topoSort g == Nothing.
--
-- Seznam [v1, v2, .. vn] je topologické uspořádání, pokud pro každou
-- orientovanou hranu (vi, vj) platí i < j.
--
-- > topoSort example1
-- Nothing
--
-- > topoSort example2
-- Just [0,1,2,3]
--
-- Pro testování můžete použít tuto funkci:

checkTopoSort :: Graph -> [Vertex] -> Bool
checkTopoSort (Graph g) topo = Map.foldrWithKey (\v es r -> all (go v) (map vertex es) && r) True g
  where
    check [x] = x
    check _   = False

    go vi vj = check $ do
        i <- elemIndices vi topo
        j <- elemIndices vj topo
        return (i < j)

-- >>> checkTopoSort example2 [0,1,2,3]
-- True
--
-- >>> checkTopoSort example2 [1,2,0,3]
-- False
--
-- 2) Implementujte Floyd-Warshallův algoritmus.

floydWarshall :: Graph -> Map (Vertex, Vertex) Double
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
