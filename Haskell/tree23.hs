module Tree23 where

-- | A key-value map represented as a 2-3 tree.
data Map k v
    = Nil                                          -- ^ Leaf
    | Node2 (Map k v) k v (Map k v)                -- ^ 2-node
    | Node3 (Map k v) k v (Map k v) k v (Map k v)  -- ^ 3-node
    deriving (Show)

{- | The empty map.

> empty == fromList []
-}
empty :: Map k v
empty = Nil

{- | Check whether a map is empty.

> null empty == True
-}
null :: Map k v -> Bool
null Nil = True
null _   = False

{- | Create a map containing exactly one key and its value.

> singleton 1 'a' == fromList [(1, 'a')]
-}
singleton :: k -> v -> Map k v
singleton k v = Node2 Nil k v Nil

{- | Find the value associated with a given key, if it exists.

> find 1 (fromList [(1, 'a')]) == Just 'a'
> find 0 (fromList [(1, 'a')]) == Nothing
-}
find :: (Ord k) => k -> Map k v -> Maybe v
find _ Nil = Nothing
find k (Node2 a bk bv c)
    | k < bk    = find k a
    | k == bk   = Just bv
    | otherwise = find k c
find k (Node3 a bk bv c dk dv e)
    | k < bk    = find k a
    | k == bk   = Just bv
    | k < dk    = find k c
    | k == dk   = Just dv
    | otherwise = find k e

{- | Calculate the depth of all leaf nodes. Internal function.

>>> depths (fromList (zip [1..] "hello"))
[2,2,2,2,2,2]
-}
depths :: Map k v -> [Int]
depths = go 0
  where
    go n Nil                   = [n]
    go n (Node2 a _ _ c)       = concatMap (go (n + 1)) [a, c]
    go n (Node3 a _ _ c _ _ e) = concatMap (go (n + 1)) [a, c, e]

{- | Build a map from a list of key-value pairs.

>>> fromList [(1, 'a')]
Node2 Nil 1 'a' Nil
-}
fromList :: (Ord k) => [(k, v)] -> Map k v
fromList = foldr (uncurry insert) empty

{- | Build a key-value pair list representing the given map.

>>> toList (fromList [(1, 'a')])
[(1,'a')]
-}
toList :: Map k v -> [(k, v)]
toList = flip go []
  where
    go Nil = id
    go (Node2 a bk bv c) =
        go a . ((bk, bv):) . go c
    go (Node3 a bk bv c dk dv e) =
        go a . ((bk, bv):) . go c . ((dk, dv):) . go e

-- | Internal data type which represents the result of an insertion.
data Inserted k v
    = Done (Map k v)                 -- ^ The insertion resulted in no change or in a node expansion
    | Split (Map k v) k v (Map k v)  -- ^ The insertion caused a node to split

{- | Insert a new key and an associated value into a map. If the key was already
present, the value is replaced.

> insert 1 'b' (fromList [(1, 'a')]) == fromList [(1, 'b')]
> insert 2 'b' (fromList [(1, 'a')]) == fromList [(1, 'a'), (2, 'b')]
-}
insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v t = case go t of
    Done t'         -> t'
    Split a bk bv c -> Node2 a bk bv c
  where
    go Nil = Split Nil k v Nil

    go (Node2 a bk bv c)
        | k < bk    = case go a of
            Done a'             -> Done (Node2 a' bk bv c)
            Split aa abk abv ac -> Done (Node3 aa abk abv ac bk bv c)
        | k == bk   = Done (Node2 a k v c)
        | otherwise = case go c of
            Done c'             -> Done (Node2 a bk bv c')
            Split ca cbk cbv cc -> Done (Node3 a bk bv ca cbk cbv cc)

    go (Node3 a bk bv c dk dv e)
        | k < bk     = case go a of
            Done a'             -> Done (Node3 a' bk bv c dk dv e)
            Split aa abk abv ac -> Split (Node2 aa abk abv ac) bk bv (Node2 c dk dv e)
        | k == bk    = Done (Node3 a k v c dk dv e)
        | k < dk     = case go c of
            Done c'             -> Done (Node3 a bk bv c' dk dv e)
            Split ca cbk cbv cc -> Split (Node2 a bk bv ca) cbk cbv (Node2 cc dk dv e)
        | k == dk    = Done (Node3 a bk bv c k v e)
        | otherwise  = case go e of
            Done e'             -> Done (Node3 a bk bv c dk dv e')
            Split ea ebk ebv ec -> Split (Node2 a bk bv c) dk dv (Node2 ea ebk ebv ec)
