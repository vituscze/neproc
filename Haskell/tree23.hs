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
find k (Node2 l xk xv r)
    | k < xk    = find k l
    | k == xk   = Just xv
    | otherwise = find k r
find k (Node3 l xk xv m yk yv r)
    | k < xk    = find k l
    | k == xk   = Just xv
    | k < yk    = find k m
    | k == yk   = Just yv
    | otherwise = find k r

{- | Calculate the depth of all leaf nodes. Internal function.

>>> depths (fromList (zip [1..] "hello"))
[2,2,2,2,2,2]
-}
depths :: Map k v -> [Int]
depths = go 0
  where
    go n Nil                   = [n]
    go n (Node2 l _ _ r)       = concatMap (go (n + 1)) [l, r]
    go n (Node3 l _ _ m _ _ r) = concatMap (go (n + 1)) [l, m, r]

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
    go (Node2 l xk xv r) =
        go l . ((xk, xv):) . go r
    go (Node3 l xk xv m yk yv r) =
        go l . ((xk, xv):) . go m . ((yk, yv):) . go r

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
    Split l xk xv r -> Node2 l xk xv r
  where
    go Nil = Split Nil k v Nil

    go (Node2 l xk xv r)
        | k < xk    = case go l of
            Done l'           -> Done (Node2 l' xk xv r)
            Split ll lk lv lr -> Done (Node3 ll lk lv lr xk xv r)
        | k == xk   = Done (Node2 l k v r)
        | otherwise = case go r of
            Done r'           -> Done (Node2 l xk xv r')
            Split rl rk rv rr -> Done (Node3 l xk xv rl rk rv rr)

    go (Node3 l xk xv m yk yv r)
        | k < xk     = case go l of
            Done l'           -> Done (Node3 l' xk xv m yk yv r)
            Split ll lk lv lr -> Split (Node2 ll lk lv lr) xk xv (Node2 m yk yv r)
        | k == xk    = Done (Node3 l k v m yk yv r)
        | k < yk     = case go m of
            Done m'           -> Done (Node3 l xk xv m' yk yv r)
            Split ml mk mv mr -> Split (Node2 l xk xv ml) mk mv (Node2 mr yk yv r)
        | k == yk    = Done (Node3 l xk xv m k v r)
        | otherwise  = case go r of
            Done r'           -> Done (Node3 l xk xv m yk yv r')
            Split rl rk rv rr -> Split (Node2 l xk xv m) yk yv (Node2 rl rk rv rr)
