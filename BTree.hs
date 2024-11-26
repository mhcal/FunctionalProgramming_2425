-- type for binary tree:
data BTree a
    = Empty
    | Node a (BTree a) (BTree a)

instance Foldable BTree -- foldable instance for binary trees
-- the minimal instance definitions for the Foldable class are foldMap and foldr
                                                                                 where
    foldMap g (Node n l r) = g n <> foldMap g l <> foldMap g r
    foldr f z Empty = z
    foldr f z (Node n l r) = foldr f (f n (foldr f z r)) l
