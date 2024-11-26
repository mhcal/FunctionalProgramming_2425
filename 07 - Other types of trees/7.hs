-- 1) let the following type represent integer expressions
data IntExp
    = Const Int
    | Inverse IntExp
    | Add IntExp IntExp
    | Sub IntExp IntExp
    | Mult IntExp IntExp

-- the terms of this type can be thought of as trees in which the leafs are integers and the non-leaf nodes are operations. define the following functions:
-- 1.a) compute (given two expressions, return the result)
compute :: IntExp -> Int
compute e =
    case e of
        Const x -> x
        Inverse x -> (-1) * (compute x)
        Add x y -> (+) (compute x) (compute y)
        Sub x y -> (-) (compute x) (compute y)
        Mult x y -> (*) (compute x) (compute y)

-- 1.b) infix (returns the expression in the form of a string)
infixIntExp :: IntExp -> String
infixIntExp e =
    case e of
        Const x -> show x
        Inverse x -> concat ["-", (infixIntExp x)]
        Add x y -> concat ["(", (infixIntExp x), " + ", (infixIntExp y), ")"]
        Sub x y -> concat ["(", (infixIntExp x), " - ", (infixIntExp y), ")"]
        Mult x y -> concat ["(", (infixIntExp x), " * ", (infixIntExp y), ")"]

-- 1.c) postfix (returns a string with values followed by all operations applied)
postfixIntExp :: IntExp -> String
postfixIntExp e =
    case e of
        Const x -> show x
        Inverse x -> (postfixIntExp x) ++ " - "
        Add x y -> ((postfixIntExp x) ++ " " ++ (postfixIntExp y)) ++ " + "
        Sub x y -> ((postfixIntExp x) ++ " " ++ (postfixIntExp y)) ++ " - "
        Mult x y -> ((postfixIntExp x) ++ " " ++ (postfixIntExp y)) ++ " * "

-- 2) let the following type represent rose trees
data RTree a =
    R a [RTree a]
    deriving (Show)

-- define the following functions
-- a) sumRT (returns the sum of all the elements of the tree)
sumRT :: Num a => RTree a -> a
sumRT (R n []) = n
sumRT (R n t) = n + (sum $ map sumRT t)

-- b) heightRT (returns the height of a rose tree)
heightRT :: RTree a -> Int
heightRT (R n []) = 1
heightRT (R n t) = 1 + (maximum $ map heightRT t)

-- c) prune (removes all the elements below a certain depth from a tree)
prune :: Int -> RTree a -> RTree a
prune 1 (R n t) = (R n [])
prune h (R n t) = (R n (map (prune (h - 1)) t))

-- d) mirror (returns a symmetric tree)
mirror :: RTree a -> RTree a
mirror (R n []) = (R n [])
mirror (R n t) = (R n (reverse $ map mirror t))

-- e) postorder (returns the path equivalent from a postorder traversing)
postorder :: RTree a -> [a]
postorder (R n t) = concat (map postorder t) ++ [n]

-- 3) let the following type represent leaf trees (binary trees in which only leaves contain data)
data LTree a
    = Tip a
    | Fork (LTree a) (LTree a)
    deriving (Show)

-- define the following functions:
-- a) ltSum (returns the sum of a leaf tree)
ltSum :: Num a => LTree a -> a
ltSum lt =
    case lt of
        Tip a -> a
        Fork l r -> (ltSum l) + (ltSum r)

-- b) listLT (returns a list with all the data contained in a leaf tree from ltr)
listLT :: LTree a -> [a]
listLT lt =
    case lt of
        Tip x -> [x]
        Fork l r -> (listLT l) ++ (listLT l)

-- c) ltHeight (returns the height of a leaf tree)
ltHeight :: LTree a -> Int
ltHeight (Tip _) = 1
ltHeight (Fork l r) = 1 + max (ltHeight l) (ltHeight r)

-- 4) let the first type definition represent full trees (trees that contain data in nodes and in leaves)
data FTree a b
    = Leaf b
    | Node a (FTree a b) (FTree a b) -- you can have separate types for leaves and nodes
    deriving (Show)

data BTree a
    = Empty
    | BNode a (BTree a) (BTree a) -- same type as from the last exercise sheet; changed node constructor name to avoid confusion with ftree's node constructor
    deriving (Show)

-- define the following functions:
-- a) splitFTree (splits a full tree in a pair where the first element has the information contained in the nodes and the second pair has the information contained in the leaves)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (Node n l r) =
    ( BNode n (fst $ splitFTree l) (fst $ splitFTree r)
    , Fork (snd $ splitFTree l) (snd $ splitFTree r))

-- b) joinTrees (if possible, zip a binary tree and a leaf tree into a full tree)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees bt lt =
    case (bt, lt) of
        (Empty, Tip x) -> Just (Leaf x)
        (BNode n bl br, Fork ll lr) ->
            let Just fl = joinTrees bl ll
                Just fr = joinTrees br lr
             in Just (Node n fl fr)
        _ -> Nothing
