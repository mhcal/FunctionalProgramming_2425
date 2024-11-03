-- 1) let the following type represent binary trees
data BTree a = Empty
             | Node a (BTree a) (BTree a)
  deriving Show

-- define the following functions
-- a) height (returns the height of a tree)
height :: BTree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- b) countNodes (returns the total amount of nodes in a tree)
countNodes :: BTree a -> Int
countNodes Empty = 0
countNodes (Node _ l r) = 1 + countNodes l + countNodes r

-- c) leafs (returns the amount of leafs -- i.e. nodes that don't contain any subtrees)
leafs :: BTree a -> Int
leafs Empty = 1
leafs (Node _ Empty Empty) = 1
leafs (Node _ l r) = (leafs l) + (leafs r)

-- d) prune (removes all elements below a given depth from a tree)
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune h (Node n l r) = (Node n (prune (h - 1) l) (prune (h - 1) r))

-- e) path (given a path (True -> left; False -> right) and a binary tree, return a list of the nodes traversed)
path :: [Bool] -> BTree a -> [a]
path [] (Node n _ _) = [n]
path (x:xs) (Node n l r) =
  case x of
    True -> (n:(path xs l))
    False -> (n:(path xs r))
path _ _ = []

-- f) mirror (returns a symmetric tree)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node n l r) = (Node n (mirror r) (mirror l))

-- g) zipWithBT (zipWith for binary trees)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node n1 l1 r1) (Node n2 l2 r2) = (Node (f n1 n2) (zipWithBT f l1 l2) (zipWithBT f r1 r2))
zipWithBT _ _ _ = Empty

-- h) unzipBT (unzip implementation for trees of triples)
unzipBT :: BTree (a, b, c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (fn, sn, tn) l r) =
  let (fl, sl, tl) = unzipBT l
      (fr, sr, tr) = unzipBT r
  in (Node fn fl fr, Node sn sl sr, Node tn tl tr)


-- 2) define the following functions for binary search trees
-- a) minTree (returns the smallest value in a non-empty binary search tree)
minTree :: Ord a => BTree a -> a
minTree (Node n l r) =
  case l of
    Empty -> n
    _ -> minTree l

-- b) noMinTree (removes the smallest value from a non-empty binary search tree)
noMinTree :: Ord a => BTree a -> BTree a
noMinTree (Node n Empty Empty) = Empty
noMinTree (Node n l r) = (Node n (noMinTree l) r)

-- c) minNmin (returns, in a pair, the result of the two previous functions in only one traversal)
minNmin :: Ord a => BTree a -> (a, BTree a)
minNmin (Node n Empty Empty) = (n, Empty)
minNmin (Node n Empty r) = (n, r)
minNmin (Node n l r) =
  let (m, nt) = minNmin l
  in (m, Node n nt r)

-- d) remove (using the previous definition, write a function that removes a given element from a binary search tree)
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node n l r)
  | x < n = Node n (remove x l) r
  | x > n = Node n l (remove x r)
  | x == n = aux l r
  where
    aux :: Ord a => BTree a -> BTree a -> BTree a
    aux Empty r = r
    aux l Empty = l
    aux l r =
      let (m, nr) = minNmin r
      in (Node m l nr)


-- 3) consider the following data structure to store a classroom's information
type Student = (Number, Name, Regime, Classification)
type Number = Int
type Name = String
data Regime = ORD | SW | MEL
data Classification = Passed Int
                    | Failed
                    | Absent
  deriving Show
type Class = BTree Student -- binary search tree (ordered by number)

-- define the following functions
-- a) registeredNum (check if a student, with a given number, is registered in the class)
registeredNum :: Number -> Class -> Bool
registeredNum num Empty = False
registeredNum num (Node (snum, _, _, _) l r)
  | num == snum = True
  | num < snum = registeredNum num l
  | num > snum = registeredNum num r

-- b) registeredName (given a student's name, check if he/she is enrolled in the class)
registeredName :: Name -> Class -> Bool
registeredName name Empty = False
registeredName name (Node (_, sname, _, _) l r)
  | name == sname = True
  | otherwise = registeredName name l || registeredName name r

-- c) listSw (returns the number and name of all the student workers enrolled in the class in a list of pairs)
listSw :: Class -> [(Number, Name)]
listSw Empty = []
listSw (Node (snum, sname, sreg, _) l r) =
  case sreg of
    SW -> (listSw l) ++ [(snum, sname)] ++ (listSw r)
    _ -> (listSw l) ++ (listSw r)

-- d) grade (given a student's number, return his/her grade)
grade :: Number -> Class -> Maybe Classification
grade _ Empty = Nothing
grade num (Node (snum, _, _, sclas) l r)
  | num == snum = Just sclas
  | num < snum = grade num l
  | num > snum = grade num r

-- e) percAbs (returns the percentage of students that were absent)
percAbs :: Class -> Float
percAbs Empty = 0
percAbs c =
  let as :: Class -> Int
      as Empty = 0
      as (Node (_, _, _, sclas) l r) =
        case sclas of
          Absent -> 1 + (as l) + (as r)
          _ -> (as l) + (as r)
  in (fromIntegral $ as c)/(fromIntegral $ countNodes c) * 100

-- f) avgGrade (returns the average of all passing grades)
avgGrade :: Class -> Float
avgGrade Empty = 0
avgGrade c =
  let gradeSum :: Class -> Int
      gradeSum Empty = 0
      gradeSum (Node (_, _, _, sclas) l r) = 
        case sclas of
          Passed g -> g + (gradeSum l) + (gradeSum r)
          _ -> (gradeSum l) + (gradeSum r)
  in (fromIntegral $ gradeSum c)/(fromIntegral $ countNodes c)

-- g) avgPass (returns the ratio of students with passing grades with only one tree traversal)
avgPass :: Class -> Float
avgPass Empty = 0
avgPass c = 
  let (ps, ts) = traverse c
  in (fromIntegral ps)/(fromIntegral ts) * 100
  where
    traverse :: Class -> (Int, Int)
    traverse Empty = (0, 0)
    traverse (Node (_, _, _, Passed _) l r) =
      let (lp, lt) = traverse l
          (rp, rt) = traverse r
      in (1 + lp + rp, 1 + lt + rt)
    traverse (Node _ l r) =
      let (lp, lt) = traverse l
          (rp, rt) = traverse r
      in (lp + rp, 1 + lt + rt)
