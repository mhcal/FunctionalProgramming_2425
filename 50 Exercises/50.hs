import Data.Char

-- write the following functions using recursion

-- 1) enumFromTo (pre-defined) (given two integers x and y, return a list with all integers between x and y included (e.g. enumFromTo 1 5 = [1, 2, 3, 4, 5]))
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y
  | x == y    = [y]
  | otherwise = (x:(enumFromTo1 (x + 1) y))

-- 2) enumFromThenTo (pre-defined) (given three integers x, c, and y, return the arithmetic progression from x to y with c steps (e.g enumFromThenTo 1 3 10 = [1, 3, 5, 7, 9]))
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x c y
  | c > y     = [x]
  | otherwise = (x:(enumFromThenTo1 c ((2 * c) - x) y))
                      
-- 3) (++) (pre-defined) (given two lists a and b, return a list equal to a set a U b)
plusplus :: [a] -> [a] -> [a]
plusplus a []     = a
plusplus [] (h:t) = plusplus [h] t
plusplus (h:t) b  = (h:(plusplus t b))

-- 4) (!!) (pre-defined) (given a list l and an integer n, return the n-th element of l (0-indexed))
exclexcl :: [a] -> Int -> a
exclexcl (h:t) x
  | x == 0    = h
  | otherwise = exclexcl t (x - 1)

-- 5) reverse (pre-defined) (given a list, return a new list with the old elements reversed)
reverse1 :: [a] -> [a]
reverse1 []  = []
reverse1 [a] = [a]
reverse1 l   = ((last l):(reverse1 (init l)))

-- 6) take (pre-defined) (given an integer n and a list l, return a list with, atmost, the first n elements of l)
take1 :: Int -> [a] -> [a]
take1 0 l     = []
take1 _ []    = []
take1 x (h:t) = (h:(take1 (x - 1) t))

-- 7) drop (pre-defined) (given an integer n and a list l, return l without its first n elements)
drop1 :: Int -> [a] -> [a]
drop1 0 l     = l
drop1 _ []    = []
drop1 x (h:t) = drop1 (x - 1) t

-- 8) zip (pre-defined) (given two lists l1 and l2, return a list of pairs containing the n-th elements of l1 and l2)
zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] _            = []
zip1 _ []            = []
zip1 (h1:t1) (h2:t2) = ((h1, h2):(zip1 t1 t2))

-- 9) replicate (pre-defined) (given an integer n and an element x, return a list with n elements in which all elements = x)
replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 1 x = [x]
replicate1 n x = (x:(replicate1 (n - 1) x))

  -- 10) intersperse (pre-defined) (given an element x of type a and a list l of type [a], return a list where element x is interleaved between all the elements of list l)
intersperse1 :: a -> [a] -> [a]
intersperse1 x l =
  case l of
    []     -> l
    (h:[]) -> l
    (h:t)  -> (h:x:(intersperse1 x t))

-- 11) group (pre-defined) (given a list l, return a list where every consecutive and identical element of l is grouped together in a nested list)
group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 (x:xs) = group1' [x] xs
  where
    group1' current [] = [current]
    group1' current (y:ys)
      | y == head current = group1' (y:current) ys
      | otherwise         = (current:(group1' [y] ys))

-- 12) concat (pre-defined) (given a list of lists, return a single list with all contained elements)
concat1 :: [[a]] -> [a]
concat1 l =
  case l of
    []    -> []
    (h:t) -> plusplus h (concat1 t)

-- 13) inits (pre-defined) (returns a list with all prefixes of a given list)
inits1 :: [a] -> [[a]]
inits1 l =
  case l of
    [] -> []
    l  -> ((il):(inits1 il))
  where il = init l

-- 14) tails (pre-defined) (retuns a list with all suffixes of a given list)
tails1 :: [a] -> [[a]]
tails1 l =
  case l of
    [] -> []
    l  -> ((tl):(tails1 tl))
  where tl = tail l

-- 15) heads (given a list of lists, returns the first element of each of the nested lists)
heads :: [[a]] -> [a]
heads l =
  case l of
    []    -> []
    [[]]  -> []
    (h:t) -> ((head h):(heads t))

-- 16) total (given a list of lists, returns the total amount of elements considering all lists)
total :: [[a]] -> Int
total l =
  case l of
    []    -> 0
    [[]]  -> 0
    (h:t) -> length h + (total t)

-- 17) fun (given a list of triples, return a list of pairs consisting of the first and third elements of each triple)
fun :: [(a, b, c)] -> [(a, c)]
fun l =
  case l of
    []            -> []
    ((a, b, c):t) -> ((a, c):(fun t))

-- 18) glue (given a list of triples, in which the first element of each triple is a string, return a string with each first element concatenated)
glue :: [(String, b, c)] -> String
glue l =
  case l of
    []            -> []
    ((s, _, _):t) -> plusplus s (glue t)

-- 19) age (given an year y, an age a, and a list of (String, Int) s.t. the first element is a name and the second element is a birth year, return return the name of people that have an age >= a at year y)
age :: Int -> Int -> [(String, Int)] -> [String]
age y a [] = []
age y a ((n, by):t)
  | (y - by) >= a = (n:(age y a t))
  | otherwise     = (age y a t)

-- 20) powerEnumFrom (given values n, m, return [n^0, ..., n^(m-1)])
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = [n^i | i <- [0..(m - 1)]]

-- 21) isPrime (given an integer n >= 2, return if n is prime)
isPrime :: Int -> Bool
isPrime n
  | n > 1     = null [x | x <- [2..(sqrtn)], mod n x == 0]
  | otherwise = False
  where sqrtn = floor (sqrt (fromIntegral n))

-- 22) isPrefixOf (pre-defined) (tests if a list is the prefix of another)
isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 [] _ = True
isPrefixOf1 l [] = False
isPrefixOf1 (h1:t1) (h2:t2)
  | h1 == h2     = isPrefixOf1 t1 t2
  | otherwise    = False

-- 23) isSuffixOf (pre-defined) (tests if a list is the suffix of another)
isSuffixOf1 :: Eq a => [a] -> [a] -> Bool
isSuffixOf1 l1 l2 = isPrefixOf1 (reverse l1) (reverse l2)

-- 24) isSubsequenceOf (pre-defined) (tests if the elements of one list appear in another in the same relative order)
isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 [] [] = True
isSubsequenceOf1 l []  = False
isSubsequenceOf1 (h1:t1) (h2:t2)
  | h1 == h2  = True && isSubsequenceOf1 t1 t2
  | otherwise = isSubsequenceOf1 (h1:t1) t2

-- 25) elemIndices (pre-defined) (returns the positions in which a given element shows up on a list)
elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 e l = elemIndices1' e l 0 
  where elemIndices1' :: Eq a => a -> [a] -> Int -> [Int]
        elemIndices1' e [] pos = []
        elemIndices1' e (h:t) pos
          | e == h    = [pos] ++ elemIndices1' e t (pos + 1)
          | otherwise = elemIndices1' e t (pos + 1)

-- 26) nub (pre-defined) (returns a given list without repeated elements)

nub1 :: Eq a => [a] -> [a]
nub1 l = nub1' l []
  where nub1' :: Eq a => [a] -> [a] -> [a]
        nub1' [] acc = reverse acc
        nub1' (x:xs) acc
          | elem x acc = nub1' xs acc
          | otherwise  = nub1' xs (x:acc)

-- 27) delete (pre-defined) (returns a list without the first instance of a given element)
delete1 :: Eq a => a -> [a] -> [a]
delete1 _ [] = []
delete1 x (h:t)
    | x == h    = t
    | otherwise = (h:(delete1 x t))

-- 28) (\\) (pre-defined) (given two lists l1, l2, returns l1 without the first instances of each of the elements of l2)
slashslash :: Eq a => [a] -> [a] -> [a]
slashslash l []    = l
slashslash l (h:t) = slashslash (delete1 h l) t

-- 29) union (pre-defined) (given two lists, returns a list that acts like the set union between them)
union1 :: Eq a => [a] -> [a] -> [a]
union1 l1 l2 = plusplus l1 (slashslash l2 l1)
  
-- 30) intersect (pre-defined) (given two lists, returns a list that acts like the set intersection between them)
intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 [] l = []
intersect1 (h:t) l
  | h `elem` l = (h:(intersect1 t l))
  | otherwise  = intersect1 t l

-- 31) insert (pre-defined) (given an element x and an ordered list l, returns l ordered with x added)
insert1 :: Ord a => a -> [a] -> [a]
insert1 x [] = [x]
insert1 x (h:t)
  | h >= x    = (x:h:t)
  | otherwise = (h:(insert1 x t))

-- 32) unwords (pre-defined) (given a list of strings, returns all elements separated by a space character in a single string)
unwords1 :: [String] -> String
unwords1 []  = []
unwords1 (h:t)
  | null t    = h
  | otherwise = h ++ " " ++ unwords1 t

-- 33) unlines (pre-defined) (given a list of strings, returns all elements separated by a newline character in a single string)
unlines1 :: [String] -> String
unlines1 [] = []
unlines1 (h:t)
  | null t    = h
  | otherwise = (h ++ ('\n':unwords1 t))

-- 34) pGreater (given a non-empty list, returns the position of the greatest element of the list)
pGreater :: Ord a => [a] -> Int
pGreater l = pGreater' l ((head l), 0, 0)
  where pGreater' :: Ord a => [a] -> (a, Int, Int) -> Int
        pGreater' [] (g, gp, cp) = gp
        pGreater' (h:t) (g, gp, cp)
          | h > g     = pGreater' t (h, cp, cp + 1)
          | otherwise = pGreater' t (g, gp, cp + 1)

-- 35) lookup (pre-defined) (given a list of pairs (a, b), returns the corresponding b of a given a)
lookup1 :: Eq a => a -> [(a, b)] -> Maybe b
lookup1 _ [] = Nothing
lookup1 a (h:t)
  | (fst h) == a = Just (snd h)
  | otherwise    = lookup1 a t

-- 36) preAscending (returns the largest ascending prefix of a given list)
preAscending :: Ord a => [a] -> [a]
preAscending []  = []
preAscending [x] = [x]
preAscending (h1:h2:t)
  | h1 <= h2  = (h1:preAscending (h2:t))
  | otherwise = [h1]

-- 37) iSort (sorts a given list using an insert function)
iSort :: Ord a => [a] -> [a]
iSort []    = []
iSort (h:t) = insert1 h (iSort t)
          
-- 38) comesFirst (given two strings, returns True iff the first string comes before the second string in lexicographic order)
comesFirst :: String -> String -> Bool
comesFirst [] _ = True
comesFirst _ [] = False
comesFirst (h1:t1) (h2:t2)
  | l1 < l2   = True
  | l1 == l2  = comesFirst t1 t2
  | otherwise = False 
  where l1 = ord (toLower h1)
        l2 = ord (toLower h2)

-- let the type [(a, Int)] represent multisets of elements of type a s.t., in this list, there are no repeated instances of a first element and no second element is less than or equal to 0

-- 40) convertMSet (given a multiset, return a list of its elements (e.g. convertMSet [('b', 2), ('a', 4), ('c', 1)] = "bbaaaac"))
convertMSet :: [(a, Int)] -> [a]
convertMSet [] = []
convertMSet ((x, n):t)
  | n <= 0    = convertMSet t
  | otherwise = (x:(convertMSet ((x, n - 1):t)))

-- 41) insertMSet (adds an element to a given multiset (e.g. insertMSet 'c' [('b', 2), ('a', 4), ('c', 1)] = [('b', 2), ('a', 4), ('c', 2)]))
insertMSet :: Eq a => a -> [(a, Int)] -> [(a, Int)]
insertMSet e [] = [(e, 1)]
insertMSet e ((x, n):t)
  | x == e    = ((x, n + 1):t)
  | otherwise = ((x, n):(insertMSet e t))

-- 42) removeMSet (removes an element from a given multiset (e.g. removeMSet 'c' [('b', 2), ('a', 4), ('c', 1)] = [('b', 2), ('a', 4)]))
removeMSet :: Eq a => a -> [(a, Int)] -> [(a, Int)]
removeMSet _ [] = []
removeMSet e ((x, n):t)
  | x == e && n == 1 = t
  | x == e && n /= 1 = ((x, n - 1):t)
  | otherwise        = ((x, n):(removeMSet e t))

-- 43) buildMSet (given an ordered list in ascending order, build its correspondent multiset (e.g. buildMSet "aabccc" = [('a', 3), ('b', 1), ('c', 3)]))
buildMSet :: Ord a => [a] -> [(a, Int)]
buildMSet []    = []
buildMSet (h:t) = insertMSet h (buildMSet t)

-- 44) partitionEithers (pre-defined) (divides a given list of eithers into two separate lists)
partitionEithers1 :: [Either a b] -> ([a], [b])
partitionEithers1 l  = partitionEithers1' l ([], [])
  where partitionEithers1' :: [Either a b] -> ([a], [b]) -> ([a], [b])
        partitionEithers1' [] acc = acc
        partitionEithers1' (h:t) (l, r) =
          case h of
            Left x  -> partitionEithers1' t (x:l, r)
            Right x -> partitionEithers1' t (l, x:r)

-- 45) catMaybes (pre-defined) (returns the elements of type a in a list)
catMaybes1 :: [Maybe a] -> [a]
catMaybes1 [] = []
catMaybes1 (h:t) =
  case h of
    Just x  -> (x:(catMaybes1 t))
    Nothing -> catMaybes1 t

-- let the following type represent the movements of a robot:
data Movement = North | South | East | West
              deriving Show

-- 46) path (given the initial and final positions (coordinates) of a robot, returns a list of movements the robot has to perform to go from one position to another)
path :: (Int, Int) -> (Int, Int) -> [Movement]
path (x1, y1) (x2, y2) = horizontalMoves ++ verticalMoves
  where
    horizontalMoves
      | x1 < x2   = replicate (x2 - x1) East
      | x1 > x2   = replicate (x1 - x2) West
      | otherwise = []
    verticalMoves
      | y1 < y2   = replicate (y2 - y1) North
      | y1 > y2   = replicate (y1 - y2) South
      | otherwise = []

-- 47) hasLoops (given an initial position and a list of movements, returns if the robot goes through the initial position again throughout its path)
hasLoops :: (Int, Int) -> [Movement] -> Bool
hasLoops ip m = checkLoop ip m
  where
    checkLoop cp [] = False
    checkLoop cp (h:t) =
      let np = move cp h
      in if np == ip
      then True
      else checkLoop np t
    move (x, y) North = (x, y + 1)
    move (x, y) South = (x, y - 1)
    move (x, y) East  = (x + 1, y)
    move (x, y) West  = (x - 1, y)

-- let the following type represent points and rectangles s.t. rectangle have sides parallel to the axes and are represent by their two diagonal vertices
type Point     = (Float, Float)
data Rectangle = Rect Point Point

-- 48) countSquares (given a list of rectangles, returns how many of them are squares)
countSquares :: [Rectangle] -> Int
countSquares [] = 0
countSquares ((Rect (x1, y1) (x2, y2)):t)
  | (x1 - x2) == (y1 - y2) = (countSquares t) + 1
  | otherwise              = countSquares t

-- 49) totalArea (given a list of rectangles, returns the total sum of their areas)
totalArea :: [Rectangle] -> Float
totalArea [] = 0
totalArea ((Rect (x1, y1) (x2, y2)):t) =
  (b * h) + totalArea t
  where b = abs (x1 - x2)
        h = abs (y1 - y2)

-- let the following data type represent the state of an item
data Item = Good | Moderate | Broken

-- 50) notRepair (returns the amount of items that don't need repair in a given list)
notRepair :: [Item] -> Int
notRepair [] = 0
notRepair (h:t) =
  case h of
    Broken -> (notRepair t) + 1
    _      -> notRepair t
