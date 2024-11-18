import Data.Char
import Data.List

-- 1) let the following type represent fractions
data Frac = F Integer Integer

-- 1.a) define a normalizing function (given a fraction, returns an equivalent, irreducible, positive fraction)
gcd' :: Integer -> Integer -> Integer
gcd' m 0 = m
gcd' 0 n = n
gcd' m n =
  let r = mod m n
  in gcd n r

normalize :: Frac -> Frac
normalize (F n d) =
  case (d >= 0) of
    True -> (F (div n x) (div d x))
    False -> (F (div (-n) x) (div (-d) x))
  where
    x = (gcd' n d)

-- 1.b) define Frac as an instance of the Eq type class
instance Eq Frac where
  (==) (F n1 d1) (F n2 d2) = (n1 * d2) == (d1 * n2)

-- 1.c) define Frac as an instance of the Ord class
instance Ord Frac where
  compare a b =
    let (F n1 d1) = normalize a
        (F n2 d2) = normalize b
    in compare (n1 * d2) (n2 * d1) -- we take advantage of the fact that Integers are already an instance of Ord

-- 1.d) define Frac as an instance of the Show class (pretty printing)
instance Show Frac where
  show (F n d) = concat [show n, "/", show d]
  
-- 1.e) define Frac as an instance of the Num class
instance Num Frac where
  (+) (F n1 d1) (F n2 d2) = normalize $ F ((n1 * d2) + (n2 * d1)) (d1 * d2)
  (*) (F n1 d1) (F n2 d2) = normalize $ F (n1 * n2) (d1 * d2)
  (-) (F n1 d1) (F n2 d2) = normalize $ F ((n1 * d2) - (n2 * d1)) (d1 * d2)
  negate (F n d) = normalize $ F (-n) d
  abs (F n d) = normalize $ F (abs n) (abs d)
  signum (F n d) =
    case compare (n * d) 0 of
      LT -> -1
      EQ -> 0
      GT -> 1
  fromInteger n = (F n 1)

-- 1.f) define a function that, given a fraction f and a list of fractions l, returns all the elements of l greater than 2f
filter2f :: Frac -> [Frac] -> [Frac]
filter2f f l = filter (> 2 * f) l


-- 2) recall the type defined to represent integer expressions in the last form
data IntExp = Const Int
            | Inverse IntExp
            | Add IntExp IntExp
            | Sub IntExp IntExp
            | Mult IntExp IntExp
            
-- (respective compute reduction function)
compute :: IntExp -> Int
compute e =
  case e of
    Const x -> x
    Inverse x -> (-1) * (compute x)
    Add x y -> (+) (compute x) (compute y)
    Sub x y -> (-) (compute x) (compute y)
    Mult x y -> (*) (compute x) (compute y)

-- 2.a) define IntExp as an instance of Show
instance Show IntExp where -- don't need additional constraints since regular Int is already an instance of Show
  show (Const a) = show a
  show (Inverse a) = concat ["-", show a] -- recursive
  show (Add a b) = concat ["(", show a, " + ", show b, ")"]
  show (Sub a b) = concat ["(", show a, " - ", show b, ")"]
  show (Mult a b) = concat ["(", show a, " * ", show b, ")"]

-- 2.b) define IntExp as an instance of Eq
instance Eq IntExp where
  (==) a b = (compute a) == (compute b)

-- 2.c) define IntExp as an instance of Num
instance Num IntExp  where
  (+) a b = (Add a b)
  (*) a b = (Mult a b)
  (-) a b = (Sub a b)
  negate x = (Inverse x)
  abs x =
    case (compute x) >= 0 of
      True -> x
      False -> (Inverse x)
  signum x = Const (signum $ compute x)
  fromInteger n = (Const (fromInteger n))


-- 3) recall the type definition for bank accounts in the third problem set:
data Operation = Credit Float | Debit Float
               deriving Show
data Date = D Int Int Int
data Statement = Stmt Float [(Date, String, Operation)]

-- 3.a) define Date as an instance of Ord
dateToDays :: Date -> Int
dateToDays (D d m y) = (y * 365) + (m * 30) + d

instance Eq Date where
  (==) d1 d2 = (dateToDays d1) == (dateToDays d2)

instance Ord Date where
  compare d1 d2 = compare (dateToDays d1) (dateToDays d2)

-- 3.b) define Date as an instance of Show (pretty print dates)
instance Show Date where
  show (D d m y) = concat [show d, "/", show m, "/", show y]

-- 3.c) define a statement ordering function (given a bank statement, return the list ordered by dates)
order :: Statement -> Statement
order (Stmt x l) = (Stmt x (sortBy (\(d1, _, _) (d2, _, _) -> compare d1 d2) l))

-- 3.d) define Statement as an instance of Show (pretty print Statement)
balance :: Statement -> Float
balance (Stmt x []) = x
balance (Stmt x ((_, _, op):ss)) =
  case op of
    Credit c -> (balance (Stmt x ss)) - c
    Debit d  -> (balance (Stmt x ss)) + d

instance Show Statement where
    show (Stmt pb l) =
      concat
      ["Previous balance: ", show pb, 
       "\n", replicate 39 '-', replicate (sl + crl + dbl - 39) '-',
       "\nDate", replicate (dl - 4) ' ', "  Description", replicate (sl - 11) ' ', "  Credit", replicate (crl - 6) ' ', "  Debit",
       "\n", replicate 39 '-', replicate (sl + crl + dbl - 39) '-', "\n",
        concatMap showline l,
        replicate 39 '-', replicate (dl + sl + crl + dbl - 39) '-',
       "\nCurrent balance: ", show (balance (Stmt pb l))]
      where
        dl = 10
        sl = maximum $ map (\(_, s, _) -> length s) l
        (crl, dbl) =
          let isCredit (Credit _) = True
              isCredit _ = False
              (credl, debl) = partition isCredit $ (map (\(_, _, op) -> op) l)
          in (maximum $ map (length . show . (\(Credit x) -> x)) credl, maximum $ map (length . show . (\(Debit x) -> x)) debl)
        showline (d, s, op) =
          let credordeb (Credit x) = show x
              credordeb (Debit x) = concat [replicate 8 ' ', replicate (crl - 6) ' ', show x]
          in concat [show d, replicate (12 - length (show d)) ' ', map (toUpper) s, replicate (15 - length (show s)) ' ', credordeb op, "\n"]
                          
