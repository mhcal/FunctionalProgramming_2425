-- import Ficha1
-- datatypes and functions defined in the first worksheet
data Hour = H Int Int deriving (Show, Eq)

data PlanePoint = Cartesian Double Double | Polar Double Double
                deriving (Show,Eq)

data Figure = Circle PlanePoint Double
            | Rectangle PlanePoint PlanePoint
            | Triangle PlanePoint PlanePoint PlanePoint
            deriving (Show, Eq)

dist2points :: PlanePoint -> PlanePoint -> Double
dist2points p1 p2 = sqrt ((posx p2 - posx p1)^2 + (posy p2 - posy p1)^2)

posx :: PlanePoint -> Double
posx (Cartesian x _) = x
posx (Polar r t) = r * (cos t)

posy :: PlanePoint -> Double
posy (Cartesian _ y) = y
posy (Polar r t) = r * sin t


-- 1) assuming that time is represent by a pair of integers, a trip can be represented as a sequence of steps, in which individual step is represented by a pair of times (start, arrival)
type Step = (Hour, Hour)
type Trip = [Step]

-- 1.a) test if a step is well defined (arrival time is after departure time)
checkStep :: Step -> Bool
checkStep ((H h1 m1), (H h2 m2)) | h2 > h1                  = True
                                 | (h2 == h1) && (m2 >= m1) = True
                                 | otherwise                = False

-- 1.b) test if a trip is well defined (each step is well defined, and the next step always starts after the last step was over)
checkTrip :: Trip -> Bool
checkTrip []     = True
checkTrip (h:[]) = checkStep h
checkTrip (h:t)  = ((checkStep h && checkTrip t) && (checkStep ((snd h), (fst (head t)))))

-- 1.c) compute the departure and arrival time of a trip
depArr :: Trip -> Step
depArr a = (fst (head a), snd (last a))

-- 1.d) compute the total effective length of a well defined trip in minutes
hourMins :: Hour -> Int
hourMins (H h m) = (60 * h) + m

stepMins :: Step -> Int
stepMins (a, b) = abs ((hourMins a) - (hourMins b))

tripMins :: Trip -> Int
tripMins []    = 0
tripMins (h:t) = stepMins h + (tripMins t)
               
-- 1.e) compute the total waiting time (time between each step)
waitingMins :: Trip -> Int
waitingMins a = (stepMins ((fst (head a)), (snd (last a)))) - (tripMins a)

-- 1.f) compute the overall length of the trip (effective trip time + waiting time)
totalMins :: Trip -> Int
totalMins a = tripMins a + waitingMins a


-- 2) consider the following type definition to represent polygonal lines (can use the functions defined in 1.hs)
type Polygonal = [PlanePoint] -- (PlanePoint is defined in 1.hs)

-- 2.a) define a function that returns the length of a polygonal line
polygonalLength :: Polygonal -> Double
polygonalLength []  = 0
polygonalLength [_] = 0
polygonalLength (h:t) = (dist2points h (head t)) + (polygonalLength ((head t):(tail t)))

-- 2.b) define a function that tests if a given polygonal line is closed
isClosedPolygonal :: Polygonal -> Bool
isClosedPolygonal []  = False
isClosedPolygonal [x] = False
isClosedPolygonal l | (head l) == (last l) = True
                    | otherwise            = False

-- 2.c) define a function that given a closed and convex polygonal line, computes a list of triangles which the sum of the areas is equal to the area delimited by a polygonal line
triangulate :: Polygonal -> [Figure]
triangulate ps = createTriangles (head ps) (init (tail ps))

createTriangles _ []          = []
createTriangles _ [_]         = []
createTriangles p1 (p2:p3:ps) = (Triangle p1 p2 p3):(createTriangles p1 (p3:ps))

-- 2.d) define a function that computes the area delimited by a convex and closed polygonal line
getCartesian :: PlanePoint -> (Double, Double)
getCartesian (Polar r t)     = (r * cos t, r * sin t)
getCartesian (Cartesian x y) = (x, y)

polygonalArea :: [PlanePoint] -> Double
polygonalArea l = abs (sum [x1 * y2 - x2 * y1 | (p1, p2) <- zip l (tail (cycle l)), 
                            let (x1, y1) = getCartesian p1,
                            let (x2, y2) = getCartesian p2]) / 2


-- 3) let the following data type be used to storage names and contact information. there are no repeated names in in the (phone)book, and for earch name, there is a contact list
data Contact = Home Integer
             | Work Integer
             | Phone Integer
             | Email String
             deriving Show

type Name = String
type Phonebook = [(Name, [Contact])]

-- 3.a) define a function that given a name, email address, and phonebook, returns a new phonebook with the email added to the pair of the given name
addEmail :: Name -> String -> Phonebook -> Phonebook
addEmail n e [] = [(n, [Email e])]
addEmail n e (p:ps) | (n == (fst p)) = (((fst p), ((Email e):(snd p))):ps)
                    | otherwise      = (p:(addEmail n e ps))

-- 3.b) define a function that returns all email addresses associated with a given name in a phonebook
getEmails :: [Contact] -> [String]
getEmails x = case x of
                []             -> []
                ((Email x):cs) -> [x] ++ getEmails cs
                (_:cs)         -> getEmails cs

returnEmails :: Name -> Phonebook -> Maybe [String]
returnEmails n [] = Nothing
returnEmails n p | (fst (head p)) == n = Just (getEmails (snd (head p)))
                 | otherwise           = (returnEmails n (tail p))

-- 3.c) define a function that returns all phone numbers (Home, Work, Phone) in a given contact list
-- (changed data type from the one used on the exercise sheet in order to distinguish different numbers by their constructor)
getNums :: [Contact] -> [Contact]
getNums x = case x of
                   []             -> []
                   ((Email _):cs) -> getNums cs
                   (c:cs)         -> [c] ++ getNums cs

-- 3.d) define a function that returns the home number in a phonebook associated with a given name
-- (changed data type from the one used on the exercise sheet in order to support multiple numbers associated with the same name)
getHome :: [Contact] -> [Integer]
getHome x = case x of
              []            -> []
              ((Home c):cs) -> [c] ++ getHome cs
              (_:cs)        -> getHome cs

returnHome :: Name -> Phonebook -> Maybe [Integer]
returnHome n [] = Nothing
returnHome n p | (fst (head p)) == n = Just (getHome (snd (head p)))
               | otherwise           = (returnHome n (tail p))


-- 4) let the following synonyms and data types represent a person's birthday
type Day = Int
type Month = Int
type Year = Int

data Date = D Day Month Year
          deriving Show

type BdTable = [(Name, Date)]

-- 4.a) define a function that returns the date associated with a given name in a given birth day table
getBd :: Name -> BdTable -> Maybe Date
getBd n [] = Nothing
getBd n ((cn, cd):ts) | cn == n   = Just cd
                      | otherwise = getBd n ts

-- 4.b) define a function that returns the age of a person in a given date
ageDiff :: Date -> Maybe Date -> Maybe Int
ageDiff (D _ _ y1) (Just (D _ _ y2))
  | y1 >= y2  = Just (y1 - y2)
  | otherwise = Nothing
ageDiff _ Nothing = Nothing

age :: Date -> Name -> BdTable -> Maybe Int
age d n bdt = let bd = getBd n bdt
              in ageDiff d bd

-- 4.c) define a function that tests if a given date came before another
before :: Date -> Date -> Bool
before (D d1 m1 y1) (D d2 m2 y2)
  | (y1 > y2) || ((y1 == y2) && (m1 > m2)) || ((y1 == y2) && (m1 == m2) && (d1 >= d2)) = False
  | otherwise = True

-- 4.d) define a function that sorts a birthday table by ascending order of the dates
insertBdTable :: (Name, Date) -> BdTable -> BdTable
insertBdTable entry [] = [entry]
insertBdTable entry (x:xs)
  | before (snd entry) (snd x) = (entry:x:xs)
  | otherwise                  = (x:(insertBdTable entry xs))

sortBdTable :: BdTable -> BdTable
sortBdTable []     = []
sortBdTable (x:xs) = insertBdTable x (sortBdTable xs)

-- 4.e) define a function that returns the name and age of a person in a given date by ascending order of age
bdToAge :: Date -> (Name, Date) -> (Name, Int)
bdToAge (D _ _ y1) (x, D _ _ y2) = (x, y1 - y2)

byAge :: Date -> BdTable -> [(Name, Int)]
byAge d t = let ascAge = reverse (sortBdTable t)
            in map (\x -> bdToAge d x) ascAge


-- 5) let the following types describe banking information s.t. each object of type Statement indicates an initial balance and a list of banking operations. each operation is represented by a tuple containing a date, a description, and the (positive) amount traded
data Operation = Credit Float | Debit Float
               deriving Show

data Statement = Stmt Float [(Date, String, Operation)]
               deriving Show

-- 5.a) define a function that shows all operations (in credit or debit) over a given amount
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

getOpValue :: (a, b, Operation) -> Float
getOpValue x = case x of
                 (_, _, Credit x) -> x
                 (_, _, Debit x)  -> x
                 
stmtAmount :: Statement -> Float -> [Operation]
stmtAmount (Stmt _ []) _ = []
stmtAmount (Stmt x (s:ss)) a
  | getOpValue s > a = ((thd3 s):(stmtAmount (Stmt x ss) a))
  | otherwise        = (stmtAmount (Stmt x ss) a)

-- 5.b) define a function that only returns information relevant for an operation with the description on a given string list
stmtFilter :: Statement -> [String] -> [(Date, Operation)]
stmtFilter (Stmt x []) keyword = []
stmtFilter (Stmt x ((dt, st, op):ss)) keyword
  | st `elem` keyword = ((dt, op):(stmtFilter (Stmt x ss) keyword))
  | otherwise         = (stmtFilter (Stmt x ss) keyword)

-- 5.c) define a function that returns the total of Credit and Debit in a given statement in the form of a pair
creDeb :: Statement -> (Float, Float)
creDeb (Stmt _ []) = (0, 0)
creDeb (Stmt x ((_, _, op):ss)) =
  let (tc, td) = creDeb (Stmt x ss)
  in case op of
       Credit x -> (tc + x, td)
       Debit x  -> (tc, td + x)

-- 5.d) define a function that returns the final balance resulting from all the operations applied on the initial balance
balance :: Statement -> Float
balance (Stmt x []) = x
balance (Stmt x ((_, _, op):ss)) = case op of
                                     Credit c -> (balance (Stmt x ss)) - c
                                     Debit d  -> (balance (Stmt x ss)) + d
