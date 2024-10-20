-- module Ex1 where
import Data.Char

-- 1) define the following functions and their types
-- 1.a) compute the perimeter of a circumference given its radius
perimeter :: Double -> Double
perimeter r = 2 * (pi * r)

-- 1.b) compute the distance between 2 points in the cartesian plane
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)

-- 1.c) given a list, return a pair with the first and last elements of this list
firstLast :: [a] -> (a, a)
firstLast l = (head l, last l)

-- 1.d) tests if a number m is a multiple of n
multiple :: Int -> Int -> Bool
multiple m n
  | mod m n == 0 = True
  | otherwise    = False

-- 1.e) given a list, remove the first element of it if its length is odd
truncateOdd :: [a] -> [a]
truncateOdd l
  | mod (length l) 2 == 0 = l
  | otherwise             = tail l

-- 1.f) output the greatest of two given integers
max2 :: Int -> Int -> Int
max2 a b
  | a >= b    = a
  | otherwise = b

-- 1.g) output the greatest of three given integers using the function max2
max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 c (max2 a b)


-- 2) define the following functions about second degree polynomials
-- 2.a) given the three coefficients of a polynomial return the number of real roots it has
nRoots :: Double -> Double -> Double -> Int
nRoots a b c
  | delta > 0  = 2
  | delta == 0 = 1
  | otherwise  = 0
  where delta = (b^2 - 4 * a * c)

-- 2.b) using the last function, given the coefficients of a polynomial, return a list with its roots
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c
  | nRoots a b c == 2 = ((-b + delta)/2, (-b - delta)/2)
  | nRoots a b c == 1 = ((-b)/2 * a, 0)
  | otherwise         = (0, 0)
  where delta = (b^2 - 4 * a * c)
  
-- 3) let the following data type represent time (hours and minutes) using a pair of integers
type Time = (Int, Int)

-- 3.a) define a function that checks if a given time is valid
testHours :: Time -> Bool
testHours (h, m)
  | (0 <= h) && (h <= 23) && (0 <= m) && (m <= 59) = True
  | otherwise                                      = False

-- 3.b) check if a given time is after another
isAfter :: Time -> Time -> Bool
isAfter (h1, m1) (h2, m2)
  | (h1 > h2) || (h1 == h2 && m1 > m2) = True
  | otherwise                          = False

-- 3.c) convert from hh:mm to minutes
convertToMins :: Time -> Int
convertToMins (a, b) = b + (60 * a)

-- 3.d) convert from minutes to hh:mm
convertToHours :: Int -> Time
convertToHours x = (div x 60, mod x 60)

-- 3.e) calculate the difference between two times in minutes
timeDifference :: Time -> Time -> Int
timeDifference a b = abs ((convertToMins a) - (convertToMins b))

-- 3.f) add a certain amount of minutes to the time
addMinutes :: Time -> Int -> Time
addMinutes h c = (mod (div ((convertToMins h) + c) 60) 24, mod ((convertToMins h) + c) 60)


-- 4) repeat the previous exercise, assuming now that time is represented by the following type
data Hour = H Int Int deriving (Show, Eq)
-- 4.a)
testTime1 :: Hour -> Bool
testTime1 (H h m)
  | ((h >= 0) && (h <= 23) && (m >= 0) && (m <= 59)) = True
  | otherwise                                        = False

-- 4.b)
compareTime1 :: Hour -> Hour -> Bool
compareTime1 (H h1 m1) (H h2 m2)
  | (h1 > h2) || (h1 == h2 && m1 > m2) = True
  | otherwise                          = False

-- 4.c)
convertToMins1 :: Hour -> Int
convertToMins1 (H h m) = m + (60 * h)

-- 4.d)
convertToHours1 :: Int -> Hour
convertToHours1 x = H (div x 60) (mod x 60)

-- 4.e)
timeDifference1 :: Hour -> Hour -> Int
timeDifference1 a b = abs ((convertToMins1 a) - (convertToMins1 b))

--4.f)
addMinutes1 :: Hour -> Int -> Hour
addMinutes1 a x = (H (mod (div ((convertToMins1 a) + x) 60) 24) (mod ((convertToMins1 a) + x) 60)) 


-- 5) let the following type represent all possible states of a stop light
data Stoplight = Green | Yellow | Red deriving (Show,Eq)

-- 5.a) define a function that returns the next color of a stoplight given the current state
next :: Stoplight -> Stoplight
next c =
  case c of
    Green  -> Yellow
    Yellow -> Red
    Red    -> Green

-- 5.b) define a function that says if you're required to stop at a given stoplight color
stop :: Stoplight -> Bool
stop c =
  case c of
    Red -> True
    _   -> False

-- 5.c) define a function that says if the current state of two stoplights at a crossroad is safe
safe :: Stoplight -> Stoplight -> Bool
safe c1 c2 =
  case c1 of
    Red -> False
    _   -> if c2 == Red then True else False
    
-- 6) a point in a plane can be represented as a cartesian system of coordinates (distance to the horizontal/vertical axes) or by a polar system of coordinates (distance to the origin and angle between the vector and the horizontal axis); e.g. the following type can represent a coordinate either as Cartesian (-1) 0 or Polar 1 pi
data PlanePoint = Cartesian Double Double | Polar Double Double
                deriving (Show,Eq)

-- define the following functions
-- 6.a) compute the distance from a point to the vertical axis
posx :: PlanePoint -> Double
posx (Cartesian x _) = x
posx (Polar r t) = r * (cos t)

-- 6.b) compute the distance from a point to the horizontal axis
posy :: PlanePoint -> Double
posy (Cartesian _ y) = y
posy (Polar r t) = r * sin t

-- 6.c) compute the distance from a point to the origin
radius :: PlanePoint -> Double
radius (Cartesian x y) = sqrt (x^2 + y^2)
radius (Polar r _)     = r

-- 6.d) compute the angle between the vector that connects the origin to the point and the horizontal axis
angle :: PlanePoint -> Double
angle (Cartesian x y) = atan2 y x
angle (Polar _ t)     = t

-- 6.e) compute the distance between two points
dist2points :: PlanePoint -> PlanePoint -> Double
dist2points p1 p2 = sqrt ((posx p2 - posx p1)^2 + (posy p2 - posy p1)^2)


-- 7) let the following data type represent geometric figures in a plane; the circle is centered at a given point and has a given radius, a rectangle parallel to the axes is represented by two points which are its diagonal vertices, or a triangle represented by the three points of its vertices points. Define the following functions
data Figure = Circle PlanePoint Double
            | Rectangle PlanePoint PlanePoint
            | Triangle PlanePoint PlanePoint PlanePoint
            deriving (Show, Eq)

-- 7.a) define a function that tests if a figure is a polygon
collinear :: PlanePoint -> PlanePoint -> PlanePoint -> Bool
collinear p1 p2 p3 =
  let x1 = posx p1
      y1 = posy p1
      x2 = posx p2
      y2 = posy p2
      x3 = posx p3
      y3 = posy p3
      area = 0.5 * abs (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2))
  in area == 0
  
polygon :: Figure -> Bool
polygon (Circle _ _)        = False
polygon (Rectangle p1 p2)   = 
  let width = abs (posx p2 - posx p1)
      height = abs (posy p2 - posy p1)
  in width /= 0 && height /= 0
polygon (Triangle p1 p2 p3) = not (collinear p1 p2 p3)

-- 7.b) define a function that computes the list of the vertices of a given figure
vertices :: Figure -> [PlanePoint]
vertices (Circle _ _) = []
vertices (Rectangle p1 p2)   = [p1, Cartesian (posx p1) (posy p2), p2, Cartesian (posx p2) (posy p1)]
vertices (Triangle p1 p2 p3) = [p1, p2, p3]

-- 7.c) complete the following definition that calculates the area of a figure
area :: Figure -> Double
area (Triangle p1 p2 p3) =
  let a = dist2points p1 p2
      b = dist2points p2 p3
      c = dist2points p3 p1
      s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))
area (Rectangle p1 p2) = abs ((posx p2 - posx p1) * (posy p2 - posy p1))
area (Circle _ r)      = pi * r^2

-- 7.d) define a function that computes the perimeter of a figure
perimeter2 :: Figure -> Double
perimeter2 (Triangle p1 p2 p3) = dist2points p1 p2 + dist2points p2 p3 + dist2points p3 p1
perimeter2 (Rectangle p1 p2)   = 2 * (abs (posx p2 - posx p1) + abs (posy p2 - posy p1))
perimeter2 (Circle _ r)        = 2 * pi * r


-- 8) using the functions ord :: Char -> Int and chr :: Int -> Char from the module Data.Char, define the following functions:
-- 8.a) tests if a given char is lowercase
isLower1 :: Char -> Bool
isLower1 x
  | (ord x) >= 97 && (ord x) <= 122 = True
  | otherwise                       = False
           
-- 8.b) tests if a char is a digit
isDigit1 :: Char -> Bool
isDigit1 x
  | (ord x) >= 48 && (ord x) <= 57 = True
  | otherwise                      = False

-- 8.c) tests if a char is a letter
isAlpha1 x
  | ((ord x) >= 65 && (ord x) <= 90)  = True
  | ((ord x) >= 97 && (ord x) <= 122) = True
  | otherwise                         = False
           
-- 8.d) converts from lowercase to uppercase
toUpper1 :: Char -> Char
toUpper1 x = chr (ord x - 32)

-- 8.e) converts a number from 0 to 9 to its digit
intToDigit1 :: Int -> Char
intToDigit1 x = chr (x + 48)

-- 8.f) converts a digit to its respective integer
digitToInt1 :: Char -> Int
digitToInt1 x = ord x - 48
