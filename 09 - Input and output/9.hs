import Data.List
import GHC.Base (VecElem(Int16ElemRep))
import System.Random

-- 1) using the randomIO and randomRIO functions from the Random class, write the following functions
-- 1.a) bingo (generate a unique random number between 1 and 90 every time a character is pressed)
bingo' :: [Int] -> IO ()
bingo' [] = putStrLn "No numbers left."
bingo' l = do
    putStrLn "Press a key to generate a number (Ctrl + C to quit)"
    _ <- getChar
    i <- randomRIO (0, length l - 1)
    let n = l !! i
    putStrLn $ "> " ++ show n ++ "\n"
    bingo' (delete n l)

bingo :: IO ()
bingo = do
    let numbers = [1 .. 90]
    bingo' numbers

-- 1.b) mastermind (implement the code-breaking game mastermind)
mastermind' :: [Int] -> IO ()
mastermind' nl = do
    putStrLn "> 1st digit:"
    g1 <- getChar
    putStrLn "\n> 2nd digit:"
    g2 <- getChar
    putStrLn "\n> 3rd digit:"
    g3 <- getChar
    putStrLn "\n> 4th digit:"
    g4 <- getChar
    let gl = map (subtract 48 . fromEnum) [g1, g2, g3, g4]
    let cg =
            concatMap
                (\(a, b) ->
                     if a == b
                         then concat [" ", show a, " "]
                         else " _ ")
                (zip nl gl)
    putStrLn $ "\n>>" ++ cg
    if gl == nl
        then putStrLn "> Correct.\n"
        else do
            putStrLn $ "> Try again.\n"
            mastermind' nl

mastermind :: IO ()
mastermind = do
    a <- randomRIO (0, 9) :: IO Int
    b <- randomRIO (0, 9) :: IO Int
    c <- randomRIO (0, 9) :: IO Int
    d <- randomRIO (0, 9) :: IO Int
    mastermind' [a, b, c, d]

-- 2) an euromillions bet consists of 5 integers between 1 and 50 (numbers) and 2 integers between 1 and 9 (stars). consider the following type:
data Bet =
    B [Int] (Int, Int)
    deriving (Show)

-- define the following functions
-- 2.a) validate (checks if the values on a given bet are well-defined and unique)
validate :: Bet -> Bool
validate (B n (s1, s2)) =
    (s1 /= s2)
        && (min s1 s2 >= 1)
        && (max s1 s2 <= 9)
        && (length n == 5)
        && foldr
               ((&&) . (\x -> x <= 50 && x >= 1 && not (elem x (delete x n))))
               True
               n

-- 2.b) common (given two bets, return a tuple where the first value is the amount of numbers in common and the second value is the amount of stars in common)
common :: Bet -> Bet -> (Int, Int)
common (B n1 (fs1, ss1)) (B n2 (fs2, ss2)) =
    let cn = length $ filter (`elem` n1) n2
        cs = length $ filter (`elem` [fs1, ss1]) [fs2, ss2]
     in (cn, cs)

-- 2.c) using the previous function:
-- 2.c.i) define Bet as an instance of Eq
instance Eq Bet where
    (==) b1 b2 = (common b1 b2) == (5, 2)

-- 2.c.ii) given a bet and the result of the contest, returns the corresponding prize (table in the form's pdf)
prize :: Bet -> Bet -> Maybe Int
prize b p =
    case (common b p) of
        (5, 2) -> Just 1
        (5, 1) -> Just 2
        (5, 0) -> Just 3
        (4, 2) -> Just 4
        (4, 1) -> Just 5
        (4, 0) -> Just 6
        (3, 2) -> Just 7
        (2, 2) -> Just 8
        (3, 1) -> Just 9
        (3, 0) -> Just 10
        (1, 2) -> Just 11
        (2, 1) -> Just 12
        (2, 0) -> Just 13
        _ -> Nothing

-- 2.d) define the following functions to make the program interactive
-- 2.d.i) readBet (reads a bet from the standard input, making sure it's a valid bet) 
readVal :: (Int, Int) -> String -> IO Int
readVal (l, u) o = do
    putStrLn $ "> " ++ o
    input <- getLine
    let n = read input :: Int
    if n >= l && n <= u
        then return n
        else do
            putStrLn $ "> Invalid input. Try again."
            readVal (l, u) o

readBet :: IO Bet
readBet = do
    let number = (1, 50)
    let star = (1, 9)
    n1 <- readVal number "1st number: "
    n2 <- readVal number "2nd number: "
    n3 <- readVal number "3rd number: "
    n4 <- readVal number "4th number: "
    n5 <- readVal number "5th number: "
    s1 <- readVal star "1st star: "
    s2 <- readVal star "2nd star: "
    let bet = (B [n1, n2, n3, n4, n5] (s1, s2))
    if validate bet
        then return bet
        else do
            putStrLn $ "> Invalid bet. Try again."
            readBet

-- 2.d.ii) play (given the result of the contest, reads the bet from standard input, and outputs the respective prize)
play :: Bet -> IO ()
play r = do
    b <- readBet
    case prize b r of
        Nothing -> putStrLn $ "> No prize."
        Just x -> putStrLn $ "> Prize: " ++ show x

-- 2.e) genBet (generates a random valid bet)
randomL :: [a] -> IO a
randomL l = do
    i <- randomRIO (0, length l - 1)
    let r = l !! i
    return r

genBet :: IO Bet
genBet = do
    let number = [1 .. 50]
    let star = [1 .. 9]
    n1 <- randomL number
    n2 <- randomL $ delete n1 number
    n3 <- randomL $ filter (\x -> notElem x [n1, n2]) number
    n4 <- randomL $ filter (\x -> notElem x [n1, n2, n3]) number
    n5 <- randomL $ filter (\x -> notElem x [n1, n2, n3, n4]) number
    s1 <- randomL star
    s2 <- randomL $ delete s1 star
    let b = (B [n1, n2, n3, n4, n5] (s1, s2))
    return b

-- 2.f) complete the definition of the cycle function so the player can play several times
main :: IO ()
main = do
    r <- genBet
    cycle' r

menu :: IO String
menu = do
    putStrLn menutxt
    putStr "Option: "
    c <- getLine
    return c
  where
    menutxt =
        unlines
            [ ""
            , "Bet...............1"
            , "Generate new key..2"
            , ""
            , "Quit..............0"
            ]

cycle' :: Bet -> IO ()
cycle' r = do
    o <- menu
    case o of
        "1" -> do
            play r
            cycle' r
        "2" -> do
            nb <- genBet
            cycle' nb
        "0" -> do
            putStrLn "> Thank you for playing."
        _ -> do
            putStrLn "> Invalid option. Please try again."
            cycle' r
