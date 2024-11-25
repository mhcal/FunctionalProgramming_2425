import System.Random
import Data.List
import GHC.Base (VecElem(Int16ElemRep))

-- 1) using the randomIO and randomRIO functions from the Random class, write the following functions
-- 1.a) bingo (generate a unique random number between 1 and 90 every time a character is pressed)
bingo' :: [Int] -> IO ()
bingo' [] = putStrLn "No numbers left."
bingo' l = do
   putStrLn "Press a key to generate a number (Ctrl + C to quit)"
   _ <- getChar
   i <- randomRIO (0, length l - 1)
   let n = l!!i
   putStrLn $ "> " ++ show n ++ "\n"
   bingo' (delete n l)

bingo :: IO ()
bingo = do 
    let numbers = [1..90]
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
    let cg = concatMap (\(a, b) -> if a == b then concat [" ", show a, " "] else " _ ") (zip nl gl)
    putStrLn $ "\n>>" ++ cg
    if gl == nl then putStrLn "> Correct.\n"
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
data Bet = B [Int] (Int, Int)

-- define the following functions
-- 2.a) validate (checks if the values on a given bet are well-defined and unique)
validate :: Bet -> Bool
validate (B n (s1, s2)) =
    (s1 /= s2) && (min s1 s2 >= 1) && (max s1 s2 <= 9) && (length n == 5) && foldr ((&&) . (\x -> x <= 50 && x >= 1 && not (elem x (delete x n)))) True n

-- 2.b) common (given two bets, return a tuple where the first value is the amount of numbers in common and the second value is the amount of stars in common)
common :: Bet -> Bet -> (Int, Int)
common (B n1 (fs1, ss1)) (B n2 (fs2, ss2)) =
    let cn = length $ filter (`elem` n1) n2
        cs = length $ filter (`elem` [fs1, ss1]) [fs2, ss2]
    in (cn, cs)

-- 2.c) using the previous function, define Bet as an instance of Eq
instance Eq Bet where
    (==) b1 b2 = (common b1 b2) == (5, 2)


