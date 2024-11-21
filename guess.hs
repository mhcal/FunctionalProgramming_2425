import System.Random

rounds :: Int -> Int -> IO () 
rounds ng r = do putStr "Make a guess: "
                 guess <- readLn :: IO Int
                 case (compare guess r) of
                   EQ -> putStr (concat ["> Correct! Amount of guesses: ", show ng, "\n"]) 
                   LT -> putStr (concat ["> Higher than ", show guess, ". Try again\n"]) >> rounds (ng + 1) r
                   GT -> putStr (concat ["> Lower than ", show guess, ". Try again\n"]) >> rounds (ng + 1) r

main :: IO ()
main = do putStr "Insert range limit: "
          n <- readLn :: IO Int
          r <- randomRIO (1, n)
          rounds 1 r 
