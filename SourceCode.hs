--Made by Tuna AYDIN for the CENG 124 Haskell Assignment  

--Q1
-- countfor
countfor :: Char -> String -> Int
countfor c str = length [x | x <- str, x == c]
-- End of Q1

--Q2
-- substring
helper :: String -> String -> Bool
helper [] _ = True
helper _ [] = False
helper (x:xs) (y:ys) = (x == y) && helper xs ys


substring :: String -> String -> Bool
substring [] _ = True
substring _ [] = False
substring xs (y:ys)
  |helper xs (y:ys) = True
  |otherwise  = substring xs ys
-- End of Q2

--Q3
--do the truth table of xor
-- p q p<+>q
-- ---------
-- T T   F
-- T F   T
-- F T   T
-- F F   F

--Q4
-- show that -(p<->q) and xor have the same value
-- p q p<->q -(p<->q)
-- ------------------
-- T T   T       F
-- T F   F       T
-- F T   F       T
-- F F   T       F

-- we can see that p<+>q from Q3 and -(p<->q) hold the same values now with haskell code:
--for xor
xor :: Bool -> Bool -> Bool
xor x y = x /= y
--my compiler did not let me use infixr 2 <+> or the symbolic
--representation of <+>

-- for printing Xor Table
printXorTable:: IO ()
printXorTable = do
    putStrLn "p|\t   q|\t  p<+>q"
    mapM_ printRow[(p,q) | p <- [True, False], q <- [True, False]]
 -- to get all combinations of p and q
    where
        printRow (p,q) = putStrLn (show p ++ "|\t"++ show q ++ "|  \t" ++ show(xor p q))
 -- to print the xor truth table
-- End of Q3

-- haskell interperation of <-> equivalance
equiv :: Bool -> Bool -> Bool
equiv p q = p == q

-- Negated version of equivalance
notEquiv :: Bool -> Bool -> Bool
notEquiv p q = not (equiv p q)

-- to print the -(p<->q) truth table
printQ4Table :: IO ()
printQ4Table = do
    putStrLn "p|\t  q|\t  -(p<->q)"
    mapM_ printRow[(p,q) | p <- [True, False], q <- [True, False]] 
-- to get all combinations of p and q
    where
        printRow (p,q)= putStrLn(show p ++ "|\t" ++ show p ++ "|\t" ++ show (notEquiv p q)) 
-- to print the negated equivalance truth table
--End of Q3

--Q5
--show that p and (p<+>q)<+>q hold the same value with truth table
-- p q p<+>q (p<+>q)<+>q
-- ---------------------
-- T T   F        T
-- T F   T        T
-- F T   T        F
-- F F   F        F

-- With truth table we can see that (p<+>q)<+>q and p hold the same value

printQ5Table :: IO ()
printQ5Table = do
    putStrLn "p|\t  q|\t  ((p<+>q)<+>q)|\t  p"
    mapM_ printRow[(p,q) | p <- [True, False], q <- [True, False]]
 -- to get all combinations of p and q
    where
        printRow (p,q) = let temp = xor (xor p q) q in putStrLn (show p ++ "|\t" ++ show q ++ "|\t" ++ show temp ++ "|     \t\t" ++ show p) 
-- stores the value of (p<+>q)<+>q in temp and prints it with p 
--End of Q5

--main function to try all of the functions
main :: IO ()
main = do
    -- Q1 countfor 
    putStrLn "Enter a string:"
    str <- getLine
    putStrLn "Enter a character to count:"
    c <- getChar
    sepratist <- getLine -- to seperate the sub and c
    print ("The character '" ++ [c] ++ "' appears " ++ show (countfor c str) ++ " times.")

    -- Q2 substring
    putStrLn "\nEnter the substring to search for:"
    sub <- getLine
    putStrLn "Enter the string to search within:"
    str2 <- getLine
    if substring sub str2
        then putStrLn "Yes, it is a substring!\n"
        else putStrLn "No, it is NOT a substring.\n"
    
    -- Q3 xor truth table
    putStrLn "\n__Q3:__"
    printXorTable
    
    -- Q4 negated equivalance truth table
    putStrLn "\n__Q4:__"
    printQ4Table
    print("We can see that Xor from Q3 and negated equivalance have the same values")
    
    -- Q5 p and (p<+>q)<+>q truth table
    putStrLn "\n__Q5:__"
    printQ5Table
    print("We can see that p and ((p<+>q)<+>q) have the same values")
