main :: IO ()
main = do
    putStrLn "Enter a number (or type 'exit' to quit):"
    loop

loop :: IO ()
loop = do
    input <- getLine
    if input == "exit"
        then putStrLn "Goodbye!"
        else do
            let number = read input :: Int
            putStrLn $ "You entered: " ++ show number
            loop