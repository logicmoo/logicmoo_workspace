-- Define an I/O action filelength that reads requests the name
-- of a file from the user and prints the length of the file,
-- i.e., the number of characters contained in this file.

filelength =
 do putStrLn "Name of the input file?"
    filename <- getLine
    filecontents <- readFile filename
    putStrLn ("Length of the file: " ++ show (len filecontents) ++ " bytes")


-- Length of a list (also defined in the prelude as "length")
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

