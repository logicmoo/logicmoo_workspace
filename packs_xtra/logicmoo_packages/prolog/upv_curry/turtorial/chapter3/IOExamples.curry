-- A few simple examples for I/O programming

import Char


-- An I/O action thats copies the standard input to standard output
-- until the first period character:

echo = getChar >>= \c -> if c=='.' then done else putChar c >> echo


-- The same I/O action using the "do" notation:

echoDo = do c <- getChar
            if c=='.'
              then done
              else do putChar c
                      echoDo


-- Copy a file with transforming all lowercase into uppercase letters:

convertFile input output =
   do s <- readFile input
      writeFile output (map toUpper s)

-- end of I/O examples
