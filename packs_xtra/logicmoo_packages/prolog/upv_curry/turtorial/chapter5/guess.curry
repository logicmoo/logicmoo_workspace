------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- a recursive form for a number guessing game
-- which also counts the number of guesses
------------------------------------------------------------------------------

import HTML
import Read

guessForm :: IO HtmlForm
guessForm = return $ form "Number Guessing" (guessInput 1)

guessInput :: Int -> [HtmlExp]
guessInput n =
  [htxt "Guess a natural number: ", textfield nref "",
   button "Check" (guessHandler n nref)]   where nref free

guessHandler :: Int -> CgiRef -> (CgiRef -> String) -> IO HtmlForm
guessHandler n nref env =
  let nr = readInt (env nref) in
  return $ form "Answer"
            (if nr==42
             then [h1 [htxt $ "Right! You needed "++show n++" guesses!"]]
             else [h1 [htxt $ if nr<42 then "Too small!"
                                       else "Too large!"],
                   hrule] ++ guessInput (n+1))

-- Install the CGI program by:
-- makecurrycgi -m guessForm guess
