------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- a sequence of forms to enter a first and a last name
-- demonstrating the handling of intermediate states
------------------------------------------------------------------------------

import HTML

main = return $ form "First Name Form"
  [htxt "Enter your first name: ", textfield firstref "",
   button "Continue" fhandler]

 where
   firstref free

   fhandler _ = return $ form "Last Name Form"
                 [htxt "Enter your last name: ", textfield lastref "",
                  button "Continue" lhandler]

     where
       lastref free

       lhandler env = return $ form "Answer"
                       [htxt $ "Hi, " ++ env firstref ++ " " ++ env lastref]

-- Install the CGI program by:
-- makecurrycgi state
