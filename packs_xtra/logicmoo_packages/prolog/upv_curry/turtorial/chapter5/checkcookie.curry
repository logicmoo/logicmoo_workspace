------------------------------------------------------------------------------
-- Example for CGI programming with cookies in Curry:
-- a login form that checks whether cookies are enabled and that
-- sets a cookie
--
-- Michael Hanus
------------------------------------------------------------------------------

import HTML

loginForm = return $ cookieForm "Login" [("SETCOOKIE","")]
      [htxt "Enter your name: ", textfield tref "", hrule,
       button "Login" handler
      ]
 where
   tref free

   handler env = do
     cookies <- getCookies
     return $
       if lookup "SETCOOKIE" cookies == Nothing
       then form "No cookies" [h2 [htxt "Sorry, can't set cookies."]]
       else cookieForm "Logged In" [("LOGINNAME",env tref)]
                       [h2 [htxt $ env tref ++ ": thank you for visiting us"]]

-- Install the script by:
-- makecurrycgi -m loginForm checkcookie
