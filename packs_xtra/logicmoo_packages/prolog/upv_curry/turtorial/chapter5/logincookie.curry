------------------------------------------------------------------------------
-- Example for CGI programming with cookies in Curry:
-- a login form setting a cookie and another form using this cookie
--
-- Michael Hanus
------------------------------------------------------------------------------

import HTML

loginForm = return $ form "Login"
      [htxt "Enter your name: ", textfield tref "", hrule,
       button "Login" handler
      ]
 where
   tref free

   handler env =
     return $ cookieForm "Logged In" [("LOGINNAME",env tref)]
                       [h2 [htxt $ env tref ++ ": thank you for visiting us"]]

getNameForm =
   do cookies <- getCookies
      return $ form "Hello" $
       maybe [h1 [htxt "Not yet logged in"]]
             (\n->[h1 [htxt $ "Hello, " ++ n]])
             (lookup "LOGINNAME" cookies)

-- Install the scripts by:
-- makecurrycgi -o login.cgi   -m loginForm   logincookie
-- makecurrycgi -o getname.cgi -m getNameForm logincookie
