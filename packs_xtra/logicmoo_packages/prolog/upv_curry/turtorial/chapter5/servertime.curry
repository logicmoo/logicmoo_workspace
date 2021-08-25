-- CGI program to show the current date and time of the server

import HTML
import Time

-- A form that generates the HTML document on demand:
main :: IO HtmlForm
main = do
  time <- getLocalTime
  return $ form "Current Server Time"
            [h1 [htxt $ "Current date and time: " ++ calendarTimeToString time]]

-- Install the CGI program by:
-- makecurrycgi servertime
