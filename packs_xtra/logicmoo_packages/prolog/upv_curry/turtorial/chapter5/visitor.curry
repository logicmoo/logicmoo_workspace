------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A CGI program counting the number of visitors as a simple example
-- for updating information on the server by clients.
------------------------------------------------------------------------------

import HTML
import System
import Directory
import Read

visitorForm = do
  visitnum <- incVisitNumber
  return $ form "Access Count Form"
           [h1 [htxt $ "You are the " ++ show visitnum ++ ". visitor!"]]

-- Increment the current visitor number and return the new number:
incVisitNumber :: IO Int
incVisitNumber = do
 existnumfile <- doesFileExist visitFile
 if existnumfile
   then do vfcont <- readFile visitFile
           writeVisitFile (readInt vfcont +1)
   else writeVisitFile 1

writeVisitFile n =
 do writeFile (visitFile++".new") (show n)
    system ("mv "++visitFile++".new "++visitFile)
    return n

-- the file where the current visitor number is stored:
visitFile = "numvisit"

-- Install the CGI program by:
-- makecurrycgi -m visitorForm visitor
