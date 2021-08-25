------------------------------------------------------------------------------
-- Example for CGI programming in Curry:
-- A form to browse the structure of a directory.
-- The form is parameterized by the (URL-encoded) name of the directory.
-- Subdirectories are presented as links to browse them.
--
-- Michael Hanus
------------------------------------------------------------------------------

import HTML
import Directory

showDirForm = do
  param <- getUrlParameter
  let dir = if param=="" then "." else urlencoded2string param
  entries <- getDirectoryContents dir
  hexps <- mapIO (entry2html dir) entries
  return $ form "Browse Directory"
                [h1 [htxt $ "Directory: " ++ dir], ulist hexps]

-- Transform directory and entry in this directory into a link (if it is
-- a directory) or a text:
entry2html :: String -> String -> IO [HtmlExp]
entry2html dir e = do
  direx <- doesDirectoryExist (dir++"/"++e)
  if direx
   then return [href ("browsedir.cgi?" ++ string2urlencoded (dir++"/"++e))
                     [htxt e]]
   else return [htxt e]


-- Install with:
-- makecurrycgi -m showDirForm browsedir
--
-- Call with: http://.../browsedir.cgi?<directory (urlencoded)>
