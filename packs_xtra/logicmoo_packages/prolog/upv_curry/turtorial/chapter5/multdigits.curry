-- Generate HMTL document containing a list of all multiplications of digits

import HTML

-- Generate list of digit multiplications:
multiplications :: [(Int,Int,Int)]
multiplications = [ (x,y,x*y) | x <- [1..10], y <- [1..x] ]

-- Generate HTML expression for a single multplication:
mult2html :: (Int,Int,Int) -> [HtmlExp]
mult2html (x,y,z) =
 [htxt "The product of ", bold [htxt (show x)],
  htxt " and ", bold [htxt (show y)],
  htxt " is ", bold [htxt (show z)], breakline]

-- The complete HTML document:
htmlMultiplications :: [HtmlExp]
htmlMultiplications =
 [h1 [htxt "Multiplication of Digits"]] ++
 concatMap mult2html multiplications


-- Write a file with the HTML document:
writeHtmlFile :: IO ()
writeHtmlFile =
  writeFile "multtable.html"
     (showHtmlPage (page "Multiplication of Digits" htmlMultiplications))


-- A form that generates the HTML document on demand:
multForm :: IO HtmlForm
multForm = return $ form "Multiplication of Digits" htmlMultiplications

-- Install the CGI program by:
-- makecurrycgi -m multForm multdigits
