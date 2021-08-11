---+ SWI-Prolog interface to C# and F#

The interface to C# is maintained by Uwe Lesta.  Here is an example:

==
PlQuery q = new PlQuery("member(A, [a,b,c])");
foreach (PlTermV s in q.Solutions)
    Console.WriteLine(s[0].ToString());
==

The documentation is online available [here](http://www.lesta.de/prolog/swiplcs/Generated/Index.aspx).  The documentation page also
provides a link for [downloading](http://www.lesta.de/prolog/swiplcs/download/index.htm). 
We provide an infrequently updated binary below.

    * [[SwiPlCs_1.1.60301.0.zip][<SwiPlCs_1.1.60301.0.zip>]]

@author Uwe Lesta
@see DotNetInterface.txt from Daniel Sullivan
