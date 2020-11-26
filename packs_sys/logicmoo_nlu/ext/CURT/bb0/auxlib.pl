/*************************************************************************

         name: auxlib.pl
      version: November 14, 2002
  description: Auxiliary predicates for the comsem software package.
       author: Christof Rumpf
 
*************************************************************************/

:- module(auxlib, [myformat/2]).

myformat(Format, Args):-
	\+ \+ (numbervars(Args, 0, _),
	       format(Format, Args)).
	       