:-use_module(library(trill)).

:- tornado. % or :- trillp. or :- tornado.

/** <examples>

?- instanceOf(commander,john,Expl).

*/
subClassOf(allValuesFrom(commands,soldier),commander).
classAssertion(guard,pete).
classAssertion(guard,al).
classAssertion(allValuesFrom(commands,guard),john).
equivalentClasses([guard,soldier]).
