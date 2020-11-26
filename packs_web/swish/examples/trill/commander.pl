:-use_module(library(trill)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.

:- trill. % or :- trillp. or :- tornado.

/** <examples>

?- instanceOf(commander,john,Expl).

*/
subClassOf(allValuesFrom(commands,soldier),commander).
classAssertion(guard,pete).
classAssertion(guard,al).
classAssertion(allValuesFrom(commands,guard),john).
subClassOf(guard,soldier).
