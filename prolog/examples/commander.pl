/** <examples>

?- instanceOf(commander,john,Expl).

*/
subClassOf(allValuesFrom(commands,soldier),commander).
classAssertion(guard,pete).
classAssertion(guard,al).
classAssertion(allValuesFrom(commands,guard),john).
subClassOf(guard,soldier).
