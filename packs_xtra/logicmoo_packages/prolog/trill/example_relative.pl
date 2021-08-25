/*

QUERY 1:
instanceOf('Person','andrea',LE).

EXPL:
LE = [classAssertion(allValuesFrom(kin,'Person'),kevin),propertyAssertion(relative,kevin,laura),subPropertyOf(relative,kin,kevin,andrea),transitive(relative,[kevin,laura,andrea]),propertyAssertion(ancestor,laura,diana),subPropertyOf(ancestor,relative,laura,andrea),transitive(ancestor,[laura,diana,andrea]),propertyAssertion(ancestor,diana,andrea)] ? ;

LE = [classAssertion(allValuesFrom(kin,'Person'),kevin),propertyAssertion(relative,kevin,laura),subPropertyOf(relative,kin,kevin,andrea),transitive(relative,[kevin,laura,andrea]),propertyAssertion(ancestor,laura,erica),subPropertyOf(ancestor,relative,laura,andrea),transitive(ancestor,[laura,erica,andrea]),propertyAssertion(ancestor,erica,andrea)] 

*/

classAssertion(allValuesFrom('kin','Person'),'kevin').
propertyAssertion('relative','kevin','laura').
propertyAssertion('ancestor','laura','diana').
propertyAssertion('ancestor','diana','andrea').
transitive('relative').
transitive('ancestor').
subPropertyOf('relative','kin').
subPropertyOf('ancestor','relative').
propertyAssertion('ancestor','laura','erica').
propertyAssertion('ancestor','erica','andrea').
