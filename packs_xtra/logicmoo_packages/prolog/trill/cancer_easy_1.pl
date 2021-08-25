/*

QUERY 1:
instanceOf('WomanUnderLifetimeBRCRisk','Helen',LE).

EXPL:
LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanAged3040',intersectionOf(['Woman',someValuesFrom(hasAge,'Age3040')])]),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('WomanAged3040','Woman'),'Helen'),classAssertion('WomanAged3040','Helen')] ? 

*/


equivalentClasses(['WomanAged3040',intersectionOf(['Woman',someValuesFrom('hasAge','Age3040')])]).
subClassOf('Woman','WomanUnderLifetimeBRCRisk').
subClassOf('WomanAged3040','Woman').
classAssertion('WomanAged3040','Helen').
