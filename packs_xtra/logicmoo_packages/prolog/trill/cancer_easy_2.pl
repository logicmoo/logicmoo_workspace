/*

QUERY 1:
instanceOf('WomanUnderLifetimeBRCRisk','Helen',LE).

EXPL:
LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),classAssertion('Woman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('WomanTakingEstrogen','Woman'),'Helen'),classAssertion('WomanTakingEstrogen','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('PostmenopausalWoman','Woman'),'Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanAged3040',intersectionOf(['Woman',someValuesFrom(hasAge,'Age3040')])]),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('WomanAged3040','Woman'),'Helen'),classAssertion('WomanAged3040','Helen')]



*/

equivalentClasses(['WomanAged3040',intersectionOf(['Woman',someValuesFrom('hasAge','Age3040')])]).
subClassOf('WomanTakingEstrogen','Woman').
subClassOf('PostmenopausalWoman','Woman').
subClassOf('Woman','WomanUnderLifetimeBRCRisk').
subClassOf('WomanAged3040','Woman').
classAssertion('Woman','Helen').
classAssertion('WomanTakingEstrogen','Helen').
classAssertion('PostmenopausalWoman','Helen').
classAssertion('WomanAged3040','Helen').
