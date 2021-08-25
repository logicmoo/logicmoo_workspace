/*

QUERY 1:
instanceOf('WomanUnderLifetimeBRCRisk','Helen',LE).

EXPL:
LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanUnderBRCRisk',intersectionOf(['Woman',someValuesFrom(hasRisk,'BRCRisk')])]),'Helen'),(equivalentClasses(['WomanUnderIncreasedBRCRisk',intersectionOf(['WomanUnderBRCRisk',someValuesFrom(hasRisk,'IncreasedBRCRisk')])]),'Helen'),(equivalentClasses(['WomanUnderModeratelyIncreasedBRCRisk',intersectionOf(['WomanUnderIncreasedBRCRisk',someValuesFrom(hasRisk,'ModeratelyIncreasedBRCRisk')])]),'Helen'),(subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),(equivalentClasses(['WomanTakingEstrogen',someValuesFrom(hasRiskFactor,'Estrogen')]),'Helen'),classAssertion('WomanTakingEstrogen','Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanUnderBRCRisk',intersectionOf(['Woman',someValuesFrom(hasRisk,'BRCRisk')])]),'Helen'),(equivalentClasses(['WomanUnderIncreasedBRCRisk',intersectionOf(['WomanUnderBRCRisk',someValuesFrom(hasRisk,'IncreasedBRCRisk')])]),'Helen'),(subClassOf('WomanUnderModeratelyIncreasedBRCRisk','WomanUnderIncreasedBRCRisk'),'Helen'),(subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),(equivalentClasses(['WomanTakingEstrogen',someValuesFrom(hasRiskFactor,'Estrogen')]),'Helen'),classAssertion('WomanTakingEstrogen','Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),classAssertion('Woman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanTakingEstrogen',intersectionOf(['Woman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),classAssertion('WomanTakingEstrogen','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('WomanTakingEstrogen','Woman'),'Helen'),classAssertion('WomanTakingEstrogen','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWoman',intersectionOf(['Woman',someValuesFrom(hasRiskFactor,'AfterMenopause')])]),'Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('PostmenopausalWoman','Woman'),'Helen'),classAssertion('PostmenopausalWoman','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanAged3040',intersectionOf(['Woman',someValuesFrom(hasAge,'Age3040')])]),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanUnderShortTermBRCRisk',intersectionOf(['Woman',someValuesFrom(hasRisk,'ShortTermBRCRisk')])]),'Helen'),(subClassOf('WomanAged3040','WomanUnderShortTermBRCRisk'),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(equivalentClasses(['WomanUnderAbsoluteBRCRisk',intersectionOf(['Woman',someValuesFrom(hasRisk,'AbsoluteBRCRisk')])]),'Helen'),(subClassOf('WomanUnderShortTermBRCRisk','WomanUnderAbsoluteBRCRisk'),'Helen'),(subClassOf('WomanAged3040','WomanUnderShortTermBRCRisk'),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

LE = [(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),'Helen'),(subClassOf('WomanAged3040','Woman'),'Helen'),classAssertion('WomanAged3040','Helen')] ? ;

=================================

QUERY 2:
instanceOf('WomanUnderModeratelyIncreasedBRCRisk','Helen',LE).

EXPL
LE = [(subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),(equivalentClasses(['WomanTakingEstrogen',intersectionOf(['Woman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),classAssertion('WomanTakingEstrogen','Helen'),classAssertion('PostmenopausalWoman','Helen')],

LE = [(subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk'),'Helen'),(equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom(hasRiskFactor,'Estrogen')])]),'Helen'),(equivalentClasses(['WomanTakingEstrogen',someValuesFrom(hasRiskFactor,'Estrogen')]),'Helen'),classAssertion('WomanTakingEstrogen','Helen'),classAssertion('PostmenopausalWoman','Helen')]
*/

equivalentClasses(['WomanAged3040',intersectionOf(['Woman',someValuesFrom('hasAge','Age3040')])]).
equivalentClasses(['WomanUnderShortTermBRCRisk',intersectionOf(['Woman',someValuesFrom('hasRisk','ShortTermBRCRisk')])]).
equivalentClasses(['WomanUnderBRCRisk',intersectionOf(['Woman',someValuesFrom('hasRisk','BRCRisk')])]).
equivalentClasses(['WomanUnderAbsoluteBRCRisk',intersectionOf(['Woman',someValuesFrom('hasRisk','AbsoluteBRCRisk')])]).
equivalentClasses(['PostmenopausalWoman',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','AfterMenopause')])]).
equivalentClasses(['WomanTakingEstrogen',intersectionOf(['Woman',someValuesFrom('hasRiskFactor','Estrogen')])]).
equivalentClasses(['WomanUnderModeratelyIncreasedBRCRisk',intersectionOf(['WomanUnderIncreasedBRCRisk',someValuesFrom('hasRisk','ModeratelyIncreasedBRCRisk')])]).
equivalentClasses(['PostmenopausalWomanTakingEstrogen',intersectionOf(['PostmenopausalWoman',someValuesFrom('hasRiskFactor','Estrogen')])]).
equivalentClasses(['WomanUnderIncreasedBRCRisk',intersectionOf(['WomanUnderBRCRisk',someValuesFrom('hasRisk','IncreasedBRCRisk')])]).
equivalentClasses(['WomanTakingEstrogen',someValuesFrom('hasRiskFactor','Estrogen')]).
equivalentClasses(['WomanUnderIncreasedBRCRisk',intersectionOf(['WomanUnderBRCRisk',someValuesFrom('hasRisk','IncreasedBRCRisk')])]).
subClassOf('WomanTakingEstrogen','Woman').
subClassOf('PostmenopausalWoman','Woman').
subClassOf('Woman','WomanUnderLifetimeBRCRisk').
subClassOf('WomanAged3040','Woman').
subClassOf('WomanAged3040','WomanUnderShortTermBRCRisk').
subClassOf('WomanUnderShortTermBRCRisk','WomanUnderAbsoluteBRCRisk').
subClassOf('PostmenopausalWomanTakingEstrogen','WomanUnderModeratelyIncreasedBRCRisk').
subClassOf('WomanUnderModeratelyIncreasedBRCRisk','WomanUnderIncreasedBRCRisk').
classAssertion('Woman','Helen').
classAssertion('WomanTakingEstrogen','Helen').
classAssertion('PostmenopausalWoman','Helen').
classAssertion('WomanAged3040','Helen').
classAssertion('WomanAged3040','Emma').
