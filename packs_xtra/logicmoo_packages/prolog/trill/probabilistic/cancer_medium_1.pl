/*

QUERY 1:
prob_instance('WomanUnderLifetimeBRCRisk','Helen',P).
P = 0.132


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

p(subClassOf('WomanTakingEstrogen','Woman'),0.3).
p(classAssertion('Woman','Helen'),0.9).
p(subClassOf('Woman','WomanUnderLifetimeBRCRisk'),0.132).
