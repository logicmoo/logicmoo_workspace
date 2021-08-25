% annotationProperty(comment) 
% annotationProperty(label) 
% class('ClosedPizza') 
% class('EmptyPizza') 
% class('FishPizza-Closed') 
% class('FishPizza-Open') 
% class('FishTopping') 
% class('Gas') 
% class('IceCream') 
% class('Liquid') 
% class('LiquidSolidIceCream') 
% class('MaterialState') 
% class('MeatAndFishPizza-Closed') 
% class('MeatAndFishPizza-Open') 
% class('MeatFishAndVegetarianPizza-Closed') 
% class('MeatFishAndVegetarianPizza-Open') 
% class('MeatPizza-Closed') 
% class('MeatPizza-Open') 
% class('MeatTopping') 
% class('MeltedIceCream') 
% class('OneToppingPizza') 
% class('OpenPizza') 
% class('Pizza') 
% class('PizzaDomainConcept') 
% class('PizzaTopping') 
% class('Solid') 
% class('TestPizza') 
% class('VegePizza01') 
% class('VegePizza02') 
% class('VegePizza03') 
% class('VegePizza04') 
% class('VegePizza05') 
% class('VegePizza06') 
% class('VegePizza07') 
% class('VegePizza08') 
% class('VegePizza09') 
% class('VegePizza10') 
% class('VegePizza11') 
% class('VegetarianTopping') 
% class('VegetarianToppingsPizza-Closed') 
% class('VegetarianToppingsPizza-Open') 
% disjointClasses(['FishTopping','MeatTopping']) 
'owl:Nothing'(X):-
     'FishTopping'(X),'MeatTopping'(X).
% disjointClasses(['FishTopping','VegetarianTopping']) 
'owl:Nothing'(X):-
     'FishTopping'(X),'VegetarianTopping'(X).
% disjointClasses(['MeatTopping','FishTopping']) 
'owl:Nothing'(X):-
     'FishTopping'(X),'MeatTopping'(X).
% disjointClasses(['MeatTopping','VegetarianTopping']) 
'owl:Nothing'(X):-
     'MeatTopping'(X),'VegetarianTopping'(X).
% disjointClasses(['Pizza','PizzaTopping']) 
'owl:Nothing'(X):-
     'Pizza'(X),'PizzaTopping'(X).
% disjointClasses(['PizzaTopping','Pizza']) 
'owl:Nothing'(X):-
     'Pizza'(X),'PizzaTopping'(X).
% disjointClasses(['VegetarianTopping','FishTopping']) 
'owl:Nothing'(X):-
     'FishTopping'(X),'VegetarianTopping'(X).
% disjointClasses(['VegetarianTopping','MeatTopping']) 
'owl:Nothing'(X):-
     'MeatTopping'(X),'VegetarianTopping'(X).
% equivalentClasses(['LiquidSolidIceCream',intersectionOf(['IceCream',someValuesFrom(hasMaterialState,'Solid'),someValuesFrom(hasMaterialState,'Liquid')])]) 
'IceCream'(X):-
     'LiquidSolidIceCream'(X).
'LiquidSolidIceCream'(X):-
     'IceCream'(X),'Solid'(VEx_1),hasMaterialState(X,VEx_1),'Liquid'(VEx_2),hasMaterialState(X,VEx_2).
% equivalentClasses(['MeltedIceCream',intersectionOf(['IceCream',someValuesFrom(hasMaterialState,'Liquid')])]) 
'IceCream'(X):-
     'MeltedIceCream'(X).
'MeltedIceCream'(X):-
     'IceCream'(X),'Liquid'(VEx_3),hasMaterialState(X,VEx_3).
% equivalentClasses(['VegePizza01',intersectionOf(['Pizza',someValuesFrom(hasTopping,'VegetarianTopping')])]) 
'Pizza'(X):-
     'VegePizza01'(X).
'VegePizza01'(X):-
     'Pizza'(X),'VegetarianTopping'(VEx_4),hasTopping(X,VEx_4).
% equivalentClasses(['VegePizza02',intersectionOf([allValuesFrom(hasTopping,'VegetarianTopping'),'Pizza'])]) 
'VegetarianTopping'(Y):-
     hasTopping(X,Y),'VegePizza02'(X).
'Pizza'(X):-
     'VegePizza02'(X).
'VegePizza02'(X):-
     false.
% equivalentClasses(['VegePizza03',intersectionOf([allValuesFrom(hasTopping,'VegetarianTopping'),'Pizza',someValuesFrom(hasTopping,'VegetarianTopping')])]) 
'VegetarianTopping'(Y):-
     hasTopping(X,Y),'VegePizza03'(X).
'Pizza'(X):-
     'VegePizza03'(X).
'VegePizza03'(X):-
     false.
% equivalentClasses(['VegePizza04',intersectionOf(['Pizza',complementOf(someValuesFrom(hasTopping,'FishTopping')),complementOf(someValuesFrom(hasTopping,'MeatTopping'))])]) 
'Pizza'(X):-
     'VegePizza04'(X).
'VegePizza04'(X):-
     'Pizza'(X),false.
% equivalentClasses(['VegePizza05',intersectionOf(['Pizza',allValuesFrom(hasTopping,complementOf(unionOf(['MeatTopping','FishTopping'])))])]) 
'Pizza'(X):-
     'VegePizza05'(X).
% equivalentClasses(['VegePizza06',intersectionOf([allValuesFrom(hasTopping,complementOf(intersectionOf(['MeatTopping','FishTopping']))),'Pizza'])]) 
'Pizza'(X):-
     'VegePizza06'(X).
'VegePizza06'(X):-
     false.
% equivalentClasses(['VegePizza07',intersectionOf([someValuesFrom(hasTopping,complementOf(unionOf(['MeatTopping','FishTopping']))),'Pizza'])]) 
'Pizza'(X):-
     'VegePizza07'(X).
'VegePizza07'(X):-
     false,hasTopping(X,VEx_6),'Pizza'(X).
% equivalentClasses(['VegePizza08',intersectionOf([someValuesFrom(hasTopping,complementOf(intersectionOf(['MeatTopping','FishTopping']))),'Pizza'])]) 
'Pizza'(X):-
     'VegePizza08'(X).
'VegePizza08'(X):-
     false,hasTopping(X,VEx_7),'Pizza'(X).
% equivalentClasses(['VegePizza09',intersectionOf([allValuesFrom(hasTopping,complementOf(unionOf(['MeatTopping','FishTopping']))),'Pizza',someValuesFrom(hasTopping,complementOf(unionOf(['MeatTopping','FishTopping'])))])]) 
'Pizza'(X):-
     'VegePizza09'(X).
'VegePizza09'(X):-
     false.
% equivalentClasses(['VegePizza10',intersectionOf([someValuesFrom(hasTopping,complementOf(intersectionOf(['MeatTopping','FishTopping']))),'Pizza',allValuesFrom(hasTopping,complementOf(intersectionOf(['MeatTopping','FishTopping'])))])]) 
'Pizza'(X):-
     'VegePizza10'(X).
% equivalentClasses(['VegePizza11',intersectionOf(['Pizza',allValuesFrom(hasTopping,'VegetarianTopping'),someValuesFrom(hasTopping,'Thing')])]) 
'Pizza'(X):-
     'VegePizza11'(X).
'VegetarianTopping'(Y):-
     hasTopping(X,Y),'VegePizza11'(X).
'VegePizza11'(X):-
     'Pizza'(X),false.
% functionalProperty(hasMaterialState) 
sameIndividuals(X,Y):-
     hasMaterialState(Z,X),hasMaterialState(Z,Y).
% objectProperty(hasMaterialState) 
% objectProperty(hasTopping) 
% ontology('http://owl.cs.manchester.ac.uk/2009/07/sssw/pizza') 
% propertyDomain(hasTopping,'Pizza') 
'Pizza'(X):-
     hasTopping(X,_).
% subClassOf('ClosedPizza','TestPizza') 
'TestPizza'(X):-
     'ClosedPizza'(X).
% subClassOf('EmptyPizza','TestPizza') 
'TestPizza'(X):-
     'EmptyPizza'(X).
% subClassOf('EmptyPizza',exactCardinality(0,hasTopping)) 
% subClassOf('FishPizza-Closed','ClosedPizza') 
'ClosedPizza'(X):-
     'FishPizza-Closed'(X).
% subClassOf('FishPizza-Closed',allValuesFrom(hasTopping,'FishTopping')) 
'FishTopping'(Y):-
     hasTopping(X,Y),'FishPizza-Closed'(X).
% subClassOf('FishPizza-Closed',someValuesFrom(hasTopping,'FishTopping')) 
% subClassOf('FishPizza-Open','OpenPizza') 
'OpenPizza'(X):-
     'FishPizza-Open'(X).
% subClassOf('FishPizza-Open',someValuesFrom(hasTopping,'FishTopping')) 
% subClassOf('FishTopping','PizzaTopping') 
'PizzaTopping'(X):-
     'FishTopping'(X).
% subClassOf('Gas','MaterialState') 
'MaterialState'(X):-
     'Gas'(X).
% subClassOf('IceCream','PizzaDomainConcept') 
'PizzaDomainConcept'(X):-
     'IceCream'(X).
% subClassOf('IceCream',someValuesFrom(hasTopping,'FishTopping')) 
% subClassOf('Liquid','MaterialState') 
'MaterialState'(X):-
     'Liquid'(X).
% subClassOf('MeatAndFishPizza-Closed','ClosedPizza') 
'ClosedPizza'(X):-
     'MeatAndFishPizza-Closed'(X).
% subClassOf('MeatAndFishPizza-Closed',allValuesFrom(hasTopping,unionOf(['FishTopping','MeatTopping']))) 
% subClassOf('MeatAndFishPizza-Closed',someValuesFrom(hasTopping,'FishTopping')) 
% subClassOf('MeatAndFishPizza-Closed',someValuesFrom(hasTopping,'MeatTopping')) 
% subClassOf('MeatAndFishPizza-Open','OpenPizza') 
'OpenPizza'(X):-
     'MeatAndFishPizza-Open'(X).
% subClassOf('MeatAndFishPizza-Open',someValuesFrom(hasTopping,'FishTopping')) 
% subClassOf('MeatAndFishPizza-Open',someValuesFrom(hasTopping,'MeatTopping')) 
% subClassOf('MeatFishAndVegetarianPizza-Closed','ClosedPizza') 
'ClosedPizza'(X):-
     'MeatFishAndVegetarianPizza-Closed'(X).
% subClassOf('MeatFishAndVegetarianPizza-Closed',allValuesFrom(hasTopping,unionOf(['FishTopping','MeatTopping','VegetarianTopping']))) 
% subClassOf('MeatFishAndVegetarianPizza-Closed',someValuesFrom(hasTopping,'FishTopping')) 
% subClassOf('MeatFishAndVegetarianPizza-Closed',someValuesFrom(hasTopping,'MeatTopping')) 
% subClassOf('MeatFishAndVegetarianPizza-Closed',someValuesFrom(hasTopping,'VegetarianTopping')) 
% subClassOf('MeatFishAndVegetarianPizza-Open','OpenPizza') 
'OpenPizza'(X):-
     'MeatFishAndVegetarianPizza-Open'(X).
% subClassOf('MeatFishAndVegetarianPizza-Open',someValuesFrom(hasTopping,'FishTopping')) 
% subClassOf('MeatFishAndVegetarianPizza-Open',someValuesFrom(hasTopping,'MeatTopping')) 
% subClassOf('MeatFishAndVegetarianPizza-Open',someValuesFrom(hasTopping,'VegetarianTopping')) 
% subClassOf('MeatPizza-Closed','ClosedPizza') 
'ClosedPizza'(X):-
     'MeatPizza-Closed'(X).
% subClassOf('MeatPizza-Closed',allValuesFrom(hasTopping,'MeatTopping')) 
'MeatTopping'(Y):-
     hasTopping(X,Y),'MeatPizza-Closed'(X).
% subClassOf('MeatPizza-Closed',someValuesFrom(hasTopping,'MeatTopping')) 
% subClassOf('MeatPizza-Open','OpenPizza') 
'OpenPizza'(X):-
     'MeatPizza-Open'(X).
% subClassOf('MeatPizza-Open',someValuesFrom(hasTopping,'MeatTopping')) 
% subClassOf('MeatTopping','PizzaTopping') 
'PizzaTopping'(X):-
     'MeatTopping'(X).
% subClassOf('OneToppingPizza','TestPizza') 
'TestPizza'(X):-
     'OneToppingPizza'(X).
% subClassOf('OneToppingPizza',exactCardinality(1,hasTopping)) 
% subClassOf('OpenPizza','TestPizza') 
'TestPizza'(X):-
     'OpenPizza'(X).
% subClassOf('Pizza','PizzaDomainConcept') 
'PizzaDomainConcept'(X):-
     'Pizza'(X).
% subClassOf('PizzaTopping','PizzaDomainConcept') 
'PizzaDomainConcept'(X):-
     'PizzaTopping'(X).
% subClassOf('Solid','MaterialState') 
'MaterialState'(X):-
     'Solid'(X).
% subClassOf('TestPizza','Pizza') 
'Pizza'(X):-
     'TestPizza'(X).
% subClassOf('VegetarianTopping','PizzaTopping') 
'PizzaTopping'(X):-
     'VegetarianTopping'(X).
% subClassOf('VegetarianToppingsPizza-Closed','ClosedPizza') 
'ClosedPizza'(X):-
     'VegetarianToppingsPizza-Closed'(X).
% subClassOf('VegetarianToppingsPizza-Closed',allValuesFrom(hasTopping,'VegetarianTopping')) 
'VegetarianTopping'(Y):-
     hasTopping(X,Y),'VegetarianToppingsPizza-Closed'(X).
% subClassOf('VegetarianToppingsPizza-Closed',someValuesFrom(hasTopping,'VegetarianTopping')) 
% subClassOf('VegetarianToppingsPizza-Open','OpenPizza') 
'OpenPizza'(X):-
     'VegetarianToppingsPizza-Open'(X).
% subClassOf('VegetarianToppingsPizza-Open',someValuesFrom(hasTopping,'VegetarianTopping')) 
% annotationAssertion(comment,'ClosedPizza',literal(type(string,'Pizzas that have been closed to limit what toppings they can have'))) 
% annotationAssertion(comment,'IceCream',literal(type(string,'All IceCreams must have at least 1 topping.\nAny individual that has a topping must be a Pizza.\nTherefore all IceCreams must be Pizzas.\nIcecream is inferred to be a subclass of Pizza.'))) 
% annotationAssertion(comment,'LiquidSolidIceCream',literal(type(string,'A class that uses a functional property (hasMaterialState) twice. This is not inconsistent unless MaterialStates have been declared disjoint'))) 
% annotationAssertion(comment,'MaterialState',literal(type(string,'The MaterialStates should be disjoint, but this has been left out - see LiquidSolidIceCream'))) 
% annotationAssertion(comment,'OpenPizza',literal(type(string,'Pizzas that could be extended - they have the given toppings, but could also have other, unspecified toppings'))) 
% annotationAssertion(comment,'Pizza',literal(type(string,'We have not asserted that all pizzas must have at least 1 topping'))) 
