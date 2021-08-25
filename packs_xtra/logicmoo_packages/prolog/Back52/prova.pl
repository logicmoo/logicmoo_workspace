
% per vedere se il classificatore funziona.

concetti :- 
  plant :< anything,
  product :< anything,
  energy :< product,
  mechanical_plant :< plant,
  produces :< domain(plant) and range(product),
  place :< anything,
  type :< anything,
  plant :< anything and all(located_at_place,place) and all(is_of_type,type).

definiti :-
  wind_energy_plant :< plant and all(produces,energy),
  water_energy_plant :< plant and all(produces,energy),
  wind_power_plant := mechanical_plant and wind_energy_plant,
  water_power_plant := mechanical_plant and water_energy_plant,
  mechanical_energy_plant := mechanical_plant and all(produces,energy).

oggetti :-
  w1 :: wind_power_plant,
  e1 :: energy,
  w2 :: plant and produces : e1.
  

domande :-
  backretrieve(X=describe_fully mechanical_energy_plant),write(X),nl,
  backretrieve(Y=[describe] for getall(wind_power_plant)),write(Y),nl,
  Goal=(wind_power_plant ?< mechanical_energy_plant),
  Goal,
  write(Goal), write(' ok.'),nl.
