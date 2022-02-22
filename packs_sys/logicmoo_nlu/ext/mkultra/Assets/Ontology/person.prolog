adjectival_property(gender).
nominal_property(job).
valid_property_value(job, X) :-
   kind_of(X, job).
adjectival_property(age).
valid_property_value(age, X) :-
   number(X).
