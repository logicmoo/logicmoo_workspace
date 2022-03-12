


%=autodoc
%% adjectival_property( ?Age1) is semidet.
%
% Adjectival Property.
%
adjectival_property(gender).



%=autodoc
%% nominal_property( ?Job1) is semidet.
%
% Nominal Property.
%
nominal_property(job).

valid_property_value(job, X) :-
   kind_of(X, job).

adjectival_property(age).

valid_property_value(age, X) :-
   number(X).
