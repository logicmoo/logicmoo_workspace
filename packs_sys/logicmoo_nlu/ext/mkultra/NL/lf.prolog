%%
%% Predicates for manipulating logical forms
%%

%% core_predication(:LF, -Core)
%  Strips ancillary predicates from LF and returns its core predication.
core_predication((P, _), C) :-
   !,
   core_predication(P, C).
core_predication(P,P).
