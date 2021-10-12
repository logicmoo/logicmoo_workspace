%%
%% Prepositional phrases
%%

:- randomizable opt_pp//3.

opt_pp(_, S, S) --> [ ].
opt_pp(Predication, S1, S3) -->
   [ Preposition ],
   { preposition(Preposition),
     prepositional_slot(Preposition, X, Predication) },
   np((X^S1)^S2, object, _, GapInfo),
   opt_pp(Predication, S2, S3).

%% preposition(?Word)
%  Word is a preposition
:- randomizable preposition/1.
preposition(to).

%% prepositional_slot(?Preposition, ?Referrent, ?Predication
%  True when the slot of Predication corresponding to Preposition
%  has the value Referrent.
:- randomizable prepositional_slot/3.
prepositional_slot(to, Destination, go(_Agent, Destination)).
   
