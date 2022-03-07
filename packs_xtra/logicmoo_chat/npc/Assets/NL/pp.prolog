%%
%% Prepositional phrases
%%
%% VPs produce predications as their meanings (e.g. go(Agent, Destination)).
%% Syntactically, go is non-transitive, so the predication of the phrase "I go"
%% go($me, _Dest), meaning the destination slot is left blank on exit from
%% the parsing of the verb.  PPs can be used to fill these slots in.  The parser
%% does not support fancier uses of PPs, since logical forms are restricted to
%% look like: quantifiers(modal(predication(args...))).
%%

%% opt_pp(Predication, +Gap, SentenceLFIn, SentenceLFOut)
%  Fills in slots of Predication, specified by PPs, and wrapping SentenceLFIn
%  in any further quantifiers from the NPs of the PPs, to produce SentenceLFOut

opt_pp(ForcePPs, Predication, Gap, SIn, SOut) -->
   { generating_nl, ! },
   generator_pp([],ForcePPs, Predication, Gap, SIn, SOut).

opt_pp(_ForcePPs, Predication, Gap, SIn, SOut) -->
   parser_opt_pp([],Predication, Gap, SIn, SOut).

% This must not be randomized.
generator_pp(_PrevPreps,[], _Predication, nogap, S, S) --> [ ].
generator_pp(PrevPreps,[ Preposition | Prepositions ], Predication, Gap, S1, S2) -->
   { prepositional_slot(Preposition, X, Predication), var(X), !, 
     \+ member(Preposition,PrevPreps),
     enforce_set([ Preposition | Prepositions ]) },
   generator_pp([Preposition|PrevPreps],Prepositions, Predication, Gap, S1, S2).
generator_pp(PrevPreps,[ Preposition | Prepositions ], Predication, Gap, S1, S3) -->
   [ Preposition ],
   { prepositional_slot(Preposition, X, Predication),
     \+ member(Preposition,PrevPreps),
     enforce_set([ Preposition | Prepositions ]) },
   np((X^S1)^S2, object, _, Gap, NewGap),
   generator_pp([Preposition|PrevPreps],Prepositions, Predication, NewGap, S2, S3).
   

:- randomizable parser_opt_pp//3.

parser_opt_pp(_PrevPreps,_, nogap, S, S) --> [ ].
% We model here as a kind of weird PP rather than as an adverb
% That lets us specifically annotate verbs with information about
% what slots to bind, rather than needing to have a general theory.
parser_opt_pp(_PrevPreps,Predication, nogap, S, S) -->
   [ Here ],
   { here_there_adverb(Here),
     prepositional_slot(here, Selection, Predication),
     /perception/mouse_selection:Selection }.
parser_opt_pp(PrevPreps,Predication, Gap, S1, S3) -->
   [ Preposition ],
   { preposition(Preposition),      
    \+ member(Preposition,PrevPreps),
     prepositional_slot(Preposition, X, Predication) },
   np((X^S1)^S2, object, _, Gap, nogap),
   parser_opt_pp([Preposition|PrevPreps],Predication, nogap, S2, S3).

%% preposition(?Word)
%  Word is a preposition
:- randomizable preposition/1.
preposition(at).
preposition(from).
preposition(to).
preposition(about).
preposition(with).
preposition(on).
preposition(in).
preposition(for).







%% prepositional_slot(?Preposition, ?Referrent, ?Predication
%  True when the slot of Predication corresponding to Preposition
%  has the value Referrent.
:- randomizable prepositional_slot/3.

:- register_all_lexical_items([P], preposition(P)).
