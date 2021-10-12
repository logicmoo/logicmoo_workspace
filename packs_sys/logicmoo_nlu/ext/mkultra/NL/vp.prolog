%%%                  Verb Phrases

%% aux_vp(LF, Person, Number, Tense, Progressive, Perfect)
%  Verb phrases, optionally augmented with auxilliaries and/or negative particle.

test_aux_vp(LF) :-
   aux_vp(LF, _, _, _, _, [can, halt], []).

aux_vp(VP, Polarity, Agreement, Tense, Aspect) --> 
   aux(nogap, Polarity, Agreement, Tense, Aspect, Form, M),
   vp(Form, VP, Tense, Agreement, nogap).

%% vp(?Form, ?Meaning, ?Tense, ?Agreement ?Gap)

:- randomizable vp//5.

vp(Form, X^S, Tense, Agreement, GapInfo) -->
   tv(Form, Agreement, X^VP, Tense), 
   np(VP^S, object, _, GapInfo).
vp(Form, VP, Tense, Agreement, nogap) --> 
   iv(Form, Agreement, VP, Tense).
