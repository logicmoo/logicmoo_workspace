% ======================================================================
% Tiny Probabilistic Event Calculus
%
% Authors:
%   Rolf Schwitter   (Macquarie University, Australia)
%   Fabrizio Riguzzi (University Ferrara, Italy)
%
% Date:
%   24th February 2017
% ======================================================================

/** <examples>

?- test_ec_pita(Probs).


*/

:- use_module(library(pita)).

:- pita.

test_ec_pita([P1, P2, P3, P4, P5, P6, P7]) :-
  prob( holds_at([fluent:located, pers:bob, loc:garden],  tp:3),  P1),
  prob( holds_at([fluent:located, pers:bob, loc:garden],  tp:5),  P2),
  prob( holds_at([fluent:located, pers:bob, loc:garden],  tp:7),  P3),
  prob( holds_at([fluent:located, pers:bob, loc:kitchen], tp:7),  P4),
  prob( holds_at([fluent:located, pers:bob, loc:kitchen], tp:10), P5),
  prob( holds_at([fluent:located, pers:bob, loc:kitchen], tp:11), P6),
  prob( holds_at([fluent:located, pers:bob, loc:garage],  tp:12), P7).


% ----------------------------------------------------------------------

:- begin_lpad.


% ----------------------------------------------------------------------
% Domain-independent axioms
% ----------------------------------------------------------------------

holds_at(F, tp:T) :-
  initially(F),
  \+ clipped(tp:0, F, tp:T).

holds_at(F, tp:T2) :-
  initiated_at(F, tp:T1),
  T1 < T2,
  \+ clipped(tp:T1, F, tp:T2).

clipped(tp:T1, F, tp:T3) :-
  terminated_at(F, tp:T2),
  T1 < T2, T2 < T3.


% ----------------------------------------------------------------------
% Initial situation
% ----------------------------------------------------------------------

initially([fluent:located, pers:bob, loc:garden]).


% ----------------------------------------------------------------------
% Noisy events
% ----------------------------------------------------------------------

happens_at([event:arrive, pers:bob, loc:kitchen], tp:5):0.95;'':0.05.
happens_at([event:arrive, pers:bob, loc:garage], tp:10):0.99;'':0.01.


% ----------------------------------------------------------------------
% Probabilistic effect axioms
%   - output of: learn_effect_axioms.pl
% ----------------------------------------------------------------------

initiated_at([fluent:located, pers:A, loc:B], tp:D):0.8;'':0.19999999999999996 :-
  happens_at([event:arrive, pers:A, loc:B], tp:D).

terminated_at([fluent:located, pers:A, loc:B], tp:E):0.8;'':0.19999999999999996 :-
  happens_at([event:arrive, pers:A, loc:_], tp:E),
  location([loc: B]).


% ----------------------------------------------------------------------
% Auxiliary facts to guarantee range restriction
% ----------------------------------------------------------------------

location([loc: garden]).
location([loc: garage]).
location([loc: kitchen]).


:- end_lpad.
