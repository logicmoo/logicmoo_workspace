% ======================================================================
% Learning Probabilistic Effect Axioms
%
% Authors:
%   Rolf Schwitter   (Macquarie University, Sydney)
%   Fabrizio Riguzzi (University Ferrara, Italy)
%
% Date:
%   24th February 2017
% ======================================================================


/** <examples>

?- learn_effect_axioms(C).

*/
:- use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.


:- sc.

:- set_sc(max_var, 4).
:- set_sc(verbosity, 3).
:- set_sc(megaex_bottom, 5).
:- set_sc(depth_bound, false).
:- set_sc(neg_ex, given).


% ----------------------------------------------------------------------
% Language bias: initiated_at/2
% ----------------------------------------------------------------------

/*
output(initiated_at/2).
input(happens_at/2).

determination(initiated_at/2, happens_at/2).

modeh(*, initiated_at([fluent: -#fl, pers: +pers, loc: +loc], tp: +tp)).
modeb(*, happens_at([event: -#ev, pers: +pers, loc: +loc],  tp: +tp)).
*/

% ----------------------------------------------------------------------
% Language bias: terminated_at/2
% ----------------------------------------------------------------------

output(terminated_at/2).
input(happens_at/2).

determination(terminated_at/2, happens_at/2).
determination(terminated_at/2, location/1).

modeh(*, terminated_at([fluent: -#fl, pers: +pers, loc: +loc2], tp: +tp)).
modeb(*, happens_at([event: -#ev, pers: +pers, loc: -loc1], tp: +tp)).
modeb(*, location([loc: +loc2])).

lookahead_cons_var(location([loc:_L2]), [happens_at([event:_E, pers:_P, loc:_L1], tp:_T)]).
lookahead_cons_var(happens_at([event:_E, pers:_P, loc:_L1], tp:_T), [location([loc:_L2])]).


% ----------------------------------------------------------------------
% Background information
% ----------------------------------------------------------------------

:- begin_bg.

location([loc: bathroom]).
location([loc: bedroom]).
location([loc: garage]).
location([loc: hallway]).
location([loc: kitchen]).
location([loc: office]).
location([loc: yard]).

:- end_bg.

% Example generation:
% ----------------------------------------------------------------------
% Positive effect axiom
% ----------------------------------------------------------------------

initiated_at(ID, [fluent:F2, pers:P, loc:L2], tp:T2) :-
  holds_at(ID, [fluent:_F1, pers:P, loc:_L1], tp:T1),
  happens_at(ID, [event:_E, pers:P, loc:L2], tp:T2),
  T1 < T2,
  holds_at(ID, [fluent:F2, pers:P, loc:L2], tp:T3),
  T2 < T3.

neg(initiated_at(ID, [fluent:F2, pers:P, loc:_L], tp:T2)) :-
  holds_at(ID, [fluent:_F1, pers:P, loc:_L1], tp:T1),
  happens_at(ID, [event:_E, pers:P, loc:L2], tp:T2),
  T1 < T2,
  neg(holds_at(ID, [fluent:F2, pers:P, loc:L2], tp:T3)),
  T2 < T3.

% Example generation:
% ----------------------------------------------------------------------
% Negative effect axiom
% ----------------------------------------------------------------------

terminated_at(ID, [fluent:F1, pers:P, loc:L1], tp:T2) :-
  holds_at(ID, [fluent:F1, pers:P, loc:L1], tp:T1),
  happens_at(ID, [event:_E, pers:P, loc:L2], tp:T2),
  T1 < T2,
  holds_at(ID, [fluent:_F2, pers:P, loc:L2], tp:T3),
  T2 < T3.

neg(terminated_at(ID, [fluent:F1, pers:P, loc:L1], tp:T2)) :-
  holds_at(ID, [fluent:F1, pers:P, loc:L1], tp:T1),
  happens_at(ID, [event:_E, pers:P, loc:L2], tp:T2),
  T1 < T2,
  neg(holds_at(ID, [fluent:_F2, pers:P, loc:L2], tp:T3)),
  T2 < T3.


% ----------------------------------------------------------------------
% Example interpretations
% ----------------------------------------------------------------------

begin(model(f1)).
holds_at([fluent:located, pers:mary, loc:yard], tp:0).
happens_at([event:arrive, pers:mary, loc:kitchen], tp:3).
holds_at([fluent:located, pers:mary, loc:kitchen], tp:5).
end(model(f1)).

begin(model(f2)).
holds_at([fluent:located, pers:mary, loc:yard], tp:0).
happens_at([event:arrive, pers:mary, loc:garage], tp:4).
neg(holds_at([fluent:located, pers:mary, loc:garage], tp:5)).
end(model(f2)).

begin(model(f3)).
holds_at([fluent:located, pers:mary, loc:yard], tp:1).
happens_at([event:arrive, pers:mary, loc:kitchen], tp:2).
holds_at([fluent:located, pers:mary, loc:kitchen], tp:3).
end(model(f3)).

begin(model(f4)).
holds_at([fluent:located, pers:sue, loc:hallway], tp:1).
happens_at([event:arrive, pers:sue, loc:office], tp:3).
holds_at([fluent:located, pers:sue, loc:office], tp:5).
end(model(f4)).

begin(model(f5)).
holds_at([fluent:located, pers:sue, loc:bedroom], tp:1).
happens_at([event:arrive, pers:sue, loc:bathroom], tp:2).
holds_at([fluent:located, pers:sue, loc:bathroom], tp:4).
end(model(f5)).


% ----------------------------------------------------------------------
% Fold
% ----------------------------------------------------------------------

fold(train, [f1, f2, f3, f4, f5]).


% ----------------------------------------------------------------------
% Learn effect axioms
% ----------------------------------------------------------------------

learn_effect_axioms(C) :-
  induce([train], C).
