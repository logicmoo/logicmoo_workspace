%%% An example that demonstrates the inadequacy of verifier.pl,
%%% by Brian W. DeVries.

:- ['v.pl'].

proposition(p).
proposition(q).

state(s0).
state(s1).
state(s2).


holds(s0, p).
holds(s0, q).
holds(s1, p).
holds(s2, p).
holds(s2, q).


% Swap the order of these two transitions and win prizes
trans(s0, s2).
trans(s0, s1).

trans(s1, s1).
trans(s2, s2).


q :- check(s0, ~ (g p ^ g q)).
q_gq :- check(s0, ~ g q).
q_gp :- check(s0, ~ g p).



% If the transition s0 -> s1 is tried before the transition s0 -> s2, then G p
% and G q will produce different paths ([s0, s1] and [s0, s2], respectively).
% Due to the `once` at the top level and in the calls to `coverify`, these will
% be the only paths that are produced. However, if the transitions are tried
% in the reverse order, then they will produce the same path [s0, s2]. So when
% G p ^ G q is run, the result now depends on the ordering of the `trans`
% clauses above: if s0 -> s1 is tried first, then G p and G q will not hold on
% the same path, so it will fail, but if s0 -> s2 is tried first, then they
% will both hold on the same path and succeed.* Due to the `once` surrounding
% the calls to `coverify`, the verifier will not attempt to check G p on the
% path s0 -> s2 in the former case, causing an erroneous success (meaning that
% the transition system does not violate the formula).
%
% * If s0 -> s2 is tried first, the verifier will loop.

