%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Copyright 2016 Hannes Strass, strass@informatik.uni-leipzig.de
%%
%% This file is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This file is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this file. If not, see <http://www.gnu.org/licenses/>.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% derivations.lp
%%
%% Compute all possible derivations for defeasible theories.
%%
%% Defeasible theories are specified by giving for each rule a head literal and
%% body literals.
%% A literal is either positive (a term) or negative (neg(T) where T is a term).
%% Rules are represented by terms.
%% To say that a literal L is the head of a rule R, use head(R, L).
%% To say that a literal L occurs in the body of a rule R, use body(R, L).
%% A rule is considered strict unless specified otherwise.
%% To say that a rule R is defeasible, use def(R).
%%
%% A derivation consists of a set of rules along with a partial order on rules such that:
%% The partial order has a greatest element.
%% For each rule (B,h) in the set: For each y\in B, there is a rule (B_y,y) in the set, where (B_y,y) is strictly below (B,h) in the partial order.
%% The rule set must be subset-minimal; equivalently, no two rules in the set can have the same head.

%% error handling: looking for rules without heads
syntax_error(rule_without_head(R)) :- hasBody(R), not hasHead(R).

hasBody(R) :- body(R, _).
hasHead(R) :- head(R, _).

%% extracting information from the theory

rule(R) :- head(R, _).
rule(R) :- body(R, _).

strict(R) :- rule(R), not def(R).

% a literal is anything that occurs in the body or head of a rule

literal(L) :- occurs(L).

occurs(L) :- head(_, L).
occurs(L) :- body(_, L).

% any non-negated literal is considered to be an atom
atom(A) :- literal(neg(A)).

atom(L) :- occurs(L), not negative(L).
negative(neg(A)) :- occurs(neg(A)).

% (negated) atoms are literals
literal(A) :- atom(A).
literal(neg(A)) :- atom(A).

% the negation relation for general literals
neg(A, neg(A)) :- atom(A).
neg(neg(A), A) :- atom(A).

%% computing derivations

% a derivation consists of a set of rules
{ used(R) } :- rule(R).

% that is minimal, that is, no two distinct rules for the same literal are used

:- used(R1), used(R2), R1 != R2, head(R1, L), head(R2, L).

% rule applicability

applicable(R) :- rule(R), h(L) : body(R, L).

h(L) :- used(R), head(R, L), applicable(R).

% groundedness

:- used(R), body(R, L), not h(L).

% ordering

less(R1, R2) :- used(R1), used(R2), head(R1, L), body(R2, L).

% antisymmetric
:- less(R1, R2), less(R2, R1).

% transitive
less(R1, R3) :- less(R1, R2), less(R2, R3).

% greatest element
maximal(R) :- used(R), not less(R, _).

:- maximal(R1), maximal(R2), R1 != R2.

caseFor(L) :- maximal(R), head(R, L).

% show
#show used/1.
#show less/2.
#show caseFor/1.
#show maximal/1.
