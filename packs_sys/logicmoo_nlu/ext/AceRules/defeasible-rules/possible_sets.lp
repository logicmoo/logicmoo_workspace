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
%% possible_sets.lp
%%
%% Direct possible-set semantics for defeasible theories.
%%
%% Defeasible theories are specified by giving for each rule a head literal and
%% body literals.
%% A literal is either positive (a term) or negative (neg(T) where T is a term).
%% Rules are represented by terms.
%% To say that a literal L is the head of a rule R, use head(R, L).
%% To say that a literal L occurs in the body of a rule R, use body(R, L).
%% A rule is considered strict unless specified otherwise.
%% To say that a rule R is defeasible, use def(R).

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

%% semantics

%% a possible set is a consistent set of literals

inconsistent :- literal(L), h(L), neg(L, NegL), h(NegL).

:- inconsistent.

%% that is closed under strict rules

% a rule is applicable if its body holds
applicable(R) :- rule(R), h(L) : body(R, L).

% strict rules *must* be applied
applied(R) :- strict(R), applicable(R).

% if a rule has been applied, its head holds
h(L) :- applied(R), head(R, L).

%% and there is some subset of applicable defeasible rules

{ applied(R) } :- def(R), applicable(R).

remains(R) :- def(R), applicable(R), not applied(R).

%% that is maximal with respect to consistency

x_applied(R) | x_notApplied(R) :- remains(R).

% saturate if ok
x_applied(R) :- remains(R), ok.
x_notApplied(R) :- remains(R), ok.

% ok must be grounded

ok :- not ok.

ok :- rules_ok.

rules_ok :- rule_ok(R) : remains(R).

rule_ok(R) :- x_applied(R), x_inconsistent.
rule_ok(R) :- x_notApplied(R).

% x_in are extra-in literals

x_h(L) :- h(L).

x_applicable(R) :- rule(R), x_h(L) : body(R, L).

x_applied(R) :- strict(R), x_applicable(R).

x_h(L) :- x_applied(R), head(R, L).

x_inconsistent :- literal(L), x_h(L), neg(L, NegL), x_h(NegL).

% holding literals need not have justifications

{ h(L) } :- literal(L).

% show 
	
#show h/1.
