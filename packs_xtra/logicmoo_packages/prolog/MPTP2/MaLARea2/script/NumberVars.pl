%%- -*-Mode: Prolog;-*--------------------------------------------------
%%
%% File  : NumberVars.pl
%%
%% Author: Josef Urban
%%
%%  Expects a set of TPTP fof's in input. Outputs the same fof's,
%%  renaming the vars in a Prolog way. Compile: gplc NumberVars.pl
%%------------------------------------------------------------------------

    :- op(99,fx,'$').
    :- op(100,fx,++).
    :- op(100,fx,--).
    :- op(100,xf,'!').
    :- op(405,xfx,'=').
    :- op(405,xfx,'~=').
    :- op(450,fy,~).
    :- op(502,xfy,'|').
    :- op(502,xfy,'~|').
    :- op(503,xfy,&).
    :- op(503,xfy,~&).
    :- op(504,xfy,=>).
    :- op(504,xfy,<=).
    :- op(505,xfy,<=>).
    :- op(505,xfy,<~>).
%----! and ? are of higher precedence than : so !X:p(X) is :(!(X),p(X))
%----Otherwise !X:!Y:p(X,Y) cannot be parsed.
    :- op(400,fx,!).
    :- op(400,fx,?).
%----Need : stronger than + for equality and otter in tptp2X
%----Need : weaker than quantifiers for !X : ~p
    :- op(450,xfy,:).
%---- .. used for range in tptp2X. Needs to be stronger than :
    :- op(400,xfx,'..').

portray(~ A):- write(' ~ ('), print(A), write(') ').
portray(A & B):- format(' (~p & ~p) ',[A,B]).
portray(A + B):- format(' (~p | ~p) ',[A,B]).
portray(A => B):- format(' (~p => ~p) ',[A,B]).
portray(A <=> B):- format(' (~p <=> ~p) ',[A,B]).
portray(A : B):- var(A), format(' ( ~p : ~p) ',[A,B]).
portray(! A : B):- format(' (! ~p : ~p) ',[A,B]).
portray(? A : B):- format(' (? ~p : ~p) ',[A,B]).
portray(A != B):- format(' (~p != ~p) ',[A,B]).

write_fof(F):- numbervars(F,0,_),print(F),write('.'),nl.

number_vars:-
	set_prolog_flag(char_conversion,on),
	char_conversion('|','+'),
	repeat,
	read(F),	
	(
	  F = end_of_file -> halt;
	  write_fof(F),
	  fail
	).

:-initialization(number_vars).
