%%- -*-Mode: Prolog;-*--------------------------------------------------
%%
%% File  : fof2graph1.pl
%%
%% Author: Josef Urban
%%
%%  Expects a set of TPTP fof's in input. Outputs the fof's
%%  as a numbered graph. Compile: gplc fof2graph1.pl
%%------------------------------------------------------------------------
:-dynamic(spec/2).

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


is_numvar(X):- \+ var(X), X=..['$VAR'|_].
 
%% print_as_numbered_tree(+Term)
%%
%% prints term edges in the format 11:23,45:3,56:78
%% numbering the vertices uniquely
print_as_numbered_tree(Term):- p2t(Term),(!).
make_numbered_tree(Term,L):- p2tl(Term,L),(!).

p2tl(X,[]):- (atomic(X);var(X);is_numvar(X)),(!).
p2tl(X,Out):-
	X =.. [H|T],
	mk_number(H,H1),
	print_edges_to(H,H1,T,Out1),
	p2tls(T,Out2),
	append(Out1,Out2,Out).

p2tls([],[]).
p2tls([H|T],Out):- p2tl(H,Out1),(!),p2tls(T,Out2),append(Out1,Out2,Out).

mk_name(X,N):-
	name(X,K),name(n,L),append(L,K,M),name(N,M).

%% print_edges_to(+From,+ToTerms)
print_edges_to(_,_,[],[]).
print_edges_to(From,FromN,[H|T],[FromN:H1|OutT]):-
	(
	 atomic(H) -> mk_number(H,H1)
	;
	 H=.. [H0|_], mk_number(H0,H1)
	),
	print_edges_to(From,FromN,T,OutT).


p2t(X):- (atomic(X);var(X);is_numvar(X)),(!).
p2t(X):-
	X =.. [H|T],
	print_edges_to(H,T),
	p2ts(T).

p2ts([]).
p2ts([H|T]):- p2t(H),(!),p2ts(T).

mk_number(X,C):-
	number(X),(!),mk_name(X,N),mk_number(N,C).

mk_number(X,C1):-
	(
	 g_read(X,0) ->
	 g_inc(mmmm_counter,C0),
	 C is 2000000 + C0,
	 assertz(mmmm_arr(X,C)),
	 g_assign(X,C)
	;
	 g_read(X,C)
	),
	C1 is C - 1.
	
nwrite(X):-
	mk_number(X,C),
	write(C).

%% print_edges_to(+From,+ToTerms)
print_edges_to(_,[]).
print_edges_to(From,[H|T]):-
	nwrite(From),
	write(':'),
	(
	 (atomic(H);is_numvar(H)) -> H1 = H
	;
	 H=.. [H1|_]
	),
	nwrite(H1),
	write(','),
	print_edges_to(From,T).

% reference numbering
ref_num(X,C1):-
	(
	 g_read(X,0) ->
	 g_inc(ref_counter,C),
	 assertz(ref_arr(X,C)),
	 g_assign(X,C)
	;
	 g_read(X,C)
	),
	C1 is C - 1.

ref_numbers([],[]).
ref_numbers([H|T],[H1|T1]):- ref_num(H,H1),ref_numbers(T,T1).
	    
	    

write_fof_as_edge(F):-
	F =.. [fof,Name,_,Fla|_],
	(
	 spec(Name,Refs) -> ref_numbers([Name|Refs],RefNums)
	;
	 ref_numbers([Name],RefNums)
	),
	numbervars(Fla,0,_),
	make_numbered_tree(Fla,L),
	write(edges(Name,L,RefNums)),
	write('.'),nl.

write_fof_as_edge_old(F):-
	F =.. [fof,Name,_,Fla|_],
	numbervars(Fla,0,_),
	write('edges('),
	write(Name),
	write(',['),
	print_as_numbered_tree(Fla),
	write(']).'),nl.

print_tables:-
	tell('.symnr'),findall(X,(mmmm_arr(X,C), C1 is C - 1, write(X:C1),nl),_),told,
	tell('.refnr'),findall(X,(ref_arr(X,C), C1 is C - 1, write(X:C1),nl),_),told.

fof2edge:-
	set_prolog_flag(char_conversion,on),
	char_conversion('|','+'),
	read(SpecFile),
	read(FofFile),
	[SpecFile],(!),
	see(FofFile),
	repeat,
	read(F),	
	(
	  F = end_of_file -> print_tables,told,halt;
	  write_fof_as_edge(F),
	  fail
	).

:-initialization(fof2edge).
