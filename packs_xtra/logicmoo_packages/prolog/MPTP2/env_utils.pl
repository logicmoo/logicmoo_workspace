%%- -*-Mode: Prolog;-*--------------------------------------------------
%%
%% File  : env_utils.pl
%%
%% Author: Josef Urban
%%
%%  Prolog code for optimizing the MML order of articles,
%%  tested only with SWI Prolog 5.2 now.
%%  Top-level predicate is print_in_order/0.
%%------------------------------------------------------------------------

:- [utils].

%% Kind must be in theory_exts

load_theory_files1(Kind):-
	atom(Kind),
	theory_exts(Exts),
	member(Kind, Exts),
	mml_dir_atom(MMLDir),
	concat_atom([MMLDir, '*.', Kind, '3'], Anames),
	string_to_atom(WildCard, Anames),
	expand_file_name(WildCard, Names),
	load_files(Names,[silent(true)]).


:- abolish(dependents/2).
:- dynamic(dependents/2).
:- abolish(rec_dependents/2).
:- dynamic(rec_dependents/2).


make_dependents(A,D):-
	theory(A,
	       [vocabularies(_),notations(X1),definitions(X2),
		theorems(X3),schemes(X4),registrations(X5),
		constructors(X6),requirements(X7)|_]),
	flatten([X1,X2,X3,X4,X5,X6,X7],D1),
	sort(D1,D), assert(dependents(A,D)).

depends(A,B):- dependents(A,D),member(B,D).

make_rec_dependents(A,R):- rec_dependents(A,R),!.
make_rec_dependents(A,R):-
	dependents(A,D),
	findall(BR,(member(B,D),make_rec_dependents(B,BR)),Rec),!,
	flatten([D,Rec],R1),
	sort(R1,R),
	assert(rec_dependents(A,R)).

rec_depends(A,B):- rec_dependents(A,D),member(B,D).

date(A1,Date1):-
	theory(A1,[_V1,_N1,_D1,_T1,_S1,_Reg1,_C1,_Req1,date([Date1|_])|_]),!.

date1(A1,Date1):-
	theory(A1,[_V1,_N1,_D1,_T1,_S1,_Reg1,_C1,_Req1,date(Date1)|_]),!.


structural(cat_1):-!.
structural(graph_1):-!.
structural(incsp_1):-!.
structural(net_1):-!.
structural(petri):-!.
structural(qmax_1):-!.
structural(struct_0):-!.
structural(X):- flag(X,1,1),!.
structural(X):- flag(X,2,2),!,fail.
structural(X):-
	flag(X,N,N),
%	write(X:entered:N),nl,
%	theory(X,Y),
%	member(constructors(Cs),Y),
%	member(A,Cs), % write(trying:A:for:X),nl,
	rec_depends(X,A),
	structural(A),!, % write(X:yes),nl,
	flag(X,_,1),!.
structural(X):-
	flag(X,_,2),!,
	% write(X:no),nl,
	fail.


assert_deps:-
	all_articles(L),!,member(A,L), once(make_dependents(A,_)),fail.

assert_rec_deps:-
	all_articles(L),!,member(A,L), once(make_rec_dependents(A,_)),fail.


assert_structural:-
	all_articles(L),!,member(A,L), once(structural(A)),fail.

%% article A2 comes after A1, if it recursively depends on it
smaller1(A1,A2):- rec_dependents(A2,R1), member(A1,R1).
%% otherwise structural article A2 comes after nonstructural A1
smaller2(A1,A2):- structural(A2), not(structural(A1)).
%% otherwise article A2 comes after A1, if it is received later
smaller3(A1,A2):- date(A1,Date1), date(A2,Date2), Date1 < Date2.
%% otherwise article A2 comes after A1, if it has more recursive dependecies
smaller4(A1,A2):-
	rec_dependents(A1,R1), rec_dependents(A2,R2),
	length(R1,L1), length(R2,L2), L1 < L2.	
%% otherwise do normal compare/3
smaller5(A1,A2):- compare(<,A1,A2).

%% A is minimal wrt a binary Pred among members of [H|T]
minimal(_,_,[]).
minimal(Pred,A,[H|T]):- G=..[Pred,H,A], not(G), minimal(Pred,A,T).

%% take minimal members of L1 wrt. smaller1, and put them to L2;
%% then select from L2 the minimal members wrt. smaller2, etc.
%% deterministic if only one element remains (this is true for smaller5)
least(L1,X):-
	findall(A1,(member(A1,L1),minimal(smaller1,A1,L1)),L2),!,
	findall(A2,(member(A2,L2),minimal(smaller2,A2,L2)),L3),!,
	findall(A3,(member(A3,L3),minimal(smaller3,A3,L3)),L4),!,
	findall(A4,(member(A4,L4),minimal(smaller4,A4,L4)),L5),!,
	findall(A5,(member(A5,L5),minimal(smaller5,A5,L5)),L6),!,
%	write([L2,L3,L4,L5,L6]),nl,
	sort(L6,L7),member(X,L7),!.

order1([],[]):-!.
order1(In,[X|Rest]):-
	least(In,X),!, write(X),nl,
	delete(In,X,R1),!,
	order1(R1,Rest).

initialize:-
	declare_mptp_predicates,
	load_theory_files1(evl),
	(assert_deps;true),
	(assert_rec_deps;true),
	(assert_structural;true).

%% print all nonumerical articles, which are roots (nothing depends on them)
%% ##TEST: :- initialize,findall(A,(nonnumeric(A),not(depends(_,A))),S),length(S,N), print(S).

%% ##TEST: :- print_in_order.
print_in_order:-
	initialize,all_articles(L),!,order1(L,_).


