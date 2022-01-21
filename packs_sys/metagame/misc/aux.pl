%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%============================================================
%		   METAGAME Game-Playing Workbench
%============================================================
%
% Contains general utility predicates.
% Adapted from shared code written by Fernando Pereira, 
% Martha Pollack, and Barney Pell.
%	      Changes  Copyright (c) 1992 Barney D. Pell
%============================================================

%%%%%%%%%%%%%%%%%%%%%
%%%%%	AUX	%%%%%
%%%%%%%%%%%%%%%%%%%%%


:- my_use_module(library(lists)).
:- use_module(library(ordsets)).

%  General auxiliary procedures
%  Contains:
%	append/3
%       average/2
%       between/3
%	cgensym/2
%	concat/3
%	concat_list/2
%	cons/3
%	cull/4
%	cull_funct/4
%	extract/3
%       findall/3
%	f_cons/3
%       flatten/2
%	gensym/2
%	get_nth/3
%       increase_term_arity/3
%	lastsuffix/2
%	length/2
%       maplist/3
%	max/2, max/3
%	min/2, min/3
%       mesh/3
%	member/2
%	mnl/1
%	nth/3
%	nth_letter/2, nth_letter/3
%       numlist/3
%       pair_list/3
%	percolate/3,perc2/4
%	ppl/1,ppl/2
%       remove_duplicates/2
%       remove_test_duplicates/3
%	reset_gensym/1, reset_gensym/2
%	rev_append/3
%	reverse/2,rev2/3
%	snoc/3
%	space/0,space/1
%	split_list/4
%	split_list_funct/4
%       stable_sort/2
%	subset/2
%       verify/1
%       whenever/2
%	ynp/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Print mulitple line feeds (arg1 of them).

mnl(1) :- nl.
mnl(N) :- N>1, nl, N1 is N-1, mnl(N1).


%  Pretty print the list arg1.
% Changed write to print.

ppl(L) :- ppl(L,3).
ppl([],_Ind).
ppl([[H|T]|L],Ind) :- Ind1 is Ind+3,ppl([H|T],Ind1),ppl(L,Ind),!.
ppl([H|L],Ind) :- space(Ind),print(H),nl,ppl(L,Ind),!.
ppl(A,Ind) :-  space(Ind),print(A),nl.


%  Pretty print the list arg1.
% Changed write to print.

pwl(L) :- pwl(L,3).
pwl([],_Ind).
pwl([[H|T]|L],Ind) :- Ind1 is Ind+3,pwl([H|T],Ind1),pwl(L,Ind),!.
pwl([H|L],Ind) :- space(Ind),write(H),nl,pwl(L,Ind),!.
pwl(A,Ind) :-  space(Ind),write(A),nl.



%  Print arg1 blank spaces.
%  Using the predicate name 'space' causes the compiler to crash --Barney

space :- write(' ').
space(0) :- true.
space(1) :- space.
space(N) :- N>1,space,N1 is N-1,space(N1).

%  Cons an element into a list.

cons(E,[],[E]).
cons(E,[H|T],[E,H|T]).

%  "Snoc" an element into the end of a list.

snoc(E,[],[E]).
snoc(E,L0,L1) :-
	reverse(L0,Lr),
	cons(E,Lr,Lr1),
	reverse(Lr1,L1).

%  Cons an element into a list, unless the element is itself a null list.

f_cons([],L,L).
f_cons(E,[H|T],[E,H|T]).

 


%  Append the reverse of arg1 to arg2 to give arg3.

rev_append([],L,L).
rev_append([H|T],L,R) :-
	rev_append(T,[H|L],R).


%  Extract arg1 from the list in arg2 to give arg3.
%  Fails if arg1 is not a member of arg2.

extract(Elt,[Elt|Tail],Tail).
extract(Elt,[Head|Tail],[Head|List]) :-
	extract(Elt,Tail,List).

%  Cull all the members of arg2 that match the pattern in arg1.
%  Culled entities go into arg3; everything else goes into arg4.

cull(_Pattern,[],[],[]).
cull(Pattern,[Pattern|T0],[Pattern|T1],R) :-
	cull(Pattern,T0,T1,R).
cull(Pattern,[H|T0],C,[H|T1]) :-
	cull(Pattern,T0,C,T1).

%  Cull all the members of arg2 whose functor is arg1. 
%  Culled entities go into arg3; everything else goes into arg4.

cull_funct(_Funct,[],[],[]).
cull_funct(Funct,[H0|T0],[H0|T1],R) :-
	functor(H0,Funct,_),
	cull_funct(Funct,T0,T1,R).
cull_funct(Funct,[H0|T0],C,[H0|T1]) :-
	cull_funct(Funct,T0,C,T1).

%  Split_list finds arg1 in arg2, putting everything to the left into arg3 and
%  everything to the right in arg4.  E.g.:
%     split_list(c,[a,b,c,d,e],[a,b],[d,e]).

split_list(Elt,[Elt|Tail],[],Tail).
split_list(Elt,[Head|Tail0],[Head|Tail1],Tail) :-
	split_list(Elt,Tail0,Tail1,Tail).

%  Split_list_funct finds the element with functor arg1 in arg2, putting 
%  everything to the left into arg3 and everything to the right into arg4.

split_list_funct(Funct,[Elt|Tail],[],Tail) :-
	functor(Elt,Funct,_).
split_list_funct(Funct,[H0|T0],[H0|T1],T) :-
	split_list_funct(Funct,T0,T1,T).


%  get_nth is like nth, but can be used with arg2 uninstantiated

get_nth(1,[Head|_],Head).
get_nth(P,[_|Tail],Elt) :-
      get_nth(P1,Tail,Elt),
      P is P1 + 1.


% Letter is the Nth (lowercase) letter.

nth_letter(N,Letter) :- 
	nth_letter_after(N,'a',Letter),
	N > 0, N =< 26.

% Letter is the Nth letter, starting at Letter0.

nth_letter_after(N,Letter0,Letter) :- 
	atom(Letter), !,
	name(Letter0,[A]),
	name(Letter,[L]),
	N is L - A + 1.
nth_letter_after(N,Letter0,Letter) :- 
	integer(N), 
	name(Letter0,[A]),
	L is N+A-1,
	name(Letter,[L]).


%  Percolate arg1 to the head of arg2 to give arg3.

percolate(M,L1,L2) :-  perc2(M,L1,[],L2).

perc2(M,[M|Lt],Lp,[M|L]) :-
	reverse(Lp,Lpr),
	append(Lpr,Lt,L).
perc2(M,[Lh|Lt],Lp,L) :-
	perc2(M,Lt,[Lh|Lp],L).

% Interleaving elements of arg1 and arg2, preserving order within each list
% gives arg3.

mesh([],L,L).
mesh(L,[],L).
mesh([A|As],[B|Bs],[A|Rest]) :- 
	mesh(As,[B|Bs],Rest).
mesh([A|As],[B|Bs],[B|Rest]) :- 
	mesh([A|As],Bs,Rest).


%   flatten(Tree, List)
%   flattens a Tree of cons cells into a List. 	
% This from Quintus Library Flatten.
flatten(Tree, List) :-
	flatten(Tree, List, []).

flatten([]) --> !.
flatten([Head|Tail]) --> !,
	flatten(Head),
	flatten(Tail).
flatten(Other) -->
    %	{ Other ~= [], Other ~= [_|_] },
	[Other].



%   Generate in arg2 a new symbol with prefix arg1, and a unique suffix.

gensym(Prefix, V) :-
	var(V),
	atomic(Prefix),
	lastsuffix(Prefix, M),
	N is M+1,
	asserta(flag(gensym(Prefix), N)),
	concat(Prefix, N, V),
	!.

% Like gensym, but if V is instantiated it will be left alone.

cgensym(Prefix,V) :-
      var(V), !,
      gensym(Prefix,V).
cgensym(_,_). 


lastsuffix(Prefix,M) :-
	retract(flag(gensym(Prefix),M)), !.
lastsuffix(_Prefix,0).


% Set all gensym suffixes back to 1.

reset_gensym :-
      retractall(flag(gensym(_Prefix),_M)).

reset_gensym(Prefix) :-
      retractall(flag(gensym(Prefix),_M)).


% BI_NAME(?Atom,?List)
% A version of name/2 facilitating bidirectional programs, 
% where both args can be unbound at the time it is called, and they
% will be frozen until one is bound.

bi_name(Atom,List) :-
	when((ground(Atom);ground(List)),
	  name(Atom,List)).


%  Form an atom in arg3 that is the concatenation of arg1 and arg2.

concat(N1, N2, N3) :-
	name(N1, Ls1),
	name(N2, Ls2),
	append(Ls1, Ls2, Ls3),
	name(N3, Ls3).

concat_list([A],A) :- !.
concat_list([A|Bs],C) :-
	concat_list(Bs,Bconc),
	concat(A,Bconc,C).


append_list([A],A) :- !.
append_list([A|Bs],C) :-
	append_list(Bs,Bconc),
	append(A,Bconc,C).


% BI_CONCAT(?N1, ?N2, ?N3)
% A bidirectional version of concat/3.  
% This is also provided in quintus string_append/3 procedure.
bi_concat(N1, N2, N3) :-
	( atom(N3) 
	-> name(N3,L3),
	   append(L1,L2,L3),
	   name(N2,L2),
	   name(N1,L1)
	;  atom(N1) 
	-> concat(N1,N2,N3)
	; format("Error, uninstantiated args in bi_concat~n",[]),
	  fail
	).

/* This is sicstus-specific, so not used.
bi_concat(N1, N2, N3) :-
	bi_name(N1, Ls1),
	bi_name(N2, Ls2),
	append(Ls1, Ls2, Ls3),
	bi_name(N3, Ls3).
*/


% BI_CONCAT_LIST(List,Conc)
% Bi-directional verion of concat_list/3.
bi_concat_list([A],A) :- !.
bi_concat_list([A|Bs],C) :-
	bi_concat_list(Bs,Bconc),
	bi_concat(A,Bconc,C).


%  Guarantee that a response designates either yes or no.

ynp(y,y,_Goal) :- !.
ynp(yes,y,_Goal) :- !.
ynp(n,n,_Goal) :- !.
ynp(no,n,_Goal) :- !.
ynp(_Resp,_RespVal,Goal) :-
	write('Please respond with y or n.'),nl,
	call(Goal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% count_bagof and count_setof
% Measures the set returned by the base procedure.
% Will not backtrack, so A,B, better instantiate all the variables 
% in needs!
count_bagof(A,B,C) :- 
	( bagof(A,B,C1) ->
	  length(C1,C)
	; C=0
	).

count_setof(A,B,C) :- 
	( setof(A,B,C1) ->
	  length(C1,C)
	; C=0
	).

count_findall(A,B,C) :- 
	findall(A,B,C1),
	length(C1,C).

% maplist(P,ListIn,ListOut).
%   Apply the predicate P to each element of  a list  L to form a new list M.
%   We assume that P has two arguments, where the first is the input, and the
%   second the output.
%   From CLOCKSIN & MELLISH, p. 173.

maplist(_,[],[]).
maplist(P,[X|L],[Y|M]) :-
	Q =.. [P,X,Y],
	call(Q),
	maplist(P,L,M).



%   For each instance of generator, call goal.

whenever(Generator,Goal) :-
	( call(Generator), call(Goal), fail ; true).



verify(Goal) :- \+ (\+(Goal)).


/* In sicstus lists library, but not in Quintus, so now put in quintus-version.
%   remove_duplicates(+List, ?Pruned)
%   is true when Pruned is like List but with all *identical* duplicate 
%   elements removed.

remove_duplicates([], []).
remove_duplicates([Head|Tail1], [Head|Tail2]) :- 
	delete(Tail1, Head, Residue),
        remove_duplicates(Residue, Tail2).
*/


%%% remove_test_duplicates(List,Test,Clean)
%%% Test is of the form:
%%%	test(CALL,Pat1,Pat2),
%%% where CALL uses Pat1 & 2..	
%%%
%%% EX:
%%% remove_test_duplicates([f(a,b),f(b,c),f(a,d),f(g,a),f(b,a)],
%%%   test((Pat1 = f(A,_),Pat2 = f(A,_)),Pat1,Pat2),Clean).
%%% Pat1 = f(a,d),
%%% A = a,
%%% Pat2 = f(a,b),
%%% Clean = [f(a,b),f(b,c),f(g,a),f(b,a)] 

remove_test_duplicates(List,Test,Clean) :-
	remove_test_duplicates(List,Test,[],Clean).

remove_test_duplicates([],_,_,[]).
remove_test_duplicates([H|T],Test_Call,Seen,Clean) :-
	Test_Call = test(Test,Pattern1,Pattern2),
	member(Pattern2,Seen),
	H = Pattern1,
	call(Test), !,
	remove_test_duplicates(T,Test_Call,Seen,Clean).
remove_test_duplicates([H|T],Test_Call,Seen,[H|Clean]) :-
	remove_test_duplicates(T,Test_Call,[H|Seen],Clean).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Arg2 is the average of Arg1, 0 if empty.

average(List,Avg) :- 
	length(List,N),
	( N > 0 -> sum_list(List,Sum),
	  Avg is Sum/N
	; Avg = 0 ).
 
% Arg2 is the maximum number in Arg1.

max([A|Rest],Val) :-
	max1(Rest,A,Val).

max1([],A,A).
max1([H|T],Old,Val) :-
	max(H,Old,New),
	max1(T,New,Val).

% Arg3 is the maximum of arg1 and arg2.
max(A,B,A) :- A > B, !.
max(_A,B,B).


% Arg2 is the minimum number in Arg1.
min([A|Rest],Val) :-
	min1(Rest,A,Val).

min1([],A,A).
min1([H|T],Old,Val) :-
	min(H,Old,New),
	min1(T,New,Val).

% Arg3 is the minimum of arg1 and arg2.
min(A,B,A) :- A < B, !.
min(_A,B,B).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeln(Arg) :- write(Arg), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reduce_list([A|B],R) :- !,
	reduce_list(A,A1),
	reduce_list(B,B1),
	append(A1,B1,R).
reduce_list([],[]) :- !.
reduce_list(X,[X]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increase_term_arity(TermIn,NewArg,TermOut) :-
   functor(TermIn,Func,N),
   N1 is N + 1,
   functor(TermOut,Func,N1),
   arg(N1,TermOut,NewArg),
   copy_args(N,TermIn,TermOut).

copy_args(0,_TermIn,_TermOut) :- !.
copy_args(N,TermIn,TermOut) :-
   arg(N,TermIn,Arg),
   arg(N,TermOut,Arg),
   N1 is N - 1,
   copy_args(N1,TermIn,TermOut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

portray_clauses(Clauses) :-
	whenever(member(C,Clauses),
	  portray_clause(C)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dotimes(0,_) :- !.
dotimes(N,Call) :- Call, N1 is N-1, dotimes(N1,Call).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% STABLE_SORT(+In,?Out)
% This is a stable version of built-in sort/2, which does
% not merge duplicate entries.  
% It is based on the stable built-in keysort/2, so we first
% convert the list into pairs.  It would be nice if prolog
% provided this built-in also!
stable_sort(In,Out) :- 
	pair_list(In,_,Keyed),
	keysort(Keyed,SortKeyed),
	pair_list(Out,_,SortKeyed).

% PAIR_LIST(?A,?B,?C)
% C is a list whose Nth member is A(N)-B(N). 
% pair_list([1,2,3],[a,b,c],[1-a,2-b,3-c]) 
pair_list([],[],[]).
pair_list([A|As],[B|Bs],[A-B|Rest]) :- 
	pair_list(As,Bs,Rest).

rev_pair_list([],[]).
rev_pair_list([A-AV|RestA],[AV-A|RestB]) :-
	rev_pair_list(RestA,RestB).


%============================================================================
% String Pattern Matching
%============================================================================

% CONTAINS(Symbol,Text) :- con(Text, ([],Symbol)).
% True if Symbol is contained in Text 
% Both are lists of characters.
% Will backtrack over all possibilities. 
% From Sahlin's phd thesis. 

contains(Symbol,Text) :- con(Text, ([],Symbol)).

con(_,(_,[])).
con([C|Rtext],SymbInfo) :- 
	new(C,SymbInfo,SymbInfoNew),
	con(Rtext,SymbInfoNew).

new(C,(Prefix,[C|RestPostfix]),(PrefixNew,RestPostfix)) :-
	append(Prefix,[C],PrefixNew).
new(C,(Prefix,[D|RestPostfix]),(PrefixNew,PostfixNew)) :-
	C \== D,
	append(Prefix,[C],H),
	append(PrefixNew,Rest,Prefix),
	append(_,PrefixNew,H),
	append(Rest,[D|RestPostfix],PostfixNew).


% FOUND(Symbol)
% 
% Read a sequence of chars until a pattern is found which matches each 
% char.  Uses Sahlin's routines above. 
	
found(Symbol) :- 
	found1(([],Symbol)).

found1((_,[])).
found1(SymbInfo) :- 
	get0(C),
	new(C,SymbInfo,SymbInfoNew),
	found1(SymbInfoNew).

%============================================================================
%   between(+Lower, +Upper, ?Number)
%   is true when Lower, Upper, and Number are integers,
%   and Lower =< Number =< Upper.  If Lower and Upper are given,
%   Number can be tested or enumerated.  If either Lower or Upper
%   is absent, there is not enough information to find it, and an
%   error will be reported.
% From shared code by Richard O'Keefe.

between(Lower, Upper, Point) :-
	integer(Lower),
	integer(Upper),
	(   integer(Point), !,		%  These cuts must be cuts;
	    Lower =< Point, Point =< Upper
	;   var(Point), !,		%  they can't be arrows.
	    Lower =< Upper,
	    between1(Lower, Upper, Point)
	).

%%  between1(Lower, Upper, Point)
%   enumerates values of Point satisfying Lower =< Point =< Upper,
%   where it is already known that Lower =< Upper and Point was a
%   variable.  A purer version of this is left as a comment.

between1(L, L, L) :- !.
between1(L, _, L).		% between1(L, U, L) :- L =< U.
between1(L, U, N) :-		% between1(L, U, N) :- L < U,
	M is L+1,		%	M is L+1,
	between1(M, U, N).	%	between1(M, U, N).
%============================================================================


numlist(Min,Max,[]) :- Min > Max, !.
numlist(Min,Max,[Min|Rest]) :-
	Min1 is Min+1,
	numlist(Min1,Max,Rest).



% Suceeds on the unique occurrence for A if
% it is constant, otherwise backtracks over B. 
member1(A,B) :- 
	( var(A) ->
	  member(A,B)
	; memberchk(A,B)
	).
	
%============================================================================
% Association Lists
%============================================================================

member1_pair(H-T,B) :- 
	( var(H) ->
	  member(H-T,B)
	; memberchk(H-T,B)
	).


assoc(List,Prop,Val) :- 
	member1_pair(Prop-Val,List).

set_assoc(List1,Param,Val,New) :- 
	ensure_list(List1,List2),
	delete_all_assoc(Param,List2,List),
	cons(Param-Val,List,New). 

ensure_list(List1,List1) :- is_list(List1), !.
ensure_list(_,[]).

delete_all_assoc(P,L,L2) :- 
	member(P-V1,L), !,
	delete(L,P-V1,L1),
	delete_all_assoc(P,L1,L2).
delete_all_assoc(_,L,L).

%============================================================================
% Regions
%============================================================================

% in_region([a-0.7,b-0.2,c-0.1],0.89,Choice).
in_region([Choice-_Prob],_,Choice) :- !.
in_region([C-P|_Choices],Prob,C) :-
	Prob < P, !.
in_region([_C-P|Choices],Prob,Choice) :-
	PRest is Prob - P,
	in_region(Choices,PRest,Choice).


