%%- -*-Mode: Prolog;-*--------------------------------------------------
%%
%% $Revision: 1.4 $
%%
%% File  : prdx2p9.pl
%%
%% Author: Josef Urban
%%
%%  Translate Paradox models to Prover9 models.
%%
%%  :- prdx2p9(FileIn, FileOut).
%%------------------------------------------------------------------------


%% prdx2p9(+FileIn, +FileOut)
%%
%% Consult a perl-postprocessed Paradox model in FileIn,
%% and print it as a Prover9 model to FileOut.
%% The Paradox model has to contain:
%% - the cardinality info, - the functions/arity list,
%% - the predicate/arity list, - 
prdx2p9(FileIn, FileOut):-
	consult(FileIn),
	domain_size(Size),
	functors(Functors),
	predicates(Predicates),
	tell(FileOut),
	write('interpretation( '),
	write(Size),
	write(', [number = 1,seconds = 0], ['),
	nl,
	Last is Size - 1,
	my_numlist(0,Last,Elems),
	print_functors(Elems,Functors),
	(
	  (Functors = [_|_], Predicates = [_|_]) ->
	  write(','), nl
	;
	  true
	),
	print_predicates(Elems,Predicates),
	write(']).'),
	nl,
	told,!.

prdx2p9(_,_):- write('something went wrong'),nl.

%% own implementation of SWI's numlist/3, for iso-compat
my_numlist(First, Last, [First|Res]) :-
	(
	  First=:=Last ->
	  Res=[]
	;
	  Next is First+1,
	  my_numlist(Next, Last, Res)
	).


%% generate permutations with repetition,
%% hopefully in the same implcit order as used by Mace4 format.
gen_perm(_,0,[]):- !.
gen_perm(Elems,Arity,[M|Perm1]):-
	A1 is Arity - 1,
	member(M,Elems),
	gen_perm(Elems, A1, Perm1).

%% generate permutation and eval a functor on it
get_functor_res(Elems,Arity,Name,Res):-
	gen_perm(Elems, Arity, Perm),
	Call =.. [Name, Perm, Res],
	call(Call).

%% generate permutation and eval a predicate on it
get_pred_res(Elems,Arity,Name,Res):-
	gen_perm(Elems, Arity, Perm),
	Call =.. [Name, Perm],
	(
	  call(Call) -> Res = 1;
	  Res = 0
	).

%% list of N underscores separated by commas, N >0
ulist(1,['_']):-!.
ulist(N,['_',','|L]):- N1 is N-1, ulist(N1,L).

%% list with commas without spaces
my_writelist([H]):-!, write(H).
my_writelist([H|T]):-!, write(H), write(','), my_writelist(T).
my_writelist([]):-!.


%% print one functor interpretation in Mace4 format
print_functor(Elems,Arity,Name):-
	findall(Res, get_functor_res(Elems,Arity,Name,Res), Ress),
	write('function('),
	print_res(Arity, Name, Ress).

%% print one predicate interpretation in Mace4 format
print_pred(Elems,Arity,Name):-
	findall(Res, get_pred_res(Elems,Arity,Name,Res), Ress),
	write('relation('),
	print_res(Arity, Name, Ress).

%% print the functor/predicate independent stuff
print_res(Arity, Name, Ress):-
	(
	  (Arity == 0) ->
	  write(Name)
	;
	  ulist(Arity, UL),
	  atom_chars(Arg1, UL),
	  Term =.. [Name | [Arg1]],
	  write(Term)
	),
	write(', ['),
	my_writelist(Ress),
	write('])').

%% print all functors
print_functors(_,[]):- !.
print_functors(Elems,[N/A]):- !,print_functor(Elems,A,N).
print_functors(Elems,[N/A | Rest]):-
	print_functor(Elems,A,N),
	write(','), nl,
	print_functors(Elems,Rest).	

%% print all predicates
print_predicates(_,[]):- !.
print_predicates(Elems,[N/A]):- !,print_pred(Elems,A,N).
print_predicates(Elems,[N/A | Rest]):-
	print_pred(Elems,A,N),
	write(','), nl,
	print_predicates(Elems,Rest).	
	
%prdx2p9(FileIn, FileOut).


