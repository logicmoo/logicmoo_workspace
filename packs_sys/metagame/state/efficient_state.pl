%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% efficient_state.pl
%
% The naive representation of state just maintains the state as a list
% of all the preds true in it.  
%
% The following routines create an indexed structure for representing game state,
% where predicates are indexed according to functor/arity.  Thus, when finding which
% player is in control, we need not search past descriptors dealing with square occupancy,
% etc. 
%
% Within the entry for each predicate, the preds of that type currently true can
% also be impemented in a variety of ways, the most naive of which using a list  
% representation. 
% Here we are a bit more efficient, and index first on the functor of the predicate,
% and second on the particular arguments of it.  The method of indexing on arguments
% varies from predicate to predicate.  In particular, ON is represented as 
% an array. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- my_ensure_loaded(library(aux)).

%----------------------------------------
% The following are defined in sysdev.pl

% DYNAMIC_PREDS_FILE(File)

% INDEX_PREDS_FILE(File)
%----------------------------------------

%================================================================================
% Using state in predicates.
%================================================================================

% Efficient representation indexes state as a list of predicate entries,
% each of which is individually optimized.

% STATE Abstract Data Type.
% Supports following operations:
%
% new_state(-State) -- State is a new (empty) state, in which nothing is true.
%
% is_state(+State) -- True when State is a state.
%
% true_in(?Pred,+State)  -- Pred is true in State.
%
% add_in(+Pred,+StateIn,-StateOut) -- StateOut is like StateIn,
%     with the addition of Pred (which must be ground).
%
% del_in(+Pred,+StateIn,-StateOut) -- StateOut is like StateIn,
%     except that Pred (which must have been true in StateIn) is 
%     not true  in StateOut.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db indexing ADT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_state(state(S)) :-
	initialize_state([],S).

%%% INITIALIZE_STATE(+SIn,-SOut)
initialize_state(SIn,SOut) :-
	setof(P-I,pred_index(P,I),Indices),
	initialize_state_indices(Indices,SIn,SOut).

initialize_state_indices([],SIn,SIn).
initialize_state_indices([P-I|Is],SIn,SOut) :-
	initialize_state_index(P,I,SIn,S1),
	initialize_state_indices(Is,S1,SOut).
	

%%% INITIALIZE_STATE_INDEX(Pred,Index,SIn,S1) :-
%%% ensures that all the labels have initially empty lists,
%%% so that we can always be sure there is a list there
%%% when we find a label.

%initialize_state_index(_P,_I,SIn,[[]|SIn]).

initialize_state_index(P,_I,SIn,[Bucket|SIn]) :- 
	init_bucket(P,Bucket).

is_state(state(_)).

true_in(Pred,state(State)) :- db_true(Pred,State).

add_in(Pred,state(SIn),state(SOut)) :- db_add(Pred,SIn,SOut).

del_in(Pred,state(SIn),state(SOut)) :- db_del(Pred,SIn,SOut).


% Could optimize add/del key, by having
% find_bucket return a difference list and the
% rest of the list, so we can just replace here.
% This would save N traversals, where N is the
% pred_index of P (which bucket it is in).

db_true(P,SIn) :-
	db_key(P,Key), 
	in_key(Key,P,SIn).

db_add(P,SIn,SOut) :- 
	db_key(P,Key),
	add_key(Key,P,SIn,SOut).

% Ensure already inserted before deleting?
db_del(P,SIn,SOut) :- 
	db_key(P,Key),
	del_key(Key,P,SIn,SOut).

db_key(P,Key) :-
	pred_index(P,Key).

in_key(Key,P,SIn) :-
	find_bucket(Key,SIn,Bucket),
	in_bucket(P,Bucket).

add_key(Key,P,SIn,SOut) :-
	find_bucket(Key,SIn,Bucket),
	add_bucket(P,Bucket,Bucket1),
	set_bucket(Key,SIn,Bucket1,SOut).

del_key(Key,P,SIn,SOut) :-
	find_bucket(Key,SIn,Bucket),
	del_bucket(P,Bucket,Bucket1),
	set_bucket(Key,SIn,Bucket1,SOut).

find_bucket(Index,State,Bucket) :-
	nth_element(Index,State,Bucket).

set_bucket(Index,SIn,Bucket,SOut) :-
	set_nth_element(Index,SIn,Bucket,SOut).


%in_bucket(P,Bucket) :-
%	member_bag(P,Bucket).

%add_bucket(P,Bucket,Bucket1) :-
%	add_bag(P,Bucket,Bucket1).

%del_bucket(P,Bucket,Bucket1) :-
%	del_bag(P,Bucket,Bucket1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Using nested-term-arrays to represent the board
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% INIT_BUCKET

init_bucket(on(_,_),Bucket) :- !,
	new_game_board(Bucket).
init_bucket(_,[]).


empty_row(X,Term) :- 
	functor(Term1,row,X),
	empty_elements(X,Term1,Term).


empty_elements(0,Board,Board) :- !.
empty_elements(N,Board1,Board) :- 
	N>=1,
	emptify(N,Board1,Board2),
	N1 is N-1,
	empty_elements(N1,Board2,Board).

emptify(N,Board1,Board2) :-
	empty_filler(E),
	arg(N,Board1,E),
	Board1 = Board2.

% For a generic, unspecified state, all squares should
% be unbound.  A program to set up the board should thus
% either add them as empty or not. 
% 
%empty_filler(empty).
empty_filler(_).

%emptify(N,Board1,Board2) :-
%	arg(N,Board1,empty),
%	Board1 = Board2.


empty_board(X,Y,Term) :-
	functor(Term1,gameboard,Y),
	empty_rows(Y,X,Term1,Term).

empty_rows(0,_X,Board,Board) :- !.
empty_rows(N,X,Board1,Board) :- 
	N>=1,
	emptify_row(N,X,Board1,Board2),
	N1 is N-1,
	empty_rows(N1,X,Board2,Board).

emptify_row(N,X,Board1,Board2) :-
	empty_row(X,Row),
	arg(N,Board1,Row),
	Board1 = Board2.

new_game_board(Board) :-
	current_board_size(X,Y),
	empty_board(X,Y,Board).


%%% IN_BUCKET

in_bucket(on(Piece,square(X,Y)),Board) :- !,
%	path_arg([Y,X],Board,Piece).
	piece_on_square(X,Y,Board,Piece).
in_bucket(P,Bucket) :-
	member_bag(P,Bucket).


%%% ADD_BUCKET

add_bucket(on(Piece,square(X,Y)),A,A1) :- !,
%	change_path_arg([Y,X],A,A1,Piece).
	change_piece_on_square(X,Y,Piece,A,A1).

add_bucket(P,A,A1) :- 
	add_bag(P,A,A1).

del_bucket(on(_Piece,_Sq),E,E1) :- !,
	% Don't need to do anything here, since will always 
	% set it to something on next step!
	E=E1.
del_bucket(P,E,E1) :- !,
	del_bag(P,E,E1).


%----------------------------------------------------------------------
% Low-level board accessing
%----------------------------------------------------------------------
%:- assert(library_directory('/usr/groups/ailanguages/quintus3.1.1/generic/qplib3.1.1/library/')).

%:- my_use_module(library(changearg)).
%:- my_use_module(library(arg)).

:- my_use_module(library(args)).

piece_on_square(X,Y,Board,Piece) :- 
	path_arg([Y,X],Board,Piece).

change_piece_on_square(X,Y,Piece,Board1,Board2) :- 
	same_functor(Board1,Board2,YMax),
	change_item_in_column(YMax,Y,X,Board1,Piece,Board2).

change_item_in_column(Curr,Curr,X,Board1,Item,Board2)  :- !,
	corresponding_arg(Curr,Board1,Row1,Board2,Row2),
	same_functor(Row1,Row2,XMax),
	change_item_in_row(XMax,X,Row1,Item,Row2),
	Curr1 is Curr-1,
	copy_columns(Curr1,Board1,Board2).
change_item_in_column(Curr,Y,X,Board1,Item,Board2)  :- 
	same_arg(Curr,Board1,Board2),
	Curr1 is Curr-1,
	change_item_in_column(Curr1,Y,X,Board1,Item,Board2).
	
copy_columns(0,_Board1,_Board2) :- !.
copy_columns(Curr,Board1,Board2) :- 
	same_arg(Curr,Board1,Board2),
	Curr1 is Curr-1,
	copy_columns(Curr1,Board1,Board2).
	

change_item_in_row(Curr,Curr,Row1,Item,Row2)  :- !,
	corresponding_arg(Curr,Row1,_Old,Row2,Item),
	Curr1 is Curr-1,
	copy_rows(Curr1,Row1,Row2).
change_item_in_row(Curr,X,Row1,Item,Row2)  :- 
	same_arg(Curr,Row1,Row2),
	Curr1 is Curr-1,
	change_item_in_row(Curr1,X,Row1,Item,Row2).
	
copy_rows(0,_Row1,_Row2) :- !.
copy_rows(Curr,Row1,Row2) :- 
	same_arg(Curr,Row1,Row2),
	Curr1 is Curr-1,
	copy_rows(Curr1,Row1,Row2).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List implementation of arrays
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% nth_element(N,List,Elt):  rewrite of nth/3
% set_nth_element(N,List,Val,NewList)

nth_element(1,[H|_T],H) :- !.
nth_element(N,[_H|T],Nth) :- 
	Next is N-1,
	nth_element(Next,T,Nth).


nth_element_between(G,G,[H|Rest],[],H,Rest) :- !.
nth_element_between(Goal,Current,[H|T],[H|Before],Nth,Rest) :- 
	Next is Current+1,
	nth_element_between(Goal,Next,T,Before,Nth,Rest).

% set_nth_element(N,LIn,Val,LOut)
% LOut is a list LIn, with the Nth element replaced
% by Val.

set_nth_element(1,[_H|Rest],Val,[Val|Rest]) :- !.
set_nth_element(N,[H|T],Nth,[H|Rest]) :- 
	Next is N-1,
	set_nth_element(Next,T,Nth,Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BAG Abstract Data Type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  member_bag(Elem, Bag)

member_bag(Elem,[Elem|_Bag]).
member_bag(Elem,[_|Rest]) :- member_bag(Elem,Rest).


%   add_bag(Elem, Bag1, Bag2)
%   is true when Bag1 and Bag2 are bags represented as unordered lists,
%   and Bag2 = Bag1 U {Elem}.  It may only be used to calculate Bag2
%   given Elem and Bag1.  However, if Bag1 is a list with a variable at
%   the end, it may still be used, and will add new elements at the end.

add_bag(Elem, Bag,[Elem|Bag]).


%   del_bag(Elem, Bag1, Bag2)
%   is true when Bag1 and Bag2 are bags represented as unordered lists,
%   and Bag2 = Bag1 \ {Elem}.  It may only be used to calculate Bag2
%   given Elem and Bag1.  If Bag1 does not contain Elem, this fails.
%   If Set1 is not an unordered list, but contains more than one copy of Elem,
%   only the first will be removed.

%del_bag(Elem, [Elem|Bag2], Bag2) :- !.
%del_bag(Elem, [X|Bag1], [X|Bag2]) :- !,
%	del_bag(Elem, Bag1, Bag2).

del_bag(Elem, [H|T], Rest) :-
	Elem = H 
         -> Rest = T
         ;  Rest = [H|Rest1],
	    del_bag(Elem,T,Rest1).
	    

%================================================================================
% Indexing Dynamic Predicates.
%================================================================================

% Here we precompute a unique integer index for each dynamic predicate.
% This relies on the domain theory not changing after we've done this 
% optimization.


index_dynamic_preds :-
	index_preds_file(File),
	index_dynamic_preds_to_file(File),
	compile(File).

index_dynamic_preds_to_file(File) :-
	format("~nIndexing Dynamic Predicates to file: ~w~n",[File]),
	assert_pred_indices,
	tell(File),
	listing(pred_index),
	told,
	retractall(pred_index(_,_)).

assert_pred_indices :-
	abolish(pred_index/2),
	pred_index_slow(Pred,Index),
	assert(pred_index(Pred,Index)),
	fail
        ; true.

% This is the predicate we partially execute to form an indexed table. 
pred_index_slow(Pred,Index) :-
	dynamic_predicates(Preds),
	nth(Index,Preds,Pred).

