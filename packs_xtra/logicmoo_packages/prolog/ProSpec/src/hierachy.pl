%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% hierachy construction from prospec sort specification
%% ---------------------------------------------------------

%% Author: bernd thomas 
%% E-mail: bthomas@informatik.uni-koblenz.de
%% --------------------------------------------------
%% $Id: hierachy.pl,v 1.1 1998/02/10 18:01:01 bthomas Exp $
%% $Log: hierachy.pl,v $
%% Revision 1.1  1998/02/10 18:01:01  bthomas
%% Initial revision
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% build hierachy of sorts
%% get the subsort definitions and create open lists
%% and associate them with the last type of the hierachy
%% list: xx_typ(boy,[human,man,boy|_]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_hierachy :-
	( xx_sort(Child), \+ xx_subsort(Child subsort_of _)
    ;     
	  xx_subsort(Child subsort_of _Father) ),

	( \+ xx_sort(Child) ->
	    error(specsort(Child))
	; 
	    true 
        ),
	get_path(Child,Path),
%	append(Path,[Child|_],SPath),
	append(Path, _,SPath),	
	( xx_typ(Child,_) -> 
	    error(doublesort(Child))
	;
	  true ),
	assert(xx_typ(Child,SPath)),
	fail.

build_hierachy.

get_path(Child,Path) :-
	xx_subsort(Child subsort_of Father),
	get_path(Father,FPath),
	append(FPath,[Child],Path), !.

get_path(Child, Path) :-
	xx_subsort(Child eqisort_of Father),
	get_path(Father, Path),
	!.

get_path(X,[X]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% create prototyps of sorted preds,const & functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_rel_prototyps :-
	xx_relation(Relation),
	rel_proto(Relation),
	fail.

build_rel_prototyps.

build_func_prototyps :-
	xx_function(Relation),
	func_proto(Relation),
	fail.

build_func_prototyps.


% functions
func_proto(Functor : Sorts => Sort) :-
	term_list(Sorts,SortsL),
	length(SortsL,Le),
	check_double(Functor:Sorts => Sort,Le),
	length(EmptyArgs,Le),
	sorted_args(EmptyArgs,SortsL,ProtoSortArgs),
	HalfProtoTyp =.. [Functor|ProtoSortArgs],
	( nonvar(Sort) ->
	    check_s_o_s(Sort,OpenHierachy),
	    close_list(OpenHierachy,Hierachy)
	;
	    Hierachy = Sort ),
	ProtoTyp = (HalfProtoTyp : Hierachy),
	assert(xx_proto(Functor,ProtoTyp)), !.

% const
func_proto(Functor:Sort) :-
	( nonvar(Sort),
	  check_s_o_s(Sort,OpenHierachy),
          close_list(OpenHierachy,Hierachy)
        ;
	  Hierachy = Sort ),
	 check_double(Functor:Sort,0),
        assert(xx_proto(Functor,Functor:Hierachy)), !.

% pred
rel_proto(NullPred) :-
	\+ ( NullPred =.. [:|_] ),
	( \+ xx_proto(NullPred,NullPred),
	  assert(xx_proto(NullPred,NullPred))
        ;
	  true ), !.
		
rel_proto(Functor:Sorts) :-
	term_list(Sorts,SortsL),
	length(SortsL,Le),
	check_double(Functor:Sorts,Le),
	length(EmptyArgs,Le),
	sorted_args(EmptyArgs,SortsL,ProtoSortArgs),
	ProtoTyp =.. [Functor|ProtoSortArgs],
	assert(xx_proto(Functor,ProtoTyp)), !.

check_double(Functor:Sorts,Le) :-
	( xx_proto(Functor,Proto:_Sort)
        ;
	  xx_proto(Functor,Proto)
        ),
	functor(Proto,Functor,Le),
	error(double('in specification',Functor:Sorts)).

check_double(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extend the argument of a term with its typ of sort
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sorted_args([],[],[]).
sorted_args([Arg|MoreArgs],[SortTyp|MoreSortTyps],[Arg:Hierachy|MoreSorted]) :-
	( nonvar(SortTyp) -> 
	   check_s_o_s(SortTyp,Hierachy)
	;
	   Hierachy = SortTyp
        ),
	sorted_args(MoreArgs,MoreSortTyps,MoreSorted).

sorted_args(_,[S|_],_) :-
	error(sortunknown(S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% if we have a sort definition which is defined over a sort
% s and this sort s is explicitly defined, we have to insert
% the sort hierachy according to s.
%
% assume::   liste : list(nat)
% becomes::  liste : list([integer,nat|_]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%check_s_o_s(Sort,[_|_]) :-
%     var(Sort), !.
check_s_o_s(Sort,Sort) :-
	var(Sort), !.

check_s_o_s(SortTyp,Hierachy) :-
	SortTyp =.. [SortOver,Sort],
	nonvar(Sort),
	xx_typ(Sort,SortHierachy),
        SortOverSort =.. [SortOver,SortHierachy],
	xx_typ(SortOverSort,Hierachy).

check_s_o_s(SortTyp,Hierachy) :-
	xx_typ(SortTyp,Hierachy).

% special case for arguments of skolem function arguments
% e.g. Y : [s(U)|_]
check_s_o_s([_SortTyp|_],_H).
