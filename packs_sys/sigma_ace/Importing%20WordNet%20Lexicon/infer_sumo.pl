%-------------------------------
% CODE FOR USING STANDARD UPPER ONTOLOGY
%-------------------------------

% Copyright © 2002 Teknowledge Corporation
% All Rights Reserved. This software is delivered under Goverment Purpose Rights (GPR) only.
:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement
:- style_check(-atom).           % allows strings more than 5 lines long

% is_subclass(+X,+Y) succeeds if X is a subclass of Y
% e.g., is_subclass('Bird','Animal'). If X is a list then
% it succeeds if any element of X is a subclass of Y.

% Note: we do not check for loops in the hierarchy but
% trust that the SUMO is free of class cycles.

is_subclass(X,Y) :- subclass(X,Y).
is_subclass(X,Z) :- subclass(X,Y),is_subclass(Y,Z).

% Handle first var being a list,
% E.g., is_subclass(['Male','Human','FullyFormed'],'Agent') is true.
is_subclass([First|Rest],Y) :- is_subclass(First,Y);is_subclass(Rest,Y). 

% is_subclass_or_equal(+X,+Y) succeeds if X is a subclass of Y or equal to Y
% e.g., is_subclass('Bird','Animal').

is_subclass_or_equal(X,X) :- !.
is_subclass_or_equal(X,Y) :- subclass(X,Y).
is_subclass_or_equal(X,Z) :- subclass(X,Y),is_subclass_or_equal(Y,Z).

is_subclass_or_equal([First|Rest],Y) :- is_subclass_or_equal(First,Y);is_subclass_or_equal(Rest,Y). % Handle first var being a list

% ancestors(+X,-Y) finds all the ancestor classes of X.
% e.g., ancestors('Bird',X).

ancestors(Class,Ancestors) :- findall(Parent,is_subclass(Class,Parent),Ancestors).

ancestors([First|Rest],Ancestors) :-
	ancestors(First,First_Set),ancestors(Rest,Rest_Set),union(First_Set,Rest_Set,Ancestors). % Handle first var being a list

% descendants(+X,-Y) finds all the descendant classes of X.
% e.g., descendants('Vertebrate',X).

descendants(Parent,Descendants) :- findall(Descendant,is_subclass(Descendant,Parent),Descendants).

descendants([First|Rest],Descendants) :-
	descendants(First,First_Set),descendants(Rest,Rest_Set),union(First_Set,Rest_Set,Descendants). % Handle first var being a list

% instances(+X,-Y) finds all instances of class X.
% e.g., instances('BinaryPredicate',X).
% e.g., instances('Relation',I),print_list(I).

instances(Parent,Instances) :-
	descendants(Parent,Descendants),
	findall(Instance,(member(Descendant,[Parent|Descendants]),instance(Instance,Descendant)),Instances).

% instance_of(+Instance,+Ancestor) is true if Instance is an instance of the Ancestor class or one of its subclasses.
% e.g., instance_of(range,'BinaryPredicate').

instance_of(Instance,Ancestor) :- instance(Instance,Ancestor),!.
instance_of(Instance,Ancestor) :- instance(Instance,Subclass),is_subclass(Subclass,Ancestor).

% is_subrelation(+X,+Y) succeeds if X is a subrelation of Y.
% e.g., is_subrelation('Bird','Animal').

is_subrelation(X,Y) :- subrelation(X,Y).
is_subrelation(X,Z) :- subrelation(X,Y),is_subrelation(Y,Z).

% ancestor_relations(+X,-Y) finds all the ancestor relations of X.
% e.g., ancestor_relations('containsInformation',X).

ancestor_relations(Relation,Ancestors) :- findall(Parent,is_subrelation(Relation,Parent),Ancestors).

% descendant_relations(+X,-Y) finds all the descendant relations of X.
% e.g., descendant_relations('refers',X).

descendant_relations(Parent,Descendants) :- findall(Descendant,is_subrelation(Descendant,Parent),Descendants).

% type_check(+Relation,+Arg1,+Arg2)
% checks that the arguments to the relation are of the proper types and prints warnings when
% they are not.

% e.g., type_check(property,'Animal','Red').
% e.g., type_check(property,'Animal','Bird').

type_check(Relation,Arg1,Arg2) :-
	check_arg(Relation,1,Arg1),
	check_arg(Relation,2,Arg2).

% check_arg checks that argument number Index of relation Relation
% is of the proper type.

check_arg(Relation,Index,Arg) :-
	domain(Relation, Index, Type),
	format("Arg ~d of relation ~a, ~a,  should be of type ~a",[Index,Relation,Arg,Type]),
	( check_arg_type(Arg,Type) -> format(" and it is.~n"); format(" but it is NOT.~n") ).

% check_arg_type(+Arg,+Class) is true if Arg satisfies the type restrictions of Class
% by being either an instance or subclass of it.

check_arg_type(Arg,Arg).
check_arg_type(Arg,Class) :- is_subclass(Arg,Class).
check_arg_type(Arg,Class) :- instance_of(Arg,Class).
	