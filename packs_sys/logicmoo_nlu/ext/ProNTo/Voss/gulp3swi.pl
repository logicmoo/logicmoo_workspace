% GULP ready to compile for SWI Prolog.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File GULP3SWI.PL
% Michael A. Covington
% Artificial Intelligence Center
% The University of Georgia
% Athens, Georgia 30602-7415
% July 28, 1994; April 18, 2001; January 30, 2003; August 9, 2004
%
%---------------------------------------------------------------------------%
%
%   GULP -- Graph Unification and Logic Programming
%   Version 3.1aa  (with module support turned on)
%
%   CONDITIONS OF DISTRIBUTION:
%   Copyright (C) 1994 Michael A. Covington.
%   Portions copyright 1988 Quintus Computer Systems, Inc.
%   GULP is distributed with no warranty.
%
%   For documentation see "GULP 3.1: An Extension of Prolog
%   for Unification-Based Grammar," available as a research
%   report from the above address.
%
%%  Modified for use with SWI-Prolog 2.x September 1995.  Most of my
%%  changes can be found by searching for `%%  ' and/or `gulp__'.
%%  Last modified March 1996 by martin.jansche@urz.uni-heidelberg.de
%
%---------------------------------------------------------------------------%
%
% HOW TO RUN GULP:
%  The simplest way is to simply consult gulp3.pl into Prolog.
%  However, you can also build executable versions of Prolog
%  (including the compiler) in which GULP is built-in.
%
%  ... non-SWI instructions deleted here ...
%
%
% TO BUILD THE SWI-PROLOG VERSION (tested in SWI-Prolog 2.1.14 and 2.5.2):
%   (1) Edit this file:
%        Uncomment SWI-Prolog specific code both at the beginning
%        and at the end of this file.
%   (2) From your shell:
%        prompt$ plcon -O -o gulp -c gulp3.pl
%                                 ^^^^^^^^ this file 
%       Then use file 'gulp' as the executable.  It is a script
%       which will call SWI-Prolog with GULP loaded into it.
%   (3) If you use the Quintus Gnu Emacs interface that comes with
%       SWI-Prolog, cf. item (3) in the above instructions for building
%       the Quintus version.
%
%---------------------------------------------------------------------------%
%
% New in Version 3.1:
%   R. O'Keefe's DCG translator is built into GULP and is used in place
%     of whatever may be built into the implementation.
%     This gives greater portability and tighter integration of
%     GULP with the Prolog environment.
%
% New in Version 3.0:
%   GULP translation is performed automatically
%     by consult, reconsult, and compile.  The old user interface
%     (ed, load, etc.) is not used.
%   Considerably more efficient data structures are used to represent
%     feature structures internally.
%   Declarations (g_features) are still optional, but there is a gain
%     in efficiency if some or all features are declared.
%
% New in Version 2.0:
%   The separator for feature-value pairs is .. rather than ::. For
%     compatibility, :: is still accepted.
%   A completely different method of translation using stored schemas,
%     resulting in much faster translation of GULP notation into
%     the internal representation for feature structures and vice versa.
%   The g_features clause is OPTIONAL.
%   Many minor changes have been made to the utility predicates
%     available to the user.
%   Backtranslation of feature structures containing variables is
%     now correct.
%   Nested loads are now supported. That is, a file being loaded can
%     contain a directive such as ':- load file2.' which will be
%     executed correctly.  /* Dropped in GULP 3. */
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*****************************
 * Source file compatibility *
 *****************************/

% Make sure the right lines are commented out.


%%  Quintus Prolog
%%
/***************************************************************************

% :- write('This is the Quintus Prolog source code.'), nl.

g_version('GULP 3.1a for Quintus Prolog, 28 July 1994, 30 January 2003').

:- g_version(X), version(X).

:- public g_translate/2.
:- public g_display/1.
:- public display_feature_structure/1.
:- public g_fs/1.
:- public g_not_fs/1.
:- public g_vl/1.
:- public g_printlength/2.
:- public writeln/1.
:- public member/2.
:- public remove_duplicates/2.
:- public call_if_possible/1.
:- public g_herald/0.

gulp__dont_translate(Term) :-
	( var(Term)
	; atom(Term)
	; number(Term)
	).

member(A,B) :- gulp__member(A,B).
%%  or :- use_module(library(basics), [member/2]).

call_if_possible(A) :- gulp__call_if_possible(A).

gulp__window_title.

***************************************************************************/


%%  LPA Prolog
%%
/***************************************************************************

:- write('This is the LPA Prolog source code.'), nl.

g_version('GULP 3.1a for LPA 386-Prolog, 28 July 1994').

gulp__dont_translate(Term) :-
	( var(Term)
	; atom(Term)
	; number(Term)
	; string(Term)
	).

call_if_possible(A) :- call(A).

gulp__window_title :-
	(  wtext((0,0), `GULP + LPA 386-Prolog`)  % main window title
	-> true
	;  true
	).

***************************************************************************/


%%  SWI-Prolog 2.0 and above
%%


%%  Uncomment this part if you want module support (probably unstable).
%%
%%  N.B.: No clause or directive may precede the :-module/2 directive.
%%

:- module(gulp3,
	  [
	  g_translate/2,
	  g_display/1,
	  display_feature_structure/1,
	  g_fs/1,
	  g_not_fs/1,
	  g_vl/1,
	  g_printlength/2,
%	  writeln/1,
	  remove_duplicates/2,
	  call_if_possible/1,
	  g_herald/0
	  ]).

:- module_transparent call_if_possible/1.   %%  a meta predicate

%   :- format('This is the SWI-Prolog source code.~n').

g_version('GULP 3.1aa for SWI-Prolog, 28 July 1994, 30 January 2003').

gulp__dont_translate(Term) :-
	var(Term),
	!.
gulp__dont_translate(Term) :-
	atomic(Term),
	!.
gulp__dont_translate(Module:_) :-
	current_module(Module).
	%%  If Module is the name of a new module -- such as in
	%%  assert(foo:baz), which creates a module `foo' if that did
	%%  not exist before -- we are in trouble.

call_if_possible(A) :- gulp__call_if_possible(A).

gulp__window_title.



%%***************************************************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/************************
 * Start of GULP proper *
 ************************/


/*************************
 * Operator declarations *
 *************************/

% These are repeated in the LPA Prolog initialization, below (at end).

:- op(600, xfy, ':').
:- op(602, xfy, '..').
:- op(602, xfy, '::').


/**********************
 * Other declarations *
 **********************/

:- dynamic   g_features/1.
:- multifile g_features/1.

:- dynamic g_forward_schema/3.
:- dynamic g_backward_schema/2.


/******************************************************************
 * Translation of feature structures to value lists or vice versa *
 ******************************************************************/

g_translate(X,X) :-
	var(X),
	!.       /* Rare case, but not covered by other clauses */

g_translate(Structure,List) :-
	var(List),
	!,
	nonvar(Structure),
	g_tf(Structure,List).

g_translate(Structure,List) :-
	nonvar(List),
	g_tb(Structure,List).


/*************************************************************
 * Translation backward -- value lists to feature structures *
 *************************************************************/

/*
 * g_tb(FeatureStructure,ValueList)     "Translate Backward"
 *
 *   Translates backward using g_backward_schema.
 */


g_tb(Value, Value) :-
	gulp__dont_translate(Value),
        !.

        /* Variables and atomic terms do not need any conversion. */

g_tb(FS,Term) :-
	\+ functor(Term,g__,_),
	!,
	Term =.. [Functor | Args],
	g_tb_list(NewArgs,Args),
	FS =.. [Functor | NewArgs].

        /* Term is a structure, but not a value list.
           Recursively convert all its arguments, which
           may be, or contain, value lists. */

g_tb(FS,Term) :-
	call(g_backward_schema(RawFS,Term)),   %%  Why is call/1 here?
	g_tb_fixup(RawFS,FS).

        /* If we get here, we know Term is a value list. */


/*
 * g_tb_fixup(RawFeatureStructure,FeatureStructure)
 *
 *   Reverses the order of the feature:value pairs.
 *   Recursively backtranslates the values.
 *   Also discards pairs with uninstantiated value.
 */

g_tb_fixup(F:V,Result) :-                  /* Singleton case */
	g_tb_fixup_rest(F:V,_,Result).

g_tb_fixup(F:V..Rest,Result) :-
	g_tb(BTV,V),
	g_tb_add(F:BTV,_,FV),
	g_tb_fixup_rest(Rest,FV,Result).   /* Start the recursion */

g_tb_fixup_rest(F:V..Rest,ResultSoFar,Result) :-
	g_tb(BTV,V),
	g_tb_add(F:BTV,ResultSoFar,FVR),
	g_tb_fixup_rest(Rest,FVR,Result).  /* Continue the recursion */

g_tb_fixup_rest(F:V,ResultSoFar,FVR) :-
	g_tb(BTV,V),
	g_tb_add(F:BTV,ResultSoFar,FVR).   /* End the recursion */


g_tb_add(_:V,R,R)          :- var(V), !.   /* Unmentioned variable */
g_tb_add(F:g_(V),R,F:V)    :- var(R).      /* First contribution
                                                        to empty R */
g_tb_add(F:g_(V),R,F:V..R) :- nonvar(R).   /* Ordinary case */


/*
 * g_tb_list(FeatureStructureList,ValueListList)
 *
 *   Applies g_tb to ValueListList giving FeatureStructureList.
 */

g_tb_list([],[]).

g_tb_list([FH|FT],[VH|VT]) :-
	g_tb(FH,VH),
	g_tb_list(FT,VT).



/************************************************************
 * Translation forward -- feature structures to value lists *
 ************************************************************/

/*
 * This is more complicated than translation backward because any
 * feature can occur anywhere in the feature structure. If several
 * features are specified, separate value lists are constructed
 * for them and then unified. Recursion is performed because the
 * the value of a feature structure may itself be a feature structure.
 */

/*
 * g_tf(FeatureStructure,ValueList)     "Translate Forward"
 *
 *  Recursively examines FeatureStructure and replaces all
 *  feature structures with equivalent value lists.
 */


g_tf(Term,Term) :-
	gulp__dont_translate(Term),
        !.

        /* Simplest and most frequent case: Term is atomic. */

g_tf(Term,_) :-
	g_not_fs(Term),
	functor(Term,X,_),
	(X = ':' ; X = '..' ; X = '::'),
	!,
	g_error(['Invalid GULP punctuation: ' ,Term]).

        /* If Term is a structure with a colon as its functor,
           but is not a valid feature structure, then we have
           a syntax error. */

g_tf(Term,NewTerm) :-
	g_not_fs(Term),
	!,
	Term =.. [Functor|Args],
	g_tf_list(Args,NewArgs),
	NewTerm =.. [Functor|NewArgs].

        /* Term is a structure, but not a feature structure.
           Recurse on all its arguments, which may be, or
           contain, feature structures. */

g_tf(Feature:Value,ValueList) :-
	!,
	g_tf(Value,NewValue),
	g_tfsf(Feature,g_(NewValue),ValueList).

        /* We have a Feature:Value pair. Recursively
           translate the value, which may itself be
           or contain a feature structure, and then
           convert Feature:NewValue into a value list
           in which only one value is specified. */

        /* In Version >=2, this adds g_/1 in front
           of every value actually mentioned in
           the program. */


g_tf(FeatureStructure .. Rest,ValueList) :-
	!,
	g_tf(FeatureStructure,VL1),
	g_tf(Rest,VL2),
	g_unify(FeatureStructure..Rest,VL1,VL2,ValueList).

        /* A compound feature structure is handled by
           translating all the feature structures
           individually and then unifying the resulting
           value lists. */


g_tf(FeatureStructure :: Rest,ValueList) :-
	g_tf(FeatureStructure .. Rest,ValueList).

        /* Older notation is still accepted for
           compatibility. */


/*
 * g_tf_list(ListOfTerms,ListOfResults)  "Translate Forward List"
 *
 *       Applies g_tf to a list of arguments giving a list of results.
 */


g_tf_list([],[]).

g_tf_list([H|T],[NewH|NewT]) :-
	g_tf(H,NewH),
	g_tf_list(T,NewT).


/*
 * g_tfsf(Keyword,Value,ValueList)      "Translate Forward Single Feature"
 *
 *      Turns a keyword and a value into a value list in which
 *      only one feature is specified.
 */


/*  Totally new in version 2.0  */

g_tfsf(Keyword,Value,ValueList) :-
	call_if_possible(g_forward_schema(Keyword,Value,ValueList)),
        !.

g_tfsf(Keyword,Value,ValueList) :-
	writeln(['% Generating declaration for feature: ',Keyword]),
	( retract(g_features(List)) ; List = [] ),
	!,   /* the above line should not generate alternatives */
	append(List,[Keyword],NewList),
	asserta(g_features(NewList)),
	g_add_another_feature(Keyword),
	!,
	g_tfsf(Keyword,Value,ValueList).
             /* Try again, and this time succeed! */



/********************************
 * Output of feature structures *
 ********************************/

/*
 * g_display(X)
 *
 *   Equivalent to display_feature_structure(X).
 *   Retained for compatibility.
 *
 */

g_display(X) :- display_feature_structure(X).


/*
 * display_feature_structure(X)
 *
 *   Writes out a feature structure in a neat indented format.
 *   Feature structure can be in either Feature:Value notation
 *   or internal representation.
 */

display_feature_structure(Term) :-
	g_tb(FS,Term), /* Convert value lists into feature structures */
	g_di(0,0,FS).  /* Display them */


/*
 * g_di(CurPos,Indent,FS)     "Display Indented"
 *
 *   CurPos is the current position on the line;
 *   Indent is the indentation at which this item should be printed.
 */

% This could be made more efficient by changing the order of
% arguments so that indexing on the first argument would work.

g_di(CurPos,Indent,Variable) :-
	var(Variable),
	!,
	g_di_tab(Indent,CurPos),
	write(Variable),
	nl.

g_di(CurPos,Indent,F:V..Rest) :-
	!,
	g_di(CurPos,Indent,F:V),
	g_di(0,Indent,Rest).

g_di(CurPos,Indent,F:V::Rest) :-
	!,
	g_di(CurPos,Indent,F:V..Rest).  /* For compatibility */

g_di(CurPos,Indent,F:V) :-
	!,
	g_di_tab(Indent,CurPos),
	write(F), write(': '),
	g_printlength(F,PL),
	NewIndent is Indent+PL+2,
	g_di(NewIndent,NewIndent,V).

g_di(CurPos,Indent,OrdinaryTerm) :-
	g_di_tab(Indent,CurPos),
	write(OrdinaryTerm),
	nl.



g_di_tab(Indent,CurPos) :-
	Tabs is Indent-CurPos,
	tab(Tabs).


/**************************************
 * Maintenance of translation schemas *
 **************************************/

/*
 * g_make_backward_schema
 *
 *   Makes a backtranslation schema containing all
 *   possible features in both external and internal notation,
 *   e.g., g_backward_schema(c:Z..b:Y..a:X,g__(..etc..)).
 */

g_make_backward_schema :-
	retractall(g_backward_schema(_,_)),
	bagof((Feature:Value)/Schema,
	      g_forward_schema(Feature,Value,Schema),
	      [((F:V)/S)|Rest]),
	g_make_whole_aux(Rest,F:V,S).


g_make_whole_aux([],FSSoFar,SchemaSoFar) :-
	assertz(g_backward_schema(FSSoFar,SchemaSoFar)).

g_make_whole_aux([((F:V)/S)|Rest],FSSoFar,SchemaSoFar) :-
	NewFS = (F:V .. FSSoFar),
	SchemaSoFar = S,  /* unify SchemaSoFar with S */
	%%  Why is explicit unification used in this case?
	g_make_whole_aux(Rest,NewFS,SchemaSoFar).

/*
 * Defaults, in case the user never declares any features
 */

g_backward_schema('no features declared',g__(_)).


/*
 * g_make_forward_schemas(List)
 *
 *   Given a list of feature names, makes and stores a
 *   set of forward translation schemas for them.
 */

g_make_forward_schemas(List) :-
	length(List,L),
	L1 is L+1,
	g_make_forward_schemas_aux(List,2,L1).

g_make_forward_schemas_aux([Feature|Rest],N,L) :-
	functor(Schema,g__,L),
	arg(N,Schema,Value),
	assertz(g_forward_schema(Feature,Value,Schema)),
%Test   write('[asserting g_forward_schema('),write(Feature),write('...)]'),nl,
	N1 is N+1,
	g_make_forward_schemas_aux(Rest,N1,L).

g_make_forward_schemas_aux([],_,_).



/*
 * g_add_another_feature(Feature)
 *      adds a feature by further instantiating the first element
 *      of the schema, which is an open list (initially _);
 *      creates a forward schema, and updates the backward schema.
 */

g_add_another_feature(Feature) :-
	g_backward_schema(_,Schema),
	arg(1,Schema,Hook),

	% further instantiate Hook,
	g_add_another_feature_aux(Hook,Value),

	assertz(g_forward_schema(Feature,Value,Schema)),
%Test   write('[asserting g_forward_schema('),write(Feature),write(']'),nl,
	g_make_backward_schema.


g_add_another_feature_aux(Hook,Value) :-
	var(Hook),
	!,
	Hook = [Value|_].

g_add_another_feature_aux([_|Hook],Value) :-
	g_add_another_feature_aux(Hook,Value).




/****************************
 * Miscellaneous predicates *
 ****************************/

/*
 * g_fs(X)       "Feature Structure"
 *
 *   Succeeds if X is a feature structure.
 */

g_fs(X : _)  :- atom(X).
g_fs(X .. Y) :- g_fs(X), g_fs(Y).
g_fs(X :: Y) :- g_fs(X), g_fs(Y).  /* For compatibility */

/*
 * g_not_fs(X)   "Not a Feature Structure"
 *  (Avoids use of "not" in compiled Arity Prolog.)
 */

g_not_fs(X) :- g_fs(X), !, fail.
g_not_fs(_).


/*
 * g_vl(X)          "Value List"
 *
 *   Succeeds if X is a value list.
 */

g_vl(Term) :- functor(Term,g__,_).


/*
 * g_unify(Text,X,Y,Z)
 *      Unifies X and Y giving Z.
 *      If this cannot be done, Text is used in an
 *      error message.
 */

g_unify(_,X,X,X) :- !.

g_unify(Text,X,Y,_) :-
	\+ (X = Y),
	g_error(['Inconsistency in ',Text]).


/*
 * g_printlength(Term,N)
 *
 *     N is the length of the printed representation of Term.
 */

g_printlength(Term, N) :-
	name(Term, List),
	!,
	length(List, N).

g_printlength(_,0).  /* if not easily computable, we probably don't
                        need an accurate value anyhow */

/*
 * g_error(List)
 *    Ensures that i/o is not redirected,
 *    then displays a message and aborts program.
 */

g_error(List) :-
	repeat,
	  seen,
	  seeing(user),
	!,
	repeat,
	  told,
	  telling(user),
	!,
	writeln(['GULP ERROR: '|List]),
	abort.


/**************************************
 *           I/O utilities            *
 **************************************/

:- redefine_system_predicate(writeln(_)).  % new 2003

/*
 * writeln(List)
 *   writes the elements of List on a line, then
 *   starts a new line. If the argument is not a list,
 *   it is written on a line and then a new line is started.
 *   Any feature structures found in List are converted
 *   to Feature:Value notation.
 */

writeln(X) :- g_tb(TranslatedX,X), writeln_aux(TranslatedX).

writeln_aux(X) :- var(X), !, write(X), nl.
writeln_aux([]) :- !, nl.
writeln_aux([H|T]) :- !, write(H), writeln(T).
writeln_aux(X) :- write(X), nl.



/**************************************
 * Filling gaps in particular Prologs *
 **************************************/

/* These are built-in predicates from other Prologs that
   are defined here for implementations that lack them. */

/*
 * append(X,Y,Z)
 *   concatenates lists X and Y giving Z.
 *   Has interchangeability of unknowns.
 */

gulp__append([],   X, X).
gulp__append([H|T],X, [H|Y]) :-
	gulp__append(T, X, Y).


/*
 * member(Element,List)
 *   succeeds if Element is in List.
 *   Has interchangeability of unknowns.
 */

gulp__member(X, [X|_]).
gulp__member(X, [_|Y]) :-
	gulp__member(X,Y).

/*
 * remove_duplicates(List1,List2)
 *    makes a copy of List1 in which only the
 *    first occurrence of each element is present.
 *    List1 must be instantiated at time of call.
 */

remove_duplicates(X,Y) :-
	rem_dup_aux(X,Y,[]).

rem_dup_aux([],[],_).

rem_dup_aux([H|T],X,Seen) :-
	member(H,Seen),
	!,
	rem_dup_aux(T,X,Seen).

rem_dup_aux([H|T],[H|X],Seen) :-
	rem_dup_aux(T,X,[H|Seen]).


/*
 * retractall(Predicate)
 *    retracts all clauses of Predicate, if any.
 *    Always succeeds.
 */

gulp__retractall(Head) :-
	\+ ( clause(Head,_,Ref), \+ erase(Ref) ).


/*
 * phrase(PhraseType,InputString)
 *   Initiates DCG parsing.
 *   For example, ?- phrase(s,[the,dog,barks]) is
 *   equivalent to ?- s([the,dog,barks],[]).
 */

gulp__phrase(PhraseType, InputString) :-
	call(PhraseType, InputString, []).
%%  if your Prolog does not support call/[2-6] use
%%	apply(PhraseType, [InputString, []]).


/*
 * call_if_possible(Goal)
 *   Calls Goal.
 *   If there are no clauses for the predicate,
 *   the call fails but an error condition is not raised.
 */

gulp__call_if_possible(Goal) :-
	current_predicate(_, Goal),
	call(Goal).


/**********
 * Herald *
 **********/

g_herald :-
	gulp__window_title,
	nl,
	g_herald_line(64),
	g_version(X),
	write(X), nl,
	write('Copyright 1994 Michael A. Covington'), nl,
	write('Portions copyright 1988 Quintus Computer Systems, Inc.'), nl,
	write('and used by permission'), nl,
	g_herald_line(64),
	nl.

g_herald_line(0) :- !, nl.
g_herald_line(N) :- N>0, write('-'), NewN is N-1, g_herald_line(NewN).

/**********************
 * End of GULP proper *
 **********************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***********************
 * DCG rule translator *
 ***********************/

/* From... */
%   File   : DCG.PL
%   Author : Richard A. OKeefe
%   Updated: Tuesday July 26th, 1988.
%   Purpose: Definite Clause Grammar rule to Prolog clause translator.

%%% Portions commented out by Michael Covington, 1994,
%%% for compatibility with LPA 386-Prolog and Quintus Prolog.
%%% Modified not to use 'C'.
%%% No other alterations have been made.

/*  This file is written in the ISO 8859/1 character set.  The "Copyright"
    line contains after the right parenthesis a character which when
    transmitted was character 169, the international copyright symbol.

    Copyright (C) 1988, Quintus Computer Systems, Inc.

    This file is distributed in the hope that it will be useful,
    but without any warrantee.  You are expressly warned that it is meant
    to serve as a model, not for direct use:  all error checking and
    reporting has been omitted, and mistakes are almost surely present.
    Permission is granted to anyone to distribute verbatim copies of
    this source code as received, in any medium, provided that the
    copyright notice, the nonwarrantee warning, and this permission
    notice are preserved.  Permission is granted to distribute modified
    versions of this source code, or of portions of it, under the above
    conditions, plus the conditions that all changed files carry
    prominent notices stating who last changed them and that the derived
    material is subject to this same permission notice.  Permission is
    granted to include this material in products for which money is
    charged, provided that the customer is given written notice that the
    code is (or is derived from) material provided by Quintus Computer
    Systems, Inc., and that the customer is given this source code on
    request.


	----------------------------------------------------------------

    Now that weve got that (adapted from the GNU copyright notice)
    out of the way, here are the technical comments.

    The predicates are all named dcg_<something>/<some arity> in order
    to keep out of the way, with the exception of phrase/2 and phrase/3
|   which bear their proper names.  Only phrase/[2,3] and dcg_rule/2
|   are meant to be called directly, and dcg_rule/2 is meant to be called
|   from expand_term/2.  You need to keep dcg_body/4 and its dependents
    around at run time so that variables as nonterminals in DCG rule bodies
    will work correctly.

    So that Quintus have _something_ left to sell, this code has been
    rewritten from scratch with no error checking or reporting code at
    all, and a couple of places accept general grammar rule bodies where
    they are really supposed to demand lists of terminals.  However, any
    rule which is valid according to the Quintus Prolog manual will be
    translated correctly, except that this code makes no attempt to handle
    module: prefixes.  (The change is trivial.)	

    Note that dcg_rule/2 and phrase/[2,3] are steadfast.
*/
%   dcg rule(+Grammar Rule, -Equivalent Clause)

dcg_rule(-->(Head0,Body0), Clause) :-
	dcg_head(Head0, Head, PushBack, S0, S),
	dcg_body(Body0, Body1, S0, S),
	dcg_conj(Body1, PushBack, Body),
	Clause = :-(Head,Body).


%   dcg head(+Head0, -Head, -PushBack, -S0, -S)
%   recognises both
%	NonTerminal, [PushBackList] -->
%   and
%	NonTerminal -->
%   It returns the difference pair S0\S which the body is to parse.
%   To avoid error checking, it will accept an arbitrary body in place
%   of a pushback list, but it should demand a proper list.

dcg_head((Head0,PushBack0), Head, PushBack, S0, S1) :- !,
	dcg_goal(Head0, Head, S0, S),
	dcg_body(PushBack0, PushBack, S, S1).
dcg_head(Head0, Head, true, S0, S) :-
	dcg_goal(Head0, Head, S0, S).


%   dcg goal(+Goal0, -Goal, +S0, +S)
%   adds the arguments S0, S at the end of Goal0, giving Goal.
%   It should check that Goal0 is a callable term.

dcg_goal(Goal0, Goal, S0, S) :-
	functor(Goal0, F, N),
	N1 is N+1,
	N2 is N+2,
	functor(Goal, F, N2),
	arg(N2, Goal, S),
	arg(N1, Goal, S0),
	dcg_args(N, Goal0, Goal).


%   dcg args(+N, +Goal0, +Goal)
%   copies the first N arguments of Goal0 to Goal.

dcg_args(N, Goal0, Goal) :-
	(   N =:= 0 -> true
	;   arg(N, Goal0, Arg),
	    arg(N, Goal,  Arg),
	    M is N-1,
	    dcg_args(M, Goal0, Goal)
	).


%   dcg_body(+Body0, -Body, +S0, +S)
%   translates Body0 to Body, adding arguments as needed to parse S0\S.
%   It should complain about bodies (such as 2) which are not callable
%   terms, and about lists of terminals which are not proper lists.
%   To avoid error checking, [a|foo] is accepted as [a],foo, but it
%   really should complain.  ONLY the forms lists here should be treated;
%   other non-terminals which look like calls to built-ins could well be
%   commented on (no error reporting here) but should be expanded even
%   so.  Thus X=Y as a nonterminal is to be rewritten as =(X,Y,S0,S),
%   perhaps with a warning.  If you want the translation X=Y, use {X=Y}.

dcg_body(Var, Body, S0, S) :- var(Var), !,
	Body = phrase(Var,S0,S).
dcg_body((A0,B0), Body, S0, S) :- !,
	dcg_body(A0, A, S0, S1),
	dcg_body(B0, B, S1, S),
	dcg_conj(A, B, Body).
dcg_body((A0->B0), (A->B), S0, S) :- !,
	dcg_body(A0, A, S0, S1),
	dcg_body(B0, B, S1, S).
dcg_body((A0;B0), (A;B), S0, S) :- !,
	dcg_disj(A0, A, S0, S),
	dcg_disj(B0, B, S0, S).
dcg_body({A}, A, S, S) :- !.
dcg_body(!, !, S, S) :- !.
dcg_body([], true, S, S) :- !.
dcg_body([H0|T0], Body, S0, S) :- !,
	dcg_term(H0, H, S0, S1),
	dcg_body(T0, T, S1, S),
	dcg_conj(H, T, Body).
dcg_body(NT0, NT, S0, S) :-
	dcg_goal(NT0, NT, S0, S).


%   dcg_term(+T0, -T, +S0, +S)
%   generates code (T) which succeeds when there is a terminal T0
%   between S0 and S.  This version uses the DEC-10 Prolog predicate
%   C/3 for compatibility with DEC-10 Prolog, C Prolog, Quintus Prolog.
%   This is the only place that knows how terminals are translated, so
%   you could supply instead the definition
%	dcg_term(T0, S0=[T0|S], S0, S).
%   and reap the same benefits.  The one thing you must not do is
%   NO! dcg_term(T0, true, [T0|S], S). DONT DO THAT!

%%% dcg_term(T0, 'C'(S0,T0,S), S0, S).
    dcg_term(T0, S0=[T0|S], S0, S).      %% M. Covington 1994


%  To see why dcg disj/4 is needed, consider the translation of
%  ( [] | [a] ).  We have to insert S1=S0 somewhere, but we do it at
%  "compile"-time if we can.

dcg_disj(Body0, Body, S0, S) :-
	dcg_body(Body0, Body1, S1, S),
	(   S1==S -> dcg_conj(S1=S0, Body1, Body)
	;   S1 = S0, Body = Body1
	).

%   dcg_conj(+A, +B, -C)
%   combines two conjunctions A, B, giving C.  Basically, we want to
%   ensure that there arent any excess trues kicking around (in a
%   compiled system, that shouldnt matter).  There is room for some
%   leeway here: I have chosen to flatten A completely.

dcg_conj(A, true, A) :- !.
dcg_conj(A, B, C) :-
	dcg_CONJ(A, B, C).

dcg_CONJ(true, C, C) :- !.
dcg_CONJ((A,As), C0, (A,C)) :- !,
	dcg_CONJ(As, C0, C).
dcg_CONJ(A, C, (A,C)).


%%% phrase/2, phrase/3, 'C'/3 are nowadays already built in.
%%%                                      - M. Covington 1994

%   'C'(S0, T, S)
%   is true when the terminal T "Connects" the "string positions" S0 and S.

%%% 'C'([T|S], T, S).


%   phrase(+NT0, ?S0)
%   is true when the list S0 is in the language defined by the
%   grammar rule body NT0.  E.g. phrase(([a],[b]), [a,b]).

%%% phrase(NT0, S0) :-
%%%	phrase(NT0, S0, []).


%   phrase(+NT0, ?S0, ?S)
%   is true when the list S0\S is in the language defined by the
%   grammar rule body NT0.  E.g. phrase(([a],[b]), [a,b|X], X).

%%% phrase(NT0, S0, S) :-
%%%	dcg_body(NT0, NT, T0, T),
%%%	T0 = S0, T = S,
%%%	NT.

portable_expand_term(-->(H,B), Clause) :- !,
	(  dcg_rule( -->(H,B), Clause)
	-> true
	;  write('dcg_expansion_error->'),
	   write(H), nl,
	   fail
	).
portable_expand_term(C, C).

%%% End of DCG-rule translator.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*******************
 * Hook predicates *
 *******************/

% Display feature structures as such in interactive debugging:

portray(X) :- g_vl(X), g_tb(FS,X), write(FS).   %%  Quintus, LPA, and SWI


% Translate feature structures to internal form automatically
% during consult and reconsult, and process g_features/1 properly:

term_expansion(g_features(List),
         [(:- dynamic g_features/1),g_features(List)]) :-
       !,
       write('% Declared features: '), write(List), nl,
       retractall(g_forward_schema(_,_,_)),
%Test  write('[retracted all forward schemas]'),nl,
       g_make_forward_schemas(List),
       g_make_backward_schema.

  % At present there is nothing to prevent the user from _first_ using some
  % undeclared features and _then_ giving a g_features declaration.
  % This situation should at least be detected, if possible.

term_expansion(Term, Result) :-
	portable_expand_term(Term, X),
	g_tf(X, Result).

% Unlike expand_term, portable_expand_term does not call term_expansion
% and hence does not cause endless recursion when called by term_expansion.


% Make sure the right lines are commented out.


%%  Quintus Prolog
%%
/***************************************************************************

:- g_herald.

***************************************************************************/


%%  LPA Prolog
%%
/***************************************************************************

% Set initialization for LPA Prolog:
% (not needed if we save state)
      
:- initialization
	op(600,xfy,':'),
	op(602,xfy,'..'),
	op(602,xfy,'::'),
	g_version(X),
	version(X),
	g_herald.

% New top level for LPA Prolog, called from save_state:

g_handler :- g_herald, main_handler.

***************************************************************************/


%%  SWI-Prolog 2.0 and above
%%


%%  Uncomment this part if you want module support (probably unstable).
%%
%%  add_portray(V, Body) :-
%%	forall(user:clause(portray(V), Body, Ref), erase(Ref)),
%%	assertz(:-(user:portray(V), Body)).
%%
%%  :- add_portray(A, gulp3:portray(A)).
%%  :- assertz(:-(user:term_expansion(A,B), gulp3:term_expansion(A,B))).
%%
%%  portray/1 and term_expansion/2 cannot be exported via the
%%  export list of the :-module/2 directive as this would result
%%  in a name clash if other modules provide their own clauses
%%  of portray/1 and/or term_expansion/2.
%%  The `quick 'n' dirty' solution is to hook them directly into
%%  the user predicate space by using assert with an explicit
%%  module specification.

:- g_herald.

% :- g_version(V),write('**** '),write(V),write(' ****'),nl,nl.   % Shorter than g_herald.






% THIS MUST BE THE END OF THE FILE.
% No part of GULP itself can follow term_expansion.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EOF %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








