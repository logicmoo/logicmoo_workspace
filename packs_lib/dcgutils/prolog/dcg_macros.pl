/* Part of dcgutils
	Copyright 2012-2015 Samer Abdallah (Queen Mary University of London; UCL)

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(dcg_macros, [use_dcg_macros/0]).
/** <module> DCG utilities implememnted by term expansion.

   This module provides term expansions for the following predicates and DCG goals:
   *  nop//0
   *  out//1
   *  get//1
   *  set//1
   *  set_with//1
   *  trans//2
   *  (>>)//2
   *  run_left//3
   *  run_right//3
   *  (\<)//1
   *  (\>)//1
   *  (<\>)//1
   *  (//)//2
   *  seqmap//N
   *  seqmap_with_sep//N
   *  do_then_call//N
*/

:- op(900,fy,\<).
:- op(900,fy,\>).
:- op(900,fy,<\>).
:- op(900,xfy,\#).

:- use_module(library(apply_macros)).

mk_call(C,XX,Call) :- var(C), !, mk_call(call(C),XX,Call).
mk_call(M:C,XX,M:Call) :- !, mk_call(C,XX,Call).
mk_call(C,XX,Call) :- C =.. CL, append(CL,XX,CL2), Call =.. CL2.


/*
 * Goal expansions
 */

use_dcg_macros.
user:goal_expansion(G,E) :-
   prolog_load_context(module,Mod),
   predicate_property(Mod:use_dcg_macros, imported_from(dcg_macros)),
   dcg_macros:expansion(G,E).

cons(A,B,[A|B]).

expand_seqmap_with_prefix(Sep0, Callable0, SeqmapArgs, Goal) :-
	(   Callable0 = M:Callable ->  NextGoal = M:NextCall, QPred = M:Pred
	;   Callable  = Callable0,     NextGoal = NextCall,   QPred = Pred
	),

	append(Lists, [St1,St2], SeqmapArgs),

	Callable =.. [Pred|Args],
	length(Args, Argc),
	length(Argv, Argc),
	length(Lists, N),
	length(Vars, N),
	MapArity is N + 4,
	format(atom(AuxName), '__aux_seqmap/~d_~w_~w+~d', [MapArity, Sep0, QPred, Argc]),
	build_term(AuxName, Lists, Args, St1, St2, Goal),

	AuxArity is N+Argc+2,
	prolog_load_context(module, Module),
	(   current_predicate(Module:AuxName/AuxArity)
	->  true
	;   length(BaseLists,N),
       maplist(=([]),BaseLists),
	    length(Anon, Argc),
	    build_term(AuxName, BaseLists, Anon, S0, S0, BaseClause),

       length(Vars,N),
		 maplist(cons, Vars, Tails, NextArgs),
       (  Sep0=_:Sep -> true; Sep=Sep0 ),
		 (  is_list(Sep) -> append(Sep,S2,S1), NextThing=NextGoal
		 ;  build_term(call_dcg, [Sep0], [], S1, S2, NextSep),
			 NextThing = (NextSep,NextGoal)
		 ),
	    build_term(Pred,    Argv,     Vars, S2, S3, NextCall1),
	    build_term(AuxName, Tails,    Argv, S3, S4, NextIterate),
	    build_term(AuxName, NextArgs, Argv, S1, S4, NextHead),

		 (  expansion(NextCall1,NextCall) -> true
		 ;  NextCall1=NextCall),

	    NextClause = (NextHead :- NextThing, NextIterate),

	    (	predicate_property(Module:NextGoal, transparent)
	    ->	compile_aux_clauses([ (:- module_transparent(Module:AuxName/AuxArity)),
				      BaseClause,
				      NextClause
				    ])
	    ;   compile_aux_clauses([BaseClause, NextClause])
	    )
	).

expand_call_with_prefix(Sep0, Callable0, InArgs, (SepGoal,CallGoal)) :-
	append(CallArgs, [S1,S3], InArgs),

	(  Sep0=_:Sep -> true; Sep=Sep0 ),
	(  is_list(Sep) -> append(Sep,S2,SS), SepGoal=(S1=SS)
	;  build_term(call_dcg, [Sep0], [], S1, S2, SepGoal)
	),

	(	var(Callable0)
	->	build_term(call,[Callable0], CallArgs, S2, S3, CallGoal1)
	;	(	Callable0 = M:Callable
		->  CallGoal1 = M:NextCall
		;   Callable = Callable0,
			 CallGoal1 = NextCall
		),
		Callable =.. [Pred|Args],
		build_term(Pred, Args, CallArgs, S2, S3, NextCall)
	),
	(	expansion(CallGoal1,CallGoal) -> true
	;	CallGoal1=CallGoal
	).

:- public
      seqmap_with_sep_first_call//3,
      seqmap_with_sep_first_call//5.
      seqmap_with_sep_first_call//7.

seqmap_with_sep_first_call(P,[A1|AX],AX) --> call(P,A1).
seqmap_with_sep_first_call(P,[A1|AX],[B1|BX],AX,BX) --> call(P,A1,B1).
seqmap_with_sep_first_call(P,[A1|AX],[B1|BX],[C1|CX],AX,BX,CX) --> call(P,A1,B1,C1).

expand_seqmap_with_sep(Sep, Pred, SeqmapArgs, (dcg_macros:FirstCall,SeqmapCall)) :-
	prolog_load_context(module,Context),
	(Sep=SMod:Sep1 -> true; SMod=Context, Sep1=Sep),
	(Pred=CMod:Pred1 -> true; CMod=Context, Pred1=Pred),
	append(Lists, [St1,St3], SeqmapArgs),
	length(Lists, N),
	length(Tails, N),
	build_term(seqmap_with_sep_first_call, [CMod:Pred1|Lists], Tails, St1, St2, FirstCall),
   append(Tails,[St2,St3],SeqmapWPArgs),
   expand_seqmap_with_prefix(SMod:Sep1,CMod:Pred1,SeqmapWPArgs,SeqmapCall).
	% build_term(seqmap_with_prefix, [SMod:Sep1,CMod:Pred1], Tails, St2, St3, SeqmapCall).

build_term(H,L1,L2,S1,S2,Term) :-
	append(L2,[S1,S2],L23),
	append(L1,L23,L123),
	Term =.. [H | L123].


expand_dcg(Term, Goal) :-
	functor(Term, seqmap, N), N >= 4,
	Term =.. [seqmap, Callable | Args],
	callable(Callable), !,
	expand_seqmap_with_prefix([],Callable, Args, Goal).

expand_dcg(Term, Goal) :-
	functor(Term, seqmap_with_sep, N), N >= 5,
	Term =.. [seqmap_with_sep, Sep, Callable | Args],
	nonvar(Sep), callable(Callable), !,
	expand_seqmap_with_sep(Sep, Callable, Args, Goal).

expand_dcg(Term, Goal) :-
	functor(Term, do_then_call, N), N >= 2,
	Term =.. [do_then_call, Prefix, Callable | Args],
	nonvar(Prefix), !,
	expand_call_with_prefix(Prefix, Callable, Args, Goal).

expansion( GoalIn, GoalOut) :-
	\+current_prolog_flag(xref, true),
	expand_dcg(GoalIn, GoalOut).
expansion( run_left(P,S1,S2,T1,T2), call_dcg(P,(S1-T1),(S2-T2))).
expansion( run_right(P,S1,S2,T1,T2), call_dcg(P,(T1-S1),(T2-S2))).
expansion( \<(P,S1,S2), (S1=(L1-R),S2=(L2-R),call_dcg(P,L1,L2)) ).
expansion( \>(P,S1,S2), (S1=(L-R1),S2=(L-R2),call_dcg(P,R1,R2)) ).
expansion( <\>(A,B,S1,S2), (S1=L1-R1, S2=L2-R2, call_dcg(A,L1,L2), call_dcg(B,R1,R2))).

expansion( nop(S1,S2), (S1=S2) ).
expansion( out(X,S1,S2), (S1=[X|S2]) ).
expansion( get(S,S1,S2), (S=S1,S1=S2) ).
expansion( set(S,_,S2), (S=S2) ).
expansion( A >> B, (A,B) ).
expansion( set_with(C,_,S2), Call) :- mk_call(C,[S2],Call).
expansion( trans(A1,A2,S1,S2), (S1=A1,S2=A2) ).
expansion( //(P1,P2,S1,S2), (call_dcg(P1,S1,S2),call_dcg(P2,S1,S2))).

