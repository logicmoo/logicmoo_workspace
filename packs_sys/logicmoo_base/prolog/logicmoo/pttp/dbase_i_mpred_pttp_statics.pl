%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_pttp_static,[]).
%:- endif.

:- ensure_loaded(library(pfc_lib)).
:- ensure_loaded(library('pfc2.0'/'mpred_header.pi')).
:- '$set_source_module'(baseKB).

:- 
 %swi_module(mpred_pttp_statics,[ 
    % pttp1/2,
      op(400,fy,-),    % negation
      op(500,xfy,&),   % conjunction
      op(600,xfy,v),   % disjunction
      op(650,xfy,=>),  % implication
      op(680,xfy,<=>), % equivalence
      op( 500, fy, ~),    % negation
      op( 500, fy, all),  % universal quantifier
      op( 500, fy, ex),   % existential quantifier
  %    op( 500,xfy, :),
       % nnf/4,
       !.

/*
       pttp_tell_wid/2,
       pttp_test/2,
       search/7,
       do_pttp_test/1,
       timed_call/2,
       expand_input_proof/2,
       contract_output_proof/2
        ]).
*/



%%% ****h* PTTP/PTTP
%%% COPYRIGHT
%%%   Copyright (c) 1988-2003 Mark E. Stickel, SRI International, Menlo Park, CA 94025  USA
%%% 
%%%   Permission is hereby granted, free of charge, to any person obtaining a
%%%   copy of this software and associated documentation files (the "Software"),
%%%   to deal in the Software without restriction, including without limitation
%%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%%   and/or sell copies of the Software, and to permit persons to whom the
%%%   Software is furnished to do so, subject to the following conditions:
%%% 
%%%   The above copyright notice and this permission notice shall be included
%%%   in all copies or substantial portions of the Software.
%%% 
%%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%%   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%%   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%%   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%%   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%%   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%%   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% DESCRIPTION
%%%  
%%%      A Prolog Technology Theorem Prover
%%%  
%%%               Mark E. Stickel
%%%  
%%%   Prolog is not a full theorem prover
%%%   for three main reasons:
%%%  
%%%     It uses an unsound unification algorithm without
%%%     the occurs check.
%%%  
%%%     Its inference system is complete for Horn
%%%     clauses, but not for more general formulas.
%%%  
%%%     Its unbounded depth-first search strategy
%%%     is incomplete.
%%%  
%%%   Also, it cannot display the proofs it finds.
%%%  
%%%   The Prolog Technology Theorem Prover (PTTP)
%%%   overcomes these limitations by
%%%  
%%%     transforming clauses so that head literals have
%%%     no repeated variables and unification without the
%%%     occurs check is valid; remaining unification
%%%     is done using complete unification with the
%%%     occurs check in the body;
%%%  
%%%     adding contrapositives of clauses (so that
%%%     any literal, not just a distinguished head
%%%     literal, can be resolved on) and the model-
%%%     elimination procedure reduction rule that
%%%     matches goals with the negations of their
%%%     ancestor goals;
%%%  
%%%     using a sequence of bounded depth-first searches
%%%     to pttp prove a theorem;
%%%  
%%%     retaining information on what formulas are
%%%     used for each inference so that the proof
%%%     can be printed.
%%%  
%%%   This version of PTTP translates first-order
%%%   predicate calculus formulas written in a Prolog-like
%%%   notation into Prolog code.  The resulting code
%%%   is then compiled and executed.
%%%  
%%%   PTTP commands:
%%%  
%%%     pttp_assert(formula) - translates the first-order formula
%%%       (normally a conjunction of formulas) into Prolog
%%%       and compiles it
%%%
%%%     pttp_prove(formula) - tries to pttp prove a formula
%%%
%%%   Look at the description of these functions
%%%   and the examples for more details on how
%%%   pttp_assert and pttp_prove should be used.
%%%   For more information on PTTP, consult
%%%     Stickel, M.E.  A Prolog technology theorem prover:
%%%     implementation by an extended Prolog compiler.
%%%     Journal of Automated Reasoning 4, 4 (1988), 353-380.
%%%     and
%%%     Stickel, M.E.  A Prolog technology theorem prover:
%%%     a new exposition and implementation in Prolog.
%%%     Technical Note 464, Artificial Intelligence Center,
%%%     SRI International, Menlo Park, California, June 1989.
%%%
%%%
%%%
%%%   Several arguments are added to each predicate:
%%%   PosAncestors is list of positive ancestor goals
%%%   NegAncestors is list of negative ancestor goals
%%%   DepthIn is depth bound before goal is solved
%%%   DepthOut will be set to remaining depth bound after goal is solved
%%%   ProofIn is dotted-pair difference list of proof so far
%%%   ProofOut will be set to list of steps of proof so far after goal is solved
%%%  
%%%
%%%
%%%   Depth-first iterative-deepening search.
%%%
%%%   PTTP adds arguments DepthIn and DepthOut
%%%   to each PTTP literal to control bounded depth-first
%%%   search.  When a literal is called,
%%%   DepthIn is the current depth bound.  When
%%%   the literal exits, DepthOut is the new number
%%%   of levels remaining after the solution of
%%%   the literal (DepthIn - DepthOut is the number
%%%   of levels used in the solution of the goal.)
%%%
%%%   For clauses with empty bodies or bodies
%%%   composed only of pttp_builtin functions,
%%%   DepthIn = DepthOut.
%%%
%%%   For other clauses, the depth bound is
%%%   compared to the cost of the body.  If the
%%%   depth bound is exceeded, the clause fails.
%%%   Otherwise the depth bound is reduced by
%%%   the cost of the body.
%%%
%%%   p :- q , r.
%%%   is transformed into
%%%   p(DepthIn,DepthOut) :-
%%%       DepthIn >= 2, Depth1 is DepthIn - 2,
%%%       q(Depth1,Depth2),
%%%       r(Depth2,DepthOut).
%%%
%%%   p :- q ; r.
%%%   is transformed into
%%%   p(DepthIn,DepthOut) :-
%%%       DepthIn >= 1, Depth1 is DepthIn - 1,
%%%       (q(Depth1,DepthOut) ; r(Depth1,DepthOut)).
%%%
%%%
%%%
%%%   Complete inference.
%%%
%%%   Model elimination reduction operation and
%%%   identical ancestor goal pruning.
%%%
%%%   Two arguments are added to each literal, one
%%%   for all the positive ancestors, one for all
%%%   the negative ancestors.
%%%
%%%   Unifiable membership is checked in the list 
%%%   of opposite polarity to the goal
%%%   for performing the reduction operation.
%%%
%%%   Identity membership is checked in the list
%%%   of same polarity as the goal
%%%   for performing the ancestor goal pruning operation.
%%%   This is not necessary for soundness or completeness,
%%%   but is often effective at substantially reducing the
%%%   number of inferences.
%%%
%%%   The current head goal is added to the front
%%%   of the appropriate ancestor list during the
%%%   call on subgoals in bodies of nonunit clauses.
%%%
%%%
%%%
%%%   Proof Printing.
%%%
%%%   Add extra arguments to each goal so that information
%%%   on what inferences were made in the proof can be printed
%%%   at the end.
%%% ***
%%% ****f* PTTP/pttp
%%% DESCRIPTION
%%%   pttp is the PTTP compiler top-level predicate.
%%%   Its argument is a conjunction of formulas to be compiled.
%%% SOURCE



%%% ***

%%% ****if* PTTP/linearize
%%% DESCRIPTION
%%%   Prolog's unification operation is unsound for first-order
%%%   reasoning because it lacks the occurs check that would
%%%   block binding a variable to a term that contains the
%%%   variable and creating a circular term.  However, Prolog's
%%%   unification algorithm is sound and the occurs check is
%%%   unnecessary provided the terms being unified have no
%%%   variables in common and at least one of the terms has
%%%   no repeated variables.  A Prolog fact or rule head will
%%%   not have variables in common with the goal.  The linearize
%%%   transformation rewrites a fact or rule so that the fact
%%%   or rule head has no repeated variables and Prolog unification
%%%   can be used safely.  The rest of the unification can then
%%%   be done in the body of the transformed clause, using
%%%   the sound unify predicate.
%%%
%%%   For example,
%%%      p(X,Y,f(X,Y)) :- true.
%%%   is transformed into
%%%      p(X,Y,f(X1,Y1)) :- unify(X,X1), unify(Y,Y1).
%%% SOURCE

linearize(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut):- linearize(unify, TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut).

linearize(Pred, TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
	is_ftNonvar(TermIn) ->
		functor(TermIn,F,N),
		pttp_functor(TermOut,F,N),
		linearize_args(Pred,TermIn,TermOut,VarsIn,VarsOut,
		               MatchesIn,MatchesOut,1,N);
	identical_member_special(TermIn,VarsIn) ->
		((VarsOut = VarsIn,
                UNIFY =.. [Pred,TermIn,TermOut],
		conjoin_pttp(MatchesIn,UNIFY,MatchesOut)));
	%true ->
	      ((  TermOut = TermIn,
		VarsOut = [TermIn|VarsIn],
		MatchesOut = MatchesIn)).

linearize_args(Pred,TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
	I > N ->
		VarsOut = VarsIn,
		MatchesOut = MatchesIn;
	%true ->
		arg(I,TermIn,ArgI),
		linearize(Pred,ArgI,NewArgI,VarsIn,Vars1,MatchesIn,Matches1),
		arg(I,TermOut,NewArgI),
		I1 is I + 1,
		linearize_args(Pred,TermIn,TermOut,Vars1,VarsOut,Matches1,MatchesOut,I1,N).
%%% ***
%%% ****if* PTTP/unify
%%% DESCRIPTION
%%%   Prolog's unification operation is unsound for first-order
%%%   reasoning because it lacks the occurs check that would
%%%   block binding a variable to a term that contains the
%%%   variable and creating a circular term.  Thus, PTTP
%%%   must provide a sound unfication algorithm with the occurs
%%%   check.
%%%
%%%   unify(X,Y) is similar to Prolog's X=Y, except that operations
%%%   like unify(X,f(X)) fail rather than create circular terms.
%%% SOURCE

unify(X,Y) :- unify_with_occurs_check(X,Y).

unify_cheaper(X,Y) :- compound(X),compound(Y),!,
		functor(X,F1,N),
		functor(Y,F2,N),

                same_functor(F1,F2),
		(N = 1 ->
			arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
		%true ->
			unify_args(X,Y,N)).

unify_cheaper(X,Y) :- unify_with_occurs_check(X,Y),!.

same_functor(F1,F2):- ( F1=F2 -> true ; simular_functors(F1,F2)).

simular_functors(F1,F2):-fail,F1=F2.

unify_args(X,Y,N) :-
	N = 2 ->
		arg(2,X,X2), arg(2,Y,Y2), unify(X2,Y2),
		arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
	%true ->
		arg(N,X,Xn), arg(N,Y,Yn), unify(Xn,Yn),
		N1 is N - 1, unify_args(X,Y,N1).


%%% ***


constrain_args_pttp(_P,[AR,GS]):-!,dif(AR,GS).
constrain_args_pttp(_,[P,AR,GS]):-P\=d,P\=p,P\=l,!,dif(AR,GS).
constrain_args_pttp(_,_).

argument_type_checking(HF,HeadArgs,constrain_args(HF,HeadArgs)):-current_predicate(constrain_args/2).
argument_type_checking(HF,HeadArgs,constrain_args_pttp(HF,HeadArgs)):-current_predicate(constrain_args_pttp/2).
argument_type_checking(_,_,true).

:- meta_predicate pretest_call(0).
pretest_call(C):-call(C).




%%% ***
%%% ****if* PTTP/unifiable_member
%%% DESCRIPTION
%%%   unifiable_member(X,L) succeeds each time X is unifiable with an
%%%   element of the list L
%%% SOURCE

unifiable_member(X,[Y|L]) :- unify(X,Y); unifiable_member(X,L).

unifiable_member_cheaper(X,[Y|L]) :- unify_cheaper(X,Y); unifiable_member_cheaper(X,L).


%%% ***
%%% ****if* PTTP/identical_member_special
%%% DESCRIPTION
%%%   identical_member_special(X,L) succeeds iff X is an element of the list L
%%%   it does not use unification during element comparisons
%%% SOURCE

% from memberchk_eq(X,L).

identical_member_cheaper(X,[Y|L]) :- unify_cheaper(X,Y); identical_member_cheaper(X,L).

identical_member_special_loop_check(X,L):- 1 is random(9),!,unifiable_member(X,L).
identical_member_special_loop_check(X,L):-identical_member_special(X,L).

identical_member_special(X,[Y|Ys]) :-
	(   X == Y
	->  true
	;   identical_member_special(X,Ys)
	).

%%% ***

%%% ****if* PTTP/write_proof
%%% DESCRIPTION
%%%   write_proof prints the proof that PTTP finds
%%% SOURCE

write_proof(Proof) :-
   must_det_l((
	write('Proof:'),
	nl,
	proof_length(Proof,Len),
	write('length = '),
	write(Len),
	write(', '),
	proof_depth(Proof,Depth),
	write('depth = '),
	write(Depth),
	nl,
	write('Goal#  Wff#  Wff Instance'),
	nl,
	write('-----  ----  ------------'),
	add_proof_query_line(Proof,Proof2),
	process_proof(Proof2,0,Proof1),
	write_proof1(Proof1),
	nl,
	write('Proof end.'))).

write_proof1([]).
write_proof1([[LineNum,X,Head,Depth,Subgoals]|Y]) :-
	nl,
	write_indent_for_number(LineNum),
	write('['),
	write(LineNum),
	write(']  '),
	write_indent_for_number(X),
	write(X),
	write('   '),
	write_proof_indent(Depth),
	write(Head),
	(Subgoals = [] ->
		true;
	%true ->
		write(' :- '),
		write_proof_subgoals(Subgoals)),
	write(.),
	write_proof1(Y).	

write_proof_subgoals([X,Y|Z]) :-
	write('['),
	write(X),
	write('] , '),
	write_proof_subgoals([Y|Z]).
write_proof_subgoals([X]) :-
	write('['),
	write(X),
	write(']').

write_proof_indent(N) :-
	N > 0,
	write('   '),
	N1 is N - 1,
	write_proof_indent(N1).
write_proof_indent(0).

process_proof([Prf|PrfEnd],_LineNum,Result) :-
	Prf == PrfEnd,
	!,
	Result = [].
process_proof([[[X,Head,PosAncestors,NegAncestors]|Y]|PrfEnd],LineNum,Result) :-
	LineNum1 is LineNum + 1,
	process_proof([Y|PrfEnd],LineNum1,P),
	(is_query_lit(Head) ->
		Depth is 0;
	%true ->
		list_length_pttp(PosAncestors,N1),	% compute indentation to show
		list_length_pttp(NegAncestors,N2),	% level of goal nesting from
		Depth is N1 + N2 + 1),		% lengths of ancestor lists
	Depth1 is Depth + 1,
	collect_proof_subgoals(Depth1,P,Subgoals),
	(X = redn ->
		X1 = red,
		negated_literal(Head,Head1);
	 ((number(X) , X < 0); X= (-(_))) ->
		isNegOf(X1,X),
		negated_literal(Head,Head1);
	%true ->
		X1 = X,
		Head1 = Head),
	Result = [[LineNum,X1,Head1,Depth,Subgoals]|P].

collect_proof_subgoals(_Depth1,[],Result) :-
	Result = [].
collect_proof_subgoals(Depth1,[[LineNum,_,_,Depth,_]|P],Result) :-
	Depth = Depth1,
	collect_proof_subgoals(Depth1,P,R),
	Result = [LineNum|R].
collect_proof_subgoals(Depth1,[[_,_,_,Depth,_]|P],Result) :-
	Depth > Depth1,
	collect_proof_subgoals(Depth1,P,Result).
collect_proof_subgoals(Depth1,[[_,_,_,Depth,_]|_],Result) :-
	Depth < Depth1,
	Result = [].

add_proof_query_line(Proof,Proof2) :-
	Proof = [Prf|_PrfEnd],
	is_ftNonvar(Prf),
	Prf = [[_,query,_,_]|_],
	!,
	Proof2 = Proof.
add_proof_query_line(Proof,Proof2) :-
	Proof = [Prf|PrfEnd],
	Proof2 = [[[0,query,[],[]]|Prf]|PrfEnd].
%%% ***

%%% ****if* PTTP/clauses
%%% DESCRIPTION
%%%   Negation normal form to Prolog clause translation.
%%%   Include a literal in the body of each clause to
%%%   indicate the number of the formula the clause came from.
%%% SOURCE

clauses((A , B),L,WffNum1,WffNum2) :-
	!,
	clauses(A,L1,WffNum1,W),
	clauses(B,L2,W,WffNum2),
	conjoin_pttp(L1,L2,L).

clauses(PNF,L,WffNum1,WffNum2):- 
   save_wid(WffNum1,pttp_in,PNF),
   once(pttp_nnf(PNF,OUT)),
   save_wid(WffNum1,pttp_nnf,OUT),
   clauses1(OUT,L,WffNum1,WffNum2).

clauses1(A,L,WffNum1,WffNum2) :-
	write_clause_with_number(A,WffNum1),
	head_literals(A,Lits),
	clauses2(A,Lits,L,WffNum1),
	kb_incr(WffNum1 ,WffNum2).

clauses2(A,[Lit|Lits],L,WffNum) :-
	body_for_head_literal(Lit,A,Body1),
	(Body1 == false ->
		L = true;
	%true ->
		conjoin_pttp(infer_by(WffNum),Body1,Body),
		clauses2(A,Lits,L1,WffNum),
		conjoin_pttp((Lit :- Body),L1,L)).
clauses2(_,[],true,_).

head_literals(Wff,L) :-
	Wff = (A :- _B) ->	% contrapositives not made for A :- ... inputs
		head_literals(A,L);
	Wff = (A , B) ->
		(head_literals(A,L1),
		 head_literals(B,L2),
		 list_union(L1,L2,L));
	Wff = (A ; B) ->
		(head_literals(A,L1),
		 head_literals(B,L2),
		 list_union(L1,L2,L));
	%true ->
		L = [Wff].

body_for_head_literal(Head,Wff,Body) :-
	Wff = (A :- B) ->
		(body_for_head_literal(Head,A,A1),
		 conjoin_pttp(A1,B,Body));
	Wff = (A , B) ->
		(body_for_head_literal(Head,A,A1),
		 body_for_head_literal(Head,B,B1),
		 pttp_disjoin(A1,B1,Body));
	Wff = (A ; B) ->
		(body_for_head_literal(Head,A,A1),
		 body_for_head_literal(Head,B,B1),
		 conjoin_pttp(A1,B1,Body));
	Wff == Head ->
		Body = true;
	(once(negated_literal(Wff,Was)),Head=@=Was) ->
		Body = false;
	%true ->
		negated_literal(Wff,Body).
%%% ***
%%% ****if* PTTP/predicates
%%% DESCRIPTION
%%%   predicates returns a list of the predicates appearing in a formula.
%%% SOURCE

is_functor_like_search(Search):-atom(Search),arg(_,vv(search,pttp_prove),Search).


is_functor_like_firstOrder(Search):-atom(Search),arg(_,vv(asserted_t,secondOrder,pttp_prove),Search).
is_functor_like_firstOrder(Search):-atom(Search),is_holds_true_pttp(Search).
is_functor_like_firstOrder(Search):-atom(Search),is_holds_false_pttp(Search).

predicates(Wff,[]):-is_ftVar(Wff),!.
predicates(Wff,[]):-not(compound(Wff)),!.
predicates([Lw],L):- predicates(Lw,L),!.
predicates([Lw|ISTw],L):- !,
   predicates(Lw,L1),
   predicates(ISTw,L2),
   union(L2,L1,L).

predicates(Wff,L):- functor(Wff,Search,_),is_functor_like_search(Search),arg(1,Wff,X),predicates(X,L),!.

predicates(Wff,L) :-
        Wff = (A :- B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                predicates(A,L1),
                predicates(B,L2),
                union(L2,L1,L);
        functor(Wff,search,_) ->        % list predicates in first argument of search
                arg(1,Wff,X),
                predicates(X,L);
        pttp_builtin(Wff) ->
                L = [];
        %true ->
                functor(Wff,F,N),
                L = [[F,N]].



predicates(Wff,L) :- functor(Wff,F,A), predicates(Wff,F,A,L).

skipped_functor(F):- fail,is_2nd_order_holds_pttp(F).

predicates(Wff,F,___,L):- logical_functor_pttp(F), Wff=..[_|ARGS], predicates(ARGS,L).
predicates(Wff,F,A,  L):- pttp_builtin(F,A), Wff=..[_|ARGS], predicates(ARGS,L).
% predicates(Wff,F,___,L):- skipped_functor(F), Wff=..[_|ARGS], predicates(ARGS,L).
predicates(Wff,F,A,[[F,A]|L]):- Wff=..[_|ARGS], predicates(ARGS,L).

%%% ***
%%% ****if* PTTP/procedure
%%% DESCRIPTION
%%%   procedure returns a conjunction of the clauses
%%%   with head predicate P/N.
%%% SOURCE


procedure(P,N,Clauses,Proc) :-
       ( (Clauses = (A , B)) ->
		(procedure(P,N,A,ProcA),
		 procedure(P,N,B,ProcB),
		 conjoin_pttp(ProcA,ProcB,Proc));
	((Clauses = (A :- _B) , functor(A,P,N)) ->
		Proc = Clauses;
	%true ->
		Proc = true)).

procedures([[P,N]|Preds],Clauses,Procs) :-
	procedure(P,N,Clauses,Proc),
	procedures(Preds,Clauses,Procs2),
	conjoin_pttp(Proc,Procs2,Procs).
procedures([],_Clauses,true).
%%% ***

head_body_was(_,_).

:- was_export(is_holds_false_pttp/1).
is_holds_false_pttp(A):-not(atom(A)),!,fail.
is_holds_false_pttp(Prop):-member(Prop,[not,nholds,holds_f,mpred_f,aint,assertion_f,asserted_mpred_f,retraction,not_secondOrder,not_firstOrder]).
is_holds_false_pttp(F):-atom_concat(_,'_false',F).
%is_holds_false_pttp(F):-atom_concat(_,'_f',F).
is_holds_false_pttp(F):-is_p_to_n(_,F).
% is_holds_false_pttp(F):-atom_concat('imp',_,F).

:- was_export(is_holds_true_pttp/1).
is_holds_true_pttp(A):-not(atom(A)),!,fail.
is_holds_true_pttp(Prop):-arg(_,vvv(holds,holds_t,t,asserted_mpred_t,assertion_t,assertion,secondOrder,asserted_t),Prop).
is_holds_true_pttp(F):-atom_concat(_,'_true',F).
is_holds_true_pttp(F):-atom_concat(_,'_t',F).
%is_holds_true_pttp(F):-atom_concat('pos',_,F).
%is_holds_true_pttp(F):-atom_concat('is',_,F).
is_holds_true_pttp(F):-atom_concat(_,'_in',F).
is_holds_true_pttp(F):-is_p_to_n(F,_).

:- was_export(is_2nd_order_holds_pttp/1).
is_2nd_order_holds_pttp(Prop):- atom(Prop), is_holds_true_pttp(Prop) ; is_holds_false_pttp(Prop).


:- style_check(+singleton).

do_not_wrap(F):-not(atom(F)),!,fail.
do_not_wrap(F):-arg(_,vv(query),F).
do_not_wrap(F):-atom_concat('int_',_,F).

:- was_export(correct_pttp/2).
%:- was_dynamic t_l:second_order_wrapper/1.
:- thread_local t_l:second_order_wrapper/1.
t_l:second_order_wrapper(true_t).


correct_pttp_head(Wrapper,B,A):- locally_tl(second_order_wrapper(Wrapper), correct_pttp(B,A)),!.

correct_pttp_body(Wrapper,B,A):- locally_tl(second_order_wrapper(Wrapper), correct_pttp(B,A)),!.

correct_pttp(B,A):-must(correct_pttp([],B,A)),!.

correct_pttp(LC,B,A):-member_eq(B,LC),A=B.
correct_pttp(LC,-B,NA):-!,must((correct_pttp([B|LC],B,A),negated_literal(A,AN),correct_pttp(LC,AN,NA))),!.
correct_pttp(LC,n(_,B),NA):-!,must((correct_pttp([B|LC],B,A),negated_literal(A,AN),correct_pttp(LC,AN,NA))),!.
correct_pttp(LC,B,A):-once(correct_pttp_0([B|LC],B,A)),B==A,!.
correct_pttp(LC,B,A):-once(correct_pttp_0([B|LC],B,A)),!. % dmsg(once(correct_pttp_0(LC,B,A))),term_variables(B,BV),term_variables(A,AV),must(AV==BV).

correct_pttp_0(_,Body,Body):-is_ftVar(Body).
correct_pttp_0(_,Body,Body):-not(compound(Body)),!.
correct_pttp_0(_,BodyIn,Body):- is_ftVar(BodyIn),trace_or_throw(var_correct_lit(BodyIn,Body)).
correct_pttp_0(LC,BodyIn,Body):- functor(BodyIn,F,A),'=..'(BodyIn,[F|List]),correct_pttp_1(LC,BodyIn,F,A,List,Body).

correct_pttp_1(LC, BodyIn,F,_,_,Body):- sanity(atom(F)), atom_concat('not_',_,F),negated_literal(BodyIn,Neg),!,
   correct_pttp(LC,Neg,NegBody), 
   negated_literal(NegBody,Body).
correct_pttp_1(LC, BodyIn,F,_,_,Body):- is_holds_false_pttp(F),negated_literal(BodyIn,Neg),!,correct_pttp(LC,Neg,NegBody),negated_literal(NegBody,Body),!.
correct_pttp_1(LC, BodyIn,F,A,L,Body):- is_holds_false_pttp(F),trace_or_throw(correct_pttp_1(LC,BodyIn,F,A,L,Body)).
correct_pttp_1(LC,_BodyIn,F,_,[L|IST],Body):- length([L|IST],A), correct_pttp_2(LC,F,A,[L|IST],Body).

:- kb_shared(wrapper_for/2).

correct_pttp_2(_,F,_,[L|IST],Body):- wrapper_for(F,Wrapper),!, wrap_univ(Body ,[Wrapper,F,L|IST]).
correct_pttp_2(_,F,A,[L|IST],Body):- correct_pttp_4(F,A,[L|IST],Body),!.
correct_pttp_2(_LC,F,_A,[L|IST],Body):- do_not_wrap(F),!,wrap_univ(Body,[F,L|IST]).
correct_pttp_2(_LC,F,A,[L|IST],Body):- atom(F),pttp_builtin(F,A),!,dmsg(todo(warn(pttp_builtin(F,A)))),wrap_univ(Body,[call_builtin,F,L|IST]).
correct_pttp_2(LC,F,_, L,Body):- is_ftVar(F),!,trace_or_throw(correct_pttp_2(LC,F,L,Body)).
correct_pttp_2(_LC,F,_,[L|IST],Body):- is_holds_true_pttp(F),!,wrap_univ(Body,[F,L|IST]).
% uncomment (need it) 
correct_pttp_2(_,infer_by,1,[L|IST],Body):- infer_by = F, wrap_univ(Body ,[F,L|IST]).

% slow for 7
correct_pttp_2(LC,F,A,[L|IST],Body):-correct_pttp_3(LC,F,A,[L|IST],Body),!.

correct_pttp_3(_,F,A,[L|IST],Body):- correct_pttp_4(F,A,[L|IST],Body),!.
correct_pttp_3(_,F,_,[L|IST],Body):- t_l:second_order_wrapper(Wrapper),!, wrap_univ(Body ,[Wrapper,F,L|IST]).
correct_pttp_3(_,F,_,[L|IST],Body):- wrap_univ(Body,[true_t,F,L|IST]).

wrap_univ(Body ,[WapperPred,[P]]):-is_wrapper_pred(WapperPred),compound(P),P=..F_ARGS,!,wrap_univ(Body ,[WapperPred|F_ARGS]).
wrap_univ(Body ,[WapperPred,P]):-is_wrapper_pred(WapperPred),compound(P),P=..F_ARGS,!,wrap_univ(Body ,[WapperPred|F_ARGS]).
wrap_univ(Body ,[F1,F2|ARGS]):- F1==F2,!,wrap_univ(Body ,[F1|ARGS]).
wrap_univ(_Body,[F|ARGS]):- must((atom(F),is_list(ARGS))),length(ARGS,A),must(A>1),functor(P,F,A),fail,
  (predicate_property(P,_)->fail;(dmsg(once(warn(no_predicate_property(P)))))),fail.
wrap_univ(Body ,[F|List]):- must((Body=..[F|List])).

is_wrapper_pred(VarPred):-is_ftVar(VarPred),!,fail.
is_wrapper_pred(not_possible_t).
is_wrapper_pred(call_builtin).
is_wrapper_pred(WapperPred):-is_p_or_not(WapperPred),!.

% correct_pttp_4(F,A,[L|IST],Body):-...
correct_pttp_4(_,_,_,_):-!,fail.

%%% ***
%%% ****if* PTTP/pttp1
%%% SOURCE

%:- was_export(pttp1/2).
%pttp1(X,Y) :- must_pttp_id(ID), !, pttp1_wid(ID, X,Y).
:- was_export(pttp1_wid/3).
pttp1_wid(ID,X,Y) :-    
 must_det_l((   
   pttp1a_wid(ID,X,X0),
   pttp1b_wid(ID,X0,X8),
   pttp1c_wid(ID,X0,X8,IntProcs,Procs),
   conjoin_pttp(Procs,IntProcs,Y))).



:- was_export(pttp1a_wid/3).

% pttp1a_wid(ID,X,XX):-pttp1a_wid_0(ID,X,XX),!.

pttp1a_wid(ID,X,XX):-pttp1a_wid_0(ID,X,X0),
  ((X0=(FOO:-TRUE),TRUE==true)->pttp1a_wid_0(ID,FOO,XX);XX=X0).


pttp1a_wid_0(ID,X,X0) :-    
 must_det_l((
        subst(X , ~,-,XX1),
        subst(XX1,~,-,XX2),
        subst(XX2,not,-,XX3),
	% write('PTTP input formulas:'),        
	clauses(XX3,X0,ID,_))).

pttp1b_wid(_ID,X0,X8) :- must(apply_to_conjuncts(X0,add_features,X8)).


pttp1c_wid(_ID,X0,X8,IntProcs,Procs) :-    
 must_det_l((
	predicates(X8,IntPreds0),
	list_reverse(IntPreds0,IntPreds1),
	procedures(IntPreds1,X8,IntProcs),
	predicates(X0,Preds0),
	list_reverse(Preds0,Preds),
	apply_to_elements(Preds,make_wrapper(IntPreds1),Procs))).




% :- ensure_loaded(dbase_i_mpred_pttp_compile_stickel_orig).


%%% ***
%%% ****if* PTTP/pttp2
%%% SOURCE

:- was_export(pttp2_wid/2).
pttp2_wid(ID,Y) :- !, must(apply_to_conjuncts(Y,pttp_assert_int_wid_for_conjuncts(ID),_)).
/*
:- was_export(pttp2/1).
pttp2(Y) :- must_pttp_id(ID), pttp2_wid(ID,Y).

pttp2(Y) :-
%	nl,
%	write('PTTP output formulas:'),
%	apply_to_conjuncts(Y,write_clause,_),
%	nl,
	nl,
	tell('pttp_temp.pl'),
	apply_to_conjuncts(Y,write_clause,_),
	nl,
	told,
	compile('pttp_temp.pl'),
	nl,
	!.
%%% ***
%%% ****if* PTTP/expand_input_proof
%%% SOURCE
*/

:- was_export(expand_input_proof/2).
expand_input_proof([],_Proof).
expand_input_proof([N|L],[[N|_]|L1]) :-
	expand_input_proof(L,L1).
%%% ***
%%% ****if* PTTP/contract_output_proof
%%% SOURCE

:- was_export(contract_output_proof/2).
contract_output_proof([Prf|PrfEnd],Proof) :-
	Prf == PrfEnd,
	!,
	Proof = [].
contract_output_proof([[[N,_,_,_]|L]|PrfEnd],[N|L1]) :-
	contract_output_proof([L|PrfEnd],L1).
%%% ***
%%% ****if* PTTP/proof_length
%%% SOURCE

proof_length([Prf|PrfEnd],N) :-
	Prf == PrfEnd,
	!,
	N = 0.
proof_length([[[_,X,_,_]|L]|PrfEnd],N) :-
	proof_length([L|PrfEnd],N1),
	(X == query -> N is N1; N is N1 + 1).
%%% ***
%%% ****if* PTTP/proof_depth
%%% SOURCE

proof_depth([Prf|PrfEnd],N) :-
	Prf == PrfEnd,
	!,
	N = 0.
proof_depth([[[_,_,PosAnc,NegAnc]|L]|PrfEnd],N) :-
	proof_depth([L|PrfEnd],N1),
	list_length_pttp(PosAnc,N2),
	list_length_pttp(NegAnc,N3),
	N4 is N2 + N3,
	max(N1,N4,N).
%%% ***

%%% ****if* PTTP/pttp_functor
%%% DESCRIPTION
%%%   Sometimes the `functor' predicate doesn't work as expected and
%%%   a more comprehensive predicate is needed.  The pttp_functor'
%%%   predicate overcomes the problem of functor(X,13,0) causing
%%%   an error in Symbolics Prolog.  You may need to use it if
%%%   `functor' in your Prolog system fails to construct or decompose
%%%   terms that are numbers or constants.
%%% SOURCE

pttp_functor(Term,F,N) :-
	is_ftNonvar(F),
	atomic(F),
	N == 0,
	!,
	Term = F.
pttp_functor(Term,F,N) :-
	is_ftNonvar(Term),
	atomic(Term),
	!,
	F = Term,
	N = 0.
pttp_functor(Term,F,N) :-
	functor(Term,F,N).
%%% ***
%%% ****if* PTTP/list_append
%%% SOURCE

list_append([X|L1],L2,[X|L3]) :-
	list_append(L1,L2,L3).
list_append([],L,L).
%%% ***
%%% ****if* PTTP/list_reverse
%%% SOURCE

list_reverse(L1,L2) :-
	revappend(L1,[],L2).

revappend([X|L1],L2,L3) :-
	revappend(L1,[X|L2],L3).
revappend([],L,L).
%%% ***
%%% ****if* PTTP/list_union
%%% SOURCE

list_union([X|L1],L2,L3) :-
	identical_member_special(X,L2),
	!,
	list_union(L1,L2,L3).
list_union([X|L1],L2,[X|L3]) :-
	list_union(L1,L2,L3).
list_union([],L,L).
%%% ***
%%% ****if* PTTP/list_length_pttp
%%% SOURCE

list_length_pttp([_X|L],N) :-
	list_length_pttp(L,N1),
	N is N1 + 1.
list_length_pttp([],0).
%%% ***
%%% ****if* PTTP/min
%%% SOURCE

min(X,Y,Min) :-
	X =< Y ->
		Min = X;
	%true ->
		Min = Y.
%%% ***
%%% ****if* PTTP/max
%%% SOURCE

max(X,Y,Max) :-
	X =< Y ->
		Max = Y;
	%true ->
		Max = X.
%%% ***
%%% ****if* PTTP/conjoin_pttp
%%% SOURCE

:- was_export(conjoin_pttp/3).

conjoin_pttp(A,B,C) :- A==B, !, C=A.
conjoin_pttp(A,B,C) :- var(A),!,conjoin_pttp(varcall(A),B,C).
conjoin_pttp(A,B,C) :- var(B),!,conjoin_pttp(A,varcall(B),C).
conjoin_pttp(infer_by(_),B,B) :- !.
conjoin_pttp(false,true,call(false)).
conjoin_pttp(A,B,C) :- B==false,!,conjoin_pttp(false,A,C).
conjoin_pttp(A,B,C) :- A==false,!,must(negated_literal(B,C)).
conjoin_pttp(A,B,C) :-
	A == true ->
		C = B;
	B == true ->
		C = A;
        A == false ->
		C = false;
	B == false ->
		C = false;
	%true ->
		C = (A , B).
%%% ***
%%% ****if* PTTP/pttp_disjoin
%%% SOURCE

pttp_disjoin(A,B,C) :-
	A == true ->
		C = true;
	B == true ->
		C = true;
	A == false ->
		C = B;
	B == false ->
		C = A;
	%true ->
		C = (A ; B).



is_builtin_p_to_n('mudEquals','not_mudEquals').

%is_p_to_n_2way('answerable_t','unknown_t').
is_p_to_n_2way('askable_t','fallacy_t').


%ODD is_p_to_n(',','not_both_t').
%ODD is_p_to_n(';','not_either_t').
%ODD is_p_to_n('&','not_both_t').
%ODD is_p_to_n('v','not_either_t').
%ODD is_p_to_n('both_t','not_both_t').
%ODD is_p_to_n('not_both_t',',').
% is_p_to_n('true_t','not_possible_t').
% is_p_to_n('not_true_t','possible_t').
is_p_to_n('possible_t','not_possible_t').
is_p_to_n('not_possible_t','possible_t').

%is_p_to_n(P,N):-is_p_to_n_2way(P,N).
%is_p_to_n(P,N):-is_p_to_n_2way(N,P).
is_p_to_n('not_unknown_t','not_answerable_t').
% TODO is_p_to_n('not_true_t','possible_t').
is_p_to_n('proven_in','impossible_in').
is_p_to_n(P,N):-is_builtin_p_to_n(P,N).
is_p_to_n('isa','not_mudIsa').
is_p_to_n(P0,N0):-is_p_to_not(P),atom_concat('not_',P,N),P0=P,N0=N.
is_p_to_n(P,N):- false,is_p_to_n1(P,N).

is_p_to_not('asserted_t').
is_p_to_not('possible_t').
is_p_to_not('true_t').
is_p_to_not('not_true_t').
is_p_to_not('fallacy_t').
is_p_to_not('answerable_t').


is_p_to_not('unknown_t').
is_p_to_not('askable_t').

is_p_to_not('pred_isa_t').


is_p_to_not('pred_t').


is_p_or_not(F):-is_p_to_n(P,N),(F=P;F=N).

% possible_t TODO

is_p_to_n1(P,N):-atom(P),is_p_to_n0(PF,NF),atom_concat(Root,PF,P),atom_concat(Root,NF,N).
is_p_to_n1(P,N):-atom(N),is_p_to_n0(PF,NF),atom_concat(Root,NF,N),atom_concat(Root,PF,P).
is_p_to_n1(P,N):-atom(P),is_p_to_n0(PF,NF),atom_concat(PF,Root,P),atom_concat(NF,Root,N).
is_p_to_n1(P,N):-atom(N),is_p_to_n0(PF,NF),atom_concat(NF,Root,N),atom_concat(PF,Root,P).

is_p_to_n0('_pos','_neg').
is_p_to_n0('true_','false_').
is_p_to_n0('_true','_false').
is_p_to_n0('pos_','neg_').
is_p_to_n0('when_','unless_').
is_p_to_n0('possible_','impossible_').



%is_p_simple('not_proven_not_t','possible_t').
%is_p_simple('not_possible_t','not_true_t').
%is_p_simple('not_unknown_t','answerable_t').
%is_p_simple('not_answerable_t','unknown_t').
is_p_simple(X,X).
%%% ***
%%% ****if* PTTP/negated_functor
%%% SOURCE

negated_functor0(_,_):-!,fail.
%negated_functor0(true_t,not_possible_t).
%negated_functor0(not_true_t,possible_t).

%negated_functor0(F,NotF) :- is_p_to_n(F,NotF).
%negated_functor0(F,NotF) :- is_p_to_n(NotF,F).

:- was_export(negated_functor/2).
negated_functor(F,NotF) :- var(F),!,trace_or_throw(negated_functor(F,NotF)).
%negated_functor(F,NotF) :- sanity(atom(F)),atom_concat('not_',Now,F),!,must(NotF=Now).
negated_functor(F,SNotF) :- negated_functor0(F,NotF),!,is_p_simple(NotF,SNotF).
negated_functor((-),_):-!,dtrace(negated_functor((-),_)),fail.
negated_functor((~),_):-!,dtrace(negated_functor((~),_)),fail.
negated_functor(F,NotF) :- atom_concat('int_',Now,F),!,negated_functor(Now,Then),atom_concat('int_',Then,NotF),!.
negated_functor(F,NotF) :- must( \+member(F,[&,(,),(;),(v),(all),(:-)])),
	name(F,L),
	name('not_',L1),
	(list_append(L1,L2,L) ->
		true;
	%true ->
		list_append(L1,L,L2)),
	name(NotF,L2).
negated_functor(F,NotF) :- is_2nd_order_holds_pttp(F),trace_or_throw(negated_functor(F,NotF) ).
negated_functor(F,NotF) :- is_2nd_order_holds_pttp(NotF),trace_or_throw(negated_functor(F,NotF) ).

%%% ***
%%% ****if* PTTP/negated_literal
%%% SOURCE

negated_literal(A,B):-var(A),!,trace_or_throw(var_negated_literal(A,B)),!.
negated_literal(not(A),A):-!.
negated_literal(-(A),A):-!.
negated_literal(A,-(A)):-is_ftVar(A),!.
negated_literal(-(A),(A)):-is_ftVar(A),!.
negated_literal(A,-(A)):-atom(A),A\=(~),A\=(-),!.
negated_literal(A,B):- functor(A,F,_Arity),member(F,[&,(,),(;),(v),(all),(:-)]),must_det_l((as_dlog(A,AA),IN=not(AA), call((nnf('$VAR'('KB'),IN,BB),BB \=@= IN,baseKB:as_prolog(BB,B))))).
negated_literal(not(A),B):-negated_literal(A,AA),!,negated_literal_0(AA,B),!.
negated_literal(-A,B):-negated_literal(A,AA),!,negated_literal_0(AA,B),!.
negated_literal(A,B):- var(B),!,negated_literal_0(A,B),!.
negated_literal(B,-A):-negated_literal(A,AA),!,negated_literal_0(AA,B),!.
negated_literal(A,B):- negated_literal_0(A,B),!.
negated_literal(A,B):- ground(B),not(ground(A)),!,negated_literal(B,A),!.

negated_literal_0(Lit,NotLit) :-
	Lit =.. [F1|L1],
	negated_functor(F1,F2),
	(is_ftVar(NotLit) ->
		wrap_univ(NotLit , [F2|L1]);
	%true ->
	       
               ( wrap_univ(NotLit , [F2|L2]),
		L1 == L2) ).

%%% ***
%%% ****if* PTTP/is_negative_functor
%%% SOURCE

is_negative_functor(F) :- is_holds_false_pttp(F),!.
is_negative_functor(F) :-
	name(F,L),
	name('not_',L1),
	list_append(L1,_,L).
%%% ***
%%% ****if* PTTP/is_negative_literal
%%% SOURCE

is_negative_literal(Lit) :-
	functor(Lit,F,_),
	is_negative_functor(F).

%%% ***
%%% ****if* PTTP/internal_functor
%%% SOURCE

internal_functor(P) :-
	name(P,L),
	name('int_',L1),
	list_append(L1,_,L).

internal_functor(P,IntP) :-
	name(P,L),
	name('int_',L1),
	list_append(L1,L,L2),
	name(IntP,L2).
%%% ***
%%% ****if* PTTP/apply_to_conjuncts
%%% SOURCE

apply_to_conjuncts(Wff,P,Wff1) :-
	Wff = (A , B) ->
		apply_to_conjuncts(A,P,A1),
		apply_to_conjuncts(B,P,B1),
		conjoin_pttp(A1,B1,Wff1);
	%true ->
		P =.. G,
		list_append(G,[Wff,Wff1],G1),
		T1 =.. G1,
		call(T1).
%%% ***
%%% ****if* PTTP/apply_to_elements
%%% SOURCE

apply_to_elements([X|L],P,Result) :-
	P =.. G,
	list_append(G,[X,X1],G1),
	T1 =.. G1,
	call(T1),
	apply_to_elements(L,P,L1),
	conjoin_pttp(X1,L1,Result).
apply_to_elements([],_,true).

%%% ***
%%% ****if* PTTP/write_clause
%%% SOURCE

write_clause(A) :-
	nl,
	write(A),
	write(.).

write_clause(A,_) :-				% 2-ary predicate for use as
	write_clause(A).			% apply_to_conjuncts argument
%%% ***
%%% ****if* PTTP/write_clause_with_number
%%% SOURCE

write_clause_with_number(A,WffNum) :-
	nl,
	write_indent_for_number(WffNum),
	write(WffNum),
	write('  '),
        copy_term(A,AA),
        numbervars(AA,0,_,[attvar(bind),singletons(true)]),
	write(AA),
	write(.).

write_indent_for_number(N) :-
	((number(N) , N <  100) -> write(' ') ; true),
	((number(N) , N <   10) -> write(' ') ; true).
%%% ***

%%% ****if* PTTP/timed_call
%%% DESCRIPTION
%%%   A query can be timed by timed_call(query,'Proof').
%%% NOTES
%%%   assumes that statistics(cputime,T) binds T to run-time in seconds
%%%   different Prolog systems have different ways to get this information
%%% SOURCE
:- was_export(timed_call/2).
:- meta_predicate(timed_call(0,+)).
timed_call(X,Type) :-
	statistics(cputime,T1),			%SWI Prolog
	(call(time(X)) -> V = success ; V = failure),	%SWI Prolog
	statistics(cputime,T2),			%SWI Prolog
	Secs is T2 - T1,			%SWI Prolog
%	statistics(runtime,[T1,_]),		%Quintus/SICStus Prolog
%	(call(X) -> V = success ; V = failure),	%Quintus/SICStus Prolog
%	statistics(runtime,[T2,_]),		%Quintus/SICStus Prolog
%	Secs is (T2 - T1) / 1000.0,		%Quintus/SICStus Prolog
	nl,
	write(Type),
	write(' time: '),
	write(Secs),
	write(' seconds'),
	nl,
	V = success.
%%% ***
%%% ****if* PTTP/write_search_progress
%%% SOURCE

write_search_progress(Level) :- Level>100,!.

write_search_progress(_Level) :- !.
write_search_progress(Level) :-
	% write('cost '),
	write(Level),write('.'),current_output(S),flush_output(S).

%%% ***

%%% ****if* PTTP/pttp_builtin
%%% DESCRIPTION
%%%   List of pttp_builtin predicates that can appear in clause bodies.
%%%   No extra arguments are added for ancestor goals or depth-first
%%%   iterative-deepening search.  Also, if a clause body is
%%%   composed entirely of pttp_builtin goals, the head is not saved
%%%   as an ancestor for use in reduction or pruning.
%%%   This list can be added to as required.
%%% SOURCE

pttp_builtin(T) :-
	functor(T,F,N),
	pttp_builtin(F,N).


pttp_builtin(V,A):-is_ftVar(V),!,trace_or_throw(pttp_builtin(V,A)).
pttp_builtin(!,0).


pttp_builtin(F,A):- mpred_prop(F,A,prologHybrid),!,fail.
pttp_builtin(isa,2):-!,fail.
pttp_builtin(isa,_):-!,fail.
pttp_builtin(S2,_):-is_p_to_not(S2),!,fail.

pttp_builtin(call_proof,2).
pttp_builtin(query,0):-!,fail.
pttp_builtin(true,0).
pttp_builtin(false,0).
pttp_builtin(fail,0).
pttp_builtin(succeed,0).
pttp_builtin(dtrace,0).
pttp_builtin(atom,1).
pttp_builtin(integer,1).
pttp_builtin(number,1).
pttp_builtin(F,_):-is_p_or_not(F),!,fail.
pttp_builtin(not_asserted_t,_):-!,fail.
pttp_builtin(clause_u,1).
pttp_builtin(atomic,1).
pttp_builtin(constant,1).
pttp_builtin(functor,3).
pttp_builtin(arg,3).
pttp_builtin(var,1).
%pttp_builtin(->,2).
%pttp_builtin(->,3).
pttp_builtin(nonvar,1).
pttp_builtin(call,1).
pttp_builtin(=,2).
pttp_builtin(\=,2).
pttp_builtin(==,2).
pttp_builtin(\==,2).
pttp_builtin(=\=,2).
pttp_builtin(>,2).
pttp_builtin(<,2).
pttp_builtin(>=,2).
pttp_builtin(loop_check,_).
pttp_builtin(=<,2).
pttp_builtin(is,2).
pttp_builtin(display,1).
pttp_builtin(write,1).
pttp_builtin(nl,0).
pttp_builtin(only_if_pttp,0).
pttp_builtin(ANY,A):-atom(ANY),A==0.
pttp_builtin(infer_by,_).
pttp_builtin(search_cost,_).
pttp_builtin(mudEquals,2):-!.
pttp_builtin(F,A):-functor(P,F,A),prequent(P),!.
pttp_builtin(F,A):-is_builtin_p_to_n(P,N),member(F,[P,N]),member(A,[2,3,4]).
pttp_builtin(test_and_decrement_search_cost,_).
pttp_builtin(unify,_).
pttp_builtin(identical_member_special,_).
pttp_builtin(identical_member_special_loop_check,_).
pttp_builtin(M:P,A):-atom(M),!,pttp_builtin(P,A).
pttp_builtin(F,A):- (mpred_prop(F,A,prologBuiltin)),!. %,fail.
% TODO pttp_builtin(F,_):- (mpred_prop(F,A,prologDynamic)),!. %,fail.
pttp_builtin(unifiable_member,_).
% TODO pttp_builtin(t,_).
%pttp_builtin(F,A):-mpred_prop(F,A,prologPTTP),!,fail.
%pttp_builtin(F,A):-mpred_prop(F,A,prologKIF),!,fail.
pttp_builtin(F,A):-current_predicate(F/A),functor(P,F,A),builtin_why(P,F,A,Why),!,dmsg(todo(warn(builtin_why(F,A,Why)))).
%%% ***

builtin_why(_,int_query,_,_):-!,fail.
builtin_why(_,query,_,_):-!,fail.
builtin_why(_,F,_,int_):- atom_concat(_,'_int',F),!.
builtin_why(_,F,_,int_):- atom_concat('int_',_,F),!,fail.
builtin_why(P,_,_,meta_predicate(P)):- predicate_property(P,meta_predicate(P)).
builtin_why(P,_,_,thread_local):- predicate_property(P,thread_local).
builtin_why(P,_,_,source_file(F)):- source_file(P,F).
builtin_why(P,_,_,built_in):- real_builtin_predicate(P).
builtin_why(P,_,_,transparent):- predicate_property(P,transparent).
% builtin_why(P,_,_,number_of_rules(N)):- predicate_property(P,number_of_rules(N)),N>0.
builtin_why(X,0):-atom(X).
%builtin_why(P,2,t(P,2)):-t(P,_,_),!,fail.
%builtin_why(P,3,t(P,3)):-t(P,_,_,_),!,fail.




% -----------------------------------------------------------------
%  pttp_nnf(+Fml,?NNF)
%
% Fml is a first-order formula and NNF its Skolemized negation 
% normal form.
%
% Syntax of Fml:
%  negation: '-', disj: 'v', conj: '&', impl: '=>', eqv: '<=>',
%  quant. 'all(X,<Formula>)', where 'X' is a prolog variable.
%
% Syntax of NNF: negation: '-', disj: ';', conj: ',', quant.:
%  'all(X,<Formula>)', where 'X' is a prolog variable.
%
% Example:  pttp_nnf(ex(Y, all(X, (f(Y) => f(X)))),NNF).
%           NNF =  all(_A,(-(f(all(X,f(ex)=>f(X))));f(_A)))) ?
:- was_export(pttp_nnf/2).
pttp_nnf((A,B),(C,D)):- must(is_ftNonvar(A)), !, pttp_nnf(A,C), pttp_nnf(B,D).
pttp_nnf(Fml,NNFOUT) :- pttp_nnf(Fml,[],NNF,_),NNFOUT=NNF.

:-     op(400,fy,-),    % negation
	op(500,xfy,&),   % conjunction
	op(600,xfy,v),   % disjunction
	op(650,xfy,=>),  % implication
	op(680,xfy,<=>). % equivalence


% -----------------------------------------------------------------
%  pttp_nnf(+Fml,+FreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

pttp_nnf_pre_clean(_Type,Atomic,Atomic,[]):-atomic(Atomic),!.
pttp_nnf_pre_clean(_Type,Atomic,Atomic,[]):-is_ftVar(Atomic),!.
pttp_nnf_pre_clean(Type,pttp(A),AA,Vars):- !,pttp_nnf_pre_clean(Type,A,AA,Vars).
pttp_nnf_pre_clean(Type,[A|B],[AA|BB],Vars):-!,
   pttp_nnf_pre_clean(Type,A,AA,Vars1),
   pttp_nnf_pre_clean(Type,B,BB,Vars2),
   append(Vars1,Vars2,Vars).

pttp_nnf_pre_clean(_Type,C,CC,Vars):-
   C=..[A|B],
   logical_functor_pttp(A),!,
   pttp_nnf_pre_clean_functor(A,AA,Vars1),!,
   pttp_nnf_pre_clean(sent,B,BB,Vars2),
   append(Vars1,Vars2,Vars),
   CC=..[AA|BB],!.

pttp_nnf_pre_clean(Type,CIN,CC,Vars):-
   Type == sent,
   correct_pttp(CIN,C),
   C=..[A|B],
   pttp_nnf_pre_clean_functor(A,AA,Vars1),!,
   pttp_nnf_pre_clean(arg,B,BB,Vars2),
   append(Vars1,Vars2,Vars),
   CC=..[AA|BB],!.

pttp_nnf_pre_clean(Type,C,CC,Vars):-
   C=..[A|B],
   pttp_nnf_pre_clean_functor(A,AA,Vars1),!,
   pttp_nnf_pre_clean(Type,B,BB,Vars2),
   append(Vars1,Vars2,Vars),
   CC=..[AA|BB],!.


pttp_nnf_post_clean(Atomic,Atomic,[]):-atomic(Atomic),!.
pttp_nnf_post_clean(Atomic,Atomic,[]):-is_ftVar(Atomic),!.
pttp_nnf_post_clean(pttp(A),AA,Vars):- !,pttp_nnf_post_clean(A,AA,Vars).
pttp_nnf_post_clean(-(A),NN,Vars):- !,pttp_nnf_post_clean(A,AA,Vars),negated_literal(AA,NN).
pttp_nnf_post_clean((A,B),(AA , BB),Vars):-
   pttp_nnf_post_clean(A,AA,Vars1),
   pttp_nnf_post_clean(B,BB,Vars2),
   append(Vars1,Vars2,Vars).
pttp_nnf_post_clean((A;B),(AA ; BB),Vars):-
   pttp_nnf_post_clean(A,AA,Vars1),
   pttp_nnf_post_clean(B,BB,Vars2),
   append(Vars1,Vars2,Vars).
pttp_nnf_post_clean([A|B],[AA|BB],Vars):-
   pttp_nnf_post_clean(A,AA,Vars1),
   pttp_nnf_post_clean(B,BB,Vars2),
   append(Vars1,Vars2,Vars).
pttp_nnf_post_clean((A&B),(AA , BB),Vars):- fail,
   pttp_nnf_post_clean(A,AA,Vars1),
   pttp_nnf_post_clean(B,BB,Vars2),
   append(Vars1,Vars2,Vars).
pttp_nnf_post_clean((A v B),(AA ; BB),Vars):- fail,
   pttp_nnf_post_clean(A,AA,Vars1),
   pttp_nnf_post_clean(B,BB,Vars2),
   append(Vars1,Vars2,Vars).
pttp_nnf_post_clean(C,CC,Vars):-
   C=..[A|B],
   A=AA,
   pttp_nnf_post_clean(B,BB,Vars),
   CC=..[AA|BB],!.



pttp_nnf(Fml,FreeV,CleanNNF,Paths):-
   pttp_nnf_pre_clean(sent,Fml,Clean,FreeV),
   pttp_nnf_clean(Clean,FreeV,NNF,Paths),
   pttp_nnf_post_clean(NNF,CleanNNF,FreeV).

pttp_nnf_clean(Atomic,_,Atomic,1):-atomic(Atomic),!.
pttp_nnf_clean(Atomic,_,Atomic,1):-is_ftVar(Atomic),!.
pttp_nnf_clean(Fml,FreeV,NNF,Paths) :-   
	(Fml = -(-A)      -> Fml1 = A;
	 Fml = -all(X,F)  -> Fml1 = ex(X,-F);
	 Fml = -ex(X,F)   -> Fml1 = all(X,-F);
	 Fml = -(A v B)   -> Fml1 = (-A & -B);
	 Fml = -(A & B)   -> Fml1 = (-A v -B);
	 Fml = (A => B)   -> Fml1 = (-A v B);
	 Fml = -(A => B)  -> Fml1 = A & -B;
	 Fml = (A <=> B)  -> Fml1 = (A & B) v (-A & -B);
	 Fml = -(A <=> B) -> Fml1 = (A & -B) v (-A & B)),!,
	pttp_nnf_clean(Fml1,FreeV,NNF,Paths).

pttp_nnf_clean(all(X,F),FreeV,all(X,NNF),Paths) :- !,
	pttp_nnf_clean(F,[X|FreeV],NNF,Paths).

pttp_nnf_clean(ex(X,Fml),FreeV,NNF,Paths) :- !,
	copy_term((X,Fml,FreeV),(sk(X,Fml),Fml1,FreeV)),
	pttp_nnf_clean(Fml1,FreeV,NNF,Paths).

pttp_nnf_clean((A & B),FreeV,(NNF1,NNF2),Paths) :- !,
	pttp_nnf_clean(A,FreeV,NNF1,Paths1),
	pttp_nnf_clean(B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2.

pttp_nnf_clean((A v B),FreeV,NNF,Paths) :- !,
	pttp_nnf_clean(A,FreeV,NNF1,Paths1),
	pttp_nnf_clean(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2 -> NNF = (NNF2;NNF1);
		            NNF = (NNF1;NNF2)).

pttp_nnf_clean(Lit,_,Lit,1).


% :- ensure_loaded(dbase_i_mpred_pttp_precompiled).


:- fixup_exports.
