/* ************************************************************** */
/* Operators                                                      */
/* ************************************************************** */

/*  propositional operators are: */

:-op(140, fy, ~).
:-op(160, xfy, and).
:-op(160, xfy, or).
:-op(180, xfy, =>).

/*  unary temporal operators are: */

:-op(140, fy, sometimes).
:-op(140, fy, always).
:-op(140, fy, next).

/*  binary temporal operators are: */

:-op(160, xfy, until).
:-op(160, xfy, unless).

/*  knowledge operator: */

:-op(140, fy, k).

/**********************************************/
/*                                            */
/*  Modal Literals                            */
/*                                            */
/**********************************************/

is_new(new(_)).

is_snl_literal(unknown(_)).

is_snl_literal(~ unknown(_)).

is_modal(k L):-
  is_literal(L).

is_modal(~ k L):-
  is_literal(L).

is_modal_literal(L):-
  is_modal(L).

is_modal_literal(L):-
  is_literal(L).

/*****************************************************************************/
/* is_literal(X).                                                            */
/*                                                                           */
/* Returns true if X is a proposition or negation of a proposition.          */
/*****************************************************************************/

is_literal([_]):- !,fail.       % The exclusion of lists is needed for some predicates 
                                % in ntempres.pl (eg includes_or).
is_literal([_|_]):-!,fail.

is_literal(~ X):-
  is_literal(X).

is_literal(X):-
  is_proposition(X).

is_literal(X):-
  is_constant(X).

is_not_literal(~ X):- !,
   is_not_proposition(X).

is_not_literal(X):-
   is_not_proposition(X).


/**********************************************/
/*                                            */
/* Constants                                  */
/*                                            */
/**********************************************/

is_constant(true).
is_constant(false).
is_constant(start).

/*****************************************************************************/
/* is_proposition(X).                                                        */
/*                                                                           */
/* Returns true if X is a proposition.                                       */
/*****************************************************************************/

is_proposition(Formula) :- 
   \+(is_not_proposition(Formula)).

/*****************************************************************************/
/* is_not_proposition(X).                                                    */
/*                                                                           */
/* Returns true if X is not a proposition ie. X is a formula which includes  */
/* temporal operators, boolean operators, or are true or false.              */
/*****************************************************************************/

is_not_proposition([]).
is_not_proposition(X) :- is_constant(X).
is_not_proposition(_ and _).
is_not_proposition(_ or _).
is_not_proposition(~ _).
is_not_proposition(sometimes _).
is_not_proposition(always _).
is_not_proposition(_ => _).
is_not_proposition(next _).
is_not_proposition(k _).
is_not_proposition(_ until _).
is_not_proposition(_ unless _).

/*****************************************************************************/
/*  conjunctive(X) :- X is an alpha formula                                  */
/*                                                                           */
/*****************************************************************************/

conjunctive(_ and _).
conjunctive(~(_ or _)).
conjunctive(~(_ => _)).

/*****************************************************************************/
/* disjunctive(X) :- X is a beta formula                                     */
/*                                                                           */
/*****************************************************************************/

disjunctive(~(_ and _)).
disjunctive(_ or _).
disjunctive(_ => _).

/**********************************************/
/*                                            */
/*  Counters: definitions, functions          */
/*                                            */
/**********************************************/

/* for renaming predicates */

:-dynamic predcount/1. /* for sicstus only */

/* for skolem functions/constants */

:-dynamic rulecount/1. /* for sicstus only */

:-dynamic clear/0.     /* for sicstus only */

/* predcount(N) :- N is the current Skolem function index */

predcount(1).
rulecount(1).

clear :-retract(predcount(_)), 
        assert(predcount(1)).


/*****************************************************************************/
/*        newpredcount(N)                                                    */
/*                                                                           */
/*        N is the current new predicate index, and as a                     */
/*        side effect, the remembered value is incremented.                  */
/*                                                                           */
/*****************************************************************************/

newpredcount(N) :- predcount(N),
	           retract(predcount(N)),
		   M is N+1,
		   assert(predcount(M)).

startrulecount(N) :- rulecount(N),
                     retract(rulecount(N)),
                     assert(rulecount(1)).
startrulecount(1).

newrulecount(N) :- rulecount(N),
	          retract(rulecount(N)),
		  M is N+1,
		  assert(rulecount(M)).

/********************************************************/
/* New propositions                                     */
/********************************************************/

new_temp_prop(V):- newpredcount(N),
                   term_to_atom(N,NN),
                   string_concat('tmpp',NN,NV),
                   atom_to_term(NV,V,_).

new_dontknow_prop(X,X):- is_snl_literal(X).
new_dontknow_prop(V,X):- V=.. [unknown|[X]].

new_conj_prop(V,true):-V=[true].
new_conj_prop(V,X):- V=.. [new|[X]].

/* ************************************************************** */
/* OUTPUT UTILITIES FOR CLAUSES                                   */
/* ************************************************************** */

/* Remove comments if you do not wish a very verbose output */

% my_writef(_X,_Y):-true,!.
% my_write(_X):-true,!.
% write_ruleset(_X):-true,!.

/* For very verbose output */

 my_writef(X,Y):-writef(X,Y).
 my_write(X):-write(X).

/* ************************************************************** */
/* Formatted output of the prover: lists of clauses               */
/* ************************************************************** */

write_ruleset((Initial,Literal,Modal,Temporal)) :-
        nl,write('Initial Clauses'),nl, 
	write('['), nl, write_ruleset1(Initial),!, write(']'), nl,
        nl,write('Literal Clauses'),nl, 
	write('['), nl, write_ruleset1(Literal),!, write(']'), nl,
        nl,write('Modal Clauses'),nl, 
	write('['), nl, write_ruleset1(Modal),!, write(']'), nl,
        nl,write('Temporal Clauses'),nl, 
	write('['), nl, write_ruleset1(Temporal),!, write(']'), nl.

write_ruleset(Ruleset) :- 
	write('['), nl, write_ruleset1(Ruleset),!, write(']'), nl.

write_ruleset1([Rule | Rest]) :- 
	write('  '), write_form(Rule), nl, write_ruleset1(Rest).

write_ruleset1([]).

/* ************************************************************** */
/* Formatted output of the prover: clauses                        */
/* ************************************************************** */

write_form(r(X,Y,false)) :- 
        write(X), 
        write(' '), 
        write(Y), 
        write(' '), 
        write(' false ').

write_form(r(X,Y,P)) :-
        write(X), 
        write(' '), 
        write(Y), 
        write(' '), 
        write_form(P).

write_form(P => F) :- 
        write_form(P), 
        write(' => '), 
        write_form(F).

%write_form(neg X) :- write('~'), !, write_form(X).
write_form(~ X) :- 
        write('~'), !, 
        write_form(X).
write_form(- X) :- 
        write('~'), !, 
        write_form(X).


write_form(X and Y) :-
	write('('),
	!,
	write_form(X), 
	write(' & '),
	write_form(Y),
	write(')').

write_form(X or Y) :- 
	write('('),
	!,
	write_form(X),
	write(' | '),
	write_form(Y),
	write(')').

write_form(X until Y) :- 
	write('('),
	!,
	write_form(X),
	write(' U '),
	write_form(Y),
	write(')').

write_form(X unless Y) :- 
	write('('),
	!,
	write_form(X),
	write(' W '),
	write_form(Y),
	write(')').

write_form(next X) :- write('O'), !, write_form(X).
write_form(sometimes X) :- write('<>'), !, write_form(X).
write_form(always X) :- write('[]'), !, write_form(X).
write_form(k X) :- write('k'), !, write_form(X).
write_form(new(X)) :- write('new('),!,write_form(X),write(')').
write_form(unknown(X)) :- write('unknown('),!,write_form(X),write(')').
write_form(X) :- write(X).


/**********************************************************************/
/* write_otter_rules(Rules)                                           */
/*                                                                    */
/* Takes a list of clauses (Rules) of the Prolog form r(No,List,Rule) */
/* where Rule is true => F or P => next F and rewrites              */
/* them in an Otter format i.e. if F is a or b in the above two rules */
/* the first rules would be output as s_a | s_b. and the second as    */
/* -slast_P | s_a | s_b.                                              */
/*                                                                    */
/**********************************************************************/

write_otter_rules(X,Y,Z):-
  write_otter_rules(X),
  write_otter_rules(Y),
  write_otter_rules(Z).

write_otter_rules(X,Y):-
  write_otter_rules(X),
  write_otter_rules(Y).

write_otter_rules([]).

write_otter_rules([r(_,_,Rule)|RRest]):-
  write_otter_rule(Rule),
  write_otter_rules(RRest).

write_otter_rules([Rule|RRest]):-
  write_otter_rule(Rule),
  write_otter_rules(RRest).

/**********************************************************************/
/* write_otter_rule(Rule)                                             */
/*                                                                    */
/* Takes a claue (Rule) of the Prolog form r(No,List,Rule) where Rule */
/* is true => F or P => next F and rewrites them in an              */
/* Otter format i.e. if F is a or b in the above two rules the first  */
/* rules would be output as s_a | s_b.                                */
/* and the second as -slast_P | s_a | s_b.                            */
/*                                                                    */
/**********************************************************************/

write_otter_rule(B):-
  disjunction_of_literals(B),!,
  write_otter_disjunct(B),
  write('.'),nl.

write_otter_rule(A => next B):-
  write_otter_conjunct(A),
  write(' | '),
  write_otter_disjunct(B),
  write('.'),nl.

/**********************************************************************/
/* write_otter_conjunct(Conj)                                         */
/*                                                                    */
/* Takes a literal or conjunction of literals (Conj) of the Prolog    */
/* form a and b and c from the lhs of global rules and rewrites them  */
/* as -slast_a | -slast_b | -slast_c.                                 */
/*                                                                    */
/**********************************************************************/

write_otter_conjunct(false):-
  write('$F').

write_otter_conjunct(tmp_p(X)):-
  write('-slast_tmp_p_'),
  write(X).

write_otter_conjunct(~ A):-
  write('-slast_neg_'),
  write(A).

write_otter_conjunct(new(X)):-
  trace,
  write('-slast_new_'),
  write_otter_and_proposition(X).

write_otter_conjunct(unknown(X)):-
  write('-slast_nkn_'),
  write_otter_proposition(X).

write_otter_conjunct(A):-
  is_literal(A),
  write('-slast_'),
  write(A).

write_otter_conjunct(A and B):-
  write_otter_conjunct(A),
  write(' | '),
  write_otter_conjunct(B).


/**********************************************************************/
/* write_otter_disjunct(Disj)                                         */
/*                                                                    */
/* Takes a literal or disjunction of literals (Disj) of the Prolog    */
/* form a or b or ~ c from the rhs of step clauses and rewrites them*/
/* as s_a | s_b | -s_c. We are assuming the disjunct is from the rhs  */
/* of a step clause or a literal clause.                              */
/*                                                                    */
/**********************************************************************/

write_otter_disjunct(false):-
  write('$F').

write_otter_disjunct(~ A):-
  write('-'),
  write_otter_disjunct(A).

write_otter_disjunct(tmp_p(X)):-
  write('s_tmp_p_'),
  write(X).

write_otter_disjunct(new(X)):-
  write('s_new_'),
  write_otter_and_proposition(X).

write_otter_disjunct(unknown(X)):-
  write('s_nkn_'),
  write_otter_proposition(X).

write_otter_disjunct(A):-
  is_literal(A),
  write('s_'),
  write(A).

write_otter_disjunct(A or B):-
  write_otter_disjunct(A),
  write(' | '),
  write_otter_disjunct(B).

/*********************************************/
/* Write the different kind of propositions  */
/* in the Otter format                       */
/*********************************************/

write_otter_proposition(~ X):-
  write('neg_'),
  write_otter_proposition(X).

write_otter_proposition(new(X)):-
  write('new_'),
  write_otter_and_proposition(X).

write_otter_proposition(unknown(X)):-
  write('nkn_'),
  write_otter_proposition(X).

write_otter_proposition(X):-
  is_literal(X),
  write(X).

/************************************************/
/* The following is needed in order to write    */
/* the conjunctions that label the propositions */
/* that rename conjunctions                     */
/************************************************/

write_otter_and_proposition(tmp_p(X) and B):-
  write_otter_and_proposition(B and tmp_p(X)).

write_otter_and_proposition(A and B):-
  write_otter_and_proposition(A),
  write('_'),
  write_otter_and_proposition(B).

write_otter_and_proposition(A):-
  write_otter_proposition(A).

/*****************************************************************************/
/* write_list(ERulePaths)                                                    */
/*                                                                           */
/* Takes a list comprising of eventualites paired with a list of loop paths  */
/* already found (ERulePaths). It writes out the eventuality and the paths   */
/* already found.                                                            */
/*****************************************************************************/

write_list([]).

write_list([[Rule,Path]|Rest]):-
   write('     ['),
   write_form(Rule),
   write(', '),
   write(Path),
   write('     ]\n'),
   write_list(Rest).


/*****************************************************************************/
/*  number_rules(RuleList,FromList,NewRuleList).                             */
/*                                                                           */
/*  Takes the Rulelist and makes it into the format Number,From,Text)        */
/*  and stuffs it in NewRuleList.                                            */
/*****************************************************************************/

number_rules([],_FromList,[]):-!.

number_rules([Rule|Rest],FromList,[NewRule|NewRest]):-
   number_rule(Rule,FromList,NewRule),
   number_rules(Rest,FromList,NewRest).

number_rule(r(N1,N2,Clause),_,r(N1,N2,Clause)).
number_rule(Rule,FromList,r(N,FromList,Rule)):-
   newrulecount(N).

/****************************************************************************/
/*                                                                          */
/* because of the new normal form, modal subsumption takes place if:        */
/*                                                                          */
/* (1) a modal literal is implied by true                                   */
/* (2) the clauses are exactly the same                                     */
/* (3) a literal clause is a unit clause                                    */
/*     and the negated literal is the antecedent of the modal clause        */
/*                                                                          */
/****************************************************************************/

dis_implies(true => B, _C => B):-!.
dis_implies(_A => B, true => B):-!.
dis_implies(A => B, A => B):-!.

/* 
   this only fails: 
   cut introduced to prevent redo 
   there will never be a modal literal in a literal clause 
*/

dis_implies(_A => _B, _C):-fail,!.

dis_implies(A, NegA => _C):-
  is_literal(A),!,
  snff_negate(A,NegA).

/* literal subsumption takes place as usual */

dis_implies(A,B):-!,
  list_strip_or(A,A1),
  list_strip_or(B,B1),
  subset(A1,B1).

/***************************************************/
   
set_implies([],_):-!.

set_implies(X,X):-!.

set_implies(_,[[true]]):-!.

set_implies(_,[true]):-!.

set_implies([[false]],_):-!.

set_implies([false],_):-!.

set_implies([H|Tail],[H1]):-
    is_literal(H1),
    is_literal(H),!,
    member(H1,[H|Tail]).

set_implies([H1|Tail1],[H2|Tail2]):-
    is_literal(H1),
    is_literal(H2),!,
    subset([H2|Tail2],[H1|Tail1]).

set_implies([H1],[H|Tail]):-
    is_modal_literal(H1),
    is_modal_literal(H),!,
    modal_member(H1,[H|Tail]).

set_implies([H1|Tail1],[H2|Tail2]):-
    is_modal_literal(H1),
    is_modal_literal(H2),!,
    modal_subset([H1|Tail1],[H2|Tail2]).

set_implies([H1],[H2|Tail2]):-
    is_literal(H1),!,
    is_superset_of_member_of_list([H1],[H2|Tail2]).

set_implies([H1|Tail1],[H2|Tail2]):-
    is_literal(H1),!,
    is_superset_of_member_of_list([H1|Tail1],[H2|Tail2]).

set_implies([H1|Tail1],[H2|Tail2]):-
    is_literal(H2),!,
    set_implies(H1,[H2|Tail2]),
    set_implies(Tail1,[H2|Tail2]).

set_implies([H1|List1],List2):-
    set_implies(H1,List2),
    set_implies(List1,List2).

/*******************************************************************************/
/* strip(Clause, List)                                                         */
/*                                                                             */
/*   This predicate removes all ands or ors from the clause (they should be    */
/*   all ands or all ors) and sticks remaining propositions in a list.         */
/*******************************************************************************/

strip(X or Y, AList):-
   strip(X, XList),
   strip(Y, YList),
   append(XList, YList, AList), !. % Added cuts to stop choosing alternative rules on failure

strip(X and Y, AList):-
   strip(X, XList),
   strip(Y, YList),
   append(XList, YList, AList), !. % Added cuts to stop choosing alternative rules on failure

strip(X, [X]).

/******************************************************************************/

list_strip_or(X or Y,NewXY):-
  list_strip_or(X,NewX),
  list_strip_or(Y,NewY),
  append(NewX,NewY,NewXY),!.

list_strip_or(X,[NewX]):-
  simplify_and(X,NewX).

/*******************************************************************************/
/* strip_or(Clause, List)                                                      */
/*                                                                             */
/*   This predicate removes all ands or ors from Clause which is disjunctive   */
/*   and puts them on a bracketed List eg (a and b) or c would be [[a,b],[c]]  */
/*******************************************************************************/

strip_or(X or Y, NewXY):-
  strip_or(X, NewX),
  strip_or(Y, NewY),
  append(NewX, NewY, NewXY),!.

strip_or(X and Y, [NewXY]):-
   strip(X and Y, NewXY),!.

strip_or(X,[[X]]) :- !.

/*******************************************************************************/
/* strip_and(Clause, List)                                                     */
/*                                                                             */
/*   This predicate removes all ands or ors from Clause which is disjunctive   */
/*   and puts them on a bracketed List eg (a and b) or c would be [[a,b],[c]]  */
/*******************************************************************************/

strip_and(X and Y, NewXY):-
  strip_and(X, NewX),
  strip_and(Y, NewY),
  append(NewX, NewY, NewXY),!.

strip_and(X or Y, [NewXY]):-
   strip(X or Y, NewXY),!.

strip_and(X,[[X]]) :- !.

/**************************************************************************/

member_of_2nd_is_subset_of_1st([],List1,[_|Tail2]):-
    member_of_2nd_is_subset_of_1st(List1,List1,Tail2),!.

member_of_2nd_is_subset_of_1st([H1|_],_,[H2|_]):-
    subset(H2,H1),!.

member_of_2nd_is_subset_of_1st([_|Tail1],List1, [H2|Tail2]):-
    member_of_2nd_is_subset_of_1st(Tail1,List1, [H2|Tail2]),!.

/********************************************************************************/

modal_subset([],_):-!.

modal_subset(X, X):-!.

modal_subset([H |Tail], BList) :-
   modal_member(H, BList),!,
   modal_subset(Tail, BList).

/* modal_member(Item, List) :- Item occurs in List. */
/* Note X must be instantiated for this to work otherwise we will just
   return the head of our clause.
*/

modal_member(X, [X | _]):- !.

modal_member(X, [sometimes X|_]):- !.

modal_member(X, [~ always ~ X|_]):- !.

modal_member(X, [_ | Tail]) :- modal_member(X, Tail).

/* ******************
   * SIMPLIFICATION *
   ****************** */

/******************************************************************************/
/*   simplify(Rules, NewRules)                                                */
/*                                                                            */
/*  Looks at every rule in Rules and performs standard simplification ie.     */
/*            1. replacing a conjunction with complementary literals by false */
/*            2. replacing a disjunction with complementary literals by true  */
/*            3. removing true from a list of conjuncts                       */
/*            4. removing false from a list of conjuncts                      */
/******************************************************************************/

simplify([],[]).
simplify([Rule | Rest], NewRules) :-
	simplify_rule(Rule, SRule),
	simplify(Rest, NewRest),
	append(SRule, NewRest, NewRules).

/*****************************************************************************/
/* simplify_rule (Rule, NewRules)                                            */
/*                                                                           */ 
/* Performs various simplifications on Rule. These are                       */
/*   - dealing with true and false in conjunctions/disjunctions              */
/*   - removing duplicate literals                                           */
/*   - dealing with complemetary literals                                    */
/*   - getting rid of rules which are always true                            */
/*   - expanding rules of the form slast P => false                         */
/*****************************************************************************/

simplify_rule(r(N1,N2,X => Y),[r(N1,N2,NewR)]):-
        is_modal_rule(X => Y),
        snff_negate(X, NegX),
        simplify_rule(r(N1,N2,NegX or Y),[r(N1,N2,R)]),
        change_form(R,[NewR]).

simplify_rule(r(N1,N2,R), FinalSet) :-
	simplify_true_false(R,U),
	remove_true_rules(r(N1,N2,U),RuleSet),   % Ruleset can either be empty, 
                                                 % or the single rule as a list
	expand_false_rules(RuleSet, FinalSet).   

/*****************************************************************************/
/*  simplify_rule(Rule, NewRule)                                             */ 
/*                                                                           */ 
/*  Simplifies Rule by                                                       */
/*   - dealing with true and false in conjunctions/disjunctions              */
/*   - removing duplicate literals                                           */
/*   - dealing with complemetary literals                                    */
/*****************************************************************************/

simplify_true_false(P => next F, NewP => next NewF ) :-
        simplify_and(P, NewishP),
        simplify_and_dup(NewishP, NewP),
	simplify_or(F,NewishF),
        simplify_or_dup(NewishF, NewList),
        disjoin(NewList,NewF).

simplify_true_false(F, NewF) :-
        disjunction_of_modal_literals(F),
	simplify_or(F,NewishF),
        simplify_or_dup(NewishF, NewerF),
        simplify_or_modal(NewerF,NewList),
        disjoin(NewList, NewF).

/***********************************************************************/
/*    simplify_and(Conj,NewConj)                                       */
/*                                                                     */ 
/*   Takes a conjunction Conj and if true is a conjunct it removes it  */
/*   or if false is a conjunct false is returned in NewConj, otherwise */
/*   NewConj returns what is left.                                     */
/***********************************************************************/

simplify_and(A and true, NewA) :- simplify_and(A, NewA),!.

simplify_and(true and A, NewA) :- simplify_and(A, NewA),!.

simplify_and(_ and false, false):-!.

simplify_and(false and _, false):-!.

simplify_and(A and _, false) :- simplify_and(A, false),!.

simplify_and(_ and B, false) :- simplify_and(B, false),!.

simplify_and(A and B, NewB) :- 
	simplify_and(A, true),!,
	simplify_and(B, NewB).

simplify_and(A and B, NewA) :- 
	simplify_and(B, true),!,
	simplify_and(A, NewA).

simplify_and(A and B, NewA and NewB) :- 
	simplify_and(A, NewA),
	simplify_and(B, NewB),!.

simplify_and(~ ~ P, NewP):-
        simplify_and(P, NewP).

simplify_and(~ true,false).

simplify_and(~ false,true).

simplify_and(P, P).


/*************************************************************************/
/*  simplify_and_dup(OldConj,NewConj)                                    */
/*                                                                       */ 
/*   Takes a conjunction, removes and duplicate literals or              */
/*   complementary literals and returns the remaining conjunction in     */
/*   NewConj.                                                            */
/*************************************************************************/

simplify_and_dup(A, false):-   % If we have complementary literals in a conjunct return false
   strip(A, AList),
   comp_lits_and(AList),!.

simplify_and_dup(A, NewA):-
   strip(A, AList),
   remove_duplicate(AList, NewList),
   conjoin(NewList, NewA).

/***********************************************************************/
/*  remove_duplicate(List, NewList)                                    */
/*                                                                     */ 
/*  Takes a list of nodes (List) of the form [a,b,c,d] and returns a   */
/*  second list with any duplicates removed.                           */
/***********************************************************************/

remove_duplicate(false,false):- !.

remove_duplicate(List1,List2):-
  list_to_set(List1,List2).


/***********************************************************************/
/*    simplify_or(Disj,NewDisj)                                        */
/*                                                                     */ 
/*   Takes a disjunction Disj and if false is a disjunct it removes it */
/*   or if true is a disjunct true is returned in NewDisj, otherwise   */
/*   NewDisj returns what is left.                                     */
/***********************************************************************/

simplify_or(A or false, NewA) :- simplify_or(A, NewA).

simplify_or(false or A, NewA) :- simplify_or(A, NewA).

simplify_or(_ or true, true).

simplify_or(true or _, true).

simplify_or(A or _, true) :- simplify_or(A, true).

simplify_or(_ or B, true) :- simplify_or(B, true).

simplify_or(A or B, NewB) :- 
	simplify_or(A, false),
	simplify_or(B, NewB).

simplify_or(A or B, NewA) :- 
	simplify_or(B, false),
	simplify_or(A, NewA).

simplify_or(A or B, NewA or NewB) :- 
	simplify_or(A, NewA),
	simplify_or(B, NewB).

simplify_or(~ ~ P, NewP):-
        simplify_or(P,NewP).

simplify_or(~ true,false).

simplify_or(~ false,true).

simplify_or(P, P).

/*************************************************************************/
/*  simplify_or_dup(OldDisj,NewDisj)                                     */
/*                                                                       */ 
/*   Takes a disjunction, OldDisj removes and duplicate literals, deals  */
/*   with complementary literals and returns the remaining disjunction   */
/*   in NewDisj.                                                         */
/*************************************************************************/

simplify_or_dup(A, [true]):-        % If we have complementary literals in a disjunct return true.
   strip(A, AList),
   check_comp_lits(AList).

simplify_or_dup(A, [true]):-        % If we have complementary literals in a disjunct return true.
   strip(A, AList),
   check_modal_lits(AList).

simplify_or_dup(A, NewList):-
   strip(A, AList),
   remove_duplicate(AList, NewList).

/****************************************************/

check_comp_lits(AList):-
   comp_lits_and(AList).

/***************************************************/

check_modal_lits(AList):-
   modal_lits_and(AList).

/**************************************************/

simplify_or_modal([],[]):-!.

simplify_or_modal([~ k ~ L|Rest],[~ k ~ L |Final]):-
   remove_member(L,Rest, NewerRest), !,          % Because p -> ~K~p
   remove_member(k L,NewerRest, NewestRest),     % Because K p -> p
   simplify_or_modal(NewestRest,Final).         

simplify_or_modal([k L|Rest],Final):-
   member(L,Rest), !,                            % Because K p -> p
   simplify_or_modal(Rest,Final).            

simplify_or_modal([k L|Rest],Final):-
   member(~ k ~ L,Rest), !,                  % Because K p -> ~K~p
   simplify_or_modal(Rest,Final).            

simplify_or_modal([k L|Rest],[k L|Final]):-
   simplify_or_modal(Rest,Final).            

simplify_or_modal([L|Rest],Final):-
   member(~ k ~ L,Rest), !,            % Because p -> ~K~p
   remove_member(k L,Rest, NewerRest),     % Because K p -> p
   simplify_or_modal(NewerRest,Final).            

simplify_or_modal([L|Rest],[L|Final]):-
   remove_member(k L,Rest, NewerRest),     % Because K p -> p
   simplify_or_modal(NewerRest,Final).            

simplify_or_modal([L|Rest],[L|Final]):-
   simplify_or_modal(Rest,Final).

/**********************************************/

comp_lits(X,Y):-
  negate_modal_lit(X,Y).

/***************************************************************/
/*  comp_lits_and(List)                                        */
/*                                                             */ 
/* Returns true if complementary literals are found in List    */
/***************************************************************/

comp_lits_and([H|Tail]):-
    negate_modal_lit(H, NegH),
    member(NegH,Tail).

comp_lits_and([_|Tail]):-
  comp_lits_and(Tail).

/**************************************************************/

modal_lits_and([sometimes X|List]):- !,
  negate_modal_lit(X,NegX),
  member(NegX,List).

modal_lits_and([always _|List]):- !,
  modal_lits_and(List).

modal_lits_and([X|List]):- !,
  negate_modal_lit(X,NegX),
  member(sometimes NegX,List).

modal_lits_and([_|List]):- !,
   modal_lits_and(List).

/***************************************************************/
/*  comp_lits_or(List)                                         */
/*                                                             */ 
/* Returns true if complementary literals are found in List    */
/***************************************************************/

comp_lits_or([H|Tail]):-
     negate_all(H, NegH),
     node_is_member_of(NegH,Tail).

comp_lits_or([_|Tail]):-
  comp_lits_or(Tail).

/***************************************************************/
/*  negate_all(OldList,NewList)                                */
/*                                                             */ 
/* Negate all takes a list OldList which represents part of a  */
/* disjunct and negates all its members which are returned in  */
/* NewList.                                                    */
/***************************************************************/

negate_all([],[]).

negate_all([H|Tail],[NegH|NewTail]):-
   negate_modal_lit(H, NegH),
   negate_all(Tail,NewTail).

/*****************************************************************/
/* remove_true_rules(Rule,NewRule)                               */
/*                                                               */
/* Takes a rule in the form r(Number,FromList,Text) and if it is */
/* always true ie of the form ...=> next true                   */
/*                       or   ...=> true                        */
/* NewRule is return with the Text set to [], otherwise the rule */
/* is returned as it is.                                         */
/*****************************************************************/

remove_true_rules( r(N1,N2,_ => true), r(N1,N2,[])).
remove_true_rules( r(N1,N2,_ => next true), r(N1,N2,[])).
remove_true_rules( r(N1,N2,true),r(N1,N2,[])).
remove_true_rules( R, [R] ).

/**********************************************************************/
/*  expand_false_rules(Rules, NewRules)                               */
/*                                                                    */
/* Takes the ruleset ie rules to be simplified which is currently     */
/* either a rule of the form r(_,_,[]) ie the text bit is empty as    */
/* rule has been removed as it is always true or it is a list with a  */
/* single rule in. If the rule is of the form                         */
/*               P => next false                                     */
/* then this is rewritten into two new rules                          */
/*               true => next ~P                                     */
/*               start => ~P                                         */
/* The FromList of the original rule is just copied into the FromList */
/* of these two new rules to avoid having to save the rule which is   */
/* going to disappear. Otherwise the rule remains as it is.           */
/**********************************************************************/

expand_false_rules(r(_,_,[]),[]):-!.

expand_false_rules([],[]):-!.

expand_false_rules( [r(_,N2, P => next false)|Rest ], [r(NewR1,N2,NegP)|NewRules ] ) :-
       snff_negate(P, NegP),
       newrulecount(NewR1),
       expand_false_rules(Rest, NewRules).

expand_false_rules( [ Other | Rest ], [ Other | NewRest ] ) :-
	expand_false_rules(Rest, NewRest).

/*********************************************************/
/* disjoin(List, Disjunction)                            */
/*                                                       */ 
/* Takes a list of nodes (List) eg [a,b,c] and returns   */
/* them separated by or eg a or b or c in Disjunction    */
/*********************************************************/

disjoin([], false):-!.
disjoin(List, Form) :- disjoin2(List, Form).

/*********************************************************/

disjoin2([F|[]], F):-!.
disjoin2([F | Rest], F or NewForm) :- disjoin2(Rest, NewForm).

/*********************************************************/
/* conjoin(List, Conjunction)                            */
/*                                                       */ 
/* Takes a list of nodes (List) eg [a,b,c] and returns   */
/* them separated by "and" eg a and b and c in           */
/* Conjunction.                                          */
/*********************************************************/

conjoin([], true):-!.
conjoin(List, Form) :- conjoin2(List, Form).

/********************************************************/

conjoin2([F|[]],F):-!.
conjoin2([F | Rest], F and NewForm) :- conjoin2(Rest, NewForm).

/*****************************************************************************/
/* is_superset_of__member_of_list(Node, List)                                */ 
/*                                                                           */
/* Returns true if one of List is a subset of Node. Usful for detecting      */
/* conjunctions on the rhs of a rule given we are trying to generate a       */
/* node.                                                                     */
/*                                                                           */
/*****************************************************************************/

is_superset_of_member_of_list(Node, [H|_]):-
   subset(H, Node).

is_superset_of_member_of_list(Node, [_|List]):-
   is_superset_of_member_of_list(Node, List).

/*****************************************************************************/
/* node_is_member_of(NewNode, ListofNodes)                                   */ 
/*                                                                           */
/* Similar to node_is_superset_of_list but returns true if the new node and  */
/* one of the nodes from list of nodes are the same (set wise) ie the        */
/* predicate would return true if new node was [a,b] and either [a, b] or    */
/* [b,a] were found in ListOfNodes.                                          */
/*                                                                           */
/*****************************************************************************/

node_is_member_of(NewNode, [Node|_]):-
   same(NewNode, Node), !.

node_is_member_of(NewNode, [_|OldNodes]):-
   node_is_member_of(NewNode, OldNodes).

/*****************************************************************************/
/* same(NodeX,NodeY)                                                         */
/*                                                                           */
/* Takes two nodes and returns true if they are equivalent ie [a] and [a]    */
/* would return true, also [a,b] and [b,a], but [a,b,c] and [b,c] would not. */
/*                                                                           */
/*****************************************************************************/

same(X,X) :- !.

same(X,Y):-
  subset(X,Y),!,
  subset(Y,X).

/*****************************************************************************/
/* remove_duplicate_nodes(NodeList, NewNodes).                               */
/*                                                                           */
/* Takes a list of nodes (NodeList) and removes any duplicate nodes which    */
/* may exist, ie it returns the set of nodes obtained from the list of nodes.*/
/*                                                                           */
/*****************************************************************************/

remove_duplicate_nodes([],[]):-!.

remove_duplicate_nodes([Node|NodeList], NewNodes):-
   node_is_member_of(Node, NodeList), !,
   remove_duplicate_nodes(NodeList, NewNodes).

remove_duplicate_nodes([Node|NodeList], [Node|NewNodes]):-
   remove_duplicate_nodes(NodeList, NewNodes).

/***************************************************************************/

remove_member(X,[X|Rest],NewList):-
   remove_member(X,Rest,NewList).

remove_member(X,[Y|Rest],[Y|NewList]):-
   remove_member(X,Rest,NewList).

remove_member(_,[],[]).

/*********************************************************************************************/
/*  rewrite_and_or(List,Disjunction)                                                         */
/*                                                                                           */
/* Takes a list in the form [[...],[..],[...],[....]] and conjoins all the [..] bits ie      */
/* sticks and between the literals and replaces the , in the original lis by or.             */
/*********************************************************************************************/

rewrite_and_or([H], NewH):-
   conjoin(H,NewH).
 
rewrite_and_or([H|Tail], NewH or NewTail):-
  conjoin(H, NewH),
  rewrite_and_or(Tail, NewTail).

/******************************************************************************************/

negate_modal_lit(~ k ~ X, k ~ X).

negate_modal_lit(k ~ X, ~ k ~ X).

negate_modal_lit(~ k X, k X).

negate_modal_lit(k X, ~ k X).

negate_modal_lit(~ X, X).

negate_modal_lit(X, ~ X).


/* snff(Initial, Final) :- The Initial set of rules, is transformed to the
                           Final set, which are in DSNF_K form. */

/********************************************/
/* The Final set of rules is in DSNF_K form */
/********************************************/

snff([],[]).

snff(Final, Final) :-
        snff_in_form_list(Final).    

snff(Initial, Final) :-
        snff_anchor_to_start(Initial,Anchored),
        snff_rewrite_list(Anchored,Final).

/****************************************************************/
/* Anchor formulae to start if it is not already in DSNF_K form */
/****************************************************************/

snff_anchor_to_start([],[]):-!.

snff_anchor_to_start([H|Tail], [H|NewRest]):-
        in_form(H),
        snff_anchor_to_start(Tail,NewRest).

%% probably easier in the separation of clauses).

snff_anchor_to_start([H|Tail], [start => V, V => (H)|NewRest]):-
        new_temp_prop(V),
        snff_anchor_to_start(Tail, NewRest).

/*************************************************/
/* Rewrite rules, so they are in the DSNF_K form */
/*************************************************/

snff_rewrite_list([],[]):-!.
snff_rewrite_list([Rule|Rest],[Rule|NewRest]):-
         in_form(Rule),
         snff_rewrite_list(Rest,NewRest).

snff_rewrite_list([Rule|Rest],Other):-
         snff_rewrites(Rule,NewRule, NewInform),
         snff_rewrite_list(NewRule,NewerRule),
         append(NewInform,NewerRule,FinalRule),
         snff_rewrite_list(Rest,NewRest),
         append(FinalRule,NewRest,Other).

/*************************************************/
/* Rewrite a given clause                        */
/*************************************************/

snff_rewrites(X => (A and B),[X => A, X => B],[]).

snff_rewrites(X => ~(A and B),[X => ~ A or ~ B],[]).

snff_rewrites(X => (A => B),[X => ~ A or B],[]).

snff_rewrites(X => ~ (A => B),[X => A, X => ~ B],[]).

snff_rewrites(X => ~ ~ A ,[X => A],[]).

snff_rewrites(X => k A,[X => ~ V],[V => k A, ~ V => k A]):-
        is_literal(A),
        simplify_and(~ A,NewA),
        new_dontknow_prop(V,NewA).

snff_rewrites(X => k A ,[V => A],[X => k V]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X => ~ k A ,[X => V],[V => ~ k A, ~ V => k A]):-
        is_literal(A),
        simplify_and(~ A, NewA),
        new_dontknow_prop(V,NewA).

snff_rewrites(X => ~ k A ,[X => ~ k ~ V, V => ~ A],[]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X => A or B, Rest,[NegX or NewA or NewB]):-
        snff_negate(X,NegX),
        check_dnf(A, NewA, ARest),
        check_dnf(B, NewB, BRest),
        append(ARest,BRest,Rest).

snff_rewrites(X => next A ,[V => A],[X => next V]):-
%        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X => ~ next A,[X => next ~ A],[]).

snff_rewrites(X => (A until B),[X => V until B, V => A],[]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X => (A until B),[X => A until V, V => B],[]):-
        is_not_literal(B),
        new_temp_prop(V).

snff_rewrites(X => (A until B),[],[~ X or B or A, ~ X or B or V, V => next (B or A), V => next (B or V), X => sometimes B]):-
        is_literal(A),
        is_literal(B),
        new_temp_prop(V).

snff_rewrites(X => ~ (A until B),[X => ((~ B) unless (~ A and ~ B))],[]).

snff_rewrites(X => (A unless B),[X => V unless B, V => A],[]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X => (A unless B) ,[X => A unless V, V => B],[]):-
        is_not_literal(B),
        new_temp_prop(V).

snff_rewrites(X => (A unless B) ,[],[~ X or B or A, ~ X or B or V, V => next (B or A), V => next (B or V)]):-
        is_literal(A),
        is_literal(B),
        new_temp_prop(V).

snff_rewrites(X => ~ (A unless B) ,[X => ((~ B) until (~ A and ~ B))],[]).

snff_rewrites(X => (sometimes A) ,[V => A],[X => sometimes V]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X => (~ sometimes A) ,[X => always ~ A],[]).

snff_rewrites(X => (always A) ,[X => always V , V => A],[]):-
        is_not_literal(A),
        new_temp_prop(V).

snff_rewrites(X => (always A) ,[],[~ X or A, V => next A, ~ X or V, V => next V]):-
        is_literal(A),
        new_temp_prop(V).

snff_rewrites(X => (~ always A) ,[X => sometimes ~ A ],[]).

snff_rewrites(X => (Y),[],[~ X or Y]):-
      conjunction_of_literals(X),
      disjunction_of_modal_literals(Y).

/***********************************/
/* check if rule is in DSNF_K form */
/***********************************/

snff_in_form_list([]):-!.

snff_in_form_list([H|Tail]):-
      in_form(H),
      snff_in_form_list(Tail).        

/**********************************/
/* Normal Form                    */
/**********************************/

in_form(start => X):-
      disjunction_of_literals(X).

in_form(X):-
      disjunction_of_literals(X).

in_form(X => k A):-
      is_literal(X),
      is_literal(A).

in_form(X => ~ k A):-
      is_literal(X),
      is_literal(A).

in_form(X => sometimes Y):-
       conjunction_of_literals(X),
       is_literal(Y).

in_form(X => next(Y)):-
      conjunction_of_literals(X),
      disjunction_of_literals(Y).

/************************************/
/* Disjunction of literals          */
/************************************/

disjunction_of_literals(X):-
       strip_or(X,StrippedX),
       is_literal_list(StrippedX).

/**********************************/
/* Disjucntion of modal literals  */
/**********************************/

disjunction_of_modal_literals(X):-
       strip_or(X,StrippedX),
       is_modal_literal_list(StrippedX).

/***********************************/
/* Conjunction of literals         */
/***********************************/

conjunction_of_literals(X):-
       strip_and(X,StrippedX),
       is_literal_list(StrippedX).

/**************************************************/
/* Verifies if a given list is a list of literals */
/**************************************************/

is_literal_list([[H]|Tail]):-
      is_literal(H),
      is_literal_list(Tail).
      
is_literal_list([]).       

/********************************************************/
/* Verifies if a given list is a list of modal literals */
/********************************************************/

is_modal_literal_list([]).

is_modal_literal_list([[k H]|Tail]):-
      is_literal(H),
      is_modal_literal_list(Tail).

is_modal_literal_list([[~ k H]|Tail]):-
      is_literal(H),
      is_modal_literal_list(Tail).

is_modal_literal_list([[H]|Tail]):-
      is_literal(H),
      is_modal_literal_list(Tail).

/******************** **************/
/* Check if subformulae are in DNF */
/***********************************/           

check_dnf(A or B, NewA or NewB, Rest):-
        check_dnf(A, NewA, ARest),
        check_dnf(B, NewB, BRest),
        append(ARest,BRest,Rest).

check_dnf(A => B, NewAll, Rest):-
        check_dnf(~ A or B, NewAll, Rest).

check_dnf(A, A, []):-
       is_literal(A).

check_dnf(k A, ~ V, [V => ~ k A, ~ V => k A]):-
        is_literal(A),
        snff_negate(A,NegA),
        new_dontknow_prop(V,NegA).

check_dnf(~ k A, V, [V => ~ k A, ~ V => k A]):-
        is_literal(A),
        snff_negate(A,NegA),
        new_dontknow_prop(V,NegA).

check_dnf(A, V, [V => A]):-
        new_temp_prop(V). %% solves the case where A is nnot literal in KA, negKA

/*******************************/
/* Form to NNF_Form            */
/*******************************/

snff_negate(~ X, X):- !.

snff_negate(X and Y, NotX or NotY):- 
        !, 
        snff_negate(X, NotX),
        snff_negate(Y, NotY).

snff_negate(X or Y, NotX and NotY):- 
        !, 
        snff_negate(X, NotX), 
	snff_negate(Y, NotY).

snff_negate(true, false).

snff_negate(false, true).

snff_negate(X, ~ X):- 
       is_proposition(X).


/**************************************************************************/
/*
   This program is an implementation of nontemporal resolution. 
The predicates related to substitution have been written by Michael, 
non temporal resolution predicates are based on those written by Michael, 
and subsumtion predicates are written by Clare.

The following procedures are necessary :-

        1. Check for false
        2. Simplification ie x => next false converted to
                             true => ~x  
        3. Subsumption
        4. Non temporal resolution
        5. Provides new real resolvents
        6. Repeat 1 to 5 until false found or no new resolvents

Note :-
======
The predicate
                fullmres(OldRules,NewRules)

sets the non temporal resolution in progress and expects a set of clauses derived 
from translation to SNF (OldRules),which have been numbered and rewritten to the form

                r(Number,FromList,Text)

where Number is the number of the clause, FromList, will originally be empty, and Text 
is the actual clause itself. It returns NewRules ie what has been derived from non 
temporal resolution. This predicate is called from fullres.pl as part of the main cycle.

     A predicate cyclemres is called which recurs on itself until no new resolvents have 
been derived or false has been derived. This predicate and the predicates it calls maintains 
a distinction round each cycle of the Old ruleset and those newly derived from resolution, 
so simpilfication may just be done on the new ruleset, subsumption may be done between the 
new ruleset and the old ruleset, and then non temporal resolution may be done between the 
new ruleset and the old ruleset,before combining the two and starting again around the cycle.

********************************************************************************/

/*******************************************************************************/
/*  mres(LiteralRules,
         ModalRules,
         InitialRules,
         NewLRules,
         NewMRules)                                                            */


/* This sets the nontemporal resolution off, the first three arguments,        */
/* containing literal, modal and initial clauses,                              */
/* derived from SNF which have been rewriten into the form                     */
/* r(Number,FromList,Text), where currently FromList will be empty.            */
/* NewLRules and NewMRules will contain the new ruleset after no more          */
/* non temporal resolution can take place, or false if false has been derived. */
/*******************************************************************************/

mres(LiteralRules,ModalRules,Initial,NewLRules,NewMRules):-
     cyclemres(1,[],[],LiteralRules,ModalRules,Initial,NewLRules,NewMRules).    

% Leave 2nd and 3rd arguments empty so as to treat the original ruleset as "new" resolvents.

/*****************************************************************************/
/*                                                                           */
/* This predicate is called from fullres.pl during the full resolution       */
/* process. OldRules are in the form r(Number,FromList,Text), where          */
/* Number is the ruless number, FromList contains the numbers of the rules   */
/* which were the parents of this resolvent, and Text is the rule itself     */
/* (in SNF). NewishRules contain the new resolvents obtained from the        */
/* last temporal resolution step. SRules contains a rules which have been    */
/* subsumed on previous cycles round the non-temporal resolution process.    */
/* NewRules will contain the new ruleset                                     */
/* after no more non temporal resolution can take place, or false if a       */
/* contradiction has been derived. NewSRules contains SRules plus any new    */
/* rules which ahve been subsumed.                                           */
/*****************************************************************************/

mres(OldLiteral,OldModal,NewishLiteral,NewishModal,Initial,NewLiteral,NewModal):-
   cyclemres(1,OldLiteral,OldModal,NewishLiteral,NewishModal,Initial,NewLiteral,NewModal).

/*****************************************************************************/
/*  cyclemres(Count,
              OldLiteral,
              OldModal,
              NewishLiteral,
              NewishModal,
              Initial,
              NewLiteral,
              NewModal
              )                                                              */
/*                                                                           */
/*  Cycles around the non temporal resolution cycle, checking for false and  */
/*  if found returning false, otherwise performing simplification,           */
/*  subsumption, nontemporal resolution and then recurring on itself until   */
/*  false or an empty set of resolvents is detected. Count is the current    */
/*  cycle we are on, OldRules are rules from previous cyles rounds the       */
/*  non-temporal resolution processes. NewishResolvents are the new          */
/*  resolvents generated during the last cycle of non-temporal resolution    */
/*  and SRules are the store of rules which have been subsumed which may need*/
/*  to be kept to generate a proof if false is derived. NewRules is the      */
/*  final ruleset after no more non-temporal resolution can be done and      */
/*  NewSRules is the new set of subsumed rules.                              */
/*****************************************************************************/

cyclemres(_,[],[],[],[],_,[],[]).
%:-my_write('\ncyclemres ==> (1)\n').                     % No new resolvents.

cyclemres(_,LRules,MRules,[],[],_,LRules,MRules):-
%    my_write('\ncyclemres ==> (2)\n'),
    my_write('Have generated no new resolvents,current literal/modal clauses are :-\n'),
    write_ruleset(LRules),                   % Terminate non temporal resolution
    write_ruleset(MRules).                   % as no new Rules have been derived.

cyclemres(_,_,_,NewLRules,_,[r(_,_,X)],[r(0,[],false)],_):-
%    my_write('\ncyclemres ==> (2.5)\n'),
    snff_negate(X,NegX),
    test_member(NegX,NewLRules),
    simplify(NewLRules,SNewLRules),
    self_subsumption(SNewLRules,SubNewLRules),
    flatten(SubNewLRules,FSubNewLRules),
    my_write('\nNew literal clauses include (~ start).\n'),
    write_ruleset(FSubNewLRules),      % Terminate non temporal resolution as false
    write('\nHave generated false in initial clauses.\n').

cyclemres(_,_OldLRules,OldMRules,NewLRules,_,_,[r(0,[],false)],OldMRules):-
%    my_write('\ncyclemres ==> (3)\n'),
    test_member(false,NewLRules),
    my_write('\nNew literal clauses include false.\n'),
    write_ruleset(NewLRules),      % Terminate non temporal resolution as false
    write('\nHave generated false in literal clauses.\n').

cyclemres(_,_OldLRules,_OldMRules,_,NewMRules,_,[r(0,[],false)],[r(0,[],false)]):-
%    my_write('\ncyclemres ==> (4)\n'),
    test_member(false,NewMRules),
    my_write('\nNewModal clauses include false.\n'),
    write_ruleset(NewMRules),   % Terminate non temporal resolution as false
    write('\nHave generated false in modal clauses.\n').

% Occasionally ran out of space during non-temporal resolution so have hadded a specific garbage collect.

cyclemres(Count,OldLRules,OldMRules,NewLRules,NewMRules,Initial,NewerLRules,NewerMRules):-
%   my_write('\ncyclemres ==> (5)\n'),
%   garbage_collect,
   writef('\nCycle Number%t.',[Count]),
   my_k_to_literal(NewMRules,KRules),
   append(KRules,NewLRules,AddedNewLRules),
   simplify(AddedNewLRules,SimplifiedLRules),
   simplify(NewMRules,SimplifiedMRules),
   garbage_collect,
   subsumption(OldLRules,SimplifiedLRules,SubOldLRules,SubNewLRules1),
   trim_stacks,
   subsumption(OldMRules,SubNewLRules1,SubOldMRules1,SubNewLRules2),
   trim_stacks,
   subsumption(SimplifiedMRules,SubNewLRules2,SubNewMRules1,SubNewLRules),
   trim_stacks,
   subsumption(SubOldMRules1,SubNewMRules1,SubOldMRules,SubNewMRules),
   trim_stacks,
   my_write('\nOld Literal Clauses\n'),
   write_ruleset(SubOldLRules),
   my_write('\nOld Modal Clauses\n'),
   write_ruleset(SubOldMRules),
   my_write('\nNew Literal Clauses\n'),
   write_ruleset(SubNewLRules),
   my_write('\nNew Modal Clauses\n'),
   write_ruleset(SubNewMRules),
   do_mres(SubOldLRules,SubOldMRules,SubNewLRules,SubNewMRules,ResLRules,ResMRules,NewOldLRules,NewOldMRules),
   NewCount is Count + 1,!,garbage_collect,
   cyclemres(NewCount,NewOldLRules,NewOldMRules,ResLRules,ResMRules,Initial,NewerLRules,NewerMRules).

/* ************************ */
/* SUBSUMPTION              */
/* ************************ */

/**********************************************************************************/
/*  subsumption(Old_Rules,New_Rules,SRules,New_Old_Rules,New_New_Rules,NewSRules) */
/*                                                                                */
/*   This predicate takes every rule in New_Rules and attempts subsumption        */
/*   between every rule and every other in this list. Then subsumption is         */
/*   performed between everything in the subsumed version of New_Rules and        */
/*   Old_Rules. New_Old_Rules is then the subsumed version of Old_Rules and       */
/*   New_New_Rules is the subsumed version of New_Rules. SRules contains rules    */
/*   which have been subsumed on previous cycles and NewSRules contain            */
/*   SRules plus any rules subsumed on this cycle.                                */
/**********************************************************************************/

subsumption(Old_Rules,New_Rules,New_Old_Rules,FNew_New_Rules) :-
  self_subsumption(New_Rules,Newish_Rules),
  flatten(Newish_Rules,FNewish_Rules),!,
  subsumption2(Old_Rules,FNewish_Rules,New_Old_Rules,New_New_Rules),
  flatten(New_New_Rules,FNew_New_Rules),!.

/******************************************************************************/
/*  self_subsumption([R | Rest],SRules,New_Rules,NewSRules)                   */
/*                                                                            */
/*  Attempts subsumption between R and Rest, recurrs on Rest and outputs      */
/*  subsumed list in New_Rules. SRules are the rules which have been subsumed */
/*  on previous cycles and any new ones subsumed are added to NewSRules.      */
/******************************************************************************/

self_subsumption([],[]):-!.

self_subsumption([R|Rest],[NewR|NewRules]):-
  subsume_rule(Rest,R,Remain_Rest,NewR),!,
  self_subsumption(Remain_Rest,NewRules).


/***********************************************************************************/
/*  subsumption2(OldRules,[R | Rest],SRules,New_Old_Rules,New_New_Rules,NewSRules) */
/*                                                                                 */
/*  Attempts subsumption between R and OldRules, recurrs on Rest and outputs       */
/*  lists after subsumption in New_Old_Rules and New_New_Rules. SRules are the     */
/*  rules which have been subsumed on previous cycles and any new ones             */
/*  subsumed are added to NewSRules.                                               */
/***********************************************************************************/

subsumption2(Old_Rules,[],Old_Rules,[]):-!.

subsumption2([],NewRules,[],NewRules):-!.

subsumption2(Old_Rules,[R|Rest],New_Old_Rules,[NewR|New_Rules]):-
   subsume_rule(Old_Rules,R,Newish_Old_Rules,NewR),!,
   subsumption2(Newish_Old_Rules,Rest,New_Old_Rules,New_Rules).

/*******************************************************************************/
/* subsume_rule(Old_Rules,ThisRule,SRules,New_Old_Rules,New_ThisRule,NewSRules)*/
/*                                                                             */
/*   This predicate compares ThisRule with every rule in Old_Rules. Rules are  */
/*   in the format r(Number,FromList,Text). If ThisRule is subsumed by a       */
/*   rule in Old_Rules then Old_rules are returned and New_ThisRule will be    */
/*   and empty list but if ThisRule subsumes a Rule in Old_Rules, the rule     */
/*   that has been subsumed is removed from Old_Rules and the predicate the    */
/*   attempts subsumption between ThisRule and the rest of Old_Rules.          */
/*   It is important that the predicates are in the order they appear in, as   */
/*   if there are duplicate rules ie Rule1 subsumes Rule2 and vice versa we    */
/*   want to remove ThisRule and return an empty list as New_ThisRule          */
/*   rather than the other way around for we are depending upon no new         */
/*   resolvents to be derived for our non-temporal resolution process to       */
/*   terminate. SRules contains any rules which have been subsumed previously  */
/*   NewSRules will also contain rules subsumed on this cycle.                 */
/*******************************************************************************/

% Firstly looking at cases of strong last implies something

subsume_rule([],r(N1,N2,G),[],[r(N1,N2,G)]):-!.
subsume_rule([],R,[],[R]):-!.

subsume_rule([r(N1,N2,F) | Rules],r(_,_,G),[r(N1,N2,F) | Rules],[]) :-
             dis_implies(F,G),!.                                % P => F

subsume_rule([r(_,_,F) | Rules],r(N1,N2,G),New_Old_Rules,New_New_Rules) :-
            dis_implies(G ,F),!,                        % Q => G subsumes P => F
	    subsume_rule(Rules,r(N1,N2,G),New_Old_Rules,New_New_Rules).

% default behaviour: ie no subsumption takes place

subsume_rule([ R | Rules],r(N1,N2,G),[ R | New_Rules],New_New_Rules) :-
	subsume_rule(Rules,r(N1,N2,G),New_Rules,New_New_Rules).

% default behaviour to catch any of the cases not mentioned above

subsume_rule([ R1 | Rules],R2,[ R1 | New_Old_Rules],New_New_Rules) :-
   subsume_rule(Rules,R2,New_Old_Rules,New_New_Rules).


/* ************************* */
/* NON-TEMPORAL RESOLUTION * */
/* ************************* */

/*********************************************************************************/
/* mres(OrigRules,[Rule|Rest],NewRules,Final)                                    */
/*                                                                               */
/*   Michaels code changed slightly to do non temporal resolution between        */
/*   two lists OrigRules and [Rule|Rest] (new rules derived from resolution last */
/*   time round the loop). After resolving Rule with every rule in OrigRules it  */
/*   is added to OrigRules, and the first rule from Rest is resolved with all    */
/*   rules in OrigRules + Rule, and so on. When Rest is empty, OrigRules which   */
/*   now has all of [Rule|Rest] attached is copied to NewOrig and resolvents     */
/*   found built up in Final. The first time round the loop ie after             */
/*   initial conversion to SNF OrigRules will be empty, so gradually [Rule|Rest] */
/*   will be copied into it and effectively resolved on itself. Rules are in the */
/*   form r(Number,FromList,Text) . When resolution takes place a new            */
/*   number for the rule is placed in Number and FromList will have the two rule */
/*   numbers of the parents of the new rule.                                     */
/*********************************************************************************/


do_mres(LNewOrig,MOrigRules,LNew,RNew,FinalL,FinalM,NewOrigL2,NewOrigM2):-
%    my_write('\ndo_mres ==> (2)\n'),
    do_mres(LNewOrig,MOrigRules,LNew,FinalL1,FinalM1,NewOrigL1,NewOrigM1),
    do_mres(NewOrigL1,NewOrigM1,RNew,FinalL2,FinalM2,NewOrigL2,NewOrigM2),
    append(FinalL1,FinalL2,FinalL),
    append(FinalM1,FinalM2,FinalM).

do_mres(LNewOrig,MNewOrig,[],[],[],LNewOrig,MNewOrig).
%:-my_write('\ndo_mres ==> (1).\n').

do_mres(LOrigRules,MOrigRules,[Rule|Rest],FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\ndo_mres ==> (3).\n'),
	mresolve(Rule,LOrigRules,MOrigRules,RulesL1,RulesM1),
        add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM).

/*************************************************************************************************/

add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\nadd_to_set ==> (1)\n'),
        is_modal_rule(Rule),!,
	do_mres(LOrigRules,[Rule|MOrigRules],Rest,NewRulesL,NewRulesM,NewOrigL,NewOrigM),
	append(RulesL1,NewRulesL,FinalL),
	append(RulesM1,NewRulesM,FinalM).

add_to_set(LOrigRules,MOrigRules,Rule,Rest,RulesL1,RulesM1,FinalL,FinalM,NewOrigL,NewOrigM) :-
%        my_write('\nadd_to_set ==> (2)\n'),
	do_mres([Rule|LOrigRules],MOrigRules,Rest,NewRulesL,NewRulesM,NewOrigL,NewOrigM),
	append(RulesL1,NewRulesL,FinalL),
	append(RulesM1,NewRulesM,FinalM).

/*********************************************************************************/
/*  mresolve(Rule,OrigRules,NewRules)                                            */
/*                                                                               */
/*  Takes a rule of the form r(Number,FromList,Text) and tries resolution        */
/*  between the Text of this rule and the Text of all the other rules in         */
/*  OrigRules. Any new resolvents derived are put into new rules where the       */
/*  Number of the new resolvent is the next new rule number, and FromList is     */
/*  filled with the rule numbers of the parents of the resolvent.                */
/*********************************************************************************/

/* First,the cases where resolution cannot take place */

mresolve(_,[],[],[],[]).
% :-my_write('\nmresolve ==> (1)\n').

/* Next,resolution of initial rules: */

mresolve(r(N1,N2,F),[r(N3,_,G) | Rest],Other,LFinal,MFinal) :-
%        my_write('\nmresolve ==> (2)\n'),
        basic_nt_resolve(F,G,Resolvent,[],[]),
	mresolve(r(N1,N2,F),Rest,Other,NewLRules,NewMRules),
	generate_new_resolvent([N1,N3],Resolvent,NewLRules,NewMRules,LFinal,MFinal).

mresolve(Rule,[_ | Rest],Other,NewLRules,NewMRules) :-
%        my_write('\nmresolve ==> (3)\n'),
	mresolve(Rule,Rest,Other,NewLRules,NewMRules).

mresolve(r(N1,N2,F),[],[r(N3,_,G) | Rest],LFinal,MFinal) :-
%        my_write('\nmresolve ==> (4)\n'),
        basic_nt_resolve(F,G,Resolvent,[],[]),
	mresolve(r(N1,N2,F),[],Rest,NewLRules,NewMRules),
	generate_new_resolvent([N1,N3],Resolvent,NewLRules,NewMRules,LFinal,MFinal).

mresolve(Rule,[],[_ | Rest],NewLRules,NewMRules) :-
%        my_write('\nmresolve ==> (5)\n'),
	mresolve(Rule,[],Rest,NewLRules,NewMRules).

/*****************************************************************************/
/*  generate_new_resolvent(FromList,ResolventList,Rules,NewRules             */
/*                                                                           */
/* If Resolvent is empty this just returns Rules as they are, otherwise a    */
/* new rule is created and added to the front of Rules.                      */
/*****************************************************************************/

generate_new_resolvent(_,[],LR,MR,LR,MR).

generate_new_resolvent(FromList,[Resolvent],LRules,MRules,FinalLRules,FinalMRules):-
      is_modal_disjunct(Resolvent),!,
      simplify_rule(r(_,FromList,Resolvent),[r(_,FromList,SimpRule)]),
      change_form(SimpRule,RulesList),
      number_rules(RulesList,FromList,NumberedRules),
      simplify(NumberedRules,SimpRules),
      separate_rules(SimpRules,_ERules,_Initial,NewLRules,NewMRules,_Temporal),
      append(LRules,NewLRules,FinalLRules),
      append(MRules,NewMRules,FinalMRules).

generate_new_resolvent(FromList,[Resolvent],LRules,MRules,[SimpRule | LRules],MRules):-
      newrulecount(N),
      simplify_rule(r(N,FromList,Resolvent),[SimpRule]).

/******************************************************************************/
/*  basic_nt_resolve(Disj1,Disj2,Resolvent,S1,S2) :-                          */
/*                                                                            */
/*      Tries to resolve the clauses Disj1 and Disj2 together.                */
/*      If it fails, Resolvent is [], if it succeeds,                         */
/*	resolvent is [resolvent]. S1 is a list recording what is left          */
/*	of Disj1, while S2 is what is left of Disj2.                           */
/******************************************************************************/ 

basic_nt_resolve(A => B,Disj2,Resolvent,S1,S2):- 
        snff_negate(A,NegA),!,
        basic_nt_resolve(NegA or B,Disj2,Resolvent,S1,S2).        

basic_nt_resolve(Disj1,A => B,Resolvent,S1,S2):-
        snff_negate(A,NegA),!,
        basic_nt_resolve(Disj1,NegA or B,Resolvent,S1,S2).

basic_nt_resolve(A or B,Disj2,Resolvent,S1,S2) :-
	basic_nt_resolve(A,Disj2,Resolvent,[B|S1],S2).

basic_nt_resolve(A or B,Disj2,Resolvent,S1,S2) :-
	basic_nt_resolve(B,Disj2,Resolvent,[A|S1],S2).

basic_nt_resolve(_ or _, _,_,_,_) :- fail.

basic_nt_resolve(A,Disj2,Resolvent,S1,S2) :-
	internal_nt_resolve(A,Disj2,Resolvent,S1,S2).

/*****************************************************************************/
/*  internal_nt_resolve(L,Disj, Resolvent,S1,S2)                             */
/*                                                                           */
/*  Attempts to resolve L which is now a proposition or negated proposition  */
/*  with Disj. S1 and S2 have what remains of the original two disjuncts     */
/*  we were trying to resolve. The resolvent created is returned in          */
/*  Resolvent                                                                */
/*****************************************************************************/

internal_nt_resolve(L,A or B, Resolvent,S1,S2) :-
	internal_nt_resolve(L,A,Resolvent,S1,[B|S2]).

internal_nt_resolve(L,A or B,Resolvent,S1,S2) :- 
	internal_nt_resolve(L,B,Resolvent,S1,[A|S2]).

internal_nt_resolve(_,_ or _,_,_,_) :- fail.

internal_nt_resolve(L,M,[Resolvent],S1,S2) :-
        resolveable(L,M,S1,S2,NewS1,NewS2),
	append(NewS1,NewS2,NewLst),
	disjoin(NewLst,Resolvent).

/*********************/
/* Basic Resolution  */
/*********************/

resolveable(L,M,S1,S2,S1,S2):-
  comp_lits(L,M).

/********************/
/* Modal Resolution */
/********************/

resolveable(L,M,S1,S2,NewS1,NewS2):-
  sometime_rule(L,M,S1,S2,NewS1,NewS2). 

resolveable(L,M,S1,S2,NewS1,NewS2):- 
  always_rule(L,M,S1,S2,NewS1,NewS2). 

/**********/ 
/* MRES 2 */ 
/**********/ 

sometime_rule(k L,k M,S1,S2,S1,S2):-
  comp_lits(L,M). 

/**********/ 
/* MRES 3 */ 
/**********/ 

sometime_rule(k L,M,S1,S2,S1,S2):-
  comp_lits(L,M).

sometime_rule(L,k M,S1,S2,S1,S2):-
  comp_lits(L,M).

/**********/ 
/* MRES 4 */ 
/**********/ 

always_rule(~ k L,L,S1,S2,S1,NewS2):-
  change_resolvent(S2,NewS2).

always_rule(L,~ k L,S1,S2,NewS1,S2):-
  change_resolvent(S1,NewS1).

always_rule(~ k ~ L,M,S1,S2,S1,NewS2):-
  comp_lits(L,M), 
  change_resolvent(S2,NewS2). 

always_rule(L,~ k ~ M,S1,S2,NewS1,S2):- 
  comp_lits(L,M), 
  change_resolvent(S1,NewS1). 

/****************/ 
/* MOD FUNCTION */ 
/****************/ 

change_resolvent([A or B|Rest], NewAll):-
  change_resolvent(A or B,NewAB),
  change_resolvent(Rest, NewRest), 
  append(NewAB,NewRest, NewAll).

change_resolvent(A or B, NewAB):-
  change_resolvent(A, NewA),
  change_resolvent(B, NewB),
  append(NewA, NewB, NewAB).

change_resolvent([A|Rest],NewAll):-
  change_resolvent(A,NewA),
  change_resolvent(Rest,NewRest),
  append(NewA,NewRest,NewAll).

change_resolvent([],[]).

change_resolvent(X,[X]):-is_snl_literal(X).

change_resolvent(~ k L,[~ k L]):-is_literal(L).

change_resolvent(k L,[k L]):-is_literal(L).

change_resolvent(k ~ L,[k ~ L]):-is_literal(L).        % Clare is this right?

change_resolvent(~ k ~ L,[~ k ~ L]):-is_literal(L).

change_resolvent(~ L,[~ k L]):-is_literal(L).

change_resolvent(L,[~ k ~ L]):-is_literal(L). 

/**********************************/ 

is_modal_rule(r(_,_,Y => X)):- 
   is_modal_rule(Y => X). 

is_modal_rule(_Y => X):- 
  is_modal_disjunct(X). 

/**********************************/ 

is_modal_disjunct(k _). 

is_modal_disjunct(~ k _). 

is_modal_disjunct(A or _):- 
  is_modal_disjunct(A). 

is_modal_disjunct(_ or B):- 
  is_modal_disjunct(B). 

/***************************************/

is_list_of_only_modals(A or B):-
  is_modal(A), 
  is_list_of_only_modals(B).

is_list_of_only_modals(A):-
  is_modal(A).

is_list_of_only_literals(A or B):-
  is_literal(A), 
  is_list_of_only_literals(B).

is_list_of_only_literals(A):-
   is_literal(A). 

/*****************************************************************/
/* The following returns a modal clause in the new normal form   */
/*****************************************************************/ 

change_form(true,[true]).

change_form(Disjunction,[true => Disjunction]):-
  is_modal(Disjunction).

change_form(~ k X or Disj1, [Disj2 => ~ k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(k X or Disj1, [Disj2 => k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disj1 or ~ k X, [Disj2 => ~ k X] ):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disj1 or k X, [Disj2 => k X]):-
  is_literal(Disj1),!,
  snff_negate(Disj1,Disj2).

change_form(Disjunction,[Disjunction]):-
   is_list_of_only_literals(Disjunction).

change_form(Disjunction,List):- 
  is_list_of_only_modals(Disjunction), 
  snl_form(Disjunction,Literals,Modals),
  append([Literals],Modals,List). 

change_form(Disj1 or Disj2,Rest):-
  is_literal(Disj1), 
  snff_rewrite_list([~ Disj1 => (Disj2)],Rest).

change_form(Disj1 or Disj2,Rest):- 
  is_not_literal(Disj1),!, 
  change_order(Disj1 or Disj2, NewDisj),
  change_form(NewDisj,Rest). 

/************************************************/ 

change_order(A or B,NewOrder):- 
  strip_or(A,NewA), 
  strip_or(B,NewB), 
  flatten(NewA,FlatA), 
  flatten(NewB,FlatB), 
  append(FlatB,FlatA,New), 
  disjoin(New,NewOrder). 


/*********************************************************************/
/* The following returns the SNL literals and definition clauses     */
/* from a given formula                                              */
/*                                                                   */
/* (1) the SNL literal itself                                        */
/* (2) the disjunction of SNL literals                               */
/* (3) the SNL(NEW), in case of a conjunction                        */
/* (4) SNL(literal), in case of a non SNL literal                    */
/*                                                                   */
/*********************************************************************/

snl(Formula,Formula,_,[],[]):- 
   is_snl_literal(Formula). 

snl(NewLiteralA or NewLiteralB,A or B,N,Literals,Modals):- 
   snl(NewLiteralA,A,N,Literals1,Modals1), 
   snl(NewLiteralB,B,N,Literals2,Modals2), 
   append(Literals1,Literals2,Literals), 
   append(Modals1,Modals2,Modals). 

snl(NewLiteral,A and B,N,Literals,Modals):- 
  snl_conjunction(NewLiteral,A and B,N,Literals,Modals). 

snl(NewLiteral,Formula,N,[],[Rule1,Rule2]):- 
   is_literal(Formula),!, 
   new_dontknow_prop(NewLiteral,Formula), 
   snff_negate(Formula, NegF), 
   snff_negate(NewLiteral, NegNewLiteral), 
   number_rule(NewLiteral => ~ k NegF,N,Rule1), 
   number_rule(NegNewLiteral => k NegF,N,Rule2). 

/****************************************************************/ 
/* The following takes a formula and returns the corresponding  */
/* SNL definition clauses.                                      */
/****************************************************************/ 


snl_form(k L,[NegNewProp],[NewProp => ~ k L, NegNewProp => k L]):- 
   snff_negate(L,NegL), 
   new_dontknow_prop(NewProp, NegL), 
   snff_negate(NewProp,NegNewProp). 

snl_form(~ k L,[NewProp],[NewProp => ~ k L, NegNewProp => k L]):- 
   snff_negate(L,NegL), 
   new_dontknow_prop(NewProp, NegL), 
   snff_negate(NewProp,NegNewProp). 

snl_form(A or B,Literals,Modals):- 
   snl_form(A,Literals1,Modals1), 
   snl_form(B,Literals2,Modals2), 
   append(Literals1,Literals2,LiteralList), 
   disjoin(LiteralList,Literals), 
   append(Modals1,Modals2,Modals). 

/************************************************************/ 
/* The following deals with conjunctions of literals        */
/* and returns the new proposition renaming the conjunction */
/* as well as its corresponding definition clauses          */
/************************************************************/

/** Conjunctions of only snl literasl **/

snl_conjunction(SimpSNL,Conjunction,_,[],[]):-
  strip_and(Conjunction,List), 
  flatten(List,FList), 
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  subset(NewishList,[]), 
  conjoin(OldSNLList,OldSNL),      
  simplify_and(OldSNL,SimpSNL). 

/** conjunctions with at most one non snl-literal **/

snl_conjunction(NewConj,Conjunction,N,[],[Rule1,Rule2]):- 
  strip_and(Conjunction,List),
  flatten(List,FList),
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  test_conjunction(NewishList,Prop), 
  conjoin(OldSNLList,OldSNL), 
  new_dontknow_prop(NewSNL,Prop), 
  simplify_and(OldSNL and NewSNL,NewConj), 
  snff_negate(Prop,NegProp), 
  snff_negate(NewSNL,NegNewSNL), 
  number_rule(NewSNL => ~ k NegProp,N,Rule1), 
  number_rule(NegNewSNL => k NegProp,N,Rule2). 

/** conjunction with more than one non snl literal **/

snl_conjunction(NewConj,Conjunction,N,Literals,SimplifiedRules):-
  strip_and(Conjunction,List),
  flatten(List,FList),
  remove_news(FList,NewList),
  separate_snl_literals(NewList,NewishList,OldSNLList),
  conjoin(OldSNLList,OldSNL),
  conjoin(NewishList,Formula),
  simplify_and(Formula,SFormula),
  new_conj_prop(NewProp,SFormula),
  snff_negate(SFormula,NFormula),
  number_rule(NewProp or NFormula,N,Definition),
  simplify([Definition],Literals),  
  new_dontknow_prop(NewSNL,NewProp),
  generate_new_modals(NewSNL,N,NewishList,NewModals),
  simplify_and(OldSNL and NewSNL,NewConj),
  snff_negate(NewProp,NegNewProp),
  snff_negate(NewSNL,NegNewSNL),
  number_rule(NewSNL => ~ k NegNewProp,N,Rule1),
  number_rule(NegNewSNL => k NegNewProp,N,Rule2),
  append([Rule1],[Rule2],Definitions),
  append(Definitions,NewModals,Rules),
  simplify(Rules,SimplifiedRules).

/* generate new modals for conjunctions */

generate_new_modals(_,_,[],[]).

generate_new_modals(NewSNL,N,[Head|Tail],[Definition|NewModals]):-
  snff_negate(NewSNL,NegNewSNL),
  snff_negate(Head,NegHead),
  number_rule(NegNewSNL => k NegHead,N,Definition),
  generate_new_modals(NewSNL,N,Tail,NewModals).

test_conjunction([Head|Tail],Head):- 
  subset(Tail,[]). 

/* takes a new proposition and returns its label */

remove_news([],[]). 

remove_news([new(X)|List],[X|NewList]):- 
  remove_news(List,NewList). 

remove_news([X|List],[X|NewList]):- 
  remove_news(List,NewList). 

/* remove SNL literals from a list of conjuncts */

separate_snl_literals(List,Literals,SNLLiterals):-
  sublist(is_snl_literal,List,SNLLiterals),
  subtract(List,SNLLiterals,Lit),
  sort(Lit,Literals).

/* My implementation of MRES5                    */

my_k_to_literal([],[]):- !. 

my_k_to_literal([r(N,_M,X => k Y)|Rest],[Rule|NewRest]):-!, 
  snff_negate(X,NegX),
  number_rule(NegX or Y,[N,mres5],Rule),
  my_k_to_literal(Rest,NewRest).
 
my_k_to_literal([r(_,_,_ => ~ k _)|Rest],NewRest):-!, 
  my_k_to_literal(Rest,NewRest).

/*
This program controls the temporal resolution part of the temporal resolution
theorem prover. To use the theorem prover start up sicstus by typing
"sicstus" and at the promt type [rules, snff, nontemp, tempres, fullres].
(or whatever the programs are called at this moment) which will load the
programs.

This program is called from fullres.pl which drives the resolution cycle of
             1. Translation to SNF
             2. Non temporal resolution
             3. Temporal resolution
*/

/*****************************************************************************/
/* tempres(Rules, ERulePaths, NewERule, RelatedRules,RulesUsed, LoopPath).   */
/*                                                                           */
/* Takes a list of rules output by SNF (Rules), which have been subsequently */
/* numbered and have a record of the numbers oftheir parent resolvents,      */
/* ie. they are in the form          r(Number, FromList, Text)          and  */
/* after attempts at non temporal resolution have been exhausted, tries      */
/* temporal resolution. ERulePaths contains a list made up of sublists. Each */
/* sublist has an eventuality which has been taken from the global and       */
/* initial sometimes rules and a list of previous paths detected. Thus       */
/* ERulePaths is of the form [[p,[]],[~ q,[]]] if we have only two         */
/* sometimes rules which may be  a => sometimes ~ p and               */
/* wlast false => sometimes q, say.                                          */
/* Currently in this example no loops have been found so the list containing */
/* paths previously detected in each case is empty. The program attempts to  */
/* find a loop in an eventuality form this list. It first tries to look for  */
/* a loop for the first eventuality in the ERulePaths list (in our example p)*/
/* If the program detects a loop which has not been found before it returns  */
/* the eventuality (p) in the variable NewERule and the loop it has found in */
/* LoopPath. If no new loops are found in the first eventuality in the       */
/* ERulePaths list the progam looks for loops in the second, third etc until */
/* a new loop is found or no new loops can be found.                         */
/* RelatedRules are the set of rules of the form  R => next p, where p is   */
/* the current eventuality, and RulesUsed contains a list of the numbers of  */
/* rules used in this loop.                                                  */
/*                                                                           */
/*****************************************************************************/

/* Related rules are the new clauses generated by temporal resolution */

tempres(Rules,[[ERule,PrevPath]|ERulePaths],NewERule,RelatedRules,RulesUsed,LoopPath):-
  temp_resolution(ERule,ERulePaths,Rules,PrevPath,NewERule,RelatedRules,RulesUsed,LoopPath).


/*****************************************************************************/
/* temp_resolution(ERule, ERulePaths, Rules, PrevPaths, NewERule,            */
/*                                        RelatedRules,RulesUsed, LoopPath)  */
/*                                                                           */
/* Takes an eventuality ERule (say p), a list of loops previously found for  */
/* this eventuality (PrevPaths), a list of the remaining eventualities       */
/* paired with loops which have been found previously (ERulePaths), and the  */
/* list of rules (Rules) which have been output from previous cycles of      */
/* temporal and non temporal resolution. The global box rules are extracted  */
/* from Rules and then the program looks for new loops in ERule from these   */
/* global box rules. If a new loop is found in ERule the loop is returned in */
/* LoopPath and the eventuality it matches is returned in NewERule. If no    */
/* loop is found for this ERule the first eventuality and list of previous   */
/* paths is stripped from the front of ERulePaths and a new loop is looked   */
/* in this new eventuality.                                                  */
/* RelatedRules are the set of rules of the form  R => next p, where p is   */
/* the current eventuality, and RulesUsed contains a list of the numbers of  */
/* rules used in this loop.                                                  */
/*                                                                           */
/*****************************************************************************/

temp_resolution(ERule,ERulePaths,Rules,PrevPaths,NewERule,RelatedRules,RulesUsed,NewLoopPath):-
  my_writef('Attempting temporal resolution with %t.\n',[ERule]),
  resolve_eventuality(ERule, Rules, RelatedRules1,RulesUsed1,LoopPath),   
  test_path(ERule,ERulePaths,Rules,PrevPaths,LoopPath,NewERule,RelatedRules1,RulesUsed1,RelatedRules,RulesUsed,NewLoopPath).

/*****************************************************************************/
/*   test_path(ERule,ERulePaths, Rules, PrevPaths, LoopPath, NewERule,       */
/*         RelatedRules1,RulesUsed1, RelatedRules,RulesUsed, NewLoopPath).   */
/*                                                                           */
/* Checks to see whether we need to do any more temporal resolution or not.  */
/* We terminate without having found a loop (NewLoopPath is empty) if there  */
/* are no more eventualities to deal with (ERulePaths is empty) and if we    */
/* detected a loop this time (LoopPath is empty) or we have detected a loop  */
/* which is the same a a previous loop. Otherwise we recursively call        */
/* temp_resolution with the next eventuality on the ERulePaths list          */
/* with a loop if we detect a loop that is not the same as a loop we have    */
/* before for this eventuality.                                              */
/*                                                                           */
/*****************************************************************************/

                       % Last loop detected was empty and there are no eventualities
                       % left to process.

test_path(_,[],_Rules,_,[],_NewERule,_,_,[],[],[]):- !.

                       % Last loop detected was empty so look for loops for the
                       % next eventuality.

test_path(_,[[ERule,PrevPath]|ERulePaths], Rules,_,[],NewERule,_,_,RelatedRules,RulesUsed,LoopPath):- !,
   temp_resolution(ERule,ERulePaths,Rules,PrevPath,NewERule,RelatedRules,RulesUsed,LoopPath).

                       % Last loop was the same as one we found previously and
                       % there are no eventualities left to process.

test_path(_,[],_Rules,PrevPath1,LoopPath,_NewERule,_,_,[],[],[]):-
   same_node_in_list(PrevPath1,LoopPath),!.

                       % Last loop found was the same as one we found previously
                       % so look for loops for the next eventuality.

test_path(_,[[ERule,PrevPath]|ERulePaths],Rules,PrevPath1,LoopPath,NewERule,_,_,RelatedRules,RulesUsed,NewLoopPath):-
   same_node_in_list(PrevPath1,LoopPath),!,
   temp_resolution(ERule,ERulePaths,Rules,PrevPath,NewERule,RelatedRules,RulesUsed,NewLoopPath).

                       % We have found a new loop that is not empty and is not the
                       % same as a loop we have found earlier for this eventuality
                       % so return this new loop.

test_path(ERule,_, _, _, LoopPath,ERule,RelatedRules,RulesUsed,RelatedRules,RulesUsed,LoopPath).


/*****************************************************************************/
/* same_node_in_list(PrevLoops,LoopPath)                                     */
/*                                                                           */
/* This checks to see whether The new loop we have found LoopPath is the     */
/* as any of the loops we have found already PrevLoops.                      */ 
/*                                                                           */
/*****************************************************************************/

same_node_in_list([PrevLoop|_],LoopPath):-
   same_node(PrevLoop,LoopPath),!.

same_node_in_list([_|PrevLoops],LoopPath):-
   same_node_in_list(PrevLoops,LoopPath).

/*****************************************************************************/
/* resolve_eventuality(Eventuality, Rules, RelatedRules,RulesUsed, LoopPath) */
/*                                                                           */
/* Takes a single eventuality (Eventuality), and a list of Global Box Rules  */
/* (Rules), negates the eventuality (Eventuality), and finds a list of rules */
/* which imply this proposition. The lhs of these rules are added as initial */
/* nodes in graph, and a loop through the graph is searched for starting     */
/* from the top node which is the simplified disjunction of the left hand    */
/* sides of these initial nodes. A graph is built by looking for the largest */
/* set of rules (or combinations of rules whose left hand side imply the     */
/* previous node and whose right hand side imply the top node.               */
/* RelatedRules are the set of rules of the form  R => p, where p is   */
/* the current eventuality, and RulesUsed should contain a list of the       */
/* numbers of rules used in this loop. Unfortunately this has not yet been   */
/* coded properly but has been left in returning [] in the hope that it will */
/* be fixed. All the related rules are for is for generating the proof at the*/
/* end.                                                                      */
/*                                                                           */
/*****************************************************************************/

                       % Note RulesUsed is set to [] !

resolve_eventuality(F,Others, RelatedRules,[], LoopList):-      
   snff_negate(F, NewF),                  % Negates F (ie ~F  --> F, and F --> ~F)
   relatedrules(NewF, Others, RelatedRules, NewOthers),   
   write_ruleset(RelatedRules),
   write_ruleset(NewOthers),
   add_initial(RelatedRules, OrigNodes), !,                
   simplify_top_node(F,OrigNodes,SimplerOrig),
   build_graph(F,Others, SimplerOrig,SimplerOrig, LoopList).       

/*****************************************************************************/
/* related_rules(Prop, [Rule|RuleList], RelatedRules, RemainingRules)        */
/*                                                                           */
/* This predicate takes a proposition, the negation of the proposition,      */
/* relating to the current eventuality, and a list of rules which are global */
/* box rules. It searches these rules for rules implying the eventuality,    */
/* returning them in RelatedRules.                                           */
/* Note relatedrules will not include rules which have Prop as part of a     */
/* disjunction on the rhs of the implication, or those which have            */
/* wlast false on the lhs of the implication ie wlast false => Prop.         */
/*****************************************************************************/

relatedrules(Prop, [r(N1,N2,P => next Prop)|RuleList], [r(N1,N2, P => next Prop)|RelatedRules], RemainingRules):- !,
   relatedrules(Prop, RuleList, RelatedRules, RemainingRules).

relatedrules(Prop, [r(N1,N2,Prop)|RuleList], [r(N1,N2, Prop)|RelatedRules], RemainingRules):- 
   disjunction_of_literals(Prop),!,
   relatedrules(Prop, RuleList, RelatedRules, RemainingRules).

relatedrules(Prop, [Rule|RuleList], RelatedRules, [Rule|RemainingRules]):-
   relatedrules(Prop, RuleList, RelatedRules, RemainingRules).

relatedrules(_,[],[],[]).        

/*****************************************************************************/
/* add_initial(Rules, Nodes)                                                 */
/*                                                                           */
/* Takes a list of rules which imply the negation of the current eventuality */
/* and returns a list with the rhs of these rules as a list of nodes.        */
/* eg a list of rules [ a => p,  b => next p,  a & c => next p]           */
/* would return a node list of the form [[a], [b], [a,c]]                    */
/*                                                                           */
/*****************************************************************************/

add_initial([r(_,_, F => next _)|Rules], [StrippedF |Nodes]):-
   strip(F, StrippedF),
   add_initial(Rules, Nodes).

add_initial([r(_,_, F)|Rules], [[true] |Nodes]):-
   disjunction_of_literals(F),
   add_initial(Rules, Nodes).

add_initial([], []).

/*****************************************************************************/
/* build_graph(F,Rules, OrigNodes, RulesUsed,Solution)                         */
/*                                                                           */
/* Takes current set of Rules (will be Global Box Rules without rules        */
/* implying current eventuality), the set of OrigNodes (the Original Nodes   */
/* the graph), the negation of the current eventuality (OurP). It returns    */
/* and returns the loop detected if one has been found in Solution and       */
/* should store the numbers of the rules it used to detect the loop in       */
/* RulesUsed but this is not implemented correctly yet. Build graph          */
/* when either the new node is "true", the last two nodes are equivalent, or */
/* the new node is empty.                                                    */
/*                                                                           */
/*****************************************************************************/

                    % Top node has been simplified to "true"
      
build_graph(_,_,_,[[true]],[[true]]):- !.

build_graph(F,Rules, PrevNode, TopNode,FinalNode):- 
      writef('\nThe New Node is %t.\n',[PrevNode]),
      build_node(F,Rules, PrevNode,TopNode,NewNode),!,
      test_newnode(F,Rules, PrevNode, TopNode,NewNode,FinalNode).

/*****************************************************************************/
/* test_newnode(Rules, PrevNode, TopNode,NewNode,FinalNode).                 */
/*                                                                           */
/* Checks termination conditions for build graph. We stop recalling          */
/* build_graph if the new node (NewNode) is empty returning with an empty    */
/* loop. We terminate if our new node has been simplified to "true" or if    */
/* our new node is equivalent to our previous node.                          */
/*                                                                           */
/*****************************************************************************/

test_newnode(_,_,_,_,[],[]):- !.              % Empty node.

test_newnode(_,_,_,_,[[true]],[[true]]):-!.   % Node equivalent to "true"

test_newnode(_,_, PrevNode, _,NewNode,NewNode):-
      same_node(PrevNode,NewNode),!.          % Node equivalent to the previous node.

test_newnode(F,Rules, _, TopNode,NewNode,FinalNode):-
      build_graph(F,Rules, NewNode, TopNode,FinalNode).
                                              % Try and build a new node

/*****************************************************************************/
/* build_node(F,Rules, PrevNode, TopNode,NewNode).                             */
/*                                                                           */
/* To build a new node we take each rule from our global box rules (Rules)   */
/* in turn and check whether its rhs implies the previous node (PrevNode)    */
/* and its lhs implies the top node (TopNode). If the former is the case but */
/* not the latter then the rule is combined with the initial rules (obtained */
/* from TopNode) so the latter holds also. If the rules may be used if       */
/* combined with others they are stored in a separate list and combined at   */
/* the end. The lhs of all the rules which satisfy the above two criteria    */
/* are disjoined and simplified to  create the new node (NewNode).           */
/*                                                                           */
/*****************************************************************************/

build_node(F,Rules, PrevNode, TopNode,FinalNode):-
      flatten(PrevNode,FlatPrev),
      get_relevant_rules(Rules,PrevNode,FlatPrev,TopNode, Useful,[],NewishNode),
      combine_useful_rules(Useful,PrevNode,FlatPrev,TopNode,NewishNode,NewThisNode),
      simplify_new_node(F,NewThisNode,FinalNode).

/*****************************************************************************/
/* combine_useful_rules(Rules, [D1|DRest], PrevNode,TopNode, ThisNode,FinalNode) */
/*                                                                           */
/* Takes all the rules that we think we may be able to combine together so   */
/* the rhs implies the previous node (PrevNode) and its lhs implies the top  */
/* node (TopNode). For disjunct (D1) we separate out all the rules that may  */
/* combine together to give us a D1 on the rhs and then do the same with the */
/* remaining disjuncts. These combined rules are tested to see if the rhs    */
/* implies the previous node (PrevNode) and its lhs implies the top node     */
/* (TopNode) and if so the lhs is returned in FinalNode.                     */
/*                                                                           */
/*****************************************************************************/

combine_useful_rules([],_,_,_, FinalNode,FinalNode):- !.    % We have no useful rules.

combine_useful_rules(Rules, PrevNode,FlatPrev,TopNode,ThisNode,FinalNode):- !,
    split_into_sublists(Rules,FlatPrev,SplitRules),
    combine_for_disjuncts(PrevNode,SplitRules,PrevNode,TopNode,ThisNode,FinalNode).

/*****************************************************************************/
/* get_relevant_rules(Rules,PrevNode,FlatPrev,TopNode,ThisNode,Useful,NewNode). */
/*                                                                           */
/* Takes the set of global box rules (Rules) and extracts rules whose right  */
/* hand sides imply the previous node. If the left hand side of this rule    */
/* also implies the top node then the lhs is added as a disjunct of NewNode. */
/* If the lhs does not it is combined with the lhs of each initial           */
/* rule. Also rules are searched for where the propositions on the rhs of the*/
/* rule are a subset of the proposition in the previous node but the rhs     */
/* does not imply the previous node. Subsets of these are combined in an     */
/* attempt that their rhs will imply the previous node.                      */
/*                                                                           */
/*****************************************************************************/

get_relevant_rules([r(_,_, P => next Q)|Rest],PrevNode,FlatPrev,TopNode,Useful,ThisNode,FinalNode):-
       strip_or(Q,StrippedQ),            % Rhs => PrevNode & lhs => TopNode
       set_implies(StrippedQ,PrevNode),  % Add lhs to the list for the new node.
       strip_or(P,[StrippedP]),
       set_implies([StrippedP],TopNode),!,
       remove_subsumed(StrippedP,ThisNode,NewThisNode),
       get_relevant_rules(Rest,PrevNode,FlatPrev,TopNode,Useful,NewThisNode,FinalNode).

get_relevant_rules([r(_,_,P => next Q)|Rest],PrevNode,FlatPrev,TopNode,Useful,ThisNode, FinalNode):-
       strip_or(Q,StrippedQ),             % Rhs => PrevNode & not (lhs => TopNode)
       set_implies(StrippedQ,PrevNode),!, % combine lhs with initial nodes and add.
       strip_or(P,StrippedP),
       combine_lhs_with_initial(StrippedP,TopNode,NewDisjuncts),
       remove_subsumed_list(NewDisjuncts,ThisNode,NewThisNode),
       get_relevant_rules(Rest,PrevNode,FlatPrev,TopNode,Useful,NewThisNode,FinalNode).

get_relevant_rules([r(N1,N2,P => next Q)|Rest],PrevNode,FlatPrev,TopNode,[r(N1,N2,P => next Q)|Useful],ThisNode,FinalNode):-
       strip(Q,StrippedQ),
       subset(StrippedQ,FlatPrev),!,      % in the previous node. Save for combining.
       get_relevant_rules(Rest,PrevNode,FlatPrev,TopNode,Useful,ThisNode,FinalNode).

get_relevant_rules([r(_,_,Q)|Rest],PrevNode,FlatPrev,TopNode,Useful,ThisNode,FinalNode):-
       disjunction_of_literals(Q),
       strip_or(Q,StrippedQ),             % Rhs => PrevNode & not (lhs => TopNode)
       set_implies(StrippedQ,PrevNode),!, % combine lhs with initial nodes and add.
       remove_subsumed_list(TopNode,ThisNode,NewThisNode),
       get_relevant_rules(Rest,PrevNode,FlatPrev,TopNode,Useful,NewThisNode,FinalNode).

get_relevant_rules([r(N1,N2,Q)|Rest],PrevNode,FlatPrev,TopNode,[r(N1,N2,Q)|Useful],ThisNode,FinalNode):-
       disjunction_of_literals(Q),
       strip(Q,StrippedQ),
       subset(StrippedQ,FlatPrev),!,      % in the previous node. Save for combining.
       get_relevant_rules(Rest,PrevNode,FlatPrev,TopNode,Useful,ThisNode,FinalNode).

get_relevant_rules([_|Rest],PrevNode,FlatPrev,TopNode,Useful,ThisNode,FinalNode):- !,
        get_relevant_rules(Rest,PrevNode,FlatPrev,TopNode,Useful,ThisNode,FinalNode).
                                        % Not useful.

get_relevant_rules([],_,_,_,[],FinalNode,FinalNode).        % Recursion base case.

/*****************************************************************************/
/* combine_lhs_with_initial(Lhs,TopNode,NewDisjuncts)                        */
/*                                                                           */
/* Takes the left hand side of a rule (Lhs) and the top node and combines    */
/* Lhs with each disjunct of Top Node storing output on NewDisjuncts.        */
/*                                                                           */
/*****************************************************************************/
                                         % This fires if our Conj is just a
                                         % literal and saves doing append. 

combine_lhs_with_initial([[Conj]],[H|Tail],[NewDisjunct|NewDisjuncts]):- !,
     simplify_a_disjunct([Conj|H],NewDisjunct),       
     combine_lhs_with_initial([[Conj]],Tail,NewDisjuncts).

combine_lhs_with_initial([Conj],[H|Tail],[NewDisjunct|NewDisjuncts]):-
     append(Conj,H,NewConj),
     simplify_a_disjunct(NewConj,NewDisjunct),       
     combine_lhs_with_initial([Conj],Tail,NewDisjuncts).

combine_lhs_with_initial(_,[],[]).

/*****************************************************************************/
/* same_node(N1,N2).                                                         */
/*                                                                           */
/* N1 and N2 are lists made up of sublists that represent a formula in DNF.  */
/* To check that N1 and N2 are equivalent we must make sure that N1 => N2    */
/* and N2 => N1 (remembering that we have lists and sublists.                */
/*                                                                           */
/*****************************************************************************/

same_node([],[]):-!.

same_node(List1,List2):-
     set_implies(List1,List2),
     set_implies(List2,List1).
   
/*****************************************************************************/
/* combine_for_disjunct(Rules, D1,PrevNode,TopNode, ThisNode,FinalNode)      */
/*                                                                           */
/* Takes a disjunct D1 from the previous node (PrevNode) and finds all the   */
/* rules from Rules (which is a subset of the set of global box rules of     */
/* rules that do not satisfy our two criteria on the lhs and rhs of rules but*/
/* may be combined together to satisfy these criteria) that may be combined  */
/* together to give us D1. These are then combined together and the left     */
/* hand sides of rules that do satisfy our criteria are returned in          */
/* NewDisjuncts.                                                             */
/*                                                                           */
/*****************************************************************************/

combine_for_disjuncts([D1|Disjuncts],Rules,PrevNode,TopNode,ThisNode,FinalNode):-
     get_rules_for_disjunct(Rules,D1,NewRules),
     combine_rules(Rules,NewRules,D1,PrevNode,TopNode,ThisNode,NewerNode,NewerRules),
     combine_for_disjuncts(Disjuncts, NewerRules,PrevNode,TopNode,NewerNode,FinalNode).

combine_for_disjuncts([], _Rules, _PrevNode, _TopNode, FinalNode,FinalNode).

/*****************************************************************************/
/* get_rules_for_disjunct(Rules, Disjunct,NewRules)                          */
/*                                                                           */
/* Takes the set of rules we think we may be able to combine together (Rules)*/
/* and a disjunct (Disjunct) and we extract the rules that may give us this  */
/* disjunct at least by looking for rules that have a literal or literals on */
/* their rhs in common with the disjunct.                                    */
/*                                                                           */
/*****************************************************************************/

get_rules_for_disjunct([[Lit]|Rest],[Lit|Disjuncts],[NewRest]):- !,   % No rules for Lit
     get_rules_for_disjunct(Rest,Disjuncts,NewRest).

get_rules_for_disjunct([[Lit,Rules]|Rest],[Lit|Disjuncts],[Rules|NewRest]):- !,
     get_rules_for_disjunct(Rest,Disjuncts,NewRest).

get_rules_for_disjunct([[Lit,Rules]|Rest],Disjuncts,[Rules|NewRest]):-
     member(Lit,Disjuncts),!,
     get_rules_for_disjunct(Rest,Disjuncts,NewRest).

get_rules_for_disjunct([[_,_]|Rest],Disjuncts,NewRest):-
     get_rules_for_disjunct(Rest,Disjuncts,NewRest).

get_rules_for_disjunct(_,[],[]).           % Recursion base case.

get_rules_for_disjunct([],_,[]).           % Recursion base case.

/*****************************************************************************/
/* combine_rules(NewRules, PrevNode, TopNode,ThisNode,FinalNode)    */
/*                                                                           */
/* Takes the set of rules that we think we can combine to give us one of the */
/* disjuncts Disjunct, splits them into sublists, one for each literal of the*/
/* disjunct and does the combination by combining one rule form each sublist.*/
/* Each combined rule is checked to see whether its rhs implies the previous */
/* node and its left hands side implies the top node and if so its lhs is    */
/* returned in NewDisjuncts.                                                 */
/*                                                                           */
/*****************************************************************************/

combine_rules(Rules,SplitRules, D1,PrevNode, TopNode,ThisNode,FinalNode,NewRules):-
    combine_sublists(SplitRules,Combined),
    get_relevant_combined(Combined,Rules,D1,PrevNode,TopNode,ThisNode,FinalNode,NewRules).

/*****************************************************************************/
/* split_into_sublists(NewRules,Disjunct,SplitRules)                         */
/*                                                                           */
/* Takes the set of rules that we think we can combine to give us one of the */
/* disjuncts Disjunct, splits them into sublists, one for each literal of the*/
/* disjunct, returning the sublists in SplitRules.                           */
/*                                                                           */
/*****************************************************************************/

split_into_sublists(Rules,[C1|Conjuncts],[[C1,C1Rules]|RestRules]):-
   rules_that_give_a_conjunct(C1,Rules,C1Rules),
   split_into_sublists(Rules,Conjuncts,RestRules).

split_into_sublists(_,[],[]).

/*****************************************************************************/
/* rules_that_give_a_conjunct(C1,Rules,C1Rules)                              */
/*                                                                           */
/* Takes a conjunct C1 from one of the disjuncts of our previous node and    */
/* returns C1Rules, a subset of the rules we give it (Rules) where C1 is a   */
/* member of the set of literals formed from the rhs of a rule.              */
/*                                                                           */
/*****************************************************************************/

rules_that_give_a_conjunct(C1,[r(N1,N2,P => next Q)|Rest],[r(N1,N2,P => next Q)|NewRest]):-
   strip(Q,StrippedQ),
   member(C1,StrippedQ),!,
   rules_that_give_a_conjunct(C1,Rest,NewRest).

rules_that_give_a_conjunct(C1,[r(N1,N2,Q)|Rest],[r(N1,N2,Q)|NewRest]):-
   disjunction_of_literals(Q),
   strip(Q,StrippedQ),
   member(C1,StrippedQ),!,
   rules_that_give_a_conjunct(C1,Rest,NewRest).

rules_that_give_a_conjunct(C1,[_|Rest],NewRest):-
   rules_that_give_a_conjunct(C1,Rest,NewRest).

rules_that_give_a_conjunct(_,[],[]).

/*****************************************************************************/
/* combine_sublists(SplitRules,Combined)                                     */
/*                                                                           */
/* Takes a list split into sublists (SplitRules) and combines the rules      */
/* so as to take one from each sublist returning the combined rules in       */
/* Combined.                                                                 */
/*                                                                           */
/*****************************************************************************/

combine_sublists([_],[]):-!.               % Only one sublist

combine_sublists([L1|[L2]],L12):- !,       % Two sublists
   combine_two_sublists(L1,L2,L12).

combine_sublists([L1|L2],NewL1L2):- !,     % More than two sublists
   combine_sublists(L2,NewL2),
   combine_two_sublists(L1,NewL2,NewL1L2).

combine_sublists([],[]).                   % No sublists
/*****************************************************************************/
/* combine_two_sublists(L1,L2,NewL1L2).                                      */
/*                                                                           */
/* Takes two sublists L1 and L2 an combines every rule in L1 with every rule */
/* in L2, returning the combined rules in NewL1L2.                           */
/*                                                                           */
/*****************************************************************************/

combine_two_sublists([H1|Tail1],List2,NewList):-
   combine_element_with_all(H1,List2,H1List2),
   combine_two_sublists(Tail1,List2,Tail1List2),
   append(H1List2,Tail1List2,NewList).

combine_two_sublists([],_,[]).

/*****************************************************************************/
/* combine_element_with_all(H1,List2,H1List2)                                */
/*                                                                           */
/* Takes a rule H1 and combines it with every rule in List2 to give H1List2. */
/*                                                                           */
/*****************************************************************************/

combine_element_with_all(H1,[H2|Tail2],FinalList):-
  combine_two_rules(H1,H2,H1H2),
  test_combine_rules(H1H2,H1,Tail2,FinalList).

combine_element_with_all(_,[],[]).

/****************************************************************************/

test_combine_rules(r(_,_,false => next _),H1,Tail2,H1Tail2):- !,
  combine_element_with_all(H1,Tail2,H1Tail2).

test_combine_rules(Rule,H1,Tail2,[Rule|H1Tail2]):-
  combine_element_with_all(H1,Tail2,H1Tail2).

/*****************************************************************************/
/* get_relevant_combined(Combined,PrevNode,TopNode,ThisNode,FinalNode).      */
/*                                                                           */
/* Does the same as get_relevent_rules but for those we have combined. We    */
/* search for rules from Combined whose right hand sides imply the previous  */
/* node PrevNode). If the left hand side of the rule also implies the top    */
/* node (TopNode) then the lhs is added as a disjunct of NewDisjuncts.       */
/* If the lhs does not it is combined with the lhs of each initial           */
/* rule and then added to NewDisjuncts.                                      */
/*                                                                           */
/*****************************************************************************/

get_relevant_combined(_,Rules,_,_,_,[[true]],[[true]],Rules):-!.

get_relevant_combined([r(_,_,P => next Q)|Rest],Rules,D1,PrevNode,TopNode,ThisNode,FinalNode,NewRules):-
       strip_or(Q,StrippedQ),             % Rhs => PrevNode & lhs => TopNode
       set_implies(StrippedQ,PrevNode),   % Add lhs to the list for the new node.
       strip_or(P,[StrippedP]),
       set_implies([StrippedP],TopNode),!,
       remove_subsumed(StrippedP,ThisNode,NewerThisNode),
       get_relevant_combined(Rest,Rules,D1,PrevNode,TopNode,NewerThisNode,FinalNode,NewRules).


get_relevant_combined([r(_,_,P => next Q)|Rest],Rules,D1,PrevNode,TopNode,ThisNode,FinalNode,NewRules):-
       strip_or(Q,StrippedQ),             % Rhs => PrevNode & not (lhs => TopNode)
       set_implies(StrippedQ,PrevNode),!,   % combine lhs with initial nodes and add.
       strip_or(P,StrippedP),
       combine_lhs_with_initial(StrippedP,TopNode,NewDisjuncts),
       remove_subsumed_list(NewDisjuncts,ThisNode,NewerThisNode),
       get_relevant_combined(Rest,Rules,D1,PrevNode,TopNode,NewerThisNode,FinalNode,NewRules).

get_relevant_combined([r(_,_,Q)|Rest],Rules,D1,PrevNode,TopNode,ThisNode,FinalNode,NewRules):-
       disjunction_of_literals(Q),
       strip_or(Q,StrippedQ),             % Rhs => PrevNode & not (lhs => TopNode)
       set_implies(StrippedQ,PrevNode),!,   % combine lhs with initial nodes and add.
       remove_subsumed_list(TopNode,ThisNode,NewerThisNode),
       get_relevant_combined(Rest,Rules,D1,PrevNode,TopNode,NewerThisNode,FinalNode,NewRules).

get_relevant_combined([Rule|Rest],Rules,D1,PrevNode,TopNode,ThisNode,FinalNode,NewRules):- !,
       add_combined_to_rules(Rule,Rules,D1,PrevNode,NewerRules),
       get_relevant_combined(Rest,NewerRules,D1,PrevNode,TopNode,ThisNode,FinalNode,NewRules).
                                        % Not useful.

get_relevant_combined([],Rules,_,_,_,FinalNode,FinalNode,Rules).         % Recursion base case.
/*****************************************************************************/
/* combine_two_rules(R1,R2,NewRule).                                         */
/*                                                                           */
/* Takes two rules R1 and R2, combines them and simplifies them to give      */
/* NewRule.                                                                  */
/*                                                                           */
/*****************************************************************************/

combine_two_rules(r(N1,_,P => next F), r(N3,_,Q => next G), r(N1,[N1,N3],NewLHS => next RHS)):-
           simplify_and(P and Q, NewerLHS),
           simplify_and_dup(NewerLHS, NewLHS),       
	   simplify_dnf(F and G, RHS).

combine_two_rules(r(N1,_,F), r(N3,_,Q => next G), r(N1,[N1,N3],Q => next RHS)):-
           disjunction_of_literals(F),
	   simplify_dnf(F and G, RHS).

combine_two_rules(r(N1,_,P => next F), r(N3,_,G), r(N1,[N1,N3],P => next RHS)):-
           disjunction_of_literals(G),
	   simplify_dnf(F and G, RHS).

combine_two_rules(r(N1,_,F), r(N3,_,G), r(N1,[N1,N3],RHS)):-
           disjunction_of_literals(F),
           disjunction_of_literals(G),
	   simplify_dnf(F and G, RHS).

/*****************************************************************************/
/*   simplify_dnf(X,Y)                                                       */
/*                                                                           */
/* Called during the combinations coding (combine_two_rules), as when two    */
/* are combined eg  A => B, and  X => Y, we will get                       */
/*  (A and X) => (B and Y). If B or Y were previously disjunctive           */
/* then the right hand side will need to be converted to DNF and simplified. */
/* This predicate converts to disjunctive normal form, checks for            */
/* complementary literals, or duplicated literals, and removes "false" from  */
/* disjunction.                                                              */
/*                                                                           */
/*****************************************************************************/

simplify_dnf(X,Y):-
  cnf_to_dnf(X,NewX),
  simplifylist(NewX,SimplerX),
  simplify_in_dnf(SimplerX,SimplerDNF),
  simplify_dnf1(SimplerDNF,Y).

/****************************************************************************/

cnf_to_dnf(X,NewX):-
  strip_cnf(X,StrippedX),
  combine_sublists_in_dnf(StrippedX,NewX).

/*****************************************************************************/
/* simply_dnf1(X,Y)                                                          */
/*                                                                           */
/* If the list X is empty returns false otherwise removes duplicate          */
/* disjuncts, and returns Y which has the relevant and and or included.      */                           
/*                                                                           */
/*****************************************************************************/

simplify_dnf1([],false):-!.

simplify_dnf1([true],true):-!.

simplify_dnf1(SimpleX,Y):-
  rewrite_and_or(SimpleX,Y).

/*****************************************************************************/
/* simplify_a_disjunct(Dis,SimpleDis).                                       */
/*                                                                           */
/* Simplifies a single disjunct i.e. removes true, replaces false by false   */
/* removes duplicate literals and replaces complementary literals by false.  */
/*                                                                           */
/*****************************************************************************/

simplify_a_disjunct(Dis,NewDis):-
   simplifylist([Dis],[NewDis]).

/*****************************************************************************/
/*                                                                           */
/*  simplifylist(X,SimplerX)                                                 */
/*  Takes a list X which is of the form [[a,a],[b,a]] (representing dnf on   */
/*  (a or b) and a, which gives us (a and a) or (a and b). It searches each  */
/*  member of the list (ie [a,a] and then [b,a]) for complementary literals  */
/*  and replaces them with false, otherwise it looks for duplicate literals  */
/*  and removes any duplicates. SimplerX is the list which is returned.      */                        
/*                                                                           */
/*****************************************************************************/

simplifylist([H|Tail],[[false]|NewTail]):-
        simplify_and_false(H),!,
        simplifylist(Tail,NewTail).

simplifylist([H|Tail], [[false]|NewTail]):-
        comp_lits_and(H),!,
        simplifylist(Tail, NewTail).

simplifylist([H|Tail], [NewerH|NewTail]):-
        remove_duplicate(H, NewH),!,
        simplify_and_true(NewH,NewerH),
        simplifylist(Tail, NewTail).

simplifylist([],[]).

/*****************************************************************************/
/*    remove_true(X,NewX),                                                   */
/*                                                                           */
/* Takes (X) a list of lists of disjuncts eg [[a],[a,b]] which has had any   */
/* sublists containing complementary literals replaced by "false" and any    */
/* sublists containing occurances of duplicate literals replaced by just a   */
/* single instance of that one instance. It checks X and if it finds "true"  */
/* as a member of the list returns "true" otherwise it returns X.            */
/*                                                                           */
/*****************************************************************************/

remove_true(List,[[true]]):-
    member([true],List),!.

remove_true(List,[[true]]):-
    member([],List),!.

remove_true(List,List):-!.

/*****************************************************************************/
/*    remove_false(X,NewX),                                                  */
/*                                                                           */
/* Takes (X) a list of lists of disjuncts eg [[a],[a,b]] which has had any   */
/* sublists containing complementary literals replaced by "false" and any    */
/* sublists containing occurances of duplicate literals replaced by just a   */
/* single instance of that one instance. It checks X and removes occurances  */
/* of false.                                                                 */
/* N.B. In the case where X is [[false],[false],[false]] the removal of all  */
/* the falses will leave us with the empty list ([]). This is replaced       */
/* by false in simply_dnf1 above.                                            */
/*                                                                           */
/*****************************************************************************/

remove_false([[true]],[[true]]):-!.

remove_false([false|Rest],NewRest):-
   !,remove_false(Rest,NewRest).

remove_false([[false]|Rest],NewRest):-
   !,remove_false(Rest,NewRest).

remove_false([Nodelist|Rest],[Nodelist|NewRest]):-
   !,remove_false(Rest,NewRest).

remove_false([],[]).

/*****************************************************************************/
/* simplify_and_false(List).                                                 */
/*                                                                           */
/* Called from simplifylist. List will be a list of literals (or false) which*/
/* make up a disjunct as part of the right hand side of a (combined) rule    */
/* of the form [a,b,c,~ d,false,f]. If false is a member of this list then */
/* the whole list reduces to false so this predicate succeeds.               */
/*                                                                           */
/*****************************************************************************/

simplify_and_false([false|_]):- !.

simplify_and_false([_|Rest]):-
   simplify_and_false(Rest).

/*****************************************************************************/
/* simplify_and_true(List).                                                  */
/*                                                                           */
/* Called from simplifylist. List will be a list of literals (or false) which*/
/* make up a disjunct as part of the right hand side of a (combined) rule    */
/* of the form [a,b,c,~ d,false,f]. If false is a member of this list then */
/* the whole list reduces to false so this predicate succeeds.               */
/*                                                                           */
/*****************************************************************************/

simplify_and_true([true|Rest],NewRest):- !,
   simplify_and_true(Rest,NewRest).

simplify_and_true([H|Rest],[H|NewRest]):- !,
   simplify_and_true(Rest,NewRest).

simplify_and_true([],[]).

/*****************************************************************************/
/* simplify_top_node(F,ListofDisjuncts,SimplifiedDNF).                         */
/*                                                                           */
/* Simplifies our nodes given that ListofDisjuncts is of the form            */
/* [[..],[..],[..]] where each ... is a list of literal and are supposed to  */
/* conjoined together.                                                       */
/*                                                                           */
/*****************************************************************************/

simplify_top_node(_,[],[]):-!.   % Need this in case initial node is empty.

simplify_top_node(_,[[true]],[[true]]):-!.   
                                 % Need this in case initial node is true.

simplify_top_node(F,ListofDisjuncts,FinalDNF):-
  simplifylist(ListofDisjuncts,SimpleDisjuncts),
  simplify_in_dnf(SimpleDisjuncts,SimplerDNF),
  simplify_comp_lits_in_dnf(SimplerDNF,SimplifiedDNF1),
  simplify_in_dnf(SimplifiedDNF1,SimplifiedDNF),
  remove_bad_disjuncts(F,SimplifiedDNF,FinalDNF).

/*****************************************************************************/
/* simplify_new_node(F,ListofDisjuncts,SimplifiedDNF).                         */
/*                                                                           */
/* Simplifies our nodes given that ListofDisjuncts is of the form            */
/* [[..],[..],[..]] where each ... is a list of literal and are supposed to  */
/* conjoined together.                                                       */
/*                                                                           */
/*****************************************************************************/

simplify_new_node(_,[],[]):-!.   % Need this in case initial node is empty.

simplify_new_node(F,SimplerDNF,FinalDNF):-
  simplify_comp_lits_in_dnf(SimplerDNF,SimplifiedDNF1),
  simplify_in_dnf(SimplifiedDNF1,SimplifiedDNF),
  remove_bad_disjuncts(F,SimplifiedDNF,FinalDNF).


/*****************************************************************************/
/*  remove_bad_disjuncts(F,SimplifiedDNF,FinalDNF).                          */
/*                                                                           */
/*  Takes the eventuality we are resolving with F and the current node in    */
/*  DNF. If any of the disjuncts contain our eventuality then we will never  */
/*  be able to build a rule which has both the eventuality and its negation  */
/*  on the RHS to extend this node so we can just delete this disjunct.      */
/*****************************************************************************/

remove_bad_disjuncts(_,[],[]).

remove_bad_disjuncts(F,[H|Tail],NewTail):-
   member(F,H),!,
   remove_bad_disjuncts(F,Tail,NewTail).

remove_bad_disjuncts(F,[H|Tail],[H|NewTail]):-
   remove_bad_disjuncts(F,Tail,NewTail).

/*****************************************************************************/
/* simplify_in_dnf(List,NewList)                                             */
/*                                                                           */
/* Takes a list of disjuncts each of which have been simplified (removing    */
/* true from conjuncts, making false disjuncts that have false as a conjunct */
/* deleting duplicate literals in variables and replacing complementary      */
/* by false) and simplifies the complete disjunction. This is done by        */
/* replacing the whole disjunction by true is one of the disjuncts is true,  */
/* deleting disjuncts that are false and removing any disjuncts D2 where     */
/* D2 => D1, where D1 is another disjunct.                                   */
/*                                                                           */
/*****************************************************************************/

simplify_in_dnf(List,NewList):-
     remove_true(List,NewerList),
     remove_false(NewerList,NoFalseList),
     order_node(NoFalseList,OrderedList),
     remove_sublists(OrderedList,NewList).

/*****************************************************************************/
/* order_node(Node,NewNode).                                                 */
/*                                                                           */
/* Takes a list made up from sublists (Node) and orders it dependent on the  */
/* length of the sublist. This is to make it easier to remove disjuncts D2   */
/* where D2 => D1.                                                           */
/*                                                                           */
/*****************************************************************************/
order_node([[true]],[[true]]):-!.   % If we have a node (a v ~a) we need this case.

order_node([[false]],[[false]]):-!.

order_node([],[[false]]):-!.

order_node(Node,NewNode):-
   give_length_key_list(Node,KeyList),
   keysort(KeyList,Sorted),
   take_key(Sorted,NewNode).

/*****************************************************************************/
/* give_length_key_list(Node,KeyList)                                        */
/*                                                                           */
/* Gives a key which is the length of the sublist to each sublist.           */
/*                                                                           */
/*****************************************************************************/

give_length_key_list([H|Tail],[Key-H|NewTail]):-
  give_key_length(H,Key),
  give_length_key_list(Tail,NewTail).

give_length_key_list([],[]).
/*****************************************************************************/
/* give_key_length(H,Key)                                                    */
/*                                                                           */
/* Takes a sublist H and calculates its length returning the value in Key.   */    
/*                                                                           */
/*****************************************************************************/
give_key_length([_|Tail],NewKey):-
   give_key_length(Tail,Key),
   NewKey is Key + 1.

give_key_length([],0).
/*****************************************************************************/
/* take_key(List, NewList).                                                  */
/*                                                                           */
/* Takes a list with keys attached for ordering (List) removes the key and   */
/* returns the list in NewList.                                              */
/*                                                                           */
/*****************************************************************************/

take_key([_-H|Tail],[H|NewTail]):-
  take_key(Tail,NewTail).

take_key([],[]).

/*****************************************************************************/
/* remove_sublists(OrderedList,NewList).                                     */
/*                                                                           */
/* Looks for sublists S1 and S2 of OrderedList where S1 is a subset of S2    */
/* deletes S2 from OrderedList until this can be done no more.               */
/*                                                                           */
/*****************************************************************************/

remove_sublists([H|Tail],[H|NewTail]):-
   remove_sublist(H,Tail,NewerTail),
   remove_sublists(NewerTail,NewTail).

remove_sublists([],[]).

/*****************************************************************************/
/*    remove_sublist(H,Tail,NewerTail)                                       */
/*                                                                           */
/* Takes a sublist H and the remaining sublists the same length or longer    */
/* than it, looks for sublists of which H is a subset and deletes them from  */
/* Tail to give NewerTail.                                                   */
/*                                                                           */
/*****************************************************************************/

remove_sublist(H,[H1|Tail],NewTail):-
    subset(H,H1),!,
    remove_sublist(H,Tail,NewTail).

remove_sublist(H,[H1|Tail],[H1|NewTail]):-
    remove_sublist(H,Tail,NewTail).

remove_sublist(_,[],[]).

/*****************************************************************************/
/* simplify_comp_lits_in_dnf(SimplerDNF,SimplifiedDNF1)                      */
/*                                                                           */
/* Looks for literals in a node (in DNF) which are complementary and adds    */
/* new disjuncts that occur if we write from DNF into CNF and back to DNF.   */
/* For example if we have [[s,a],[t,~a]] we add the disjunct [s,t].          */
/*                                                                           */
/*****************************************************************************/

simplify_comp_lits_in_dnf(DNFList,NewList):-
     get_comp_list(DNFList,CompLits),
     add_extra_disjuncts(DNFList,CompLits,NewList).

/*****************************************************************************/
/* get_comp_list(DNFList,CompLits)                                           */
/*                                                                           */
/* Takes all the literals of DNFList and searches for literals where both    */
/* l and ~l are in the list and returns one of this pair in CompLits.        */
/*                                                                           */
/*****************************************************************************/

get_comp_list(DNFList,CompLits):-
   flatten(DNFList,FlatList),
   find_comp_lits(FlatList,CompLits).

/*****************************************************************************/
/*    find_comp_lits(ShortList,CompLits).                                    */
/*                                                                           */
/* Takes a list of literals and their negations. Looks at the each member of */
/* this list in turn and then for its negation in the remainder of the list. */
/* If its negation is found then the literal is added to CompLits.           */
/*                                                                           */
/*****************************************************************************/

find_comp_lits([H|Tail],[H|NewTail]):-
   is_comp_lit(H,Tail),!,
   find_comp_lits(Tail,NewTail).

find_comp_lits([_|Tail],NewTail):-
   find_comp_lits(Tail,NewTail).

find_comp_lits([],[]).
/*****************************************************************************/
/*    is_comp_lit(H,Tail).                                                   */
/*                                                                           */
/* Returns true if the negation of H is a member of Tail.                    */
/*                                                                           */
/*****************************************************************************/

is_comp_lit(H,Tail):-
  snff_negate(H,NegH),
  member(NegH,Tail),!.

/*****************************************************************************/
/* add_extra_disjuncts(DNFList,CompLits,CompLits,NewList).                   */
/*                                                                           */
/* For each pair of complementary literals adds a new disjuncts that occurs  */
/* if we write from DNF into CNF and back to DNF.                            */
/* For example if we have [[s,a],[t,~a]] we add the disjunct [s,t].          */
/*                                                                           */
/*****************************************************************************/

add_extra_disjuncts(List,[H|Tail],NewList):-
   solve_for_comp_lit(H, List, List, NewerList),
   add_extra_disjuncts(NewerList,Tail,NewList).

add_extra_disjuncts(List,[],List).

/*****************************************************************************/
/* solve_for_comp_lit(H, List, NewerList)                                    */
/*                                                                           */
/* Takes a complementary literal, H, the node we have built, List, which is  */
/* in DNF, searches for the first disjuncts from List that have either H or  */
/* ~H as a member. Then the rest of the list is searched for a sublist with  */
/* either ~H or H depending on what our first literal was and adds a new     */
/* disjunct that is the remaining literals from the sublist with the H or ~H */
/* in it and the sublist with the ~H or H in it. This is repeated on the     */
/* remains of the list (after we found the first sublist with H or ~H in).   */
/*                                                                           */
/*****************************************************************************/

solve_for_comp_lit(Comp,[D1|DRest],List,NewestRest):-
    member(Comp,D1),!,
    remove_member(Comp,D1,NewD1),
    snff_negate(Comp,NegComp),
    solve_for_second_comp(NegComp,DRest,List,NewD1,NewList),
    solve_for_comp_lit(Comp,DRest,NewList,NewestRest).

solve_for_comp_lit(Comp,[D1|DRest],List,NewestRest):-
    snff_negate(Comp,NegComp),
    member(NegComp,D1),!,
    remove_member(NegComp,D1,NewD1),
    solve_for_second_comp(Comp,DRest,List,NewD1,NewList),
    solve_for_comp_lit(Comp,DRest,NewList,NewestRest).

solve_for_comp_lit(Comp,[_|DRest],List,NewRest):-
    solve_for_comp_lit(Comp,DRest,List,NewRest).

solve_for_comp_lit(_,[],List,List).

/*****************************************************************************/
/* solve_for_second_comp(Comp,DRest,NewD1,NewerRest)                         */
/*                                                                           */
/* Having found a sublist with ~Comp in we look for the sublist form DRest   */
/* that includes Comp. On finding this sublist we remove Comp from the list  */
/* and append the remaining literals with those remaining from the sublist   */
/* with ~Comp and use this a s a new disjuncts. This is repeated searching   */
/* for any other sublists that may include Comp as a member.                 */
/*                                                                           */
/*****************************************************************************/

solve_for_second_comp(Comp,[[Comp]|_],_,[],[[true]]):- !.
                                   % We have found a literal and its negation.

solve_for_second_comp(Comp,[D1|DRest],All,OldConjuncts,NewestRest):-
    member(Comp,D1),!,
    remove_member(Comp,D1,NewD1),
    append(OldConjuncts,NewD1,NewerConjuncts),
    simplify_a_disjunct(NewerConjuncts,NewConjuncts),
    remove_subsumed(NewConjuncts,All,NewAll),
    solve_for_second_comp(Comp,DRest,NewAll,OldConjuncts,NewestRest).

solve_for_second_comp(Comp,[_|DRest],All,OldConjuncts,NewRest):-
    solve_for_second_comp(Comp,DRest,All,OldConjuncts,NewRest).
    
solve_for_second_comp(_,[],All,_,All).

/******************************************************************************/

remove_subsumed_list([H|Tail],All,FinalAll):-
  remove_subsumed(H,All,NewAll),
  remove_subsumed_list(Tail,NewAll,FinalAll).

remove_subsumed_list([],FinalAll,FinalAll).

/*****************************************************************************/

remove_subsumed([true],_,[[true]]):-!.

remove_subsumed([false],All,All):-!.

remove_subsumed(Dis,[H|Tail],[H|Tail]):-
  subset(H,Dis),!.

remove_subsumed(Dis,[H|Tail],NewTail):-
  subset(Dis,H), !,
  remove_subsumed(Dis, Tail,NewTail).

remove_subsumed(Dis,[H|Tail],[H|NewTail]):-
   remove_subsumed(Dis,Tail,NewTail).
    
remove_subsumed(Dis,[],[Dis]).

/*******************************************************************************/
/* strip_cnf(Clause, List)                                                      */
/*                                                                             */
/*   This predicate removes all ands or ors from Clause which is disjunctive   */
/*   and puts them on a bracketed List eg (a and b) or c would be [[a,b],[c]]  */
/*******************************************************************************/

strip_cnf(A and B,NewAB):- !,
   strip_cnf(A,NewA),
   strip_cnf(B,NewB),
   append(NewA,NewB,NewAB).

strip_cnf(A or B,[NewAB]):- !,
   strip_or(A or B, NewAB).

strip_cnf(A,[[[A]]]).

/*****************************************************************************/
/* combine_sublists_in_dnf(SplitRules,Combined)                              */
/*                                                                           */
/* Takes a list split into sublists (SplitRules) and combines the rules      */
/* so as to take one from each sublist returning the combined rules in       */
/* Combined.                                                                 */
/*                                                                           */
/*****************************************************************************/

combine_sublists_in_dnf([_],[]):-!.               % Only one sublist

combine_sublists_in_dnf([L1|[L2]],L12):- !,       % Two sublists
   combine_two_sublists_in_dnf(L1,L2,L12).

combine_sublists_in_dnf([L1|L2],NewL1L2):- !,     % More than two sublists
   combine_sublists_in_dnf(L2,NewL2),
   combine_two_sublists_in_dnf(L1,NewL2,NewL1L2).

combine_sublists_in_dnf([],[]).                   % No sublists

/*****************************************************************************/
/* combine_two_sublists_in_dnf(L1,L2,NewL1L2).                               */
/*                                                                           */
/* Takes two sublists L1 and L2 an combines every rule in L1 with every rule */
/* in L2, returning the combined rules in NewL1L2.                           */
/*                                                                           */
/*****************************************************************************/

combine_two_sublists_in_dnf([H1|Tail1],List2,NewList):-
   combine_element_with_all_in_dnf(H1,List2,H1List2),
   combine_two_sublists_in_dnf(Tail1,List2,Tail1List2),
   append(H1List2,Tail1List2,NewList).

combine_two_sublists_in_dnf([],_,[]).

/*****************************************************************************/
/* combine_element_with_all_in_dnf(H1,List2,H1List2)                         */
/*                                                                           */
/* Takes a rule H1 and combines it with every rule in List2 to give H1List2. */
/*                                                                           */
/*****************************************************************************/

combine_element_with_all_in_dnf(H1,[H2|Tail2],[H1H2|H1Tail2]):-
  append(H1,H2,H1H2),
  combine_element_with_all_in_dnf(H1,Tail2,H1Tail2).

combine_element_with_all_in_dnf(_,[],[]).

/**********************************************************************/

add_combined_to_rules(r(_,_,false => next _),Rules,_,_,Rules):- !.

add_combined_to_rules(r(_,_, _ => next false),Rules,_,_,Rules):- !.

add_combined_to_rules(r(_,_, true => false),Rules,_,_,Rules):- !.

add_combined_to_rules(r(N1,N2,P => next Q),Rules,_D1,PrevNode,NewRules):-
   strip_or(Q,StrippedQ),
   remove_ok_disjuncts(StrippedQ,PrevNode,NewStrippedQ),
   flatten(NewStrippedQ,FinalQ),
   add_combined_to_rule(r(N1,N2,P => next Q),FinalQ,Rules,NewRules).

add_combined_to_rules(r(N1,N2,Q),Rules,_D1,PrevNode,NewRules):-
   disjunction_of_literals(Q),
   strip_or(Q,StrippedQ),
   remove_ok_disjuncts(StrippedQ,PrevNode,NewStrippedQ),
   flatten(NewStrippedQ,FinalQ),
   add_combined_to_rule(r(N1,N2,Q),FinalQ,Rules,NewRules).

add_combined_to_rule(r(N1,N2,P => next Q),StrippedQ,[[Lit,LitRules]|Rest],[[Lit,NewLitRules]|NewRest]):-
   member(Lit,StrippedQ),!,
   add_combined_rule(r(N1,N2,P => next Q),LitRules,NewLitRules),
   add_combined_to_rule(r(N1,N2,P => next Q),StrippedQ,Rest,NewRest).

add_combined_to_rule(r(N1,N2,Q),StrippedQ,[[Lit,LitRules]|Rest],[[Lit,NewLitRules]|NewRest]):-
   disjunction_of_literals(Q),
   member(Lit,StrippedQ),!,
   add_combined_rule(r(N1,N2,Q),LitRules,NewLitRules),
   add_combined_to_rule(r(N1,N2,Q),StrippedQ,Rest,NewRest).

add_combined_to_rule(r(N1,N2,P => next Q),StrippedQ,[[Lit,LitRules]|Rest],[[Lit,LitRules]|NewRest]):- !,
   add_combined_to_rule(r(N1,N2,P => next Q),StrippedQ,Rest,NewRest).

add_combined_to_rule(r(N1,N2,Q),StrippedQ,[[Lit,LitRules]|Rest],[[Lit,LitRules]|NewRest]):- 
   disjunction_of_literals(Q),
   add_combined_to_rule(r(N1,N2,Q),StrippedQ,Rest,NewRest).

add_combined_to_rule(r(_,_, _ => next _),_,[],[]).

add_combined_to_rule(r(_,_,_),_,[],[]).

/***********************************************************************************/

add_combined_rule(r(_,_,P1 => next Q1),[r(M1,M2,P2 => next Q2)|LitRest],[r(M1,M2,P2 => next Q2)|LitRest]):-  
 strip_or(Q1,StrippedQ1),
 strip_or(Q2,StrippedQ2),
 same_node(StrippedQ1,StrippedQ2),
 strip(P1,StrippedP1),
 strip(P2,StrippedP2),
 set_implies(StrippedP1,StrippedP2),!.

add_combined_rule(r(_,_,Q1),[r(M1,M2,P2 => next Q2)|LitRest],[r(M1,M2,P2 => next Q2)|LitRest]):-
 disjunction_of_literals(Q1),  
 strip_or(Q1,StrippedQ1),
 strip_or(Q2,StrippedQ2),
 same_node(StrippedQ1,StrippedQ2),
 strip(P2,StrippedP2),
 set_implies([true],StrippedP2),!.

add_combined_rule(r(_,_,_ => next Q1),[r(M1,M2,Q2)|LitRest],[r(M1,M2,Q2)|LitRest]):-  
 disjunction_of_literals(Q2),
 strip_or(Q1,StrippedQ1),
 strip_or(Q2,StrippedQ2),
 same_node(StrippedQ1,StrippedQ2),!.

add_combined_rule(r(_,_,Q1),[r(M1,M2,Q2)|LitRest],[r(M1,M2,Q2)|LitRest]):-
 disjunction_of_literals(Q1),
 disjunction_of_literals(Q2),  
 strip_or(Q1,StrippedQ1),
 strip_or(Q2,StrippedQ2),
 same_node(StrippedQ1,StrippedQ2),!.

add_combined_rule(r(N1,N2,P1 => next Q1),[r(_,_,P2 => next Q2)|LitRest],NewLitRest):-  
 strip_or(Q1,StrippedQ1),
 strip_or(Q2,StrippedQ2),
 same_node(StrippedQ1,StrippedQ2),
 strip(P1,StrippedP1),
 strip(P2,StrippedP2),
 set_implies(StrippedP2,StrippedP1),!,
 add_combined_rule(r(N1,N2,P1 => next Q1),LitRest,NewLitRest).  

add_combined_rule(r(N1,N2,P1 => next Q1),[r(_,_,Q2)|LitRest], NewLitRest):-  
 disjunction_of_literals(Q2),
 strip_or(Q1,StrippedQ1),
 strip_or(Q2,StrippedQ2),
 same_node(StrippedQ1,StrippedQ2),
 strip(P1,StrippedP1),
 set_implies([true],StrippedP1),!,
 add_combined_rule(r(N1,N2,P1 => next Q1),LitRest,NewLitRest).  

add_combined_rule(r(N1,N2,Q1),[r(_,_,_ => next Q2)|LitRest], NewLitRest):-
 disjunction_of_literals(Q1),  
 strip_or(Q1,StrippedQ1),
 strip_or(Q2,StrippedQ2),
 same_node(StrippedQ1,StrippedQ2),
 add_combined_rule(r(N1,N2,Q1),LitRest,NewLitRest).  
  
add_combined_rule(r(N1,N2,P1 => next Q1),[r(M1,M2,P2 => next Q2)|LitRest],[r(M1,M2,P2 => next Q2)|NewLitRest]):-  
 add_combined_rule(r(N1,N2,P1 => next Q1),LitRest,NewLitRest).  

add_combined_rule(r(N1,N2,P1 => next Q1),[r(M1,M2,Q2)|LitRest],[r(M1,M2, Q2)|NewLitRest]):-
 disjunction_of_literals(Q2),  
 add_combined_rule(r(N1,N2,P1 => next Q1),LitRest,NewLitRest).  

add_combined_rule(r(N1,N2,Q1),[r(M1,M2,P2 => next Q2)|LitRest],[r(M1,M2, P2 => next Q2)|NewLitRest]):-  
 disjunction_of_literals(Q1),
 add_combined_rule(r(N1,N2,Q1),LitRest,NewLitRest).  

add_combined_rule(r(N1,N2,Q1),[r(M1,M2,Q2)|LitRest],[r(M1,M2,Q2)|NewLitRest]):- 
 disjunction_of_literals(Q1),
 disjunction_of_literals(Q2), 
 add_combined_rule(r(N1,N2,true => next Q1),LitRest,NewLitRest).  

add_combined_rule(r(N1,N2,P1 => next Q1),[],[r(N1,N2,P1 => next Q1)]).
                              
add_combined_rule(r(N1,N2,Q1),[],[r(N1,N2,Q1)]):-
 disjunction_of_literals(Q1).

/*********************************************************************************************/

remove_ok_disjuncts([H|Rest],Node,NewRest):-
  set_implies(H,Node),!,
  remove_ok_disjuncts(Rest,Node,NewRest).

remove_ok_disjuncts([H|Rest],Node,[H|NewRest]):-
  remove_ok_disjuncts(Rest,Node,NewRest).

remove_ok_disjuncts([],_,[]).


/* Predicates for collecting statistics */

collect_statistics((CPUTime,Inferences,LocalUsed,GlobalUsed,TrailUsed)):-
  statistics(cputime,CPUTime),
  statistics(inferences,Inferences),
  statistics(localused,LocalUsed),
  statistics(globalused,GlobalUsed),
  statistics(trailused,TrailUsed).

calculate_statistics((CPUTime1,Inferences1,LocalUsed1,GlobalUsed1,TrailUsed1),
		     (CPUTime2,Inferences2,LocalUsed2,GlobalUsed2,TrailUsed2)):-
  write('\n\nStatistics\n\n'),
  CPUTime is CPUTime2 - CPUTime1,
  writef('CPUTime:%w\n',[CPUTime]),
  Inferences is Inferences2 - Inferences1,
  writef('Inferences:%w\n',[Inferences]),
  LocalUsed is LocalUsed2 - LocalUsed1,
  writef('Local Stack:%t\n',[LocalUsed]),
  GlobalUsed is GlobalUsed2 - GlobalUsed1,
  writef('Global Stack:%t\n',[GlobalUsed]),
  TrailUsed is TrailUsed2 - TrailUsed1,
  writef('Trail Stack:%t\n',[TrailUsed]).

/* Predicate that invokes the other predicates that will perform resolution */
     
otres(Rules):-
  clear,
  collect_statistics(X),
  otres_allres(Rules,_),
  collect_statistics(Y),
  calculate_statistics(X,Y),
  newrulecount(N),
  Counter is N - 1,
  writef('\nNumber of clauses generated: %t\n',[Counter]).

/************************************************************/
/* First call to the predicates that perform resolution.    */
/* It takes a formula, transform it into the snf, set the   */
/* initial literal to be unique, number and separate rules, */
/* generate the SNL clauses and call modal resolution.      */
/************************************************************/

otres_allres(Rules,AllNewRules):-
   startrulecount(_),
   snff(Rules,SNFRules),
   check_initial(SNFRules,NewSNFRules),
   number_rules(NewSNFRules,[],NumberedSNF),
   separate_and_format_rules(NumberedSNF,ERules,ERulePaths,OtherRules),
   generate_snl(OtherRules,NOtherRules),
   simplify_lists(NOtherRules,(Initial,Literal,Modal,Temporal)),
   my_write('\nSet of clauses after translation to SNF :\n'),
   write_ruleset((Initial,Literal,Modal,Temporal)),
   mres(Literal,Modal,Initial,NewerLiteral,NewerModal),
   write('\notres_allres (1) ==> Usable rules are \n'),
   write_otter_rules(NewerLiteral),nl,
   write('\notres_allres (1) ==> Rules in the SoS are \n'),
   write_otter_rules(Temporal),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewerLiteral,NewerModal,Temporal),ERulePaths,ERules,AllNewRules).

/* predicate called after temporal resolution takes place */

otres_allres(OldRules,NewRules,ERulePaths,ERules,AllNewRules):-
   number_rules(NewRules,[],NumberedRules),
   simplify(NumberedRules,SimpleRules),
   split_temp_lit_rules(SimpleRules,NewLiteral,NewTemporal),
   my_write('otres_allres (2) ==> New literal rules are \n'),write_ruleset(NewLiteral),
   my_write('otres_allres (2) ==> New temporal rules are \n'),write_ruleset(NewTemporal),
   step_resolution(NewLiteral,NewTemporal,OldRules,ERules,ERulePaths,AllNewRules).

/************************************************************************************/
/* the next predicates are  renaming possible disjunctions on the rhs of initial    */
/* clauses. This is because in this program, the initial resolution is NOT          */
/* implemented. Instead, you have a simple check: if ~ X (in the literal clauses) */
/* is generated, checks if X is in the initial set, generating false in the literal */
/* set of clauses.                                                                  */
/************************************************************************************/

check_initial(Initial,Final):-
   new_temp_prop(V),
   change_initial(Initial,V,NewList),
   append([start => V],NewList,Final).

change_initial([],_,[]):-!.

change_initial([start => X|Rest],V,[~ V or X|NewRest]):-
   change_initial(Rest,V,NewRest).

change_initial([H|Rest],V,[H|NewRest]):-
   change_initial(Rest,V,NewRest).

/********************************************************************************/
/* The following generates the SNL clauses and definitions from a given set of  */
/* step clauses.                                                                */
/********************************************************************************/

generate_snl((Initial,Literal,Modal,Temporal),(Initial,NewLiteral,NewModal,NewTemporal)):-
   my_write('\nGenerating SNL Clauses\n'),
   my_write('\nTemporal Clauses are \n'),
   write_ruleset(Temporal),
   generate_snl(Temporal,Literal,Modal,NewTemporal,NewLiteral,NewModal),
   my_write('\nNew Temporal Clauses are \n'),
   write_ruleset(NewTemporal).

generate_snl([],Literal,Modal,[],Literal,Modal).

generate_snl([r(N1,M,X => next Y)|List],OldLiteral,OldModal,
 [r(N1,M,X => next Y),r(N2,[N1],NewX => next NewY)|NewList],NewerLiteral,NewerModal):-
   newrulecount(N2),
   snl(NewX,X,[N1],NewLiterals1,NewModals1),
   snl(NewY,Y,[N1],NewLiterals2,NewModals2),
   append(NewLiterals1,NewLiterals2,NewLiteral),
   append(NewLiteral,OldLiteral,Literal),
   append(NewModals1,NewModals2,NewModal),
   append(NewModal,OldModal,Modal),
   generate_snl(List,Literal,Modal,NewList,NewerLiteral,NewerModal).

generate_snl([X|List],OldLiteral,OldModal,[X|NewList],NewerLiteral,NewerModal):-
   generate_snl(List,OldLiteral,OldModal,NewList,NewerLiteral,NewerModal).

/********************************************************************************/
/* The following generates the SNL clauses and definitions from a given set of  */
/* step clauses, after resolution has been performed.                           */
/********************************************************************************/

generate_otter_snl(OldRules,NewOldRules,NewishLiteral,OldModal,SNewModal):-
   generate_snl(OldRules,[],OldModal,NewOldRules,NewLiteral,NewModal),
   remove_duplicate_rules(NewLiteral,NewerLiteral),
   remove_already_existing(NewerLiteral,NewOldRules,NewishLiteral),
   get_new_modal_rules(OldModal,NewModal,NewerModal),
   remove_duplicate_rules(NewerModal,SNewModal).

/* Remove already existing clauses */

remove_already_existing([],_,[]).

remove_already_existing([r(_,_,Rule)|Tail],OldRules,ReallyNew):-
   test_member(Rule,OldRules),!,
   remove_already_existing(Tail,OldRules,ReallyNew).

remove_already_existing([Head|Tail],OldRules,[Head|ReallyNew]):-   
   remove_already_existing(Tail,OldRules,ReallyNew).

/**********************************************************************************/

separate_and_format_rules(Rules,ERules,ERulePaths,(InitialRules,LiteralRules,ModalRules,TemporalRules)):-
   separate_rules(Rules,AllERules,InitialRules,LiteralRules,ModalRules,TemporalRules),
   remove_duplicate_erules(AllERules, ERules),
   my_write('\n\nThe list of eventuality rules is \n\n'),
   write_ruleset(ERules),
   process_eventualities(ERules, ERulePaths).

/*****************************************************************************/
/* separate_rules(Rules, ERules Others)                                      */
/*                                                                           */
/* This predicate takes a list of rules and splits it into two lists one     */
/* containing the separate_rules which have to be solved and the other       */
/* containg the remaining rules which are global box rules ie                */
/*           slast P => F,                                                  */
/* where P is a conjunction and F a disjunction, any other rules being       */
/* disregarded.                                                              */
/*                                                                           */
/*****************************************************************************/

separate_rules([],[],[],[],[],[]):-!.

separate_rules([r(N1,N2, F => sometimes G)|Rules],
               [r(N1,N2, F => sometimes G)|ERules],InitialRules,LiteralRules,ModalRules,TemporalRules) :- !,
    separate_rules(Rules,ERules,InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add sometimes clause to ERules


separate_rules([r(N1,N2,P => next F)|Rules],
                ERules,InitialRules, LiteralRules, ModalRules,[r(N1,N2, P => next F)|TemporalRules]) :- !,
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add step clauses to TemporalRules

separate_rules([r(N1,N2,start => F)|Rules],
               ERules,[r(N1,N2,F)|InitialRules],LiteralRules, ModalRules,TemporalRules) :- !,
   separate_rules(Rules,ERules,InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add initial clauses to InitialRules

separate_rules([r(N1,N2,X => k A)|Rules],
               ERules,InitialRules,LiteralRules, [r(N1,N2, X => k A)|ModalRules],TemporalRules) :-!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules, TemporalRules).
% Add modal clause to ModalRules

separate_rules([r(N1,N2,X => ~ k A)|Rules],
               ERules,InitialRules, LiteralRules, [r(N1,N2, X => ~ k A)|ModalRules],TemporalRules) :-!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add modal clause to ModalRules

separate_rules([r(N1,N2,F)|Rules],
               ERules,InitialRules,[r(N1,N2,F)|LiteralRules], ModalRules,TemporalRules) :-
   disjunction_of_literals(F),!,
   separate_rules(Rules,ERules, InitialRules, LiteralRules,ModalRules, TemporalRules).
% Add literal clause to LiteralRules

separate_rules([_|Rules],ERules,InitialRules, LiteralRules, ModalRules,TemporalRules) :- 
   separate_rules(Rules,ERules, InitialRules, LiteralRules, ModalRules,TemporalRules).
% Ignore other rules.

/*****************************************************************************/
/* remove_duplicate_erules(OldList, NewList).                                */
/*                                                                           */
/* Takes old list and removes duplicates to give NewList.                    */
/* N.B. Only works with  with initial and global eventuality rules and NOT   */
/* initial and global box rules.                                             */
/*                                                                           */
/* Two predicates have been maintained as the one above is called frequently */
/* during nontemporal resolution where as this one is only called            */
/* occasionally.                                                             */
/*****************************************************************************/

remove_duplicate_rules([],[]).

remove_duplicate_rules([r(_,_,Rule)|Rest], NewRules):-
   test_member(Rule, Rest),!,
   remove_duplicate_rules(Rest, NewRules).

remove_duplicate_rules([Rule|Rest], [Rule|NewRules]):-
   remove_duplicate_rules(Rest, NewRules).

/*******************************************************/

remove_duplicate_erules([],[]).

remove_duplicate_erules([Rule|Rest], NewRules):-
   erule_in_list(Rule, Rest),!,
   remove_duplicate_erules(Rest, NewRules).

remove_duplicate_erules([Rule|Rest], [Rule|NewRules]):-
   remove_duplicate_erules(Rest, NewRules).

/*****************************************************************************/
/* erule_in_list(Rule, List)                                                 */
/*                                                                           */
/* Takes a rule which is either an initial or global eventuality rule and    */
/* tests whether it is a member of List.                                     */
/*****************************************************************************/

erule_in_list(r(_,_, P => sometimes F), [r(_,_,Q => sometimes F)|_]):-
   strip(P, StrippedP),
   strip(Q, StrippedQ),
   same(StrippedP, StrippedQ).

erule_in_list(r(N1,N2, P => sometimes F), [_|Rest]):-
   erule_in_list(r(N1,N2, P => sometimes F), Rest).

/*********************************************************************************************/
/* process_eventualities(EList, ShortEList)                                                  */
/*                                                                                           */
/* Takes a list of eventualities EList removes the eventaulity with no duplicates and stores */
/* in a structured list as below.                                                            */
/*********************************************************************************************/

process_eventualities(EList, ShortEList):-
   extract_eventualities(EList, NewEList),
   remove_duplicate(NewEList, NewerEList),
   eventualities_paths(NewerEList,ShortEList).

/*********************************************************************************************/
/* extract_eventualites(EList, ShortEList)                                                   */
/*                                                                                           */
/* Takes a list of eventualities EList removes the eventuality with no duplicates            */
/*********************************************************************************************/

extract_eventualities([],[]).

extract_eventualities([r(_,_, _ => sometimes F)|Rest], [F|NewRest]):-
    extract_eventualities(Rest, NewRest).

/********************************************************************************************/
/*   eventualities_paths(EList,NewEList).                                                    */
/*                                                                                           */
/* Takes a list of eventuality rules (EList), and makes it into a list whose members are     */
/* sublists of the form [ERule, []]...etc where the [] is for filling with the previous paths*/
/* found for that eventuality.                                                               */
/*                                                                                           */
/*********************************************************************************************/

eventualities_paths([],[]).

eventualities_paths([H|Tail],[[H,[]]|NewTail]):-
  eventualities_paths(Tail,NewTail).

/*****************************************************************************************/

simplify_lists((Initial,Literal,Modal,Temporal),(SInitial,SLiteral,SModal,STemporal)):-
    simplify(Initial,SInitial),
    simplify(Literal,SLiteral),
    simplify(Modal,SModal),
    simplify(Temporal,STemporal).

/*****************************************************************************************/

step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,Modal,Temporal),ERules,ERulePaths,AllNewRules):-
   mres(Literal,Modal,NewerLiteral,[],Initial,NewLiteral,NewModal),
   do_step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,Modal,Temporal),ERules,ERulePaths,AllNewRules,NewLiteral,NewModal).

do_step_resolution(_,_,_,_,_,_,[r(0,[],false)],_):-
   otres_test_new_rules(_,r(0,[],false),_,_,_,_).

do_step_resolution(NewerLiteral,NewerTemporal,(Initial,Literal,_,Temporal),ERules,ERulePaths,AllNewRules,NewLiteral,NewModal):-
%   write(Literal),nl,write(Modal),nl,write(NewerLiteral),nl,write(NewLiteral),nl,
   my_write('step_resolution ==> New temporal rules are\n'),write_ruleset(NewerTemporal),
   get_new_literal_rules(Literal,NewLiteral,ExtraLiteral),
   my_write('step_resolution ==> New literal rules are\n'),write_ruleset(ExtraLiteral),
   write('\nstep_resolution ==> Usable rules are \n'),
   write_otter_rules(Literal,Temporal),nl,
   write('\nstep_resolution ==>Rules in the SoS are \n'),
   write_otter_rules(NewerTemporal,ExtraLiteral),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewerLiteral,NewModal,Temporal),ERulePaths,ERules,AllNewRules).

/**********************************************************************/

otres_test_otter_output([true => next false],(_,_,_,_),_,_,NewAllRules):-
   write('\nHave generated false (1).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output([false],(_,_,_,_),_,_,NewAllRules):-
   write('\nHave generated false (2).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output(OldOtterRules,([r(_,_,X)],Literal,_,Temporal),_,_,NewAllRules):-
   test_member(~ X,OldOtterRules),
   rewrite_rules(OldOtterRules,NewOldRules,NewOtterRules),
   give_rules_old_numbers(NewOldRules,Literal,Temporal,NewishOldRules),
   sort(NewishOldRules,SortedRules),
   my_write('\nOutput from otter ==> New literal rules are \n'),write_ruleset(NewOtterRules),
   my_write('\nOutput from otter ==> Remaining rules are\n'),write_ruleset(SortedRules),
   write('\nHave generated false (3).\n'),
   otres_test_new_rules(_,r(0,[],false),_,_,_,NewAllRules).

otres_test_otter_output(OldOtterRules,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   rewrite_rules(OldOtterRules,NewOldRules,NewOtterRules),
   give_rules_old_numbers(NewOldRules,Literal,Temporal,NewishOldRules),
%   my_write('\nOldOtterRules - All Rules coming from otter\n'),write_ruleset(OldOtterRules),
%   my_write('\n\nNewOtterRules - Simplified Rules (p => next false\n'),write_ruleset(NewOtterRules),
%   my_write('\n\nNewOldRules - Remaining rules\n'),write_ruleset(NewishOldRules),
    generate_otter_snl(NewishOldRules,SNLNewOldRules,NewLiteral,Modal,NewModal),
    append(NewOtterRules,NewLiteral,NewerOtterRules),
    my_write('\nOutput from otter ==> New literal rules (A => next false) are \n'),write_ruleset(NewerOtterRules),
    my_write('\nOutput from otter ==> Remaining rules (after snl) are\n'),write_ruleset(SNLNewOldRules),
    my_write('\nOutput from snl   ==> New modal rules are\n'),write_ruleset(NewModal),
    run_tempres_or_otter(SNLNewOldRules,NewerOtterRules,NewModal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

/**********************************************************************/

run_tempres_or_otter(NonTempRules,[],NewModal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (1)\n'),
   split_temp_lit_rules(NonTempRules,NewLiteral,NewTemporal),
%   my_write('NonTempRules - All rules coming from otter\n'),write_ruleset(NonTempRules),
%   my_write('NewLiteral - Literal rules coming from otter\n'),write_ruleset(NewLiteral),
%   my_write('NewTemporal - Temporal rules coming from otter\n'),write_ruleset(NewTemporal),
   get_new_literal_rules_after_temporal(Literal,NewLiteral,SmallNewLiteral),
   my_write('Literal - Old Literal clauses\n'),write_ruleset(Literal),
   my_write('NewLiteral - All Literal clauses coming from otter\n'),write_ruleset(NewLiteral),
   my_write('SmallNewLiteral - the really new clauses generated by otter\n'),write_ruleset(SmallNewLiteral),
   run_tempres_or_modal(SmallNewLiteral,NewLiteral,NewModal,NonTempRules,NewTemporal,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

run_tempres_or_otter(NonTempRules,[],_,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (2)\n'),
   write_ruleset(NonTempRules),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   full_tempres(NonTempRules,ERulePaths, ERules, MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(NonTempRules,MoreRules,(Initial,Literal,Modal,Temporal),NewERulePaths,ERules,AllNewRules).

run_tempres_or_otter(UsableRules,SoSRules,_,(Initial,Literal,Modal,Temporal),ERulePaths,ERules,AllNewRules):-
   my_write('\nrun_tempres_or_otter ==> (3)\n'),
   write('\nrun_tempres_or_otter ==> Usable rules are \n'),
   write_otter_rules(UsableRules),nl,
   write('\nrun_tempres_or_otter ==> Rules in the SoS are \n'),
   write_otter_rules(SoSRules),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

run_tempres_or_modal([],NewLiteral,_,AllTemporal,NewTemporal,(Initial,_Literal,Modal,_Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_modal ==> (1)\n'),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   full_tempres(AllTemporal,ERulePaths, ERules, MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(AllTemporal,MoreRules,(Initial,NewLiteral,Modal,NewTemporal),NewERulePaths, ERules,AllNewRules).


run_tempres_or_modal(ExtraLiteral,NewLiteral,NewModal,_,NewTemporal,(Initial,Literal,Modal,_Temporal),
                                   ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_modal ==> (2)\n'),
   mres(Literal,Modal,ExtraLiteral,NewModal,Initial,FinalLiteral,FinalModal),
   get_new_literal_rules(Literal,NewLiteral,ExtraLiteral1),
%   my_write('\nLiteral'),
%   write_ruleset(Literal),
%   my_write('\nNewLiteral'),
%   write_ruleset(NewLiteral),
%   my_write('\nExtraLiteral1'),
%   write_ruleset(ExtraLiteral1),
   run_tempres_or_temporal(ExtraLiteral1,FinalLiteral,(Initial,Literal,FinalModal,NewTemporal),ERulePaths, ERules,AllNewRules).

/**********************************************************************/

run_tempres_or_temporal(_,[r(_,_,false)],_,_,_,_):-
   otres_test_new_rules(_,r(_,_,false),_,_,_,_).

run_tempres_or_temporal([],NewLiteral,(Initial,_Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_temporal ==> (1)\n'),
   my_write('Attempting Breadth-First Temporal Resolution \n'),
   append(Temporal,NewLiteral,AllTemporal),
   full_tempres(AllTemporal,ERulePaths, ERules,MoreRules,NewERulePaths,_RulesUsed),
   otres_test_new_rules(AllTemporal,MoreRules,(Initial,NewLiteral,Modal,Temporal),NewERulePaths, ERules,AllNewRules).

run_tempres_or_temporal(ExtraLiteral,NewLiteral,(Initial,Literal,Modal,Temporal),ERulePaths, ERules,AllNewRules):-
   my_write('\nrun_tempres_or_temporal ==> (2)\n'),
%   k_to_literal(Modal,Kliteral),
   write('\nrun_tempres_or_temporal ==> Usable rules are \n'),
   write_otter_rules(Literal,Temporal),nl,
   write('\nrun_tempres_or_temporal ==> Rules in the SoS are \n'),
   write_otter_rules(ExtraLiteral),nl,
   read(OtterRules),
   otres_test_otter_output(OtterRules,(Initial,NewLiteral,Modal,Temporal),ERulePaths, ERules,AllNewRules).

/*************************************************************************************/

otres_test_new_rules(_,r(N1,N2,false),_,_,_,r(N1,N2,false)):-
   write('Formula is unsatisfiable (1).\n').

otres_test_new_rules(_,[],_,_,_,[]):-
   write('Formula is satisfiable (2).\n').    % Note this really is satisfiable        
                     
otres_test_new_rules(OldRules,NewRules,(Initial,_,Modal,_),ERulePaths, ERules,NewAllRules):-
   split_temp_lit_rules(OldRules,Literal,Temporal),
   otres_allres((Initial,Literal,Modal,Temporal),NewRules,ERulePaths,ERules,NewAllRules).

/**********************************************************************/
/* rewrite_rules(OtterRules,OrdinaryRules,ImpFalseRules)              */
/*                                                                    */
/* Goes through the list of rules output by Otter and rewrites        */
/* rules of the form slast A => false to their correct form and adds */
/* them to ImpFalseRules and after numbering copies the rest to       */
/* OrdinaryRules.                                                     */
/*                                                                    */
/**********************************************************************/

rewrite_rules([],[],[]).

rewrite_rules([false|_],[false],[]):-!.

rewrite_rules([P => next false|Rules],NewOldRules,[NewRule1|NewRules]):- !,
  snff_negate(P, NegP),
  number_rule(NegP,[],NewRule1),
  rewrite_rules(Rules,NewOldRules,NewRules).

rewrite_rules([Rule|Rest],NewOldRules,NewRest):-
  number_rule(Rule,[],NRule),
  simplify_rule(NRule, SRule),
  test_add_rule(SRule,Rest,NewOldRules,NewRest).

/********************************************************************************************/

test_add_rule([],Rest,NewOldRules,NewRest):- !,
  rewrite_rules(Rest,NewOldRules,NewRest).
  
test_add_rule([Rule],Rest,[Rule|NewOldRules],NewRest):- 
  rewrite_rules(Rest,NewOldRules,NewRest).
  
/*********************************************************************************************/
/* full_tempres(Rules, ERulePaths, ERules, NewRules,NewERulePaths)                           */
/*                                                                                           */
/* Takes the current ruleset (Rules), a list of the eventualities with previous loop paths   */
/* found (ERulePaths), a list of the sometimes rules (ERules) and attempts temporal          */
/* between one of the eventualities and Rules. The path detected (LoopPath) is stored with   */
/* the appropriate eventuality, NewERule in ERulePaths, new resolvents from this loop path   */
/* and the eventuality are generated and stored in NewRules.                                 */
/*********************************************************************************************/

full_tempres(r(N1,N2,false), _, _, r(N1,N2,false), _,_).

full_tempres(_,[],ERules,NewRules,[],_):-
   write('\nfull_tempres (1) ==> No previous loop paths.\n'),
   process_path([],_,_,[],_,ERules,NewRules, _).

full_tempres(Rules,ERulePaths,ERules,NewRules,NewERulePaths,RulesUsed):-
   write('\nfull_tempres (2) ==> Previous loop paths.\n'),
   tempres(Rules,ERulePaths,NewERule,RelatedRules,RulesUsed,LoopPath),
   process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,NewERule,ERules,NewRules,NewERulePaths).

/*********************************************************************************************/
/*  process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,ERule,ERules, NewRules,          */
/*                                                           RotatedERulePaths).             */
/*                                                                                           */
/* Takes a loop found through a set of rules (LoopPath), a list of the rules which imply the */
/* current eventuality (RelatedRules), a list of the rule numbers which have been use in the */
/* loop we have found (RulesUsed), the list which stores each                                */
/* eventuality and the list of loops already found (ERulePaths), an eventuality (ERule) which*/
/* relates to this loop and a list of the sometimes rules (ERules). The new loop for this    */
/* ERule is addeded to ERulePaths. The ERulePaths list is rotated so the head is the         */
/* eventuality which followed ERule ie the new loop and ERule will now appear at the end of  */
/* the list. This is done in an attempt to find "easy" rules first. If the ERulePaths list   */
/* is cycled round then then one loop is found for each eventuality rule (if one exists) and */
/* so on rather than trying to find all the loops for each eventuality in turn. The new      */
/* resolvents are generated from this loop path to give NewRules and the updated and rotated */
/* ERulePathsare returned in RotatedERulePaths.                                              */
/*********************************************************************************************/

process_path([],_,_,_,_ERule,_, [], _):-
   my_write('No more loop paths.\n').

process_path(LoopPath,RelatedRules,RulesUsed,ERulePaths,ERule,ERules, NewRules, RotatedERulePaths):-
   write('The eventuality (for possibly several rules) being considered is '),
   write_form(ERule),nl,
   write('New Loop detected is '),
   write(LoopPath),nl,
   write('Loops previously detected are \n'), write('    [\n'),
   write_list(ERulePaths),
   write('    ]\n'),
   add_new_path_to_found_previously(ERule,LoopPath,ERulePaths,NodePath,NewERulePaths),
   rotate_rule(ERule,NewERulePaths,RotatedERulePaths),
   new_rules(ERule,LoopPath,ERules,RulesUsed,RelatedRules,NodePath,NewRules),
   write_ruleset(NewRules).

/*********************************************************************************************/
/*  rotate_rule(ERule,ERulePaths,NewList)                                                    */
/*                                                                                           */
/* Take an eventuality ERule, and a list of eventualities paired with previous loop paths    */
/* found (ERulePaths). It searches for ERule in ERulePaths and makes the eventuality         */
/* following ERule in the list the head and the portion of the list from the front to where  */
/* ERule was located to the end of the new list (NewList). This was an attempt to find all   */
/* "easy" loops first. Rotating the eventuality rules in this manner means that the temporal */
/* resolution process will try to find a loop for each eventuality (if one exists) and then  */
/* a second, and third etc.                                                                  */
/*********************************************************************************************/

rotate_rule(ERule,ERulePaths,NewList):-
     split_list(ERule,ERulePaths,FirstBit, LastBit),
     append(LastBit,FirstBit,NewList).

/*********************************************************************************************/
/*   split_list(ERule,ERulePaths,FirstBit, LastBit)                                          */
/*                                                                                           */
/* Takes an eventuality ERule, and a list of eventualities paired with loop paths previously */
/* found (ERulePaths). It searches down ERulePaths looking for an ERule in the list. It      */
/* stores all the things before this and including this as FirstBit, and all the remaining   */
/* stuff as LastBit. The aim is the rotate the eventualities and lists in ERulePaths to try  */
/* attempt to find a loop for each eventuality in turn.                                      */
/*********************************************************************************************/

split_list(ERule,[[ERule,PrevPath]|Rest],[[ERule,PrevPath]], Rest).

split_list(ERule,[[AnERule,PrevPath]|Rest],[[AnERule,PrevPath]|FirstBit], LastBit):-
   split_list(ERule,Rest,FirstBit,LastBit).

/*********************************************************************************************/
/*  new_rules(ERule, CycleRules, ERules, RulesUsed,RelatedRules,NodePath, NewRules)          */
/*                                                                                           */
/*  Takes the eventuality (ERule) we have detected a loop for, and a list of rules           */
/*  (CycleRules) which have been generated from the the loop detected, and the complete      */
/*  list of eventuality rules (ERules) and returns a list of new resolvents (NewRules).      */
/*                                                                                           */
/*********************************************************************************************/

new_rules(ERule, CycleRules, ERules, RuleNoUsed_A,RelatedRules,NodePath,NewRules):-
  get_rel_rules(ERule, ERules, RelRules),
  generate_new_rules(RelRules, CycleRules, RuleNoUsed_A,RelatedRules,NodePath,NewRules).

/*********************************************************************************************/
/* add_new_path_to_found_previously(ERule,NewPath,ERulePaths,NodePath,NewERulePaths)         */
/*                                                                                           */
/* The new loop path NewPath found for the current eventuality ERule is added                */
/* to the list ERulePaths which contains a list of eventualities and the                     */
/* previously discovered.                                                                    */
/*                                                                                           */
/*********************************************************************************************/

add_new_path_to_found_previously(ERule,NewPath,[[ERule, OldPaths]|Rest],_NodePath,[[ERule,[NewPath|OldPaths]]|Rest]).

add_new_path_to_found_previously(ERule,NewPath,[ERulePath|Rest],NodePath,[ERulePath|NewRest]):-
   add_new_path_to_found_previously(ERule,NewPath,Rest,NodePath,NewRest).

/*********************************************************************************************/
/* get_rel_rules(Eventuality, ERules, RelRules)                                              */
/*                                                                                           */
/* Takes the eventuality (Eventuality) for which a loop has been found, and the list of      */
/* rules with eventualities (ERules) and picks out the rules related to the current          */
/* eventuality.                                                                              */
/*********************************************************************************************/

get_rel_rules(_, [], []).

get_rel_rules(F, [r(N1,N2,P => sometimes F)|Rest], [r(N1,N2,P => sometimes F)|NewRest]):-
    get_rel_rules(F, Rest, NewRest).

get_rel_rules(F, [_|Rest], NewRest):-
    get_rel_rules(F, Rest, NewRest).

/*********************************************************************************************/
/* generate_new_rules(RelRules, CycleRules, RuleNoUsed,RelatedRules,NodePath,NewRules)       */
/*                                                                                           */
/* Takes a list of eventuality rules (RelRules) which are related to the eventuality in      */
/* question, and a list of rules (CycleRules) which have generated from the loop detected    */
/* from the eventuality in question, a list of the numbers of the rules used in the loop     */
/* detected (RuleNoUsed), a list of rules of the form slast R => p, where "p" is the        */
/* eventuality being currently considered (RelatedRules), a list of the nodes used in the    */
/* loop ( and unstructured list) (NodePath) and produces a list of new resolvents.           */
/*********************************************************************************************/

generate_new_rules([],_,_,_,_,[]).

generate_new_rules([RelRule|Rest],CycleRules, _RuleNoUsed,RelatedRules,NodePath,AllNewRules):-
   generate_new_rule(RelRule, CycleRules, AllRulesUsed_ANON ,SomeNewRules),
   generate_new_rules(Rest, CycleRules, AllRulesUsed_ANON ,RelatedRules, NodePath,OtherNewRules),
   append(SomeNewRules, OtherNewRules, AllNewRules).

/*********************************************************************************************/
/*   generate_new_rule(RelRule, CycleRules, RuleNoUsed, SomeNewRules)                        */
/*                                                                                           */
/* Takes an eventuality rule (RelRule), a set of rules we have found a loop in               */
/* (CycleRules), and a list of the rule numbers used in the loop detected (RuleNoUsed) and   */
/* generates the new resolvents, (SomeNewRules).                                             */
/*********************************************************************************************/

generate_new_rule(r(N1,_, A => sometimes F), [[true]],_RuleNoUsed,[r(0,[N1],NegA or F)]):-
      snff_negate(A,NegA).

generate_new_rule(r(N1,_,true => sometimes F), [[NegF]],_RuleNoUsed,[r(0,[N1],F)]):-
      disjunction_of_literals(F),
      snff_negate(F, NegF).

generate_new_rule(r(_N1,_,P => sometimes F),DisjunctList,_RuleNoUsed,NewRules):-
      neg_cycle_list(DisjunctList,NewList),
      construct_new_rules(P,NewList,F,NewRules).

/***************************************************************************************/

construct_new_rules(P, List, Eventuality,[V => next (V or Eventuality), NegP or V or Eventuality |Rest]):-
      snff_negate(P, NegP),
      new_temp_prop(V),
      build_rules(NegP,Eventuality,V,List,Rest). 

/********************************************************************************************/

build_rules(NegP,Eventuality,V,[H|Tail],[NegP or Eventuality or H, V => next (H or Eventuality)|Rest]):-
      build_rules(NegP,Eventuality,V,Tail,Rest).

build_rules(_NegP,_Eventuality,_V,[],[]).

/*********************************************************************************************/
/*       neg_cycle_list(List,NewList).                                                       */
/*                                                                                           */
/* Takes a list (List) which is made up of sublists where the sublists are conjuncts and the */
/* main list is disjunctive. This predicate negates each item in the sublists and disjoins   */
/* them. The sublists are then conjoined together and returned in a NewList.                 */
/*********************************************************************************************/

neg_cycle_list([[P]],[NegP]):-
     snff_negate(P,NegP).

neg_cycle_list([List],[NegP]):-
     snff_negate_list(List,NegP).

neg_cycle_list([List|Lists], [NegP| Rest]):-
     snff_negate_list(List,NegP),
     neg_cycle_list(Lists,Rest).

/**************************************************************************************/

snff_negate_list([H],NegH):-
   snff_negate(H,NegH).

snff_negate_list([H|Rest],NegH or NewRest):-
   snff_negate(H,NegH),
   snff_negate_list(Rest,NewRest).

/**************************************************************************************/

split_temp_lit_rules([r(N,M,A => next B)|Rest],Literal,[r(N,M,A => next B)|Temporal]):-
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([r(N,M,B)|Rest],[r(N,M,B)|Literal],Temporal):-
   disjunction_of_literals(B),
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([A => next B|Rest],Literal,[A => next B|Temporal]):-
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([B|Rest],[B|Literal],Temporal):-
   disjunction_of_literals(B),
   split_temp_lit_rules(Rest,Literal,Temporal).

split_temp_lit_rules([],[],[]).

/*******************************************************************************/

get_new_modal_rules(Modal,[r(N,_,_)|NewModal],Other):-
   rule_member(N,Modal),!,
   get_new_modal_rules(Modal,NewModal,Other).

get_new_modal_rules(Modal,[r(_,_,Rule)|NewModal],Other):-
   test_member(Rule,Modal),
   get_new_modal_rules(Modal,NewModal,Other).
   
get_new_modal_rules(Modal,[r(N,M,Rule)|NewModal],[r(N,M,Rule)|Other]):-
   get_new_modal_rules(Modal,NewModal,Other).

get_new_modal_rules(_,[],[]).

/******************************************************************************/

get_new_literal_rules(Literal,[r(N,_,_)|NewLiteral],Other):-
   rule_member(N,Literal),!,
   get_new_literal_rules(Literal,NewLiteral,Other).

get_new_literal_rules(Literal,[r(N,M,Rule)|NewLiteral],[r(N,M,Rule)|Other]):-
   get_new_literal_rules(Literal,NewLiteral,Other).

get_new_literal_rules(_,[],[]).

/********************************************************************************/

get_new_literal_rules_after_temporal(Literal,[r(_,_,Rule)|NewLiteral],Other):-
   rule_in_list(Rule,Literal),!,
   get_new_literal_rules_after_temporal(Literal,NewLiteral,Other).

get_new_literal_rules_after_temporal(Literal,[r(N,M,Rule)|NewLiteral],[r(N,M,Rule)|Other]):-
   get_new_literal_rules_after_temporal(Literal,NewLiteral,Other).

get_new_literal_rules_after_temporal(_,[],[]).

/**************************************************/

rule_in_list(Rule,[r(_,_,Rule)|_]):- !.

rule_in_list(D,[r(_,_,E)|_]):-
    strip(D,SD),
    strip(E,SE),
    same(SD,SE),!.

rule_in_list(D,[_|Rest]):-!,
    rule_in_list(D,Rest).

/*******************************************/

rule_member(N,[r(N,_,_)|_]):- !.

rule_member(N,[_|Rest]):- 
   rule_member(N,Rest).

rule_member(_,[]):-!,fail.

/*****************************************/

test_member(X,[X|_]).

test_member(X,[r(_,_,X)|_]).

test_member(X,[_|Rest]):-
    test_member(X,Rest).

/**********************************************************/
/* Gives the clauses coming from Otter their old numbers. */
/* Previously, clauses were just renumbered, making the   */
/* reading of the proof difficult.                        */
/**********************************************************/

give_rules_old_numbers([],_,_,[]).

give_rules_old_numbers([r(N1,N2,X => next Y)|Tail],Literal,Temporal,[NRule|NewishOldRules]):-
  find_same_rule(r(N1,N2,X => next Y),Temporal,NRule),
  give_rules_old_numbers(Tail,Literal,Temporal,NewishOldRules).

give_rules_old_numbers([r(N1,N2,X)|Tail],Literal,Temporal,[NRule|NewishOldRules]):-
  find_same_rule(r(N1,N2,X),Literal,NRule),
  give_rules_old_numbers(Tail,Literal,Temporal,NewishOldRules).

find_same_rule(r(N1,N2,Clause),List,r(N1,N2,Clause)):-
   \+ (member(r(_,_,Clause),List)).

find_same_rule(r(N1,N2,Clause),List,r(N5,N6,Clause)):-
   sublist(same_rule(r(N1,N2,Clause)),List,SameList),
   sort(SameList,Sorted),
   nth1(1,Sorted,r(N3,N4,Clause)),
   test_number(N1,N2,N3,N4,N5,N6).

same_rule(r(_,_,X),r(_,_,Y)):-
   strip_or(X,SX),
   strip_or(Y,SY),
   flatten(SX,FX),
   flatten(SY,FY),
   subset(FX,FY),
   subset(FY,FX).

test_number(N1,N2,N3,_N4,N1,N2):- N1 < N3.
test_number(_N1,_N2,N3,N4,N3,N4).

% :-[definitions,output,rules,mysnff,mres,mytempres,fullres].
ex354:-otres([start => x,
              x => sometimes p,
              ~ x or ~ p,
              x => next y,
              ~ y or ~ p,
              ~ y or t,
              t => next ~ p,
              t => next t]).

ex5211:-otres([start => x,
               x => k y,
               y => sometimes p,
               x => k ~ p,
               x => ~ k ~ z,
               z => next w,
               ~ w or ~ p,
               ~ w or t,
               t => next ~ p,
               t => next t]).

ex5212:-otres([start => x,
               x => sometimes y,
               y => k p,
               x => k ~ p,
               x => next z,
               ~ z or w,
               ~ z or t,
               t => next w,
               t => next t,
               w => ~ k p]).

ex621:-otres([start => x,
              x => k y,
              y => next p,
              x => next z,
              z => ~ k p]).

ex7121:-otres([start => x,
               x => next y,
               y => k p,
               x => ~ k ~ z,
               z => next ~ p]).

ex7122:-otres([start => x,
               x => next y,
               y => k p,
               x => ~ k ~ v,
               ~ v or t,
               ~ v or u,
               t => next (w or ~ p),
               u => next (~ w or ~ p)]).

ex7123:-otres([start => x,
               ~ x or z or y,
               ~ x or z or w,
               w => next (z or y),
               w => next (z or w),
               x => sometimes z,
               z => k p2,
               y => k p1,
               x => ~ k ~ r,
               ~ r or t or s,
               ~ r or t or u,
               u => next (t or s),
               u => next (t or u),
               t => ~ k p1,
               t => ~ k p2,
               s => ~ k p2]).

ex81:-otres([~(k always p => always k p)]).
ex82:-otres([~(always k p => k always p)]).
ex83:-otres([(k sometimes p and k ~ p) => k next sometimes p]).

ex84:-otres([start => x,
             ~ x or y,
             ~ x or a,
             ~ x or b,
             a and b => next y,
             a => next a,
             b => next b,
             y => k p,
             x => ~ k ~ w,
             w => sometimes ~ p]).

ex85:-otres([start => x,
             ~ x or y,
             ~ x or a,
             ~ x or b,
             a => next y,
             b => next y,
             a => next a,
             b => next b,
             y => k p,
             x => ~ k ~ w,
             w => sometimes ~ p]).


ex86:-otres([start => x,
             ~ x or y,
             ~ x or a,
             ~ x or b,
             a => next (c or y),
             b => next (~ c or y),
             a => next a,
             b => next b,
             y => k p,
             x => ~ k ~ w,
             w => sometimes ~ p]).

ex87:-otres([start => y,
             ~ y or a,
             ~ y or b,
             ~ y or l,
             x => next l,
             a => next l,
             b => next (c or d),
             c => next a,
             d => next a,
             a => next x,
             x => next b,
             y => sometimes ~ l]).


