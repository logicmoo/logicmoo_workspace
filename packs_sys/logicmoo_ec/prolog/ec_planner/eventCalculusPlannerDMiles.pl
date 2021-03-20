:- use_module(library(logicmoo_common)).
:- style_check(-discontiguous).
:- style_check(-singleton).
:- use_module(ec_common).

:- include('planner19a.pl').
end_of_file.
/*
% NomicMUD: A MUD server written in Prolog
%
% Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
% 
% July 10,1996 - John Eikenberry 
% Copyright (C) 2004 Marty White under the GNU GPL
% 
% Dec 13, 2035 - Douglas Miles
%
%
% Logicmoo Project changes:
%
% Main file.
%
*/
%Ensure temporal points are generated correctly under PROLOG
%:-ensure_loaded('iwfms/genSymPatches.pl').

%Sort plans based on ordering
:-ensure_loaded('iwfms/sortPlans.pl').

% Removing redundant temporal orderings
:-ensure_loaded('iwfms/temporalOrderCleaner.pl').

% Map paritally ordering plans to totally ordered plans
:-ensure_loaded('iwfms/mappingTotalOrderPlan.pl').

% 
:-ensure_loaded('iwfms/loopReduction.pl').


%HTML typing and parsing
%:-ensure_loaded('../htmlTyping/typing.pl').


%:- include('../knowledgeBaseCGI.pl').

% :- include(ec_common_swi).

%:- prolog_flag(compiling,_,profiledcode).

%Ensures Prolog is not lazy and does return the entire result rather than predicate(...)
%:- prolog_flag(toplevel_print_options,_,[quoted(true),numbervars(true),portrayed(true)]).                   
 
%Ensure rules included are added rather than overwriting.
:- multifile valid_html_form/2.


/*
   ABDUCTIVE EVENT CALCULUS

   MURRAY SHANAHAN
   Version 1.15
   July 1998
   Written for LPA MacProlog 32
   
   Joseph Wilk
   Version 2
   2004
   
   Converted to SICStus Prolog.
   
   New Features
   
   1. Impossible predicate.
   2. More advanced debugging information
   3. Sort plan
   4. Loop reduction
   5. Mapping to total order plans
   6. Added high level plan generation
   
   
   In the future :
   
   1. Possiblity of using tail recurssion optimisation to improve performance.
   2. More efficent way of storing depth states of every branch,
   3. Ensuring impossible handled for happens precdicates in composite plans.
   4. Dealing with recursion generating invalid plans on investigating other potential plans.

*/


/*
   This is an abductive meta-interpreter for abduction with negation-as-
   failure, with built-in features for handling event calculus queries.
   In effect this implements a partial-order planner. not(clipped) facts
   correspond to protected links. The planner will also perform
   hierarchical decomposition, given definitions of compound events
   (happens if happens clauses).

   The form of queries is as follows.

  abdemo(Goals,Residue)

   Goals is a list of atoms. Residue is a pair of [RH,RB], where RH is a
   list of happens facts and RB is a list of temporal ordering facts.
   Negations is a list of lists of atoms.

   Roughly speaking, the above formulae should hold if,

  EC and CIRC[Domain] and CIRC[Residue] |= Goals

   where EC is the event calculus axioms, CIRC[Domain] is the completion
   of initiates, terminates and  releases, and CIRC[Residue] is the
   completion of happens. (Note that completion isn't applied to temporal
   ordering facts.)

   F is expected to be fully bound when we call abdemo(holds_at(F,T)).
   It's assumed that all primitive actions (two argument happens) are
   in the residue (ie: none are in the program).

   Although the interpreter will work with any logic program, the axioms of
   the event calculus are compiled in to the meta-level.

   The function symbol "neg" is introduced to represent classically
   negated fluents. holds_at(neg(F)) corresponds to not holds_at(F)
   (with classical "not"). terminates formulae play a role in clipping
   positive fluents and initiating negative fluents. Conversely,
   initiates formulae play a role in clipping negative fluents and
   initiating positive ones.

   Computation interleaves search (abdemo) and refinement phases. After each
   step of the computation, where a step is an iteration of abdemo or a
   refinement, the residue so far plus the remaining goal list entail the goal
   state. In other words, these computational steps are also proof steps.

   An iterative deepening search strategy is employed, where depth
   corresponds to length of plan.
*/

/* TOP LEVEL */


/*

Modified Joseph Wilk 21/01/04

   Top level calls to abdemo have to be transformed to the following form.

  abdemo_top(Goals,ResidueIn,ResidueOut,NegationsIn,NegationsOut,DepthBound, MaxDepth, HighLevel)

   The residue has the form [[HA,HC],[BA,BC]], where HA is a list of happens
   atoms, HC is the transistive closure of the before-or-equal relation on times
   that follows from HA, BA is a list of before atoms, and BC is the transitive
   closure of BA.
   
   MaxDepth indicates the limit which failure is assumed for the iterative deepening
   
*/



% 4/2/04 Joseph Wilk - Sorts Plans / currently only totally ordered plans!

plan(Gs,[HA,BA] ):- plan(Gs,[HA,BA], 199, 0 ).

plan(Gs,[SortedPlan, FinalOrderings], MaxDepth, HighLevel) :- 
 abdemo4(Gs,[HA,BA], MaxDepth, HighLevel ),
 mapToTotalOrderPlan( HA, BA ,ValidOrderings),
 sortPlan([HA,ValidOrderings], SortedPlan),
 reduceLoops(HA, ValidOrderings, FinalOrderings).

abdemo(Gs,[HA,BA] ):- abdemo4(Gs,[HA,BA], 199, 0 ).
  
abdemo4(Gs,[HA,BA], MaxDepth, HighLevel) :-
   init_gensym(t), 
   abdemo_top(Gs,[[[],[]],[[],[]]],[[HA,HC],[BA,BC]],[],N,0, MaxDepth, HighLevel).
  
   
abdemo_top(Gs,R1,R3,N1,N3,D, MaxDepth, HighLevel) :-
   
 abdemo_id(Gs,R1,R2,N1,N2,D, MaxDepth),
 % ! cut removed here to generate mutli solutions -
 abdemo_cont(R2,R3,N2,N3,MaxDepth,HighLevel).

/*
   abdemo_cont carries out one step of refinement, if necessary. It then
   resumes abdemo, but with an initial depth bound equal to the length of
   the plan so far. Obviously to start from 1 again would be pointless.
*/

abdemo_cont([[HA,TC],RB],[[HA,TC],RB],N,N,MaxDepth,HighLevel) :-  all_executable(HA, HighLevel), !.

abdemo_cont([[HA,HC],[BA,BC]],R2,N1,N3,MaxDepth,HighLevel) :-
   write('Abstract plan: '), write(HA), writeln(BA),
   refine([[HA,HC],[BA,BC]],N1,Gs,R1,N2),
   % Cut added here to prevent continous refinement with not(clipped(_,_,_)) added to goal
   % when generating multiple solutions. 
   % This does not have the implication that if something has mutliple refinements then only one of them
   % will be choosen. This is due to the fact that expand goals are treated in abdemo.
   !,
   action_count([[HA,HC],[BA,BC]]),
   abdemo_top(Gs,R1,R2,N2,N3,D,MaxDepth, HighLevel)
   
   %Joseph Wilk cut added 15/03/2004 AND removed 16/03/2004
   .

   
/* abdemo_id is an iterative deepening version of abdemo. */

/* 
Joseph Wilk
Modified 21/01/2004 - Support for bound on search */

abdemo_id(Gs,R1,R2,N1,N2,D, MaxDepth) :- 
 D >= MaxDepth, write('No solution found within bound '), write(MaxDepth), fail. %So Stop searching

abdemo_id(Gs,R1,R2,N1,N2,D, MaxDepth) :-
   D < MaxDepth, writeln('=============================================================================='),
   write('Depth: '), writeln(D),write('goals: '),writeln(Gs), 
   writeln('=============================================================================='), abdemo(Gs,R1,R2,N1,N2).

   
/* Removed by Joe - No longer needed since the planner does not use iterative deepening 
abdemo_id(Gs,R1,R2,N1,N2,D1,MaxDepth) :- D1 < MaxDepth, 
    D2 is D1+1, 
    checkForLoop(D1),
    cleanup,
    abdemo_id(Gs,R1,R2,N1,N2,D2,MaxDepth).
  
checkForLoop(D):- fail.
*/

   
all_executable([], HighLevel).

all_executable([happens(A,T1,T2)|R] ,HighLevel) :- (HighLevel =:= 0 ->  true ; (executable(A), all_executable(R, HighLevel))).



/* ABDEMO */


/*
   abdemo/6 is a depth bounded abdemo. It fails if it can't generate a plan
   with DepthBound or fewer steps. Calls to abdemo/6 have the following form.

  abdemo(Goals,ResidueIn,ResidueOut,
     NegationsIn,NegationsOut,DepthBound)

*/

:- discontiguous abdemo/6.

abdemo(Gs,[[HA,TC1],[BA,TC2]],R,N1,N2) :-
  ec_trace(on,0),
   dbginfo('Goals'=Gs),
   dbginfo('Happens'=HA),
   dbginfo('Befores'=BA),     
   dbginfo('Nafs'=N1), fail.

abdemo([],R,R,N,N).

abdemo([holds_at(F1,T3)|Gs1],R1,R6,N1,N5):- 
  F1 = neg(F) -> 
  abdemo_cons_holds_at_neg(F,T3,Gs1,R1,R6,N1,N5);
  abdemo_cons_holds_at_pos(F1,T3,Gs1,R1,R6,N1,N5).


should_swap_goals(G1,G2):- \+ functor(G1,_,1), functor(G2,_,1),!.
should_swap_goals(G1,G2):- reduce_goal(G1,R1),reduce_goal(G2,R2),!, 
   term_variables(R1,Vs1),term_variables(R2,Vs2),should_swap_goals(R1,Vs1,R2,V2),!.

reduce_goal(G,G):- \+ compound(G),!.
reduce_goal(holds_at(G1,_),G2):- !, reduce_goal(G1,G2).
reduce_goal(holds_at(G1,_,_),G2):- !, reduce_goal(G1,G2).
reduce_goal(G,G).

% should_swap_goals(R1,Vs1,R2,V2).
should_swap_goals(diff(_,_),_,NotDif,_):- NotDif \= diff(_,_).
should_swap_goals(_R1,[_|Vs1],_R2,[]).

/*
   Now we have two clauses which are meta-level representations of the two
   event calculus axioms for holds_at.

  holds_at(F,T) <- initiallyp(F) and not clipped(0,F,T)

  holds_at(F,T3) <-
     happens(A,T1,T2) and T2 < T3 and
     initiates(A,F,T1) and not clipped(T1,F,T2)

   Note the way the object-level axioms have been compiled into the
   meta-level. Note also that happens goals are not resolved against
   clauses in the object-level program. Instead, they're put straight into
   the residue. Resolving happens goals is the job of the refinement phase.
*/
abdemo_cons_holds_at_pos(F1,T,Gs1,R1,R3,N1,N4) :-   
   abresolve(initially(F1),R1,Gs2,R1,B),
   append(Gs2,Gs1,Gs3), add_neg([clipped(0,F1,T)],N1,N2),
   abdemo_naf([clipped(0,F1,T)],R1,R2,N2,N3),
   abdemo(Gs3,R2,R3,N3,N4).


/*
   The order in which resolution steps are carried out in the next
   clause is crucial. We resolve initiates first in order to instantiate 
   A, but we don't want to proceed with sub-goals of initiates until
   we've added the corresponding happens and before "b" facts to the residue.
*/

abdemo_cons_holds_at_pos(F1,T3,Gs1,R1,R6,N1,N5) :-   
   abresolve(initiates(A,F1,T1),R1,Gs2,R1,B1),
   %Provides Different ways to resolve a goal when backtracking
   abresolve(happens(A,T1,T2),R1,[],R2,B2),
   abresolve(b(T2,T3),R2,[],R3,B3),
   append(Gs2,Gs1,Gs3), check_nafs(B2,N1,R3,R4,N1,N2),
   add_neg([clipped(T1,F1,T3)],N2,N3),
   abdemo_naf([clipped(T1,F1,T3)],R4,R5,N3,N4),
   abdemo(Gs3,R5,R6,N4,N5),
    
   write('action:'),
   writeln(A),
   write('precondition:'),
   writeln(Gs2). 
   

/*
   The next two clauses are a meta-level representation of the two
   event calculus axioms for not holds_at.

  not holds_at(F,T) <- initiallyn(F) and not declipped(0,F,T)

  not holds_at(F,T3) <-
     happens(A,T1,T2) and T2 < T3 and
     terminates(A,F,T1) and not declipped(T1,F,T2)
*/
abdemo_cons_holds_at_neg(F,T,Gs1,R1,R3,N1,N4) :-
   abresolve(initially(neg(F)),R1,Gs2,R1,B),
   append(Gs2,Gs1,Gs3), add_neg([declipped(0,F,T)],N1,N2),
   abdemo_naf([declipped(0,F,T)],R1,R2,N2,N3),
   abdemo(Gs3,R2,R3,N3,N4).
 
abdemo_cons_holds_at_neg(F,T3,Gs1,R1,R6,N1,N5) :-
   abresolve(terminates(A,F,T1),R1,Gs2,R1,B1),
   abresolve(happens(A,T1,T2),R1,[],R2,B2),
   abresolve(b(T2,T3),R2,[],R3,B3),
   append(Gs2,Gs1,Gs3), check_nafs(B2,N1,R3,R4,N1,N2),
   add_neg([declipped(T1,F,T3)],N2,N3),
   abdemo_naf([declipped(T1,F,T3)],R4,R5,N3,N4),
   abdemo(Gs3,R5,R6,N4,N5),
  write('terminate action:'),
  writeln(A),
  write('precondition:'),
  writeln(Gs2).
   
   
/*
   The next two clauses cater for happens goals.

   Note that happens goals only appear either in the initial goal list or,
   in an "expand" goal, as a result of refinement. Note also that these
   clauses, because of the way abresolve(happens) works, will ensure sharing
   of sub-goals between compound events wherever possible.
*/

abdemo([happens(A,T1,T2)|Gs],R1,R4,N1,N3) :-
   !, abresolve(happens(A,T1,T2),R1,[],R2,B), 
    
   check_nafs(B,N1,R2,R3,N1,N2), abdemo(Gs,R3,R4,N2,N3).

abdemo([happens(A,T)|Gs],R1,R4,N1,N3) :-
   !, abresolve(happens(A,T),R1,[],R2,B),
   check_nafs(B,N1,R2,R3,N1,N2), abdemo(Gs,R3,R4,N2,N3).

/*
   The next clause deals with the expansion of compound actions. These appear
   as goals of the form expand([happens(A,T1,T2)|Bs]), where happens(A,T1,T2)
   is the compound action to be expanded and Bs is a list of "before" goals.
   These goals are placed in the goal list by calls to "refine". The various
   sub-goals generated by the expansion are treated in a careful order. First,
   the compound action's sub-actions are added to the residue. Then the "before"
   goals that "refine" took out of the residue are put back (with their newly
   skolemised time constants). The holds_at goals are solved next, on the
   assumption that they are either guards or supply context for the expansion.
   Then other, non event calculus goals are dealt with. Only then, with all the
   temporal ordering constraints sorted out, is it safe to tackle not(clipped)
   and not(declipped) goals, first those that are part of the compound action
   definition, then those recorded in the negations list.
*/

abdemo([expand([happens(A,T1,T2)|Bs])|Gs1],R1,R8,N1,N8) :-
   !, axiom(happens(A,T1,T2),Gs2),
   add_sub_actions(Gs2,R1,R2,N1,N2,D,Hs),
   solve_befores(Bs,R2,R3,N2,N3),
   abdemo_holds_ats(Gs2,R3,R4,N3,N4),
   % Joseph Wilk - Failure here causes a re-loop and matches another composite action resolution
   
   solve_other_goals(Gs2,R4,R5,N4,N5),
   check_clipping(Gs2,R5,R6,N5,N6),
   check_sub_action_nafs(Hs,N1,R6,R7,N6,N7),
   abdemo(Gs1,R7,R8,N7,N8).

  
    
/*
   The last two clauses cater for the general case (ie: goals other
   than holds_at and happens). They're also used to tackle domain
   constraints (ie: holds_at if holds_at clauses).
*/

abdemo([not(G)|Gs],R1,R3,N1,N4) :-
   !, abdemo_naf([G],R1,R2,N1,N2), add_neg([G],N2,N3),
   abdemo(Gs,R2,R3,N3,N4).
   
abdemo([G|Gs1],R1,R3,N1,N2) :-
   abresolve(G,R1,Gs2,R2,B), append(Gs2,Gs1,Gs3),
    % avoid duplicate ops ?!
    % (some_duped_ops(Gs3) -> ( wdmsg(some_dupe_ops(Gs3)), !, fail) ; true),
   abdemo(Gs3,R2,R3,N1,N2).
   
some_duped_ops(Gs3):- iv_list_to_set(Gs3,Gs4), !, Gs3\==Gs4.
   
%CUT added by JOE 13/03/2004 AND REMOVED! 17052004:3:23pm due to the fact that only the goal was being re-evaluated

% currently catches abstract actions!!!!
abdemo([G|Gs],R1,R3,N1,N2,D ) :- write( 'currently unknown information about(may be compound!):'), writeln(G), fail.

abdemo([G1,G2|Gs1],R1,R6,N1,N5):- fail, \+ \+  should_swap_goals(G1,G2),!, 1 is random(2),
  abdemo([G2,G1|Gs1],R1,R6,N1,N5).


/*

Removed Joseph Wilk - depth first search is used instead!

check_depth(R,D,L) :- ec_trace(on,1), 
   action_count(R,L), 
   write('Actions:'),
   writeln(R),
   write('Action count:'), write(L), fail.
  
%Keep track of the current action depth
check_depth(R,D,L) :- action_count(R,L), L =< D, D =< 1000.

% The action list is greater than the current Depth
check_depth(R,D,L) :- ec_trace(on, 1), write('Maximum bound reached'),!, fail.
*/





action_count([[HA,TC],RB],L) :- length(HA,L).




/* EXPANSION OF COMPOUND ACTIONS */


/* The following collection of predicates are called by abdemo(expand). */


abdemo_holds_ats([],R,R,N,N).

abdemo_holds_ats([holds_at(F,T)|Gs],R1,R3,N1,N3) :-
   !, 
   abdemo([holds_at(F,T)],R1,R2,N1,N2),
   %cut added Joseph Wilk 16/03/2004
   !,
   
   abdemo_holds_ats(Gs,R2,R3,N2,N3).

abdemo_holds_ats([G|Gs],R1,R2,N1,N2) :-
 abdemo_holds_ats(Gs,R1,R2,N1,N2).


non_regular_goal(G):- \+ regular_goal(G).

regular_goal(holds_at(_F, _T)).
regular_goal(happens(_A, _T)).
regular_goal(happens(_A, _T1, _T2)).
regular_goal(b(_T1, _T2)).
regular_goal(not(Clip)):- nonvar(Clip), 
  (Clip = clipped(_T1,_F,_T2) ; Clip = declipped(_DT1,_FD,_DT2)).

solve_other_goals([],R,R,N,N).

solve_other_goals([G|Gs],R1,R3,N1,N3) :-
   non_regular_goal(G), !,
   abdemo([G],R1,R2,N1,N2),
   solve_other_goals(Gs,R2,R3,N2,N3).

solve_other_goals([G|Gs],R1,R2,N1,N2) :-
   solve_other_goals(Gs,R1,R2,N1,N2).

   
   
% Modified Joseph Wilk 24/02/2004 7:54pm
% The idea is if we hit this clause we are backtracking and only want to move backwards not forwards

% solve_other_goals([G|Gs],R1,R2,N1,N2) :-  fail.



/*
   In its last argument, add_sub_actions accumulates a list of new actions
   added to the residue, so that subsequent re-checking of not(clipped) and
   not(declipped) goals can be done via check_nafs rather than the less
   efficient abdemo_nafs.
*/

add_sub_actions([],R,R,N,N,D,[]).

add_sub_actions([happens(A,T1,T2)|Gs],R1,R3,N1,N3,D,Hs2) :-
   !,
   abresolve(happens(A,T1,T2),R1,[],R2,B),
   add_sub_actions(Gs,R2,R3,N2,N3,D,Hs1), cond_add(B,happens(A,T1,T2),Hs1,Hs2).

add_sub_actions([happens(A,T)|Gs],R1,R3,N1,N3,D,Hs2) :-
   !, 
   abresolve(happens(A,T),R1,[],R2,B),     
   add_sub_actions(Gs,R2,R3,N2,N3,D,Hs1), cond_add(B,happens(A,T,T),Hs1,Hs2).

add_sub_actions([b(T1,T2)|Gs],R1,R3,N1,N3,D,Hs) :-
   !, abresolve(b(T1,T2),R1,[],R2,B),
   add_sub_actions(Gs,R2,R3,N2,N3,D,Hs).

add_sub_actions([G|Gs],R1,R2,N1,N2,D,Hs) :-
 add_sub_actions(Gs,R1,R2,N1,N2,D,Hs).


cond_add(false,H,Hs,Hs) :- !.
cond_add(true,H,Hs,[H|Hs]).


solve_befores([],R,R,N,N).
solve_befores([b(T1,T2)|Gs],R1,R3,N1,N3) :-
   !, abdemo([b(T1,T2)],R1,R2,N1,N2),
   solve_befores(Gs,R2,R3,N2,N3),!. %CUT introduced by JOSEPH WILK 30/05/04
solve_befores([G|Gs],R1,R2,N1,N2) :-
   solve_befores(Gs,R1,R2,N1,N2),!. %CUT introduced by JOSEPH WILK 30/05/04


check_sub_action_nafs([],N1,R,R,N2,N2) :- !. 

check_sub_action_nafs([happens(A,T1,T2)|Hs],N1,R1,R3,N2,N4) :-
   check_nafs(A,T1,T2,N1,R1,R2,N2,N3),
   check_sub_action_nafs(Hs,N1,R2,R3,N3,N4).


check_clipping([],R,R,N,N) :- !. 

check_clipping([not(clipped(T1,F,T2))|Gs],R1,R3,N1,N3) :-
   !, abdemo_naf([clipped(T1,F,T2)],R1,R2,N1,N2),
   check_clipping(Gs,R2,R3,N2,N3).

check_clipping([not(declipped(T1,F,T2))|Gs],R1,R3,N1,N3) :-
   !, abdemo_naf([declipped(T1,F,T2)],R1,R2,N1,N2),
   check_clipping(Gs,R2,R3,N2,N3).

check_clipping([G|Gs],R1,R2,N1,N2) :-
   check_clipping(Gs,R1,R2,N1,N2).




/* ABRESOLVE */


/*
   The form of a call to abresolve is as follows.

  abresolve(Goal,ResidueIn,Goals,ResidueOut,Flag)

   where Goals is the body of clause resolved with, and Flag is set to true
   if a happens fact has been added to the residue.
*/

abresolve(terms_or_rels_or_imposs(A,F,T),R,Gs,R,false) :- axiom(releases(A,F,T),Gs).
abresolve(terms_or_rels_or_imposs(A,F,T),R,Gs,R,false) :- axiom(terminates(A,F,T),Gs).

% impossible action supported
abresolve(terms_or_rels_or_imposs(A,F,T),R,Gs,R,false) :- !, axiom(impossible(A,T),Gs).

abresolve(inits_or_rels(A,F,T),R,Gs,R,false) :- axiom(releases(A,F,T),Gs).
abresolve(inits_or_rels(A,F,T),R,Gs,R,false) :- !, axiom(initiates(A,F,T),Gs).

/*
   happens goals get checked to see if they are already in the residue.
   This permits the re-use of actions already in the residue. However,
   this decision may lead to later failure, in which case we try adding
   a new action to the residue.

   happens goals aren't resolved against object-level program clauses.
   Only goals that are marked as expand(goal) are resolved against
   program clauses, and this is done by abdemo.

   Time variables get skolemised, but not action variables because
   they end up getting instantiated anyway.
*/

abresolve(happens(A,T),R1,Gs,R2,B) :- !, abresolve(happens(A,T,T),R1,Gs,R2,B).

abresolve(happens(A,T1,T2),[[HA,TC],RB],[],[[HA,TC],RB],false) :-
   member(happens(A,T1,T2),HA),
   %Joseph Wilk Modification
   %added to prevent re-evaluating happens with multiple goals
   %Otherwise happens occur which are duplicates in the plan
   !.

abresolve(initially(A),R,Gss,R, False) :- !, abresolve(holds_at(A, start),R,Gss,R, False).

abresolve(happens(A,T,T),[[HA,TC],RB],[],[[[happens(A,T,T)|HA],TC],RB],B) :-
   executable(A), !, B = true, skolemise_time(T).

abresolve(happens(A,T1,T2),R1,[],R2,B) :-  !, B = true, 
   skolemise_time(T1), skolemise_time(T2), add_happens(A,T1,T2,R1,R2).

/*
   If either X or Y is not bound in a call to abresolve(b(X,Y)) then
   they get skolemised.
*/

abresolve(b(X,Y),R,[],R,false) :-
   skolemise_time(X), skolemise_time(Y), demo_before(X,Y,R), !.

abresolve(b(X,Y),R1,[],R2,B) :-
   !, B = false, skolemise_time(X), skolemise_time(Y), \+ demo_beq(Y,X,R1),
   add_before(X,Y,R1,R2).

/*
   The predicates "diff" (meaning not equal) and "is" (for evaluating
   arithmetic expressions) are built in.
*/

abresolve(diff(X,Y),R,[],R,false) :- !, X \= Y.
abresolve(dif(X,Y),R,[],R,false) :- !, dif(X,Y).

abresolve(is(X,Y),R,[],R,false) :- !, X is Y.



% Joseph wilk modification - 26/02/2004 6:56pm
% access to pure prolog breaking through meta-interpreter

abresolve(prolog(Code),R,[],R,false) :- !, Code.
abresolve(call(Code),R,[],R,false) :- !, Code.

/*
%Access Knowledge base
abresolve(knowledgeBase(Username, GroupList),R,[],R,false) :- !,  
  '::'(knowledgeDb,lookupUserProlog(Username, Groups)), !,
  listMember(GroupList, Groups),!.

%Check html attributes
abresolve(valid_html_form(Type, Att),R,[],R,false):- !, valid_html_form(Type,Att).

%Check html children
abresolve(valid_html_formChildren(Type, Children),R,[],R,false):- !, valid_html_formChildren(Type,Children).


*/

:- discontiguous ec:abresolve/5. 

breakupResidual( [[X,Y],[Z,A]]   , X).


%Attempt to test for membership of Actions list
abresolve(notOccured(Action),R,[],R,false) :- 
  !, 
  breakupResidual(R,Actions),!,
  \+ member(Action, Actions).
  
abresolve(occured(Action),R,[],R,false) :- 
  !, 
  breakupResidual(R,Actions),!,
  member(Action, Actions).

  
abresolve((G1,G2),ResidueIn,Goals,ResidueOut,Flag):- !,
  (abresolve(G1,ResidueIn,Goals1,ResidueMid,Flag),
   abresolve(G2,ResidueMid,Goals2,ResidueOut,Flag)),
   append(Goals1,Goals2,Goals).
  
  
abresolve(G,R,[],[G|R],false) :-  abducible(G).

abresolve(G,R,Gs,R,false) :-   
   writeln('--------------------'), writeln(G),writeln('--------------------'),
   axiom(G,Gs).

abresolve(G,R,[],[G|R],false) :-  current_predicate(_,G),catch(G,_,fail).



/* ABDEMO_NAFS and CHECK_NAFS */


/*
   abdemo_nafs([G1...Gn],R1,R2) demos not(G1) and ... and not(Gn).

   Calls to abdemo_naf have the following form.

  abdemo_nafs(Negations,ResidueIn,ResidueOut,
     NegationsIn,NegationsOut,DepthIn,DepthOut)

   where Negations is the list of negations to be established, ResidueIn
   is the old residue, ResidueOut is the new residue (abdemo_nafs can add
   both before/"b" and happens facts, as well as other abducibles, to the
   residue), NegationsIn is the old list of negations (same as Negations
   for top-level call), and NegationsOut is the new list of negations
   (abdemo_nafs can add new clipped goals to NegationsIn).

   DepthIn and DepthOut keep track of the number of sub-goals generated,
   for iterative deepening purposes.
*/


abdemo_nafs([],R,R,N,N).

abdemo_nafs([N|Ns],R1,R3,N1,N3) :-
   abdemo_naf(N,R1,R2,N1,N2), abdemo_nafs(Ns,R2,R3,N2,N3).


/*
Joseph Wilk
Impossible introduced 21/01/04

   abdemo_naf([G1...Gn],R1,R2) demos not((G1) and ... and (Gn)).

   As for abdemo, the main event calculus axioms are compiled into the
   meta-level in abdemo_naf. In addition to the two holds_at axioms, we
   have,

  clipped(T1,F,T3) <-
     happens(A,T2) and T1 < T2 < T3 and
     [terminates(A,F,T2) or releases(A,F,T2) or impossible(A,T2)]
     
  declipped(T1,F,T3) <-
     happens(A,T2) and T1 < T2 < T3 and
     [initiates(A,F,T2) or releases(A,F,T2) or impossible(A,T2)]
        

   We have to use findall here, because all ways of achieving a goal
   have to fail for the goal itself to fail.

   Note that only the two-argument version of happens is used, because
   the residue only contains instantaneous actions, and 
   *** the effects of compound actions are assumed to match the effects of their
   component actions. ***
*/

abdemo_naf([clipped(T1,F,T4)|Gs1],R1,R2,N1,N2) :-
   !, findall(Gs3,
    (abresolve(terms_or_rels_or_imposs(A,F,T2),R1,Gs2,R1,false),
     abresolve(happens(A,T2,T3),R1,[],R1,false),
    append([b(T1,T3),b(T2,T4)|Gs2],Gs1,Gs3)),Gss),
   abdemo_nafs(Gss,R1,R2,N1,N2).

abdemo_naf([declipped(T1,F,T4)|Gs1],R1,R2,N1,N2) :-
   !, findall(Gs3,
    (abresolve(inits_or_rels_or_imposs(A,F,T2),R1,Gs2,R1,false),
    abresolve(happens(A,T2,T3),R1,[],R1,false),
    append([b(T1,T3),b(T2,T4)|Gs2],Gs1,Gs3)),Gss),
   abdemo_nafs(Gss,R1,R2,N1,N2).

/*
   To show the classical negation of holds_at(F) (which is what we want), we
   have to show that holds_at(neg(F)). Conversely, to show the classical
   negation of holds_at(neg(F)) we have to show holds_at(F). Within a call
   to abdemo_naf, we can add both happens and before (and other abducibles)
   to the residue. This removes a potential source of incompleteness.

   Note that F must be a ground term to preserve soundness and completeness.
   To guarantee this, all variables that occur in the body of an
   initiates, terminates or releases clause must occur in the fluent
   argument in its head.
*/

/* DANGER: Cut in next clause rules out other ways to solve holds_at(F2,T). */

abdemo_naf([holds_at(F1,T)|Gs1],R1,R3,N1,N3) :-
   opposite(F1,F2), copy_term(Gs1,Gs2),
   abdemo([holds_at(F2,T)],R1,R2,N1,N2), !,
   abdemo_naf_cont(R1,Gs2,R2,R3,N1,N3).

abdemo_naf([holds_at(F,T)|Gs],R1,R2,N1,N2) :-
   !, abdemo_naf(Gs,R1,R2,N1,N2).

/*
   Special facilities for handling temporal ordering facts are built in.
   We can add a before fact to the residue to preserve the failure of
   a clipped goal. The failure of a before goal does NOT mean that the
   negation of that goal is assumed to be true. (The Clark completion is
   not applicable to temporal ordering facts.) Rather, to make b(X,Y)
   fail, b(Y,X) has to follow. One way to achieve this is to add
   b(Y,X) to the residue.
*/

abdemo_naf([b(X,Y)|Gs],R,R,N,N) :- X == Y, !.

abdemo_naf([b(X,Y)|Gs],R,R,N,N) :- demo_before(Y,X,R), !.

abdemo_naf([b(X,Y)|Gs1],R1,R2,N1,N2) :-
   !, append(Gs1,[postponed(b(X,Y))],Gs2),
   abdemo_naf(Gs2,R1,R2,N1,N2).

/*
   A before fact is only added to the residue as a last resort. Accordingly,
   if we encounter a b(X,Y) goal and cannot show b(Y,X), we
   postpone that goal until we've tried other possibilities. A postponed
   before goal appears in the goal list as postponed(b(X,Y)).
*/

abdemo_naf([postponed(b(X,Y))|Gs],R1,R2,N,N) :-
   \+ demo_beq(X,Y,R1), add_before(Y,X,R1,R2).

abdemo_naf([postponed(b(X,Y))|Gs],R1,R2,N1,N2) :-
   !, abdemo_naf(Gs,R1,R2,N1,N2).

/* 
   We drop through to the general case for goals other than special event
   calculus goals.
*/

/* DANGER: Cut in next clause rules out other ways to solve G. */

abdemo_naf([not(G)|Gs1],R1,R3,N1,N3) :-
   copy_term(Gs1,Gs2), 
   abdemo([G],R1,R2,N1,N2), !,
   abdemo_naf_cont(R1,Gs2,R2,R3,N1,N3).

abdemo_naf([not(G)|Gs],R1,R2,N1,N2) :- !, abdemo_naf(Gs,R1,R2,N1,N2).

abdemo_naf([G|Gs1],R,R,N,N) :- \+ abresolve(G,R,Gs2,R,false), !.

abdemo_naf([G1|Gs1],R1,R2,N1,N2) :-
   findall(Gs2,(abresolve(G1,R1,Gs3,R1,false),append(Gs3,Gs1,Gs2)),Gss),
   abdemo_nafs(Gss,R1,R2,N1,N2).


/*
   abdemo_naf_cont gets an extra opportunity to succeed if the residue
   has been altered. This is determined by comparing R1 with R2. If
   a sub-goal has failed and the residue hasn't been altered, there's
   no need to look for other ways to prove the negation of the overall goal.
*/

abdemo_naf_cont(_R1,_Gs,R2,R2,N,N).
abdemo_naf_cont(R1,Gs,R2,R3,N1,N2) :-
   R1 \= R2, abdemo_naf(Gs,R1,R3,N1,N2).




/*
   check_nafs is just like abdemo_nafs, except that it only checks
   negated clipped and declipped facts against the most recent event
   added to the residue. To check one of these negations, not only can
   we confine our attention to the most recent event, but we can ignore
   that event if it doesn't fall inside the interval covered by the
   clipped/declipped in question. Of course, the negated clipped/declipped
   fact might depend on other holds_at facts. But their preservation is
   ensured because the clipped/declipped negation they themselves depend
   on will also be checked.
*/


check_nafs(false,N1,R,R,N2,N2) :- !.

check_nafs(true,N,[[[happens(A,T1,T2)|HA],TC],RB],R,N1,N2) :-
   check_nafs(A,T1,T2,N,[[[happens(A,T1,T2)|HA],TC],RB],R,N1,N2).

check_nafs(A,T1,T2,[],R,R,N,N).

check_nafs(A,T1,T2,[N|Ns],R1,R3,N1,N3) :-
   check_naf(A,T1,T2,N,R1,R2,N1,N2),
   check_nafs(A,T1,T2,Ns,R2,R3,N2,N3).


check_naf(A,T2,T3,[clipped(T1,F,T4)],R1,R2,N1,N2) :-
   !, findall([b(T1,T3),b(T2,T4)|Gs],
    (abresolve(terms_or_rels_or_imposs(A,F,T2),R1,Gs,R1,false)),Gss),
   abdemo_nafs(Gss,R1,R2,N1,N2).

check_naf(A,T2,T3,[declipped(T1,F,T4)],R1,R2,N1,N2) :-
   !, findall([b(T1,T3),b(T2,T4)|Gs],
    (abresolve(inits_or_rels(A,F,T2),R1,Gs,R1,false)),Gss),
   abdemo_nafs(Gss,R1,R2,N1,N2).

check_naf(A,T1,T2,N,R1,R2,N1,N2) :- abdemo_naf(N,R1,R2,N1,N2).




/* DEMO_BEFORE, ADD_BEFORE and ADD_HAPPENS */


/*
   demo_before simply checks membership of the transitive closure half of
   the temporal ordering part of the residue. Likewise demo_beq checks for
   demo_before or for membership of the transitive closure half of the
   happens part of the residue.
*/

demo_before(0,Y,R) :- !, Y \= 0.

demo_before(X,Y,[RH,[BA,TC]]) :- member(b(X,Y),TC).

/* demo_beq is demo before or equal. */

demo_beq(X,Y,R) :- X == Y, !.

demo_beq(X,Y,R) :- demo_before(X,Y,R), !.

demo_beq(X,Y,[[HA,TC],RB]) :- member(beq(X,Y),TC).


/*
   add_before(X,Y,R1,R2) adds b(X,Y) to the residue R1 giving R2.
   It does this by maintaining the transitive closure of the "b" and "beq"
   relations in R2, and assumes that R1 is already transitively closed.
   R1 and R2 are just the temporal ordering parts of the residue.
*/

add_before(X,Y,[RH,[BA,TC]],[RH,[BA,TC]]) :- member(b(X,Y),TC), !.

add_before(X,Y,[[HA,HC],[BA,BC1]],[[HA,HC],[[b(X,Y)|BA],BC2]]) :-
   \+ demo_beq(Y,X,[[HA,HC],[BA,BC1]]), find_bef_connections(X,Y,BC1,C1,C2),
     find_beq_connections(X,Y,HC,C3,C4), delete_abdemo(X,C3,C5), delete_abdemo(Y,C4,C6),
   append(C5,C1,C7), append(C6,C2,C8),
   cross_prod_bef(C7,C8,C9,BC1), append(C9,BC1,BC2).

/*
   add_happens(A,T1,T2,R1,R2) adds happens(A,T1,T2) to the residue R1
   giving R2. Because happens(A,T1,T2) -> T1 <= T2, this necessitates
   updating the transitive closures of the before "b" and "beq" facts in the residue.
   Duplicates aren't checked for, as it's assumed this is done by the
   calling predicate.
*/

add_happens(A,T1,T2,[[HA,HC1],[BA,BC1]],[[[happens(A,T1,T2)|HA],HC2],[BA,BC2]]) :-
   \+ demo_before(T2,T1,[[HA,HC1],[BA,BC1]]),
   find_beq_connections(T1,T2,HC1,C1,C2), cross_prod_beq(C1,C2,C3,HC1),
   append(C3,HC1,HC2), find_bef_connections(T1,T2,BC1,C4,C5),
     cross_prod_bef(C4,C5,C6,BC1), delete_abdemo(b(T1,T2),C6,C7),
   append(C7,BC1,BC2).

/*
   find_bef_connections(X,Y,TC,C1,C2) creates two lists, C1 and C2,
   containing respectively all the time points before X and after
   Y according to TC, which is assumed to be transitively closed.
*/

find_bef_connections(X,Y,[],[X],[Y]).

find_bef_connections(X,Y,[b(Z,X)|R],[Z|C1],C2) :-
   !, find_bef_connections(X,Y,R,C1,C2).

find_bef_connections(X,Y,[b(Y,Z)|R],C1,[Z|C2]) :-
   !, find_bef_connections(X,Y,R,C1,C2).

find_bef_connections(X,Y,[b(Z1,Z2)|R],C1,C2) :-
   find_bef_connections(X,Y,R,C1,C2).

/*
   find_beq_connections is like find_bef_connections, except that it works
   on the transtive closure of happens part of the residue.
*/

find_beq_connections(X,Y,[],[X],[Y]).

find_beq_connections(X,Y,[beq(Z,X)|R],[Z|C1],C2) :-
   !, find_beq_connections(X,Y,R,C1,C2).

find_beq_connections(X,Y,[beq(Y,Z)|R],C1,[Z|C2]) :-
   !, find_beq_connections(X,Y,R,C1,C2).

find_beq_connections(X,Y,[beq(Z1,Z2)|R],C1,C2) :-
   find_beq_connections(X,Y,R,C1,C2).


cross_prod_bef([],C,[],R).

cross_prod_bef([X|C1],C2,R3,R) :-
   cross_one_bef(X,C2,R1,R), cross_prod_bef(C1,C2,R2,R), append(R1,R2,R3).

cross_one_bef(X,[],[],R).

cross_one_bef(X,[Y|C],[b(X,Y)|R1],R) :-
   \+ member(b(X,Y),R), !, cross_one_bef(X,C,R1,R).

cross_one_bef(X,[Y|C],R1,R) :- cross_one_bef(X,C,R1,R).


cross_prod_beq([],C,[],R).

cross_prod_beq([X|C1],C2,R3,R) :-
   cross_one_beq(X,C2,R1,R), cross_prod_beq(C1,C2,R2,R), append(R1,R2,R3).

cross_one_beq(X,[],[],R).

cross_one_beq(X,[Y|C],[beq(X,Y)|R1],R) :-
   \+ member(beq(X,Y),R), !, cross_one_beq(X,C,R1,R).

cross_one_beq(X,[Y|C],R1,R) :- cross_one_beq(X,C,R1,R).




/* REFINE */


/*
   refine(R1,N1,Gs,R2,N2) takes the earliest compound action out of the residue
   R1 and puts it in the goal list Gs, along with all "before/b", not(clipped) and
   not(declipped) facts that refer to the same time points. These time points
   are unskolemised, so that they can be bound to existing time points in the
   residue, thus permitting sub-actions to be shared between abstract actions.
   This almost restores the state of computation to the state S before the chosen
   action was added to the residue in the first place. But not quite, because
   some derived before facts will be retained in the transitive closure part
   of the before part of the residue that weren't there in state S. This doesn't
   matter, however, because these derived facts will need to be there eventually
   anyway, when those before facts that have been transferred from residue to goal
   list get put back in the residue.

   The compound action extracted from the residue is marked for expansion by
   inserting it in the goal list in the form expand([happens(A,T1,T2)|Bs]),
   where Bs is the list of "before/b" facts taken out of the residue, as
   described above. When abdemo comes across a goal of this form, it will
   resolve the "happens" part against program clauses. The reason for
   not doing this resolution here is that we want to backtrack to alternative
   definitions of a compound goal inside the iterative deepening search, not
   outside it. This ensures the desired behaviour for compound actions which
   expand to one series of sub-actions given certain conditions but to another
   series of sub-actions given other conditions. Otherwise the check for these
   conditions, which will be a holds_at goal in the compound action definition,
   is in danger of being treated as a goal to be established instead of just
   checked.
*/

refine([[HA1,HC1],[BA1,BC1]],N1,Gs,[[HA2,HC2],[BA2,BC2]],N3) :-
   split_happens(HA1,[BA1,BC1],happens(A,T1,T2),HA2),
   split_beqs(HC1,T1,T2,T3,T4,HC3,HC2),
   split_befores(BA1,T1,T2,T3,T4,BA3,BA2),
   split_befores(BC1,T1,T2,T3,T4,BC3,BC2),
   split_nafs(N1,T1,T2,T3,T4,N2,N3),
   append([expand([happens(A,T3,T4)|BA3])],N2,Gs).


/*
   split_happens(RH1,RB,H,RH2) holds if H is the earliest non-executable
   (abstract) action in RH1 according to RB. The remainder of RH1 ends up in
   RH2. If there are no non-executable actions, H is the earliest action.
*/

split_happens([happens(A,T1,T2)],RB,happens(A,T1,T2),[]) :- !.

split_happens([happens(A1,T1,T2)|RH1],RB,happens(A3,T5,T6),
    [happens(A4,T7,T8)|RH2]) :-
   split_happens(RH1,RB,happens(A2,T3,T4),RH2),
   order(happens(A1,T1,T2),happens(A2,T3,T4),RB,
    happens(A3,T5,T6),happens(A4,T7,T8)).


order(happens(A1,T1,T2),happens(A2,T3,T4),RB,
    happens(A1,T1,T2),happens(A2,T3,T4)) :-
   \+ executable(A1), executable(A2), !.

order(happens(A1,T1,T2),happens(A2,T3,T4),RB,
    happens(A2,T3,T4),happens(A1,T1,T2)) :-
   \+ executable(A2), executable(A1), !.

order(happens(A1,T1,T2),happens(A2,T3,T4),RB,
    happens(A1,T1,T2),happens(A2,T3,T4)) :-
   demo_before(T1,T3,[[],RB]), !.

order(happens(A1,T1,T2),happens(A2,T3,T4),RB,happens(A2,T3,T4),happens(A1,T1,T2)).


split_befores([],T1,T2,T3,T4,[],[]).

split_befores([b(T1,T2)|Bs1],T3,T4,T5,T6,Bs2,[b(T1,T2)|Bs3]) :-
   no_match(T1,T2,T3,T4), !, split_befores(Bs1,T3,T4,T5,T6,Bs2,Bs3).

split_befores([b(T1,T2)|Bs1],T3,T4,T5,T6,[b(T7,T8)|Bs2],Bs3) :-
   substitute_time(T1,T3,T4,T5,T6,T7), substitute_time(T2,T3,T4,T5,T6,T8),
   split_befores(Bs1,T3,T4,T5,T6,Bs2,Bs3).


split_beqs([],T1,T2,T3,T4,[],[]).

split_beqs([beq(T1,T2)|Bs1],T3,T4,T5,T6,Bs2,[beq(T1,T2)|Bs3]) :-
   no_match(T1,T2,T3,T4), !, split_beqs(Bs1,T3,T4,T5,T6,Bs2,Bs3).

split_beqs([beq(T1,T2)|Bs1],T3,T4,T5,T6,[beq(T7,T8)|Bs2],Bs3) :-
   substitute_time(T1,T3,T4,T5,T6,T7), substitute_time(T2,T3,T4,T5,T6,T8),
   split_beqs(Bs1,T3,T4,T5,T6,Bs2,Bs3).


split_nafs([],T1,T2,T3,T4,[],[]).

split_nafs([[clipped(T1,F,T2)]|Bs1],T3,T4,T5,T6,Bs2,[[clipped(T1,F,T2)]|Bs3]) :-
   no_match(T1,T2,T3,T4), !, split_nafs(Bs1,T3,T4,T5,T6,Bs2,Bs3).

split_nafs([[clipped(T1,F,T2)]|Bs1],T3,T4,T5,T6,
    [not(clipped(T7,F,T8))|Bs2],Bs3) :-
   !, substitute_time(T1,T3,T4,T5,T6,T7), substitute_time(T2,T3,T4,T5,T6,T8),
   split_nafs(Bs1,T3,T4,T5,T6,Bs2,Bs3).

split_nafs([[declipped(T1,F,T2)]|Bs1],T3,T4,T5,T6,Bs2,
    [[declipped(T1,F,T2)]|Bs3]) :-
   no_match(T1,T2,T3,T4), !, split_nafs(Bs1,T3,T4,T5,T6,Bs2,Bs3).

split_nafs([[declipped(T1,F,T2)]|Bs1],T3,T4,T5,T6,
    [not(declipped(T7,F,T8))|Bs2],Bs3) :-
   !, substitute_time(T1,T3,T4,T5,T6,T7), substitute_time(T2,T3,T4,T5,T6,T8),
   split_nafs(Bs1,T3,T4,T5,T6,Bs2,Bs3).

split_nafs([N|Bs1],T3,T4,T5,T6,Bs2,[N|Bs3]) :-
   substitute_time(T1,T3,T4,T5,T6,T7), substitute_time(T2,T3,T4,T5,T6,T8),
   split_nafs(Bs1,T3,T4,T5,T6,Bs2,Bs3).


substitute_time(T1,T1,T2,T3,T4,T3) :- !.
substitute_time(T2,T1,T2,T3,T4,T4) :- !.
substitute_time(T1,T2,T3,T4,T5,T1).

no_match(T1,T2,T3,T4) :- T1 \= T3, T2 \= T3, T1 \= T4, T2 \= T4.




/* OTHER BITS AND PIECES */


/*
   add_neg(N,Ns1,Ns2) adds goal N to the list of (lists of) negations Ns1,
   giving Ns2. Duplicates are ignored, but N must be fully bound.
*/

add_neg(N,Ns,Ns) :- member(N,Ns), !.
add_neg(N,Ns,[N|Ns]).


/* append_negs is just append, but ignoring duplicates. */

append_negs([],[],[]).
append_negs([N|Ns1],Ns2,Ns4) :- add_neg(N,Ns2,Ns3), append(Ns1,Ns3,Ns4).


% redundant
delete_abdemo(X,[],[]).  
delete_abdemo(X,[X|L],L) :- !.
delete_abdemo(X,[Y|L1],[Y|L2]) :- delete_abdemo(X,L1,L2).


% Tail recursive count 
count([],Result, Result). 
count([Head|Tail], Accum, Result) :- 
 IncAccum is Accum+1,
 count(Tail,IncAccum,Result).

listlength( [Head|Tail], Result ) :-
 count([Head|Tail],0,Result). 


/* Skolemisation */

skolemise_time(T) :- notrace((ignore((var(T), gensym(t,T))))).


opposite(neg(F),F) :- !.
opposite(F,neg(F)).


ec_trace(on,0).

:- include('abdemo_tests.pl').

