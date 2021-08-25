%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE    : Eval/eval_know.pl
%
%       Possible value BAT evaluator
%	(This is just a prototype and it may contain bugs and problems)
%	
%  AUTHOR : Sebastian Sardina & Stavros Vassos 
%             Based also on Hector Levesque KPlanner from IJCAI-05
%  EMAIL  : {ssardina,stavros}@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%
%  Copyright (C): 1999-2005, University of Toronto
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%           This file allows for the projection of conditions wrt
% 	    basic action theories with possible values.
%
%   The main tool provided in this file is the following predicate:
%
% -- eval(P,H,B):  B=true/false/unknown is the truth value of P at history H 
%
%           For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             March, 2002
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
% 
%        Do not distribute without permission.
%        Include this notice in any copy made.
% 
% 
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
% 
%                          All Rights Reserved
% 
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This file provides the following:
%
% -- eval(P, H, B)  (MAIN PREDICATE, used by the transition system)
%           B is the truth value of P at history H 
%
% SYSTEM TOOLS (used by the top-level cycle)::
%
% -- initializeDB/0
%           initialize the projector
% -- finalizeDB/0
%           finalize the projector
% -- can_roll(+H1) 
%       check if the DB CAN roll forward
% -- must_roll(+H1) 
%       check if the DB MUST roll forward
% -- roll_db(+H1,-H2) 
%       perform roll forward with current history H1 and new history H2
% -- actionolling(+H1, -H2) 
%           mandatory roll forward of history H1 into new history H2
% -- handle_sensing(+A, +H, +S, -H2) 
%           H2 is H plus action A with sensing result S
% -- debug(+A, +H, -S)
%           perform debug tasks with current action A, sensing outcome S,
%           and history H
% -- system_action(+A)       
%           action A is an action used as a specific tool for the projector
%
%
% OTHER TOOLS (used by the transition system)::
%
% -- sensing(+A, -L)
%           action A is a sensing action with a list L of possible outcomes
% -- sensed(+A, +S, +H)
%           action A, when executed at history H, got sensing result S
% -- inconsistent(+H)
%           last action turned history H inconsistent, i.e., impossible 
% -- rdomain(-V, +D)       
%           object V is an element of domain D
% -- rdomain(-V, +D)       
%           object V is an element of domain D (random way)
% -- getdomain(+D, -L)   
%            L is the list representing domain D
% -- calc_arg(+A1, -A2, +H)  
%           action A2 is action A1 with its arguments replaced wrt history H
% -- before(+H1, +H2)
%           history H1 is a previous history of H2
% -- assume(+F, +V, +H1, -H2) 
%           H2 is the history resulting from assuming fluent F to 
%           have value V at history H1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  A basic action theory (BAT) is described with:
%
% -- fun_fluent(fluent)     : for each functional fluent (non-ground)
% -- cache(fluent)          : fluent should be cached (at least 1)
%
%           e.g., fun_fluent(color(C)).
%
% -- prim_action(action)    : for each primitive action (ground)
% -- exog_action(action)    : for each exogenous action (ground)
%
%           e.g., prim_action(clean(C)) :- domain(C,country).
%           e.g., exog_action(painte(C,B)):- domain(C,country), domain(B,color).
%
% -- senses(action,fluent)  : for each sensing action
%
%           e.g, poss(check_painted(C),  painted(C)).
%
% -- forget(action,fluent)  : action makes fluent unknown
%
%           e.g, poss(checkFloor,  lightFloor).
% -- poss(action,cond)      : when cond, action is executable
%
%           e.g, poss(clean(C),   and(painted(C),holding(cleanear))).
%
% -- initially(fluent,value): fluent has value in S0 (ground)
%
%          e.g., initially(painted(C), false):- domain(C,country), C\=3.
%                initially(painted(3), true).
%                initially(color(3), blue).
%
% -- causes_val_tt(action,sensing,fluent,value,cond)
%          when cond ek_holds, doing action with outcome sensing causes fluent 
%	   to have value
%
%            e.g., causes_val(paint(C2,V), color(C), V, C = C2).
%               or causes_val(paint(C,V), color(C), V, true).
%
% -- causes_true(action,fluent,cond)
%          when cond ek_holds, doing act causes relational fluent to hold
% -- causes_false(action,fluent,cond)
%          when cond ek_holds, doing act causes relational fluent to not hold
%
%            e.g., causes_true(paint(C2,_), painted(C), C = C2).
%               or causes_true(paint(C,_), painted(C), true).
%            e.g., causes_false(clean(C2),  painted(C), C = C2).
%               or causes_false(clean(C),  painted(C), true).
%
% -- sort-name(domain_of_sort).      : defines a sort
%        e.g., color([blue, green, yellow, red]).       
%              temperature([-30..45]).
%
% Requirements:
%
% -- is_list(+L) : L is a list
% -- subv(X1,X2,T1,T2) :  T2 is T1 with X1 replaced by X2
% -- multifile/1
% -- get0/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% :- module(evalbat,
%%           [eval/3,
%%            initializeDB/0,
%%            finalizeDB/0,
%%            handle_sensing/4,
%%            sensing/2,
%%            sensed/3,
%%            domain/2,
%%            getdomain/2,
%%            calc_arg/3,
%%            before/2,
%%            inconsistent/1,
%%            assume/4
%%           ]).
%% 
%% :- use_module(library(quintus)).

:- dynamic 
   currently/2,    % Used to store the actual initial fluent values
   simulator/2,    % There may be no simulator
   senses/2,       % There may be no exogenous action simulator
   forget/2,        % There may be no action that "forgets" a fluent
   has_valc/3.     % used for caching some values

% Predicates that they have definitions here but they can defined elsewhere
:- multifile(prim_action/1).
:- multifile(causes_val/4).
%:- multifile(exog_action/1).
:- multifile(poss/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Predicates to be exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   /* Move initially(-,-) to currently(-,-) and clear exog actions  */
initializeDB:- 
	retractall(currently(_,_)), 
	forall(initially(F,V), assert(currently(F,V))),
	clean_cache.

  /* Clean all the currently(-.-) predicates */
%finalizeDB:-  retractall(currently(_,_)), clean_cache.
finalizeDB.		% Leave the current beliefs and all the cache information there...

% eval(P,H,B): this is the interface of the projector
eval(P,H,true):- kholds(P,H).

% Change the history H to encode the sensing result of action A at H
% handle_sensing(A,H,Sr,[e(F,Sr)|H]):- senses(A,F). (OLD WAY)
handle_sensing(A,[A|H],Sr,[e(A,Sr),A|H]):- senses(A).


% clean_cache: remove all has_valc/3
clean_cache :- retractall(has_valc(_,_,_)).

% Set F to value V at H, return H1 (add e(F,V) to history H)
assume(F,V,H,[e(F,V)|H]).

% system_action/1 defines actions that are used by the projector for managment
system_action(e(_,_)). 

% Action A is a sensing action
sensing(A,_):- senses(A).

% sensed(+A,?V,+H): action A got sensing result V w.r.t. history H
sensed(A,V,[e(F,V2)|_]):- senses(A,F), !, V=V2.
sensed(A,V,[_|H])      :- sensed(A,V,H).

% domain/2: assigns a user-defined domain to a variable. 
%domain(V, D)  :- getdomain(D, L), member(V, L).
%rdomain(V, D) :- getdomain(D, L), shuffle(L,L2), !, member(V, L2).
domain(V, D)  :- is_list(D) -> member(V, D) ; apply(D,[V]).
rdomain(V, D) :- (is_list(D) -> L=D ; bagof(P,apply(D,[P]),L)), 
				 shuffle(L,L2), !, member(V, L2).

% ***** to go
% L is the list-domain associated to name D
%getdomain(D, L) :- is_list(D) -> L=D ; (P =.. [D,L], call(P)).

% Computes the arguments of an action or a fluent P
% Action/Fluent P1 is action/fluent P with all arguments evaluated 
calc_arg(P,P1,H):- (is_an_action(P) ; prim_fluent(P)),
	(atomic(P)-> P1=P ;
                    (P =..[Function|LArg], subfl(LArg,LArg2,H), 
                     P1=..[Function|LArg2])).

% History H1 is a previous history of H2
before(H1,H2):- append(_,H1,H2).

% No action can make a history inconsistent (simplification)
inconsistent(_):- fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Other predicates neede but not exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A primitive fluent is either a relational or a functional fluent
prim_fluent(P):- fun_fluent(P).

% Check if A has "the form" of a primitive action, though, its arguments
% may need to be evaluated yet
% We need to do  this hack because actions are defined all ground
is_an_action(A):- \+ \+ (prim_action(A) ; exog_action(A)), !.
is_an_action(A):- \+ atomic(A),
	           A =..[F|Arg], length(Arg,LArg), length(ArgV,LArg),
                   NA =..[F|ArgV], (prim_action(NA) ; exog_action(A)).

% Simulation of an action A has the same effects as action A itself
causes_val(sim(A),F,V,C)  :- !, causes_val(A,F,V,C).

% Build causes_val/4 for relational fluents
causes_val(A,F,true,C)  :- causes_true(A,F,C).
causes_val(A,F,false,C) :- causes_false(A,F,C).

% Abort if P is not grounded (to use before negations as failure)
checkgr(P):- ground(P)-> true ; warn(['CWA applied to formula: ',P]).

% Update the cache information by stripping out the subhistory H
update_cache(H) :-
	retract(has_valc(F, V, H2)),
	append(H1, H, H2),
	assert(has_valc(F, V, H1)),
	fail.
update_cache(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Evaluation procedure for projection (START)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------
% kholds(+P,+H): P is known to be true at H (i.e., P ek_holds at H)
% This is guarranteed to be sound P only when P is ground. Free vars are 
% allowed and in some special cases. ***
%---------------------------------------------------------------------------
kholds(P,H) :-	ground(P) ->  
			\+ ek_holds(neg(P),H) 
		;
			(warn(['kholds/2 called with open variables: ',P]), 
			ek_holds(P,H), 
			\+ ek_holds(neg(P),H)).	

%kholds(P,H) :- 	ground(P) -> 
%			( !, \+ ek_holds(neg(P),H), (ek_holds(P,H) -> true ;
%			(write('AAAAAAAAAAAA!:'),write(P),write('@'),writeln(H),halt)))
%		;
%			ek_holds(P,H), \+ ek_holds(neg(P),H).

%---------------------------------------------------------------------------
% ek_holds(+P,+H): P ek_holds in H (i.e., P is possibly true at H)
%---------------------------------------------------------------------------
holds(P,H):-ek_holds(P,H).

% negation normal form transformation
ek_holds(neg(or(P1,P2)),H)   :- !, ek_holds(and(neg(P1),neg(P2)),H). 	/* Loyd-Topor Transf */
ek_holds(neg(and(P1,P2)),H)  :- !, ek_holds(or(neg(P1),neg(P2)),H).  	/* Loyd-Topor Transf */
ek_holds(neg(neg(P)),H)      :- !, ek_holds(P,H). 			/* Loyd-Topor Transf */
ek_holds(neg(all(V,D,P)),H)  :- !, ek_holds(some(V,D,neg(P)),H).     	/* Loyd-Topor Transf */
ek_holds(neg(some(V,D,P)),H) :- !, ek_holds(all(V,D,neg(P)),H).      	/* Loyd-Topor Transf */
ek_holds(neg(impl(P1,P2)),H) :- !, ek_holds(and(P1,neg(P2)),H).      	/* Loyd-Topor Transf */
ek_holds(neg(equiv(P1,P2)),H):- !, ek_holds(or(and(P1,neg(P2)),and(neg(P1),P2)),H).
ek_holds(neg(P),H):- proc(P,P1), !, ek_holds(neg(P1), H).

% implication as a macro
ek_holds(impl(P1,P2),H)  	:- !, ek_holds(or(neg(P1),P2),H).
ek_holds(equiv(P1,P2),H) 	:- !, ek_holds(and(impl(P1,P2),impl(P2,P1)),H).

% non-atomic formulas
ek_holds(and(P1,P2),H)  	:- !, ek_holds(P1,H), !, ek_holds(P2,H).
ek_holds(or(P1,P2),H)   	:- !, ((ek_holds(P1,H),!) ; (ek_holds(P2,H),!)).
ek_holds(some(V,D,P),H)    :- !, domain(O,D), subv(V,O,P,P1), ek_holds(P1,H).
ek_holds(all(V,D,P),H)     :- !, \+((domain(O,D), subv(V,O,P,P1), \+ ek_holds(P1,H))).
ek_holds(P,H)           	:- proc(P,P1), !, ek_holds(P1,H).


%---------------------------------------------------------------------------
% Evaluation of ground atoms. Atoms are either equality (fluent) atoms or 
% prolog predicates possibly mentioning ground fluents.
%---------------------------------------------------------------------------
% if it's a prolog predicate then use good-old subf ***
% if it's a ground equality atom then optimize a bit ***
% ola ayta 8a allajoyn me ta kainoyria domains poy 8a dhlwnontai jexwrista apo to onoma *****
ek_holds(neg(P),H):- !, subf(P,P1,H), \+ call(P1).


%% This is a special optimized case when evaluating Fluent = Value so as to
%% feed has_value/3 with the exact match that can guide de search
ek_holds(T1=T2,H) :- ground(T1), ground(T2), 
		  liftAtom(T1, NameT1, ArgT1, LiftT1),  
		  liftAtom(T2, NameT2, ArgT2, LiftT2),
		  ( (prim_fluent(LiftT1), \+ prim_fluent(LiftT2), !,
		     subf(ArgT1,ArgT1Eval,H),
		     T1Eval =..[NameT1|ArgT1Eval],
		     has_value(T1Eval,T2,H)
		     )
		  ;
		    (prim_fluent(LiftT2), \+ prim_fluent(LiftT1), !,
		     subf(ArgT2,ArgT2Eval,H),
		     T2Eval =..[NameT2|ArgT2Eval],
		     has_value(T2Eval,T1,H)
		     )
		   ).
ek_holds(P,H) :- !, subf(P,P1,H), call(P1).

liftAtom(Atom, NameA, ArgA, LiftedAtom) :-
	Atom =..[NameA|ArgA],
	templist(ArgA,ArgAVars), 
	LiftedAtom =..[NameA|ArgAVars]. 
	
liftAtom2(Atom, NameA, ArgA, LiftedAtom) :-
	Atom =..[NameA|ArgA],
	length(ArgA, L),
	length(ArgAVars, L),
	LiftedAtom =..[NameA|ArgAVars]. 
	

% templist(X,Y) : X and Y are lists of the same length; Y used to return a list of variables of size |X|
templist([],[]) :- !.
templist([_],[_]) :- !.
templist([_,_],[_,_]) :- !.
templist([_,_,_],[_,_,_]) :- !. 
templist([_,_,_,_],[_,_,_,_]) :- !.
templist([_,_,_,_,_],[_,_,_,_,_]) :- !.
templist([_,_,_,_,_,_],[_,_,_,_,_,_]) :- !.
templist([_,_,_,_,_,_,_],[_,_,_,_,_,_,_]) :- !.
templist([_|R1],[_|R2]) :- templist(R1,R2).


%---------------------------------------------------------------------------
% subf(+P1,?P2): P2 is P1 with all fluents replaced by a possible value at H
%---------------------------------------------------------------------------
subf(P1,P2,_)  :- (var(P1) ; number(P1)), !, P2 = P1.
subf(now,H,H)  :- !.
subf(m(F),L,H) :- !, setof(V1,has_value(F,V1,H),L). 
subf(i(F),V,_) :- !, currently(F,V).

subf(P1,P2,H)  :- atom(P1), !, subf2(P1,P2,H).
subf(P1,P2,H)  :- P1=..[F|L1], subfl(L1,L2,H), P3=..[F|L2], subf2(P3,P2,H).
subf2(P3,P2,H) :- prim_fluent(P3), has_value(P3,P2,H).
subf2(P2,P2,_) :- \+ prim_fluent(P2).

subfl([],[],_).
subfl([T1|L1],[T2|L2],H) :- subf(T1,T2,H), subfl(L1,L2,H).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of top-level has_value/3
% has_value(+F,?V,+H): V is a possible value for F at history H  (top-level predicate)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_value(F,V,H) :- 
	ground(F) ->
		has_valgc(F,V,H) 
	; 
		(warn(['has_value/3 called with an open variable fluent: ',F]), has_valo(F,V,H)).

% has_valgc/3: implements caching for ground fluents
% if it is a cached fluent and there is no cache store, then compute all the cached values
% if it is a cached fluent and there is cache info, then bind those values
% it it is not a cached fluent, then just use normal regression via has_valg/3
has_valgc(F,V,H)  :- cache(F), \+ has_valc(F,_,H), 
		    has_valg(F,V,H), assert(has_valc(F,V,H)), fail.
has_valgc(F,V,H)  :- cache(F), !, has_valc(F,V,H).
has_valgc(F,V,H)  :- has_valg(F,V,H).  % F is a fluent with NO cache


% This is the case when F is contains some free variable (e.g., open(X))
% *SV*: We do not handle free variables any more (for this version at least)
has_valo(F,V,H):- has_valg(F,V,H).


% has_val(F,V,H) ek_holds if V is a possible value for fluent F at history H 
%  (proven by regression)
%
% This is the case when F is a ground fluent (e.g., open(3))
% has_valg/3 is guarranteed to work reasonably only if the following are true  
% - when a causes(A,F,_,_) exists then the next values for F will be 
%   determined by the set of causes(A,F,_,_)  which cover all the logical
%   space, i.e.  \land_i \lnot C_i is unsatisfiable
% - sensing actions and physical actions are disjoint
% - causes/4: C is a conjunction of possibly negated ground atoms.
% - settles/5: C is a conjunction of possibly negated ground atoms.
% - rejects/5: C is a general formula with V as the only free variable
% - settles and rejects do not overlap
has_valg(F,V,[])	:- currently(F,V).
has_valg(F, V, [A|H]):- sets_val(A, F, _, H), !, sets_val(A, F, V, H).
has_valg(F, V, [e(A,S), A|H]):- !, has_value(F,V,H), \+ (rejects(A,S,F,V,C), kholds(C,H)).
has_valg(F, V, [_|H]):- has_value(F,V,H).

% First try if  F is defined by causes/4, then by settles/5
sets_val(A,F,V,H)		:- causes(A,F,_,_), !, causes(A,F,V,C), ek_holds(C,H).
sets_val(e(A,S),F,V,[A|H])	:- settles(A,S,F,V1,C), kholds(C,H), !, V=V1. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  End of has_value/3 implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% So far, one forgets the value of F when it is sensed (may be improved)
forget(Act, _, F) :- forget(Act, F).

% Special high-level actions to set and unset fluent F: set(F) and unset(F)
prim_action(set(_)).
prim_action(unset(_)).
poss(set(F), ground(F)).
poss(unset(F), ground(F)).
has_val(F,V,[set(F)|_])  :- !, V=true.
has_val(F,V,[unset(F)|_]):- !, V=false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  ROLL DATABASE FORWARD
%
%  Rolling forward means advancing the predicate currently(-,-) and
%  discarding the corresponding tail of the history.
%  There are 3 parameters specified by roll_parameters(L,M,N).
%     L: the history has to be longer than this, or dont bother
%     M: if the history is longer than this, forced roll (M >= L)
%     N: the length of the tail of the history to be preserved
%		(set N=0 to never roll forward)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic temp/2.         % Temporal predicate used for rolling forward

% roll_parameters(1,1,0).  % Never roll forward
roll_parameters(30,40,15).

can_roll(H) :- roll_parameters(L,_,N), length(H,L1), L1 > L, N>0.
must_roll(H) :- roll_parameters(_,M,N), length(H,L1), L1 > M, N>0.

% H1 is the current history (H1 = H2 + H3)
% H2 will be the new history
% H3 is the tail of H1 that is going to be dropped
roll_db(H1,H2) :- 
	roll_parameters(_,_,N), 
	split(N,H1,H2,H3),
        report_message(system(3), ['(DB) ', 'Progressing the following sub-history: ', H3]), 
	preserve(H3),
        report_message(system(3), ['(DB) ', 'Updating cache...']), 
	update_cache(H3),	    % Update the cache information
        report_message(system(3), ['(DB) ', 'Subhistory completely rolled forward']).

      /* split(N,H,H1,H2) succeeds if append(H1,H2,H) and length(H1)=N. */
split(0,H,[],H).
split(N,[A|H],[A|H1],H2) :- N > 0, N1 is N-1, split(N1,H,H1,H2).

% preserve(H) : rolls forward the initial database currently/2 from [] to H
preserve([]).
preserve([A|H]) :- preserve(H), roll_action(A), update_cache([A]).


%%%%%%%%%%% THIS NEEDS SUBSTANTIAL MORE WORK, IT IS FULLY UNTESTED!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% roll_action(A): Roll Currently/2 database with respect to action A
roll_action(e(A,S)) :- 
	settles(A, S, F, V, C),			% A may settle F	
	prim_fluent(F),				
	kholds(C, []),				% Value of F is settled to V
	retractall(currently(F, _)),		
	assert(currently(F, V)),		% Update value of F to unique value V
	fail.
roll_action(e(A,S)) :- 				% A may reject F
	rejects(A, S, F, V, C),			
	prim_fluent(F),				
	currently(F,V),				% choose a potential value V for rejection
	kholds(C, []),				% V should be rejected!
	retractall(currently(F, V)),		% then, retract V from F
	fail.
roll_action(A) :- \+ A=e(_,_),			% A may affect F
	causes(A, F, _, _),					
	prim_fluent(F),				
	roll_action_fluent(A, F),
	fail.
roll_action(_).
 

% Fluent F requires update wrt executed action A
% OBS: At this point F may still contain free var
roll_action_fluent(A, F) :-
	has_value(F, V, [A]),		% compute one possible value for F (now F is ground)
	(\+ temp(F, V) -> assert(temp(F, V)) ; true), % if new value, put it in temp/2
	fail.
roll_action_fluent(_, F) :-		% now update currently/2 with the just computed temp/2
	temp(F, _),
	retractall(currently(F,_)),	% F needs a full update, remove all currently/2
		% Next obtain all values stored in temp/2 for that specific ground F
	temp(F,V),			% Get a new possible value (maybe many, backtrack)
	assert(currently(F,V)),		% Set the new possible value in currently/2
	retract(temp(F,V)),		% Remove the new possible value from temp/2
	fail.
roll_action_fluent(_, _).
		




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEBUG ROUTINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% debug(+Action, +History, -SensingResult): 
% If Action=debug then a snapshot of the system is printed out
% Otherwise, the sendRcxActionNumber/2
%     predicate failed (RCX panicked or there was a problem with the
%     communication). This predicate attempts to provide some basic debug
%     and error recovery.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debug(debug, History, _) :- !,
    write('-------------------------------------------------------------'), nl,
    write('********* A SNAPSHOT OF THE SYSTEM WAS REQUESTED ************'), nl,
    errorRecoveryData(History),
    write('-------------------------------------------------------------'), nl.

debug(Action, History, SensingResult) :-
    write('** There is a problem with the RCX. It may need to be reset.'), nl,
    errorRecoveryData(History),
    errorRecoveryProc,
    execute(Action, History, SensingResult). % Try action again

% errorRecoveryData(+History): Extract values of primitive fluents at
%     the point where Hist actions are performed.
errorRecoveryData(History) :-
    write('    Actions performed so far: '),
    write(History), nl,
    bagof(U, prim_fluent(U), FluentList),
    printFluentValues(FluentList, History).

% printFluentValues(+FluentList, +History): Print value of primitive fluents
%     at the point where History actions have been performed
printFluentValues([], _).

printFluentValues([Hf | FluentList], History) :-
    (has_value(Hf, Hv, History),    % Print all instances of Hf 
     write('    PRIMITIVE FLUENT '),
     write(Hf),
     write(' HAS VALUE '),
     write(Hv), nl, fail) ; 
    printFluentValues(FluentList, History). % Continue with other fluents

% errorRecoveryProc: What to do in case of error. In this case, ask the user
%     to reposition the RCX so that last action can be re-attempted
errorRecoveryProc:-
    write('If you wish to abort, enter "a".'), nl,
    write('If you wish to continue execution, place RCX in a position'), nl,
    write('consistent with these values and hit any other key.'), nl,
    get0(Val),
    get0(_),                     % Clear carriage return
    (Val == 65; Val == 97) ->    % 65 is ASCII 'A', 97 is ASCII 'a'
         abort;
         true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Eval/eval_know.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
