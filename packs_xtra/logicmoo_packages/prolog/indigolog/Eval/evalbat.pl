%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE      : Eval/evalbat.pl
%
%       Basic Action Projector (BAT)
%
%  AUTHOR : Sebastian Sardina (prev. Hector Levesque & Maurice Pagnucco)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
%  This file allows for the projection of conditions wrt
%  basic action theories.
%
%  The main tool provided in this file is the following predicate:        
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
% -- handle_sensing(+A, +H, +S, -H2) 
%           H2 is H plus action A with sensing result S
% -- debug(+A, +H, -S)
%           perform debug tasks with current action A, sensing outcome S,
%           and history H
% -- system_action(+A)       
%           action A is an action used by the system (e.g., action outcomes e(_,_))
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
% -- domain(-V, +D)       
%           object V is an element of domain D
% -- rdomain(-V, +D)       
%           object V is an element of domain D (random)
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
%								(at least 1 clause is required)
% -- rel_fluent(fluent)     : for each relational fluent (non-ground)
%								(at least 1 clause is required)
% -- cache(fluent)          : fluent should be cached (at least 1) MANDATORY
%
%           e.g., rel_fluent(painted(C)).
%           e.g., fun_fluent(color(C)).
%
% -- prim_action(action)    : for each primitive action (ground)
% -- exog_action(action)    : for each exogenous action (ground)
%
%           e.g., prim_action(clean(C)) :- domain(C,country).
%           e.g., exog_action(painte(C,B)):- domain(C,country), domain(B,color).
%
% -- senses(action,fluent)  : for each sensing action that senses fluent directly
%
%           e.g, senses(check_painted(C),  painted(C)).
%           e.g, senses(senseTemperature,  temp).
%
% -- senses(action,outcome,fluent,value,cond) :
%			for each sensing action that senses fluent *indirectly*
%
%           e.g, senses(senseTemperature, T, temp, T, true).
%				: equivalent to senses(senseTemperature,  temp).
%           e.g, senses(senseTemperature, T, temp, T2, T2 is T+10).
%           e.g, senses(senseTemperature, T, isHot, true,  T>30).
%           e.g, senses(senseTemperature, T, isHot, false, T<=30).
%
% -- forget(action,fluent)  : action makes fluent unknown
%
%           e.g, poss(checkFloor,  lightFloor).
% -- poss(action,cond)      : when cond, action is executable
%
%           e.g, poss(clean(C),   and(painted(C),holding(cleanear))).
%
% -- initially(fluent,value): fluent has value in S0 (ground)
%								(at least 1 clause is required)
%
%          e.g., initially(painted(C), false):- domain(C,country), C\=3.
%                initially(painted(3), true).
%                initially(color(3), blue).
%
% -- causes_val(action,fluent,value,cond)
%          when cond holds, doing act causes functional fluent to have value
%
%            e.g., causes_val(paint(C2,V), color(C), V, C = C2).
%               or causes_val(paint(C,V), color(C), V, true).
%
% -- causes_true(action,fluent,cond)
%          when cond holds, doing act causes relational fluent to hold
% -- causes_false(action,fluent,cond)
%          when cond holds, doing act causes relational fluent to not hold
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
%
% A high-level program-controller is described with:
%
% -- proc(name,P): for each procedure P 
% -- simulator(N,P): P is the N exogenous action simulator
%
% The interface for real-world execution is described with:
%
% -- actionNum(action, num)  
%         action has RCX code num
% -- simulateSensing(action)
%         sensing result for action should be asked to the user
% -- translateSensing(action, sensorValue, sensorResult) 
%         translate the sensorValue of action to sensorResult
% -- translateExogAction(codeAction, action) 
%         translateSensing action name into codeAction and vice-versa
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
   currently/2,	% Used to store the actual initial fluent values
   simulator/2,	% There may be no simulator
   senses/2,
   senses/5,	% There may be no sensing action
   forget/2,	% There may be no action that "forgets" a fluent
   has_valc/3.	% used for caching some values

% Predicates that they have definitions here but they can defined elsewhere
:- multifile(prim_action/1).
:- multifile(causes_val/4).
%:- multifile(causes_true/3).
%:- multifile(causes_false/3).
%:- multifile(exog_action/1).
:- multifile(poss/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  PREDICATES TO BE EXPORTED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   /* Move initially(-,-) to currently(-,-) and clear exog actions  */
initializeDB:- 
	retractall(currently(_,_)), 
	initially(F,V),
	assert(currently(F,V)),
	clean_cache,	
	fail.
initializeDB.

  /* Clean all the currently(-.-) predicates */
%finalizeDB:-  retractall(currently(_,_)), clean_cache.
finalizeDB.

% eval(P,H,B): this is the interface of the projector
eval(P,H,true):- holds(P,H).

% Change the history H to encode the sensing result of action A at H
%handle_sensing(A,H,Sr,[e(F,Sr)|H]):- senses(A,F). (OLD WAY)
handle_sensing(A,H,Sr,[e(A,Sr)|H]). 


% clean_cache: remove all has_valc/3
clean_cache :- retractall(has_valc(_,_,_)).

% Set F to value V at H, return H1 (add e(F,V) to history H)
assume(F,V,H,[e(F,V)|H]).

% system_action/1 defines actions that are used by the projector for managment
system_action(e(_,_)). 

% Action A is a sensing action
sensing(A,_):- senses(A,_) ; senses(A, _, _, _, _).

% sensed(+A,?V,+H): action A got sensing result V w.r.t. history H
sensed(A,V,[e(F,V2)|_]):- senses(A,F), !, V=V2.
sensed(A,V,[_|H])      :- sensed(A,V,H).

% domain/2: assigns a user-defined domain to a variable. 
domain(V, D)  :- getdomain(D, L), member(V, L).
rdomain(V, D) :- getdomain(D, L), shuffle(L,L2), !, member(V, L2).

% L is the list-domain associated to name D
getdomain(D, L) :- is_list(D) -> L=D ; (P =.. [D,L], call(P)).

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
%  OTHER PREDICATES NEEDED BUT NOT EXPORTED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A primitive fluent is either a relational or a functional fluent
prim_fluent(P):- rel_fluent(P) ; fun_fluent(P).

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
checkgr(P):- ground(P)-> true ; once(warn(['CWA applied to formula: ',P])).


% Update the cache information by stripping out the subhistory H
update_cache(H) :-
	retract(has_valc(F, V, H2)),
	append(H1, H, H2),
	assert(has_valc(F, V, H1)),
	fail.
update_cache(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  HOLDS - Here starts the evaluation procedure for projection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% kwhether(F,H): fluent F is known true or false in H
% Assumes that after sensing F, F may change but it will remain known
% We may probably want to add some "forgeting" mechanism.. either by a 
%      state condition or special actions
holds(kwhether(F),[])     :- !, initially(F,_).
holds(kwhether(F),[Act|_]):- (senses(Act,F) ; Act=e(F,_)), !.
holds(kwhether(F),[_|H])  :- holds(kwhether(F),H).

% know(F): Fluent F evaluates to something
holds(know(F),H)     :- !, holds(F=_,H).

% holds(P,H): P holds in H
holds(and(P1,P2),H)	:- !, holds(P1,H), holds(P2,H).
holds(or(P1,P2),H)	:- !, (holds(P1,H) ; holds(P2,H)).
holds(neg(P),H)	:- !, checkgr(P), \+ holds(P,H). /* Negation by failure */
holds(some([],P),H)	:- !, holds(P,H).
holds(some([V|L],P),H)	:- !, holds(some(V,some(L,P)), H).
holds(some([],P),H)	:- !, holds(P,H).
holds(some([V|L],P),H)	:- !, holds(some(V,some(L,P)), H).
holds(some(V,P),H)	:- !, subv(V,_,P,P1), holds(P1,H).
holds(some((V,D),P),H) :- !, domain(O,D), subv(V,O,P,P1), holds(P1,H).
holds(all([],P),H)	:- !, holds(P, H).
holds(all([V|L],P),H)	:- !, holds(all(V,all(L,P)), H).
holds(all((V,D),P),H)	:- !, holds(neg(some(V,D),neg(P)), H).
holds(impl(P1,P2),H) :- !, holds(or(neg(P1),P2),H).
holds(P,H) :- proc(P,P1), !, (ground(P) -> (holds(P1,H), !) ; holds(P1,H)).
holds(P,H) :- ground(P), rel_fluent(P), !, subf(P,true,H), !.
holds(P,H) :- \+ \+ rel_fluent(P), !, rel_fluent(P), subf(P,true,H).
holds(P,H) :- subf(P,P1,H), (ground(P) -> (call(P1), !) ; call(P1)).

       /*  P2 is P1 with all fluents replaced by their values at H */
subf(P1,P2,_)  :- (var(P1) ; number(P1)), !, P2 = P1.
subf(P1,P2,H)  :- atom(P1), !, subf2(P1,P2,H).
subf(P1,P2,H)  :- P1=..[F|L1], subfl(L1,L2,H), P3=..[F|L2], subf2(P3,P2,H).

subf2(P3,P2,H) :- prim_fluent(P3), has_value(P3,P2,H).
subf2(P2,P2,_) :- \+ prim_fluent(P2).

subfl([],[],_).
subfl([T1|L1],[T2|L2],H) :- subf(T1,T2,H), subfl(L1,L2,H).


%
% has_value(F,V,H): Fluent F has value V at history H
%
has_value(F,V,H) :- ground(F) -> (has_valg(F,V,H), !) ; has_valo(F,V,H).

% has_valg/3: check cache, then normal query (for ground queries)
has_valg(F,V,H)  :- cache(F), !,
	(has_valc(F,V,H) -> true ; (has_val(F,V,H),  assert(has_valc(F,V,H))) ).
has_valg(F,V,H)  :- has_val(F,V,H), !.  % F is a fluent with NO cache

% has_valo/3: check cache, then normal query (for ground queries)
has_valo(F,V,H)  :- cache(F), !,
	(has_valc(F,V,H) ; (has_val(F,V,H),  \+ has_valc(F,V,H),  assert(has_valc(F,V,H)))).
has_valo(F,V,H)  :- has_val(F,V,H).  % F is a fluent with NO cache


% has_val/3: the usual way of reasoning using regression and sensing
has_val(F,V,[])		:- currently(F,V).
has_val(F,V,[A|H])	:- sets_val(A,F,V,H).
has_val(F,V,[A|H])	:- \+ forget(A,H,F), has_value(F,V,H), \+ sets_val(A,F,_,H).

sets_val(e(F,V),F,V,_)	:- prim_fluent(F), !.  		% Fluent V is explicitly set by e(_,_)
sets_val(e(A,V),F,V,_)	:- senses(A,F).	% Action A sets F directly
sets_val(e(A,V),F,V2,H)	:- !, senses(A,V,F,V2,P), holds(P,H). % A sets F indirectly
sets_val(A,F,V,H)	:- causes_val(A,F,V,P), holds(P,H).   % Non-sensing reasoning

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
%  There are 3 parameters specified by roll_parameters(L,N,M).
%     L: the history has to be longer than this, or dont bother
%     M: if the history is longer than this, forced roll
%     N: the length of the tail of the history to be preserved
%		(set N=0 to never roll forward)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic temp/2.         % Temporal predicate used for rolling forward

%roll_parameters(1,1,0).  % Never roll forward
roll_parameters(20,40,5).

can_roll(H) :- roll_parameters(L,_,N), length(H,L1), L1 > L, N>0.
must_roll(H) :- roll_parameters(_,M,N), length(H,L1), L1 > M, N>0.

% H1 is the current history (H1 = H2 + H3)
% H2 will be the new history
% H3 is the tail of H1 that is going to be dropped
roll_db(H1,H2) :- 
	roll_parameters(_,_,N), 	
	split(N,H1,H2,H3), 
	preserve(H3),
	update_cache(H3).	    % Update the cache wrt the preserved history H3

	/* split(N,H,H1,H2) succeeds if append(H1,H2,H) and length(H1)=N. */
split(0,H,[],H).
split(N,[A|H],[A|H1],H2) :- N > 0, N1 is N-1, split(N1,H,H1,H2).

% preserve(H) : rolls forward the initial database from [] to H
preserve([]).
preserve([A|H]) :- 
	preserve(H), 
	roll_action(A), 
	update_cache([A]).

% roll_action(A): roll currently/2 database with respect to action A
roll_action(A) :-
	sets_val(A, F, V, []),
	prim_fluent(F),
	(\+ temp(F, V) -> assert(temp(F, V)) ; true),
	fail.
roll_action(_) :-
	retract(temp(F,V)),
	retractall(currently(F,_)),	% There should be just one currently/2 for F!
	assert(currently(F,V)),
	fail.
roll_action(_).




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
% EOF: Eval/evalbat.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
