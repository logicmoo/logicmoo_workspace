% -*- Mode: Prolog -*-
% cilog.pl  logic engine for Computational Intelligence: A Logical Approach
% Copyright (C) 1997-2006, David Poole.
% This is subject to the GPL. See user manual for details.
% Designed for SWI Prolog

cilog_version('2.1.1',2006).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          SYSTEM DEPENDENT CODE               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These should be the only changes that need to be made to make sure that it
%    runs.  Cilog has only been tested with SWI
%    Prolog and Sicstus Prolog (a long time ago). The only other
%    change may be the initialization on the last line of the program.

%%%%% Sicstus Specific Code
%% Comment this out if you are not using Sicstus Prolog
/*
% get_runtime(T,Units) gets accumulated runtime from the system
get_runtime(T,ms) :-   statistics(runtime,[T,_]).      % SICSTUS

:- prolog_flag(syntax_errors,_,dec10).

init :- start.
*/
%%%%% SWI Prolog Specific Code
%% Comment this out if you are not using SWI Prolog
%/*

%% get_runtime(T,Units) gets accumulated runtime from the system
get_runtime(T,U) :-   catch((statistics(cputime,T),U=secs),_,
			    (statistics(runtime,[T,_]),U=ms)).	% SWI Prolog


%init :- writeallonline(['Type "start." to start CILog.']),nl.
%at_initalization(start).
%*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          OPERATOR DEFINITIONS                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% <- is the "if"
:- op(1150, xfx, <- ).
% <= is the base-level "if" (it is not used here, but can be used in
% programs). 
:- op(1120, xfx, <= ).
% "&" is the conjunction.
:- op(950,xfy, &).
% "~" is the negation
:- op(900,fy,~).
% "\=" is inequality
:- op(700,xfx,\=).
:- op(800,xfx,:).

:- op(1170, fx, tell).
:- op(1170, fx, ask).
:- op(1170, fx, whynot).
:- op(1170, fx, how).
:- op(1170, fx, how).
:- op(1170, fx, help).
:- op(1170,fx,load).
:- op(1170,fx,prload).
:- op(1170,fx,bound).
:- op(1170,fx,stats).
:- op(1170,fx,listing).
:- op(1170,fx,clear).
:- op(1170,fx,askable).
:- op(1170,fx,assumable).
:- op(1170,fx,prob).
:- op(1170,fx,observe).
:- op(1170,fx,unobserve).
:- op(1170,fx,predict).
:- op(1170,fx,deterministic).

:- dynamic (<-)/2.
:- dynamic failed/1.
:- dynamic depth_bound/1.
:- dynamic askabl/1.
:- dynamic assumabl/1.
:- dynamic choice/3.
:- dynamic alt/1.
:- dynamic asked/2.
:- dynamic debugging_failure/0.
:- dynamic answer_found/0.
:- dynamic checking_runtime/0.
:- dynamic lastruntime/1.
:- dynamic deter/1.
:- dynamic explanations/3.
:- dynamic nogood/2.
:- dynamic anogood/2.

depth_bound(30).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            CILOG Help
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help_message(['
CILOG Version ',V,' Help','

A clause is of the form','
  Head <- Body','
  Head','
Head is an atom.','
Body is an expression made up of','
   Atom','
   ~ B1           negation as finite failure','
   B1 & B2        conjunction B1 and B2','
where B1 and B2 are expressions.','

***** Basic commands:','

tell CLAUSE.           add the clause to the knowledge base','
askable Atom.          makes Atom askable.','

ask QUERY.             ask if expression QUERY is a consequence of the KB','
whynot QUERY.          find out why QEURY failed.','

bound N.               set search depth-bound to N (default=30)','
listing.               list all clauses in the knowledge base.','
listing H.             list clauses in the knowledge base with head H.','
askables.              list all askables.','
clear.                 clear the knowledge base','
clear H.               clear clauses with head H from knowledge base','
check.                 check the knowledge base for undefined predicates','
help.                  print this help message','
help advanced.         prints help on abductive and probabilistic reasoning','
stats runtime.         turn on runtime statistics','
quit.                  quit cilog','
prolog.                exit to Prolog. Type "start." to start cilog again.','

***** Input/Output','

load ''Filename''.      load the clauses in file Filename','
prload ''Filename''.    load the clauses in Filename, using Prolog''s syntax.']) :-
   cilog_version(V,_).

help_message(advanced,[
'CILOG Version ',V,' Advanced Help','

This help is for abductive and probabilistic reasoning. This is not
useful if you are a beginner interested in deductive reasoning.

assumable Atom.        makes atom assumable.','
prob a:p.              makes atom a probabilistic with probability p.','
prob a1:p1,a2:p2,...,ak:pk.  makes atoms ai probabilistic with probability pi.','
                       a''s are exclusive and covering; the p''s must sum to 1.','

assumables.            list all assumables.','
probs.                 list all prob assertions.','

create_nogoods         construct nogoods.','
nogoods                list nogoods.','

observe QUERY.         observe that QUERY is true','
predict QUERY.         predict whether QUERY is true','
observations.          list all observations made','
unobserve.             undo last observation','
unobserve all.         undo all observations','
explanations.          describe the explanationss of the observations','
worlds.                describe the worlds of the observations']) :-
   cilog_version(V,_).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            CILOG Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpret(C) :-
   file_or_command(C,command),!.

interpret(help) :- !,
   help_message(Help),
   writeallonline(Help).
interpret(help What) :-
   help_message(What,Help),!,
   writeallonline(Help).
interpret(help What) :- !,
   writeallonline(['No help on "',What,'". Type "help." for more information.']).

interpret((tell _ :- _)) :- !,
   writeallonline(['Illegal rule: ":-" is not a legal implication. Use "<-".']).
interpret((tell C)) :- !,
   tell_clause(C).

interpret((ask Q)) :- !,
   retractall(debugging_failure),
   retractall(answer_found),
   depth_bound(DB),
   askq(Q,DB).

interpret((whynot Q)) :- !,
   depth_bound(DB),
   whynotb(Q,DB,_,ass([],[],1),_).

interpret(clear) :- !,
   retractall((_ <- _)),
   retractall(assumabl(_)),
   retractall(alt(_)),
   retractall(choice(_,_,_)), 
   retractall(askabl(_)),
   retractall(asked(_,_)),
   retractall(nogood(_,_)),
   retractall(anogood(_,_)).

interpret((clear H)) :- !,
   retractall((H <- _)),
   retractall(assumabl(H)),
   ( alt(A) , member(H:_, A)
      ->
       retract(alt(A)),
       forall(member(G:_, A), retract(choice(G,_,_)))
   ; true),
   retractall(askabl(H)),
   retractall(asked(H,_)).

interpret((stats Cmd)) :- !,
   stats_command(Cmd).

interpret((prload File)) :- !,
   prolog_load(File).

interpret((load File)) :- !,
   ci_load(File).

interpret((bound)) :- !,
   depth_bound(N),
   writeallonline(['Current depth bound is ',N]).

interpret((bound N)) :- !,
   (integer(N) ->
      retract(depth_bound(_)),
      asserta(depth_bound(N))
   ; writeallonline(['Depth bound must be an integer'])).

interpret(quit) :-
   throw(quit).

interpret(check) :- !,
   check.

interpret((observe G)) :- !,
   observe(G).

interpret((predict G)) :- !,
   predict(G).

interpret(askables) :- 
   askabl(G),
   writeallonline(['askable ',G,'.']),
   fail.
interpret(askables) :- !.

interpret(assumables) :- 
   assumabl(G),
   writeallonline(['assumable ',G,'.']),
   fail.
interpret(assumables) :- !.

interpret(probs) :-
   alt(L),
   (L=[A:P1,~A:_]
     ->
      writeallonline(['prob ',A,':',P1,'.'])
   ;
     write('prob '),
     writeAllDelim(L,', '),
     write('.'),
     nl
   ),
   fail.
interpret(probs) :- !.

interpret((listing)) :- !,
   interpret((listing _)),
   interpret(askables),
   interpret(assumables),
   interpret(probs).

interpret((listing H)) :-
   (H <- B),
   (B = true 
   -> writeallonline([H,'.'])
   ; writeallonline([H,' <- ',B,'.'])
   ),
   fail.
interpret((listing _)) :- !.

interpret(unobserve all) :- !,
   retractall(explanations(_,_,_)).

interpret(unobserve) :- !,
   (retract(explanations(_,_,_))
     ->
      true
    ;
      writeallonline(['No observations to undo.'])
   ).

interpret(observations) :- !,
   (explanations(Obs,_,_) ->
      print_observations(Obs,_)
   ;  writeallonline(['There are no observations.'])).

interpret(explanations) :- !,
   (explanations(_,ETs,_) ->
      print_explanations(ETs,0)
   ;  writeallonline(['There are no observations.'])).

interpret(worlds) :- !,
   (explanations(Obs,Expls,Pr) ->
	extract_expls(Expls,Sexpls),
	make_disjoint(Sexpls,Worlds),
	show_worlds(Worlds,0),
	writeallonline(['P(',Obs,') = ',Pr])
   ;  writeallonline(['There are no observations.'])).

interpret(create_nogoods) :- !,
   depth_bound(DB),
   construct_nogoods(DB).

interpret(nogoods) :- !,
    list_nogoods.
    
interpret((A <- B)) :- !,
   writeallonline(['Illegal command, ',(A <- B),'. You have to "tell" a clause.']).

interpret(C) :-
   writeallonline(['Unknown Command, ',C,'. Type "help." for help.']).

% listifyProbs(St,L0,L1,S0,S1)
%    St is a probability structure 
%    L1-L0 is thhe list corresponding to the structure
%    S1-S0 is the sum of the probabilities
listifyProbs((X,Y),L0,L2,S0,S2) :- !,
    listifyProbs(X,L0,L1,S0,S1),
    listifyProbs(Y,L1,L2,S1,S2).
listifyProbs((A:P),L0,[A:P|L0],S0,S1) :-
%    atomic(A),     % we want it to mean its a cilog atom
%    number(P),
    P >= 0,
    S1 is S0+P.

% file_or_command(C,W) C is a command that can be in a file or on the
% command line, W is either command or file 
file_or_command((askable G),_) :- !,
   assertz(askabl(G)).

file_or_command((assumable G),_) :- !,
   assertz(assumabl(G)).

file_or_command((prob A:P),W) :- !,
   (number(P), P>=0, P=<1 ->
      P1 is 1-P,
      assertz(alt([A:P,~A:P1])),
      assertz(choice(A,P,[~A])),
      assertz(choice(~A,P1,[A]))
   ;
       var(P) ->
             assertz(alt([A:P,~A:(1-P)])),
      assertz(choice(A,P,[~A])),
      assertz(choice(~A,(1-P),[A]))

   ;
     (W=file 
      ->
     writeallonline(['Error: ',(prob A:P)])
     ; true),
     writeallonline(['Probability must be a number in range [0,1]'])
   ).

file_or_command((prob APs),W) :- !, 
   ( listifyProbs(APs,[],L,0,Sum), Sum > 0.9999999999999, Sum< 1.0000000000001
    ->
      assertz(alt(L)),
      make_choices(L)
   ;
     (W=file 
      ->
     writeallonline(['Error: ',(prob APs)])
     ; true),
      writeallonline(['Error: command should be: prob a1:p1,a2:p2,...,ak:pk.']),
      writeallonline(['       the pi''s must be nonnegative and sum to 1.0'])
  ).

file_or_command((deterministic G),_) :- !,
   assertz(deter(G)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            ASKING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

askq(Q,_) :-
   illegal_body(Q,Why),!,
   writeallonline(['Illegal query: '|Why]).
askq(Q,DB) :-
   retractall(failed(_)),
   assert(failed(naturally)),
   reset_runtime,
   solve(Q,DB,Res,[wrule(yes,true,Q,true)],ass([],[],1),Ass),
   (answer_found -> true ; assert(answer_found)),
   mynumbervars((Q,Res,Ass),0,_),
   report(Q,Res,Ass).
askq(Q,_) :- 
   failed(naturally),!,
   (answer_found
    ->  writeallonline(['No more answers.'])
    ; writeallonline(['No. ',Q,' doesn''t follow from the knowledge base.'])),
   report_runtime.
askq(Q,DB) :- !,
   ask_failing(Q,DB).

ask_failing(Q,DB) :-
   writeallonline(['Query failed due to depth-bound ',DB,'.']),
   report_runtime,
   writel(['     [New-depth-bound,where,ok,help]: ']),
   flush_and_read(Comm),
   interpret_failing_question(Comm,Q,DB).

% interpret_failing_question(Comm,Q,DB).
interpret_failing_question(help,Q,DB) :- !,
   writeallonline([
    '     Give one of the following commands:',nl,
    '        an integer > ',DB,'      to use this depth bound.',nl,
    '           (use "bound N." to set it permanently).',nl,
    '        where.  to explore the proof tree where the depth-bound was reached.',nl,
    '        ok.     to return to the cilog prompt.',nl,
    '        help.   to print this message.']),
   ask_failing(Q,DB).
interpret_failing_question(end_of_file,_,_) :- !.
interpret_failing_question(ok,_,_) :- !.
interpret_failing_question(where,Q,DB) :-
   assert(debugging_failure),
   askq(Q,DB).
interpret_failing_question(Comm,Q,_) :-
   integer(Comm),!,
   askq(Q,Comm).
interpret_failing_question(Comm,Q,DB) :-
   writeallonline(['   Unknown command, ',Comm,' type "help." for help.']),
   ask_failing(Q,DB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            TELLING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tell_clause((~ H <- B)) :- !,
   writeallonline(['Illegal rule: ~',H,' <- ',B,'.',nl,
      'You cannot have negation in the head of a clause.']).
tell_clause((H1 & H2 <- B)) :- !,
   writeallonline(['Illegal rule: ',H1 & H2,' <- ',B,'.',nl,
      'You cannot have a conjunction in the head of a clause.']).
tell_clause((H1 , H2 <- B)) :- !,
   writeallonline(['Illegal rule: ',H1 ,',', H2,' <- ',B,'.',nl,
      'You cannot have a "," in the head of a clause.']).
tell_clause((H <- B)) :-
   !,
   ( builtin(H,_) ->
     writeallonline([H,' is built-in. It cannot be redefined.'])
   ; illegal_body(B,Why) ->
     writeallonline(['Illegal rule: ',H,'<-',B,'. '|Why])
   ; assertz((H <- B))).
tell_clause((H :- B)) :-
   !,
   writeallonline(['Illegal rule: ',H,':-',B,'. ":-" is not a legal implication. Use "<-".']).
tell_clause((A, B)) :-
   !,
   writeallonline(['Error: ',(A,B),' is not a valid clause.']).
tell_clause((A & B)) :-
   !,
   writeallonline(['Error: ',(A&B),' is not a valid clause.']).
tell_clause((~A)) :-
   !,
   writeallonline(['Error: ',~A,' is not a valid clause.']).
tell_clause(H) :-
   builtin(H,_),!,
   writeallonline([H,' is built-in. It cannot be redefined.']).
tell_clause(H) :-
   !,
   assertz((H <- true)).

illegal_body(X,['Variables cannot be atoms. Use call(',X,').']) :- var(X).
illegal_body((_,_),[' "," is not a legal conjunction. Use "&".']).
illegal_body((\+ _),[' "\+" is not a legal negation. Use "&".']).
illegal_body(!,[' "!" is not supported.']).
illegal_body([_|_],[' Lists cannot be atoms.']).

illegal_body((A&_),Why) :-
   illegal_body(A,Why).
illegal_body((_&A),Why) :-
   illegal_body(A,Why).
illegal_body((A;_),Why) :-
   illegal_body(A,Why).
illegal_body((_;A),Why) :-
   illegal_body(A,Why).
illegal_body((~A),Why) :-
   illegal_body(A,Why).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            META-INTERPRETER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve(Goal,DepthBnd,PfTree,WhyTree,Ass0,Ass1) 
%  Goal is the goal to be proved
%  DepthBnd is the depth bound
%  PfTree is the proof tree returned
%  Whytree is a list of wrule(Head,LeftTree,Current,RightBody)
%  Ass0 is the assumables coming in. 
%       This is structure of the form ass(As,PAs,P)
%  Ass1 is the assumables coming out
solve(true,_,true,_,Ass,Ass) :- !.
solve((A&B),N,(AT&BT),[wrule(H,LT,_,RB)|WT],A0,A2) :- !,
   solve(A,N,AT,[wrule(H,LT,A,(B&RB))|WT],A0,A1),
   solve(B,N,BT,[wrule(H,(LT&AT),B,RB)|WT],A1,A2).
solve(A \= B,_,if(A \= B,builtin),_,A0,A0) :- !,
   dif(A,B).
solve(call(G),_,_,WT,_,_) :- 
   var(G),!,
   debug_error(['   Error: argument to call must be bound when evaluated.'],
	       WT).
solve(call(G),N,T,WT,A0,A1) :- !,
   solve(G,N,T,WT,A0,A1).
solve((~ G),N,if(~G,T),WT,A0,A1) :- !,
	(ground(G) ->
	  failtoprove(G,N,T,WT,A0,A1)
	;
	  debug_error(['   Error: ',~G,' needs to be ground when called.'],
	        WT)).

solve(G,_,if(G,assumed),_,ass(A0,PAs,P),ass(A0,PAs,P)) :-
   assumabl(G),
%   (ground(G) -> true
%   ; debug_error(['   Error: assumable ',G,' needs to be ground when called.'],
%		 WT)),
   idmember(G,A0),!.
solve(G,_,if(G,assumed),_,ass(A0,PAs,P),ass(A1,PAs,P)) :-
   assumabl(G),
%   (ground(G) -> true
%   ; debug_error(['   Error: assumable ',G,' needs to be ground when called.'],
%		 WT)),
%   \+ idmember(G,A0),
   good(G,A0),
   insert_into_sorted_list(G,A0,A1).
solve(G,_,if(G,assumed),WT,ass(A0,PAs0,P0),ass(A0,PAs1,P1)) :-
   choice(G,P,A),
   (ground(G) -> true
   ; debug_error(['   Error: assumable ',G,' needs to be ground when called.'],
		 WT)),
   insertUpdateProbs(PAs0,P0,G,P,PAs1,P1),
   consistent(PAs0,A).
solve(G,_,if(G,asked),WT,A0,A0) :-
   askabl(G),
   ask_user(G,WT,Ans),
   Ans \== unknown,!,      % fail if Ans=unknown, otherwise don't try clauses
   Ans=yes.
solve(H,_,if(H,builtin),WT,A0,A0) :-
   builtin(H,C),!,
   (C ->  catch(H,_,debugging_builtin_error(H,WT))
   ; writeallonline(['Error: "',H,'" can''t be called in this form, as ',C,' isn''t true.']), 
     debugging_builtin_error(H,WT)
   ).
solve(H,N0,if(H,BT),WT,A0,A1) :-
   N0 > 0,
   deter(H),
   N1 is N0-1,
   (H <- B),
   solve(B,N1,BT,[wrule(H,true,B,true)|WT],A0,A1), !.
solve(H,N0,if(H,BT),WT,A0,A1) :-
   N0 > 0,
   N1 is N0-1,
   (H <- B),
   solve(B,N1,BT,[wrule(H,true,B,true)|WT],A0,A1).
solve(H,0,if(H,asked),WT,A0,A0) :-
   debugging_failure,!,
   debugging_failure_goal(H,WT).
solve(_,0,_,_,_,_) :-
   retractall(failed(_)),
   assert(failed(unnaturally)),
   fail.

% report(Query,Res,Ass)
report(Q,Res,ass([],[],1)) :- !,
   writeallonline(['Answer: ',Q,'.']),
   report_runtime,
   writel(['  [ok,more,how,help]: ']),
   flush_and_read(Comm),
   interpret_report(Comm,Q,Res,ass([],[],1)).
report(Q,Res,Ass) :-
   writeallonline(['Answer: ',Q,'.']),
   ( Ass=ass(A0,[],1) ->
       writeallonline(['Assuming: ',A0,'.'])
   ;   writeallonline(['Assuming: ',Ass,'.'])),
   report_runtime,
   writel(['  [more,ok,how,help]: ']),
   flush_and_read(Comm),
   interpret_report(Comm,Q,Res,Ass).

% interpret_report(Comm,Q,Res,Ass)
interpret_report(ok,_,_,_) :- !.
interpret_report(more,_,_,_) :- !,
   reset_runtime,fail.
interpret_report(end_of_file,_,_,_) :- !,
   writeallonline(['^D']).
interpret_report(how,Q,Res,Ass) :- !,
   traverse(Res,Rep),
   ( (Rep = top; Rep=up)
   -> report(Q,Res,Ass)
   ; Rep= retry
   -> reset_runtime,fail
   ; Rep=prompt
   -> true
   ; writeallonline(['This shouldn''t occur. Traverse reported ',Rep])
   ).
interpret_report(help,Q,Res,Ass) :- !,
   writeallonline([
     '  The system has proven an instance of your query.',nl,
     '  You can give the following commands:',nl,
     '    more.    for more answers',nl,
     '    ok.      for no more answers',nl,
     '    how.     to enter a dialog to determine how the goal was proved.']),
   report(Q,Res,Ass).
interpret_report(Comm,Q,Res,Ass) :-
   Comm \== more,
   writeallonline(['Unknown command; ',Comm,'. Type "help." if you need help.']),
   report(Q,Res,Ass).

% Depth-bound reached
debugging_failure_goal(H,WT) :-
   writeallonline(['  Depth-bound reached. Current subgoal: ',H,'.']),
   writel(['     [fail,succeed,proceed,why,ok,help]: ']),
   flush_and_read(Comm),
   interpret_debugging_failure_command(Comm,H,WT).

interpret_debugging_failure_command(help,H,WT) :- !,
   writeallonline([
     '  The system has reached the depth bound.',nl,
     '  You can give the following commands:',nl,
     '    fail.       fail this subgoal.',nl,
     '    succeed.    make this subgoal succeed.',nl,
     '    proceed.    fail and don''t stop any more at failing subgoals.',nl,
     '    why.        determine why this subgoal was called.',nl,
     '    ok.         return to cilog prompt.',nl,
     '    help.       print this message.']),
   debugging_failure_goal(H,WT).
interpret_debugging_failure_command(fail,_,_) :- !,
   retractall(failed(_)),
   assert(failed(unnaturally)),
   fail.
interpret_debugging_failure_command(succeed,_,_) :- !.
interpret_debugging_failure_command(proceed,_,_) :- !,
   retractall(debugging_failure),
   retractall(failed(_)),
   assert(failed(unnaturally)),
   fail.
interpret_debugging_failure_command(ok,_,_) :- !,
   throw(prompt).
interpret_debugging_failure_command(end_of_file,_,_) :- !,
   throw(prompt).
interpret_debugging_failure_command(why,H,WT) :- !,
   \+ \+ (mynumbervars(WT,0,_),why_question(WT,_)),
   debugging_failure_goal(H,WT).
interpret_debugging_failure_command(Comm,H,WT) :- !,
   writeallonline(['  Unknown command: ',Comm,'. Type "help." for help.']),
   debugging_failure_goal(H,WT).


% builtin(G,C) is true if goal G is defined in the system (as opposed to 
% being defined in clauses). C is the condition that must hold to make sure
% there are no errors.
builtin((A =< B), ground((A =< B))).
builtin((A >= B), ground((A >= B))).
builtin((A =\= B), ground((A =\= B))).
builtin((A < B), ground((A<B))).
builtin((A > B), ground((A>B))).
builtin((_ is E),ground(E)).
builtin(number(E),ground(E)).

% Error in a built-in
debugging_builtin_error(H,WT) :-
   debug_error(['  Error in built-in predicate: ',H,'.'],WT).

% RUNTIME ERROR HANDLING
debug_error(Message,WT) :-
   writeallonline(Message),
   writel(['     [fail,succeed,why,ok,help]: ']),
   flush_and_read(Comm),
   interpret_error_command(Comm,Message,WT).

interpret_error_command(help,Message,WT) :- !,
%   writeallonline(Message),
   writeallonline([
      '  You can give the following commands:',nl,
     '    fail.       fail this subgoal.',nl,
     '    succeed.    make this subgoal succeed.',nl,
     '    why.        determine why this subgoal was called.',nl,
     '    ok.         return to cilog prompt.',nl,
     '    help.       print this message.']),
   debug_error(Message,WT).
interpret_error_command(fail,_,_) :- !,
   fail.
interpret_error_command(succeed,_,_) :- !.
interpret_error_command(ok,_,_) :- !,
   throw(prompt).
interpret_error_command(end_of_file,_,_) :- !,
   throw(prompt).
interpret_error_command(why,H,WT) :- !,
   \+ \+ (mynumbervars(WT,0,_),why_question(WT,_)),
   debug_error(H,WT).
interpret_error_command(Comm,H,WT) :- !,
   writeallonline(['  Unknown command: ',Comm,'. Type "help." for help.']),
   debug_error(H,WT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            ASK THE USER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  ask_user(G,WT,Ans)
%     G is the goal to ask
%     WT is the why-tree
%     Ans is the reply
ask_user(G,_,Ans) :-
   ground(G),
   asked(G,Ans1),!,Ans=Ans1.
ask_user(G=V,_,yes) :-
   ground(G),
   asked(G=V1,yes),!,V=V1.

ask_user(T=V,WT,Ans) :-
   ground(T),var(V),!,
   writel(['What is the value of ',T,'? [value,unknown,why,help]: ']),
   flush_and_read(Reply),
   (member(Reply,[unknown,why,help])
     -> interpret_ask_answer(Reply,T=V,WT,Ans)
    ;  V=Reply,
       interpret_ask_answer(yes,T=V,WT,Ans)
   ).
ask_user(G,WT,Ans) :-
   ground(G),!,
   writel(['Is ',G,' true? [yes,no,unknown,why,help]: ']),
   flush_and_read(Rep),
   interpret_ask_answer(Rep,G,WT,Ans).
  
ask_user(G,WT,fail) :-
   writeallonline(['   Error: Askables with free variables not implemented.',nl,
            '   The system wanted to ask ',G,'.',nl,
            '   Entering why interation.']),
   mynumbervars(WT,0,_),
   why_question(WT,_),
   writeallonline(['   Askable subgoal ',G,' failing due to free variables.']).


% interpret_ask_answer(Rep,G,WT,Ans).
interpret_ask_answer(help,G,WT,Rep) :- !,
   writeallonline(['   The system is asking whether ',G,' is true or not. Give one of:',nl,
     '      "yes." if ',G,' is known to be true.',nl,
     '      "no." if ',G,' is known to be false.',nl,
     '      "unknown." if ',G,' is unknown (so applicable clauses can be used).',nl,
     '      "fail." to fail the subgoal (but not record an answer).',nl,
     '      "why." to see why the system was asking this question.',nl,
     '      "prompt." to return to the cilog prompt.',nl,
     '      "help." to print this message.']),
   ask_user(G,WT,Rep).
interpret_ask_answer(yes,G,_,yes) :- !,
   assertz(asked(G,yes)).
interpret_ask_answer(no,G,_,no) :- !,
   assertz(asked(G,no)).
interpret_ask_answer(unknown,G,_,unknown) :- !,
   assertz(asked(G,unknown)).
interpret_ask_answer(fail,_,_,fail) :- !.
interpret_ask_answer(prompt,_,_,_) :- !,
   throw(prompt).
interpret_ask_answer(end_of_file,_,_,_) :- !,
   writeallonline(['^D']),
   throw(prompt).

interpret_ask_answer(why,G,WT,Rep) :- !,
   \+ \+ ( mynumbervars(WT,0,_),why_question(WT,Rep),Rep \== prompt),
   ask_user(G,WT,Rep).
interpret_ask_answer(Ans,G,WT,Rep) :-
   Ans \== fail,
   writeallonline(['   Unknown response "',Ans,'". For help type "help.".']),
   ask_user(G,WT,Rep).


% why_question(WT,Rep)
%  WT is a list of wrule(Head,LeftTree,Current,RightBody)
%     Rep is the reply. It is one of:
%        down       go back one step
%        bottom     return to the ask-the-user query
%        prompt     return to prompt
why_question([wrule(H,LT,C,RB)|WT],Rep) :-
   writeallonline(['   ',C,' is used in the rule ']),
   writeallonline(['   ',H,' <-']),
   print_tree_body(LT,1,Max),
   writeallonline(['   ** ',Max,': ',C]),
   M1 is Max+1,
   print_body(RB,M1,_),
   writel(['   [Number,why,help,ok]: ']),
   flush_and_read(Comm),
   interpret_why_ans(Comm,Max,[wrule(H,LT,C,RB)|WT],Rep).
why_question([],down) :-
   writeallonline(['   This was the original query.']).

% interpret_why_ans(Comm,[wrule(H,LT,C,RB)|WT],Rep).
interpret_why_ans(help,Max,[wrule(H,LT,C,RB)|WT],Rep) :- !,
   writeallonline([
'   You can taverse the proof for a subgoal using following commands:',nl,
'      how i.     show how element i (i<',Max,') of the body was proved.',nl,
'      how ',Max,'.     show the rule being considered for ',C,'.',nl,
'      why.       show why ',H,' is being proved.',nl,
'      prompt.    return to the cilog prompt.',nl,
'      help.      print this message.']),
   why_question([wrule(H,LT,C,RB)|WT],Rep).
interpret_why_ans(up,_,WT,Rep) :- !,
   interpret_why_ans(why,_,WT,Rep).
interpret_why_ans(why,_,[WR|WT],Rep) :- !,
   why_question(WT,Rep1),
   (Rep1 = down
   -> why_question([WR|WT],Rep)
   ; Rep=Rep1).
interpret_why_ans((how N),Max,[wrule(H,LT,C,RB)|WT],Rep) :-
   integer(N),!,
   interpret_why_ans(N,Max,[wrule(H,LT,C,RB)|WT],Rep).
interpret_why_ans(N,Max,[wrule(H,LT,C,RB)|WT],Rep) :-
   integer(N),
   N > 0,
   N < Max, !,
   nth(LT,1,_,N,E),
   traverse(E,Rep1),
   (Rep1=up -> why_question([wrule(H,LT,C,RB)|WT],Rep) ;
    Rep1=top -> Rep=bottom ;
    Rep1=retry
     -> writeallonline(['   retry doesn''t make sense here.']),
        why_question([wrule(H,LT,C,RB)|WT],Rep) ;
    Rep=Rep1).
interpret_why_ans(Max,Max,_,down) :- !.
interpret_why_ans(N,Max,WT,Rep) :-
   integer(N),
   N > Max, !,
   writeallonline(['You can''t trace this, as it hasn''t been proved.']),
   why_question(WT,Rep).
interpret_why_ans(end_of_file,_,_,_) :- !,
   writeallonline(['^D']),
   throw(prompt).
interpret_why_ans(prompt,_,_,_) :- !,
   throw(prompt).
interpret_why_ans(ok,_,_,bottom) :- !.

interpret_why_ans(Comm,_,WT,Rep) :- !,
   writeallonline(['Unknown command: ',Comm,'. Type "help." for help.']),
   why_question(WT,Rep).


% print_body(B,N) is true if B is a body to be printed and N is the 
% count of atoms before B was called.
print_body(true,N,N) :- !.
print_body((A&B),N0,N2) :- !,
   print_body(A,N0,N1),
   print_body(B,N1,N2).
print_body(H,N,N1) :-
   writeallonline(['      ',N,': ',H]),
   N1 is N+1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            NEGATION AS FAILURE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% failtoprove(G,N,T,WT,A0,A1)  
% G is a ground goal, 
% N is a depth-bound
% T is the tree returned
% WT is the why tree
% A0 is the assumables before and A1 is the set after
% The complication here is due to the interaction with the depth-bound
% and assumables

failtoprove(G,N,nafproof(PA1,ETs),WT,ass(A,PAs,Pr),ass(A,PA1,Pr1)) :-
    failed(HowFailedInit),
    retractall(failed(_)),
    assert(failed(naturally)),
    findall(et(Ass,T),solve(G,N,T,~WT,ass(A,PAs,Pr),Ass),ETs),
    (failed(naturally) ->
	extract_expls(ETs,EGs),
	duals(EGs,PAs,[PAs],Ds),
	member(PA1,Ds),
	prob(PA1,1,Pr1)
    ;
    member(et(ass(A,PAs,Pr),_),ETs) ->  % found a proof with no assumptions 
	retractall(failed(_)),
	assert(failed(HowFailedInit)),
	fail			% fail naturally
    ;
	fail  % fail unnaturally as depth-bound was hit
    ).

/* interaction with depth-bound
    (failed(naturally) ->
       (solve(G,N,_,WT,[],_) ->
         retract(failed(unnaturally)), asserta(failed(naturally)),fail
              % note this just fails if failed(naturally) still true
       ; failed(naturally)
       )
    ;  retract(failed(_)),
       assert(failed(naturally)),
       (solve(G,N,_,WT,[],_) ->
         retract(failed(naturally)), asserta(failed(unnaturally)),fail
       ; retract(failed(naturally)), asserta(failed(unnaturally))
              % note this just fails if failed(unnaturally) still true
       )).
*/


% prob(AL,P0,P1) 
%  AL is a list of probabilistic assumables
%  P1 = P0*P(AL)
prob([],P,P).
prob([H|T],P0,P2) :-
   choice(H,P,_),
   P1 is P0*P,
   prob(T,P1,P2).


% The following code gives the interaction between negation and probabilities

% A composite choice is a consistent set of probabilistic assumables.

% duals(Es,R0,D0,D1) is true if Es is a list of terms of the form
% ass(A,PAs,Pr) where R0 is a subset of PAs, and 
% D1-D0 is a list of ass(A1,R1,P1) such that
% R1-R0 is a hitting set of negations of Es.

duals([],_,D,D).
duals([S|L],R0,D0,D2) :- !,
   setDiff(S,R0,NA),
   hit_every_complement(NA,D0,[],D1),
   duals(L,R0,D1,D2).

% hit_every_complement(S,R0,D0,D,D1) is true if S is a composite choice (with
% tail R0), and D2 is D together with the hitting set of negations of
% D0.

hit_every_complement([],_,D0,D0) :- !.
hit_every_complement([A|R],Ds,D0,D2) :-
   choice(A,_,NAs),
   hit_every_element(NAs,Ds,D0,D1),
   hit_every_complement(R,Ds,D1,D2).

% hit_every_element(As,A,D0,D1) is true if S is a composite choice (with
% tail R0), and D2 is D together with the hitting set of negations of
% D0.

hit_every_element([],_,D,D).
hit_every_element([E|T],Es,D0,D2) :-
   insert_into_each_exp(Es,E,D0,D1),
   hit_every_element(T,Es,D1,D2).

% insert_into_each_exp(L,A,L0,L1) is true if inserting atomic choice A
% into each explanation in L, and adding these to L0 produces L1. L,
% L0 and L1 are all lists of explanations. Subsumed and inconsistent
% explanations are removed. 

insert_into_each_exp([],_,L,L) :-!.
insert_into_each_exp([E1|R],A,L0,L1) :-
   incompatible(A,E1),!,
   insert_into_each_exp(R,A,L0,L1).
insert_into_each_exp([E1|R],A,L0,L2) :-
   insert_into_sorted_list(A,E1,E2),
   insert_exp(E2,L0,L1),
   insert_into_each_exp(R,A,L1,L2).

% insert_exp(E,Es1,Es2) is true if inserting explanation E into list
% Es1 of explanations produces list of explanations Es2.
insert_exp(E,[],[E]) :- !.
insert_exp(E0,[E|T],[E|T]) :- 
    subset_sorted(E,E0),!.  % includes the case where E0==E.
insert_exp(E0,[E|T],R) :-
    subset_sorted(E0,E),!,
    insert_exp(E0,T,R).
insert_exp(E,[H|T],[H|R]) :-
    insert_exp(E,T,R).

% mutually_incompatible(As1,As2) is true if A is inconsistent with all of As.
mutually_incompatible(E1,E2) :-
   member(A,E1),
   incompatible(A,E2).

% incompatible(A,As) is true if A is inconsistent with all of As.
incompatible(A,E) :-
   choice(A,_,NAs),
   \+ consistent(E,NAs).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            Probabilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% make_choices(L)
make_choices(L) :-
    remove(A:P,L,L0),
    setof(AA,P1^member(AA:P1,L0),Alts),
    assertz(choice(A,P,Alts)),
    fail.
make_choices(_).

% insertUpdateProbs(PAs0,P0,G,P,PAs1,P1)
%   PAs0 is the initial probabilistic assumptions
%   P0 is the probability of the probablistic assumptions
%   G is the choice
%   P is the probability of G
%   PAs1 is the resulting probabilistic assumptions
%   P1 is thew resulting probabilities
insertUpdateProbs([],P0,G,P,[G],P1) :-
   P1 is P0*P.
insertUpdateProbs([G|R],P0,G,_,[G|R],P0) :- !.
insertUpdateProbs([G0|R0],P0,G,P,[G,G0|R0],P1) :-
   G @< G0, !,
   P1 is P0*P.
insertUpdateProbs([G0|R0],P0,G,P,[G0|PAs1],P1) :-
   insertUpdateProbs(R0,P0,G,P,PAs1,P1).

% latest_explanations(Obervations,ETs,Prob)
%    ETs is a list of et(Explanation,ProofTree)
latest_explanations(O,E,P) :-
   explanations(O,E,P),!.
latest_explanations([],[et(ass([],[],1),null)],1).

% predict(G)
predict(G) :-
   latest_explanations(_,ETs,Pr0),
   depth_bound(DB),
   reset_runtime,
   find_expls(G,ETs,DB,Expls),
   extract_expls(Expls,Sexpls),
   make_disjoint(Sexpls,DExpls),
   acc_probs(DExpls,0,Pr),
  report_expls(Expls,DExpls,G,Pr0,Pr).
predict(_) :-
    writeallonline(['No (more) answers.']).

% observe(G)
observe(G) :-
   retractall(debugging_failure),
   retractall(failed(_)),
   assert(failed(naturally)),
   latest_explanations(Obs,ETs,PrObs),
   depth_bound(DB),
   reset_runtime,
   find_expls(G,ETs,DB,Expls),
   extract_expls(Expls,Sexpls),
   make_disjoint(Sexpls,DExpls),
   acc_probs(DExpls,0,Pnew),
   asserta(explanations([G|Obs],Expls,Pnew)),
   report_expls(Expls,DExpls,G,PrObs,Pnew).
observe(_) :-
    writeallonline(['No (more) answers.']).

% find_expls(G,ETs,DB,Expls) 
find_expls(G,ETs,DB,Expls) :-
   bagof(et(Expl,Res),
         E0^Tr^( member(et(E0,Tr),ETs),
                 solve(G,DB,Res,[wrule(yes,true,G,true)],E0,Expl)), 
         Expls).

% report_expls(Expls,DExpls,G,Pr0,Pr1)
%    Ets is a list of et(Explanation,ProofTree)
%    DExpls is a set of possible worlds (disjoint explanations)
report_expls(Expls,DExpls,G,Pr0,Pr1) :-
   Post is Pr1/Pr0,
   writeallonline(['Answer: P(',G,'|Obs)=',Post,'.']),
   report_runtime,
   writel(['  [ok,more,explanations,worlds,help]: ']),
   flush_and_read(Comm),
   interpret_prob_report(Comm,Expls,DExpls,G,Pr0,Pr1).

% acc_probs(Expls,P0,P1)
acc_probs([],P,P).
acc_probs([A|R],P0,P2) :-
    prob_expl(A,1,P),
    P1 is P+P0,
    acc_probs(R,P1,P2).

% probs_expl(Expl,P0,P1)
prob_expl([],P,P).
prob_expl([A|R],P0,P2) :-
    choice(A,P,_),
    P1 is P*P0,
    prob_expl(R,P1,P2).

% interpret_prob_report(Comm,Expls,DExpls,G,Pr0,Pr1)
interpret_prob_report(ok,_,_,_,_,_) :- !.
interpret_prob_report(more,_,_,_,_,_) :- !,
   reset_runtime,fail.
interpret_prob_report(end_of_file,_,_,_,_,_) :- !,
   writeallonline(['^D']).
interpret_prob_report(explanations,Expls,DExpls,G,Pr0,Pr1) :- !,
   show_explanations(Expls,DExpls,G,Pr0,Pr1).
interpret_prob_report(worlds,Expls,DExpls,G,Pr0,Pr1) :- !,
   show_worlds(DExpls,0),
   report_expls(Expls,DExpls,G,Pr0,Pr1).
interpret_prob_report(help,Expls,DExpls,G,Pr0,Pr1) :- !,
   writeallonline([
     '  The system has proven an instance of your query.',nl,
     '  You can give the following commands:',nl,
     '    more.    for more answers',nl,
     '    ok.      for no more answers',nl,
     '    explanations.     to explore how the goal is proved based ',
     ' sets of assumptions.',nl,
     '    worlds.  to see the worlds where ',G,' is true.']),
   report_expls(Expls,DExpls,G,Pr0,Pr1).
interpret_prob_report(Comm,Expls,DExpls,G,Pr0,Pr1) :-
   Comm \== more,
   writeallonline(['Unknown command; ',Comm,'. Type "help." if you need help.']),
   report_expls(Expls,DExpls,G,Pr0,Pr1).

% show_explanations(Expls,DExpls,G,Pr0,Pr1)
show_explanations(Expls,DExpls,G,Pr0,Pr1) :-
   print_explanations(Expls,0),
   writel(['  [ok,more,how i,help]: ']),
   flush_and_read(Comm),
   interpret_show_explanations_report(Comm,Expls,DExpls,G,Pr0,Pr1).

% interpret_show_explanations_report(Comm,Expls,DExpls,G,Pr0,Pr1)
interpret_show_explanations_report(ok,Expls,DExpls,G,Pr0,Pr1) :- !,
    report_expls(Expls,DExpls,G,Pr0,Pr1).
interpret_show_explanations_report(more,_,_,_,_,_) :- !,
   reset_runtime,fail.
interpret_show_explanations_report(how I,Expls,DExpls,G,Pr0,Pr1) :-
   catch(
     (
        ith(Expls,et(_,Tree),I,0),
	traverse(Tree,_)  % Report is ignored
     ),
     ithexception(Ex),
     (Ex= outofrange ->
         writeallonline(['Number out of range: how ',I,'.'])
     ;   writeallonline(['Argument to how must be an integer: how ',I,'.']) 
     )
    ),
    report_expls(Expls,DExpls,G,Pr0,Pr1).
interpret_show_explanations_report(help,Expls,DExpls,G,Pr0,Pr1) :- !,
    writeallonline([
          'ok.      go back to answer dialogue',nl,
          'more.    get another answer.',nl,
          'how i.   ask how the ith explanation was proved',nl,
          'help.    print this message']),
    report_expls(Expls,DExpls,G,Pr0,Pr1).
interpret_show_explanations_report(Comm,Expls,DExpls,G,Pr0,Pr1) :- !,
    writeallonline([
          'Unknown command: ',Comm,'. Type "help." for help']),
    report_expls(Expls,DExpls,G,Pr0,Pr1).

% print_explanations(DExpls,Count).
print_explanations([],_).
print_explanations([et(W,_)|R],C) :-
   writeallonline(['  ',C,': ', W]),
   C1 is C+1,
   print_explanations(R,C1).

% show_worlds(DExpls,Count).
show_worlds([],_).
show_worlds([W|R],C) :-
   prob_expl(W,1,Pr),
   writeallonline(['  ',C,': ', W,'  Prob=',Pr]),
   C1 is C+1,
   show_worlds(R,C1).

% print_observations(Obs,N)
print_observations([],0).
print_observations([O|Os],N1) :-
    print_observations(Os,N),
    N1 is N+1,
    writeallonline([N1,': ',O]).


% extract_expls(L1,L2) L1 is a list of et(ass(_,E,_),_) and L2 is the
% corresponding list of explanations E. 
extract_expls([],[]).
extract_expls([et(ass(_,E,_),_)|L],R1) :-
    extract_expls(L,R),
    insert_exp(E,R,R1).

% make_disjoint(L,SL) is true if L is a list of explanations and SL
% is a lists of Expl such that SL are mutually exclusive and cover
% the same worlds as the Expls in L.
% This assumes that L is a minimal set: non member of L is a subset of another.

make_disjoint([],[]).
make_disjoint([R|L],L2) :-
   member(R1,L),
   \+ mutually_incompatible(R,R1),!,
   (member(E,R1), \+ member(E,R)
     -> true; writeallonline(['Error in make_disjoint: ',compatible(R,R1)]),fail),
   choice(E,_,NE),
   split([E|NE],R,L,L1),
   make_disjoint(L1,L2).
make_disjoint([E|L1],[E|L2]) :-
   make_disjoint(L1,L2).

% split(As,R,L0,L1) splits R on As, added to L0 produces L1
split([],_,L,L).
split([A|As],E,L0,L2) :-
   insert_into_sorted_list(A,E,E1),
   insert_exp(E1,L0,L1),
   split(As,E,L1,L2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            HOW QUESTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% traverse(T,Rep) true if 
%   T is a tree being traversed
%   Rep is the reply it is one of {up,top,retry,prompt}
%     up means go up in the tree 
%     top means go to the top of the tree (where traverse was called).
%     retry means find another proof tree
%     prompt means go to the top level prompt
traverse((A&B),Rep) :-
   traverse(if(yes,(A&B)),Rep).
traverse(if(H,true),up) :- !,
    writeallonline(['   ',H,' is a fact']).
traverse(if(H,builtin),up) :- !,
    writeallonline(['   ',H,' is built-in.']).
traverse(if(H,asked),up) :- !,
    writeallonline(['   You told me ',H,' is true.']).
traverse(if(H,assumed),up) :- !,
    writeallonline(['   ',H,' is assumed.']).
traverse(if(~G,nafproof(_,[])),Rep) :- !,
   writeallonline(['   ',G,' finitely failed. You can examine the search space.']),
   depth_bound(DB),
   whynotb(G,DB,Rep,ass([],[],1),_).
traverse(if(~G,nafproof(Expl,ETs)),Rep) :- !,
   writeallonline(['   ',~G,' succeeded assuming ',Expl]),
   writeallonline(['   ',G,' succeeded with the following explanations:']),
   print_explanations(ETs,0),
   writel(['  [up,top,retry,prompt,how i,help,whynot]: ']),
   flush_and_read(Comm),
   interpret_naf_how(Comm,ETs,Rep0),
   (Rep0=repeat ->
       traverse(if(~G,nafproof(Expl,ETs)),Rep)
   ; Rep=Rep0
   ).
traverse(null,top) :- !,
   writeallonline(['   There was nothing to be proved.']).
traverse(if(H,B),Rep) :-
    writeallonline(['   ',H,' <-']),
    print_tree_body(B,1,Max),
    writel(['   How? [Number,up,retry,ok,prompt,help]: ']),
    flush_and_read(Comm),
    interpretcommand(Comm,B,Max,if(H,B),Rep).

% print_tree_body(B,N) is true if B is a body to be printed and N is the 
% count of atoms before B was called.
print_tree_body(true,N,N).
print_tree_body((A&B),N0,N2) :-
   print_tree_body(A,N0,N1),
   print_tree_body(B,N1,N2).
print_tree_body(if(H,_),N,N1) :-
   writeallonline(['      ',N,': ',H]),
   N1 is N+1.

% interpretcommand(Comm,B,Max,Goal,Reply) interprets the command Comm on body B
interpretcommand(help,_,Max,G,Rep) :- !,
   writeallonline([
     '     Give either (end each command with a period):',nl,
     '       how i.       explain how subgoal i (i<',Max,') was proved.',nl,
     '       up.          go up the proof tree one level.',nl,
     '       retry.       find another proof.',nl,
     '       ok.          stop traversing the proof tree.',nl,
     '       prompt.      return to the cilog prompt.',nl,
     '       help.        to print this message.']),
   traverse(G,Rep).
interpretcommand((how N),B,Max,G,Rep) :-
   integer(N),
   interpretcommand(N,B,Max,G,Rep).
interpretcommand(N,B,Max,G,Rep) :-
   integer(N),
   N > 0,
   N < Max,!,
   nth(B,1,_,N,E),
   traverse(E,Rep1),
   ( Rep1= up
   -> traverse(G,Rep)
   ; Rep=Rep1
   ).
interpretcommand(N,_,Max,G,Rep) :-
   integer(N),!,
   % (N < 1 ; N >= Max,Rep),
   M1 is Max-1,
   writeallonline(['Number out of range: ',N,'. Use number in range: [1,',M1,'].']),
   traverse(G,Rep).
interpretcommand(up,_,_,_,up) :-!.
interpretcommand(why,_,_,_,up) :-!.
interpretcommand(ok,_,_,_,top) :-!.
interpretcommand(prompt,_,_,_,_) :-!,
   throw(prompt).
interpretcommand(retry,_,_,_,retry) :-!.
interpretcommand(end_of_file,_,_,_,prompt) :-!,
   writeallonline(['^D']).
interpretcommand(C,_,_,G,Rep) :-
   writeallonline(['Illegal Command: ',C,'   Type "help." for help.']),
   traverse(G,Rep).

% nth(S,N0,N1,N,E) is true if E is the N-th element of structure S
nth((A&B),N0,N2,N,E) :- !,
   nth(A,N0,N1,N,E),
   nth(B,N1,N2,N,E).
nth(true,N0,N0,_,_) :- !.
nth(A,N,N1,N,A) :- !,
   N1 is N+1.
nth(_,N0,N1,_,_) :-
   N1 is N0+1.

% interpret_naf_how(Comm,ETs,Report)
%  Report is either up, or repeat or whatever travese can report
interpret_naf_how(ok,_,up).
interpret_naf_how(more,_,_) :- !,
   reset_runtime,fail.
interpret_naf_how(how I,Expls,Report) :-
   catch(
     (
        ith(Expls,et(_,Tree),I,0),
	traverse(Tree,Report)
     ),
     ithexception(Ex),
     (Ex= outofrange ->
         writeallonline(['Number out of range: how ',I,'.'])
     ;   writeallonline(['Argument to how must be an integer: how ',I,'.']) 
     ),
	 Report=repeat
    ).
interpret_naf_how(help,_,repeat) :- !,
    writeallonline([
          'ok.      go back to answer dialogue',nl,
          'more.    get another answer.',nl,
          'how i.   ask how the ith explanation was proved',nl,
          'help.    print this message']).
interpret_naf_how(Comm,_,repeat) :- !,
    writeallonline([
          'Unknown command: ',Comm,'. Type "help." for help']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            WHY NOT QUESTIONS for FAILING QUERIES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% determine why body Q failed, when it should have succeded.
% whynotb(Q,DB,Rep,Ass0,Ass1) determine why body Q, with depth-bound DB fails
%   Rep is the reply it is one of {up,top,retry,prompt}
%     up means go up in the tree 
%     top means go to the top of the tree (where traverse was called).
%     retry means find another proof tree
%     prompt means go to the top level prompt

whynotb((A & B),DB,Rep,Ass0,Ass2) :- 
   retractall(failed(_)),
   assert(failed(naturally)),
   solve(A,DB,Res,[whynot],Ass0,Ass1),
   report_whynot_conj(A,Res,B,DB,Rep,Ass0,Ass1,Ass2).
whynotb((A & _),DB,Rep,Ass0,Ass1) :- !,
   whynotb(A,DB,Rep,Ass0,Ass1).

whynotb(call(A),DB,Rep,Ass0,Ass1) :- !,
   whynotb(A,DB,Rep,Ass0,Ass1).

whynotb((~ A),DB,Rep,Ass0,Ass1) :-
   ground(A), !,
   retractall(failed(_)),
   assert(failed(naturally)),
   findall(et(Ass,T),solve(A,DB,T,[whynot],Ass0,Ass),ETs),
   (
     member(et(Ass0,T),ETs)
   ->    % found a proof with no assumables
     writeallonline(['    ',~A,' failed as ',A,' suceeded. Here is how:']),
     traverse(T,Rep)
   ; ETs=[], failed(naturally) ->
     writeallonline(['    ',~A,' succeeds, as ',A,' finitely fails.']),
     whynotb(A,DB,Rep,Ass0,_),
     Ass1=Ass0
   ; failed(unnaturally) ->
     writeallonline(['    ',~A,' failed because of the depth-bound.']),
     whynotb(A,DB,Rep,Ass0,_)
   ;
     whynotb_expls(A,ETs,Rep)
   ).


whynotb((~ A),_,up,Ass0,Ass0) :- !,
   writeallonline(['   Error: ',~A,' needs to be ground when called.']).

whynotb(A,_,up,Ass0,Ass0) :-
   builtin(A,C),!,
   (call(C)
    -> (call(A)
        -> writeallonline(['   ',A,' is built-in and succeeds.'])
         ; writeallonline(['   ',A,' is built-in and fails.']))
    ; writeallonline(['   ',A,' is built-in and is insufficiently instanciated.'])).

whynotb((A \= B),_,up,Ass0,Ass0) :-
   ?=(A,B),!,
   (A \== B
   -> writeallonline(['   ',(A \= B),' succeeds as they can never unify.'])
   ; writeallonline(['   ',(A \= B),' fails as they are identical.'])).
whynotb((A \= B),_,up,Ass0,Ass0) :-
   writeallonline(['   ',(A \= B),' cannot be resolved. It is delayed.']),
   when(?=(A,B),
     (A \== B
      -> writeallonline(['   Resolving delayed ',(A \= B),'. It succeeded.'])
      ; writeallonline(['   Failure due to delayed constraint, ',(A \= B),'.']),
        fail)).

whynotb(Q,0,up,Ass0,Ass0) :- !,
   writeallonline(['   ',Q,' fails because of the depth-bound.']).

whynotb(Q,DB,up,Ass0,Ass0) :-
   DB > 0,
   (Q <- true),!,
   writeallonline(['   ',Q,' is a fact. It doesn''t fail.']).

whynotb(Q,DB,Rep,Ass0,Ass1) :-
   DB > 0,
   (Q <- B),
   whynotrule(Q,B,DB,Rep,Ass0,Ass1).
whynotb(Q,_,up,Ass0,Ass0) :-
   writeallonline(['  There is no applicable rule for ',Q,'.']).

whynotrule(Q,B,DB,Rep,Ass0,Ass1) :-
   writeallonline(['  ',Q,' <- ',B,'.']),
   writel(['    Trace this rule? [yes,no,up,help]: ']),
   flush_and_read(Comm),
   whynotruleinterpret(Comm,Q,B,DB,Rep,Ass0,Ass1).

whynotruleinterpret(yes,Q,B,DB,Rep,Ass0,Ass1) :- !,
   DB1 is DB-1,
   whynotb(B,DB1,Rep1,Ass0,Ass1),
   (Rep1 = up
   -> whynotrule(Q,B,DB,Rep,Ass0,Ass1)
   ;  Rep=Rep1).
whynotruleinterpret(no,_,_,_,_,_,_) :- !,
   fail.
whynotruleinterpret(up,_,_,_,up,Ass0,Ass0) :- !.
whynotruleinterpret(end_of_file,_,_,_,prompt,_,_) :- !,
   writeallonline(['^D']).
whynotruleinterpret(ok,_,_,_,prompt,_,_) :- !.
whynotruleinterpret(help,Q,B,DB,Rep,Ass0,Ass1) :- !,
   writeallonline([
     '     Do you want to examine why this rule failed?',nl,
     '        yes.    look at this rule',nl,
     '        no.     try another rule',nl,
     '        up.     go back to the rule this rule was called from',nl,
     '        ok.     go to top-level prompt']),
   whynotrule(Q,B,DB,Rep,Ass0,Ass1).
whynotruleinterpret(Comm,Q,B,DB,Rep,Ass0,Ass1) :-
   writeallonline(['     Unknown command: ',Comm,'. Type "help." for help.']),
   whynotrule(Q,B,DB,Rep,Ass0,Ass1).


report_whynot_conj(A,Res,B,DB,Rep,Ass0,Ass1,Ass2) :-
   writeallonline(['  The proof for ',A,' succeeded.']),
   (Ass0 \== Ass1 ->
        writeallonline(['     assuming ',Ass1])
   ; true
   ),
   writel(['   Should this answer lead to a successful proof? [yes,no,debug,help]: ']),
   flush_and_read(Comm),
   why_not_conj_interpret(Comm,A,Res,B,DB,Rep,Ass1,Ass2).

why_not_conj_interpret(debug,A,Res,B,DB,Rep,Ass0,Ass1) :- !,
     traverse(Res,Rep1),
     (Rep1 = up
     -> report_whynot_conj(A,Res,B,DB,Rep,Ass0,Ass1)
     ; Rep=Rep1).
why_not_conj_interpret(yes,_,_,B,DB,Rep,Ass0,Ass1) :- !,
     whynotb(B,DB,Rep,Ass0,Ass1).
why_not_conj_interpret(no,_,_,_,_,_,_,_) :- !,
   fail.      % find another proof for A

why_not_conj_interpret(end_of_file,_,_,_,_,prompt,_,_) :- !,
     writeallonline(['^D']).
why_not_conj_interpret(ok,_,_,_,_,prompt,_,_) :- !,
     writeallonline(['^D']).
why_not_conj_interpret(help,A,Res,B,DB,Rep,Ass0,Ass1) :- !,
    writeallonline([
    '     yes.        this instance should have made the body succeed.',nl,
    '     no.         this instance should lead to a failure of the body.',nl,
    '     debug.      this instance is false, debug it.',nl,
    '     ok.         I''ve had enough. Go to the prompt.',nl,
    '     help.       print this message.']),
    report_whynot_conj(A,Res,B,DB,Rep,Ass0,Ass1).
why_not_conj_interpret(Comm,A,Res,B,DB,Rep,Ass0,Ass1) :-
    writeallonline([' Unknown command: ',Comm,'. Type "help." for help.']),
    report_whynot_conj(A,Res,B,DB,Rep,Ass0,Ass1).

whynotb_expls(A,ETs,Rep) :-
     writeallonline(['    ',~A,' succeeds, as ',A,' has the explanations: ']),
     print_explanations(ETs,0),
        writel(['  [up,top,retry,prompt,how i,help,whynot]: ']),
   flush_and_read(Comm),
   interpret_naf_how(Comm,ETs,Rep0),
   (Rep0=repeat ->
       whynotb_expls(A,ETs,Rep)
   ;   Rep=Rep0
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            FILE INTERACTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flush_and_read(T) :-
   flush_output,
   (read(T) -> true ; flush_and_read(T)).  % reads until no error.

ci_load(File) :-
   current_input(OldFile),
   open(File,read,Input),
   set_input(Input),
   flush_and_read(T),
   read_all(T),
   set_input(OldFile),
   writeallonline(['CILOG theory ',File,' loaded.']).

read_all(C) :-
   file_or_command(C,file),
   flush_and_read(T2),!,
   read_all(T2).

read_all(end_of_file) :- !.
read_all((H :- B)) :- !,
   writeallonline(['Error: Illegal Implication: ',H,' :- ',B,'. Use <- or prload.']).
read_all(T) :-
   tell_clause(T),
   flush_and_read(T2),!,
   read_all(T2).
read_all(_).

prolog_load(File) :-
   current_input(OldFile),
   open(File,read,Input),
   set_input(Input),
   flush_and_read(T),
   prread_all(T),
   set_input(OldFile),
   writeallonline(['CILOG theory ',File,' consulted.']).


prread_all(T) :-
   file_or_command(T,file),
   flush_and_read(T2),
   prread_all(T2).
prread_all(end_of_file) :- !.
prread_all(T) :- 
   prillegal(T,Mesg),!,
   writeallonline(Mesg).
prread_all(T) :-
   prtell(T),
   flush_and_read(T2),
   prread_all(T2).

% prillegal(R,Mesg) is true if R is illegal Prolog rule. 
%    Mesg is the corresponding error message.
prillegal((H <- B),['Error. Illegal Implication: ',H,' <- ',B,
                    '. Use :- in prload.']) :- !.
prillegal((:- B),['Error. Commands not allowed. ":- ',B,'."']) :- !.
prillegal((_ :- B),Mesg) :- !,
   prillbody(B,Mesg).
prillbody((A,_),Mesg) :-
   prillbody(A,Mesg).
prillbody((_,B),Mesg) :-
   prillbody(B,Mesg).
prillbody((_;_),['Prolog rules assume disjunction ";". Define it before loading.']) :-
   \+ ((_;_) <- _).
prillbody((A;_),Mesg) :-
   prillbody(A,Mesg).
prillbody((_;B),Mesg) :-
   prillbody(B,Mesg).
prillbody((_&_),['Error. Illegal conjunction in Prolog rules: &']):- !.
prillbody(!,['Error. Cut (!) not allowed.']) :- !.
prillbody((_ -> _ ; _),['Error. "->" is not implemented.']) :- !.
prillbody((_ -> _ ),['Error. "->" is not implemented.']) :- !.

% prtell(Cl) tells the prolog clause Cl
prtell((H :- B)) :- !,
   convert_body(B,B1),
   assertz((H <- B1)).
prtell(H) :-
   assertz((H <- true)).

% convert_body(PB,CB) converts Prolog body PB to cilog body CB
convert_body((A,B),(A1&B1)) :- !,
   convert_body(A,A1),
   convert_body(B,B1).
convert_body((A;B),(A1;B1)) :- !,
   convert_body(A,A1),
   convert_body(B,B1).
convert_body((\+ A),(~ A1)) :- !,
   convert_body(A,A1).
convert_body(A,A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            MAIN LOOP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start :-
   cilog_version(V,Y),
   writeallonline([nl,
     'CILOG Version ',V,'. Copyright ',Y,', David Poole.',nl,
     'CILOG comes with absolutely no warranty.',nl,
     'All inputs end with a period. Type "help." for help.']),
   start1.

start1 :-
   catch(go,Exc,handle_exception(Exc)). 

go :-
   writel(['cilog: ']), 
   flush_and_read(T),
   (T == prolog ->
        writeallonline(['Returning to Prolog. Type "start." to start cilog.'])
   ; T == end_of_file ->
        writeallonline(['^D']),
        writeallonline(['Returning to Prolog. Type "start." to start cilog.'])
   ; interpret(T),
     !,
     go).

handle_exception(prompt) :- !, start1.
handle_exception(quit) :- halt.
handle_exception(Exc) :-
   writeallonline(['Error: ',Exc]),
   start1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            CHECKING the KB
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% check searches through the knowledge base looking for a rule
% containing an atom in the body which doesn't have a corresponding
% definition (i.e., a clause with it at the head).
check :-
   (H <- B),
   body_elt_undefined(B,H,B).
check.

check :-
   (H <- _),
   choice(H,_,_),
   writeallonline([H,' should not be defined in a rule and as a probability.']),
   fail.

body_elt_undefined(true,_,_) :- !,fail.
body_elt_undefined((A&_),H,B) :-
   body_elt_undefined(A,H,B).
body_elt_undefined((_&A),H,B) :- !,
   body_elt_undefined(A,H,B).
body_elt_undefined((~ A),H,B) :- !,
   body_elt_undefined(A,H,B).
body_elt_undefined((A;_),H,B) :-
   body_elt_undefined(A,H,B).
body_elt_undefined((_;A),H,B) :- !,
   body_elt_undefined(A,H,B).
body_elt_undefined(call(A),H,B) :- !,
   body_elt_undefined(A,H,B).
body_elt_undefined(_ \= _,_,_) :- !,fail.
body_elt_undefined(A,_,_) :-
   askabl(A),!,fail.
body_elt_undefined(A,_,_) :-
   assumabl(A),!,fail.
body_elt_undefined(A,_,_) :-
   choice(A,_,_),!,fail.
body_elt_undefined(A,_,_) :-
   builtin(A,_),!,fail.
body_elt_undefined(A,_,_) :-
   (A <- _),!,fail.
body_elt_undefined(A,H,B) :-
   writeallonline(['Warning: no clauses for ',A,' in rule ',(H <- B),'.']),!,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            STATS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% make runtime checking the default
checking_runtime.

% stats_command(Cmd) means "stats Cmd" was given as a command.
stats_command(runtime) :- !,
   retractall(checking_runtime),
   asserta(checking_runtime),
   writeallonline(['Runtime report turned on. Type "stats none." to turn it off.']).
stats_command(none) :- !,
   retractall(checking_runtime).
stats_command(_) :-
   writeallonline(['The stats commands implemented are:']),
   writeallonline(['    stats runtime.        turn on report of runtime.']),
   writeallonline(['    stats none.           turn off report of runtime.']).

% reset_runtime means that we are storing the current valuse of
% runtime.  This means that we are leaving out much of the cilog
% overhead from the calcluation.

reset_runtime :-
   checking_runtime,
   retractall(lastruntime(_)),
   get_runtime(T,_),
   asserta(lastruntime(T)),!.
reset_runtime :-
   checking_runtime ->
       writeallonline([' Problem with runtime checking.'])
    ;  true.

report_runtime :-
   checking_runtime,
   lastruntime(T),
   get_runtime(T1,Units),
   RT is T1 - T,
   writeallonline([' Runtime since last report: ',RT,' ',Units,'.']),!.
report_runtime :-
   checking_runtime ->
   writeallonline([' Problem with runtime reporting.'])
    ;  true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            NOGOOD Handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A nogood is a set of assumables that is inconsistent
construct_nogoods(DB) :-
   depth_bound(DB),
   retractall(failed(_)),
   assert(failed(naturally)),
   reset_runtime,
   solve(false,DB,PfTree,[wrule(yes,true,false,true)],ass([],[],1),ass(Ass,_,_)),
   assert(nogood(Ass,PfTree)),
   assert_each_nogood(Ass,[]),
   fail.
construct_nogoods(DB) :-
   failed(naturally)
      -> writeallonline(['Nogoods recorded. Type "nogoods." to see them.'])
      ;  writeallonline(['Nogoods recorded, but depth-bound ',DB,' reached. Type "nogoods." to see them.']).

assert_each_nogood([],[]).
assert_each_nogood([H|NotDone],Done) :-
	append(NotDone,Done,Rest),
	assert(anogood(H,Rest)),
	assert_each_nogood(NotDone,[H|Done]).

list_nogoods :-
        nogood(NG,_),
	writeallonline(['Nogood: ',NG,'.']), fail.
list_nogoods.

good(A,E) :-
	\+ bad(A,E).
bad(A,E) :-
	ground((A,E)),
	anogood(A,N),
	subset(N,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% writeallonline(L) writes each element of list L, ends with new line
writeallonline(L) :-
   \+ \+ (mynumbervars(L,0,_),writel0(L)),
   nl.
% writel(L) writes each element of list L
writel(L) :-
   \+ \+ (mynumbervars(L,0,_),writel0(L)).

% writel0(L) writes each element of list L
writel0([]) :- !.
writel0([nl|T]) :- !,
   nl,
   writel0(T).
writel0([H|T]) :-
   mywrite(H),
   writel0(T).

% writeAllDelim(L,D) writes the elements of L using delimiter D
writeAllDelim(L,D) :-
   \+ \+ (mynumbervars(L,0,_),writeAllDelim0(L,D)).

writeAllDelim0([],_) :-!.
writeAllDelim0([E],_) :- !, mywrite(E).
writeAllDelim0([H|T],D) :- mywrite(H),mywrite(D),writeAllDelim0(T,D).

% mywrite(T) writes term T
mywrite(T) :-
   write_term(T,[numbervars(true),portray(true)]).

portray(A&B) :- mywrite(A),mywrite(' & '),mywrite(B).

% insert_into_sorted_list(E,L,R) inserts E into sorted list L producing R
insert_into_sorted_list(A,[],[A]).
insert_into_sorted_list(A,[A1|R],[A|R]) :- A == A1,!.
insert_into_sorted_list(A,[A1|R],[A,A1|R]) :- A @< A1, !.
insert_into_sorted_list(A,[B|R],[B|R1]) :-
   insert_into_sorted_list(A,R,R1).

% insertUpdateProbs(E,L) inserts E into sorted list L
insertUpdateProbs(A,[],[A]).
insertUpdateProbs(A,[A1|R],[A|R]) :- A == A1,!.
insertUpdateProbs(A,[A1|R],[A,A1|R]) :- A @< A1, !.
insertUpdateProbs(A,[B|R],[B|R1]) :-
   insertUpdateProbs(A,R,R1).

% consistent(L1,L2) is true if L1 & L2 are sorted lists with no
% elements in common. 
consistent([],_) :- !.
consistent(_,[]) :- !.
consistent([A1|R1],[A2|R2]) :-
   A1 @< A2, !,
   consistent(R1,[A2|R2]).
consistent([A1|R1],[A2|R2]) :-
   A1 @> A2, !,
   consistent([A1|R1],R2).

% remove(E,L,L1) removes E from list L producing L1
remove(E,[E|L],L).
remove(E,[A|L],[A|R]) :-
   remove(E,L,R).


% subset_sorted(L1,L2) is true if sorted list L1 is a subset of L2
% all lists are assumed to be ground.
subset_sorted([],_) :- !.
subset_sorted([A|L1],[A|L2]) :- !,
  subset_sorted(L1,L2).
subset_sorted([A1|L1],[A2|L2]) :-
  A2 @< A1,
  subset_sorted([A1|L1],L2).

% setDiff(S1,S2,S3) S3 = S1-S2. All lists are sorted.

setDiff([],_,[]) :- !.
setDiff(S,[],S) :- !.
setDiff([A|L1],[A|L2],R) :- !,
   setDiff(L1,L2,R).
setDiff([A1|L1],[A2|L2],[A1|R]) :-
   A1 @< A2, !,
   setDiff(L1,[A2|L2],R).
setDiff(L1,[_|L2],R) :-
   setDiff(L1,L2,R).

% mywhen(C,G) replaces "when". It does not delay, but instead gives an
% error. When SWI has a way to collect up the delayed goals
mywhen(C,G) :-
   C -> G 
   ;
    writeallonline(['Warning: ',C,' failing. Delaying not implemented.']),
    fail.

% ith(L,E,I,I0) is true if E is the Ith element of list L, starting to
% count from I0
ith([],_,_,_) :-
   throw(outofrange).
ith([H|_],H,I,I) :- !.
ith([_|T],E,I,I0) :-
   I>I0, !,
   I1 is I0+1,
   ith(T,E,I,I1).
ith(_,_,_,_) :-
   throw(outofrange).

idmember(A,[A1|_]) :- A==A1,!.
idmember(A,[_|L]) :- idmember(A,L).

mynumbervars(Term, From, To) :-
	numbervars(Term, From, To, [attvar(bind)]).

% Run "start" at initialization
:- initialization(start).