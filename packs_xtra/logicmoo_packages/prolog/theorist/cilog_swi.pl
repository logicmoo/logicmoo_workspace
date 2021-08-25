% cilog.pl  logic engine for Computational Intelligence: A Logical Approach
% Copyright (C) 1997, 1998, 2004 David Poole. All Rights Reserved.

cilog_version('0.14').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          SYSTEM DEPENDENT CODE               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These are the only changes that need to be made to make sure that it
%    runs. Note that there are some warnings about redefining system
%    predicates that can be ignored. Cilog has only been tested with SWI
%    Prolog and Sicstus Prolog.

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


% mywhen(C,G) replaces "when". It does not delay, but instead gives an
% error. When SWI has a way to collect up the delayed goals
mywhen(C,G) :-
   C -> G 
   ; writeallonline(['Warning: ',C,' failing. Delaying not implemented.']),
     fail.

% ?=(X,Y) :- X==Y.
% ?=(X,Y) :- \+(X=Y).

differ(X,Y) :- dif(X,Y).  % SWI version 5.3 and later
% differ(X,Y) :- different(X,Y). % pre SWI version 5.3

mywrite(T) :- write_term(T,[numbervars(true),portray(true)]). % SWI version 5.3 and later
mywrite(T) :- write(T).  % pre SWI version 5.3

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

:- op(1170, fx, tell).
:- op(1170, fx, ask).
:- op(1170, fx, whynot).
:- op(1170, fx, how).
:- op(1170,fx,load).
:- op(1170,fx,prload).
:- op(1170,fx,bound).
:- op(1170,fx,stats).
:- op(1170,fx,listing).
:- op(1170,fx,clear).
:- op(1170,fx,askable).
:- op(1170,fx,assumable).
:- op(1170,fx,deterministic).

:- dynamic (<-)/2.
:- dynamic failed/1.
:- dynamic depth_bound/1.
:- dynamic askabl/1.
:- dynamic assumabl/1.
:- dynamic asked/2.
:- dynamic debugging_failure/0.
:- dynamic answer_found/0.
:- dynamic checking_runtime/0.
:- dynamic lastruntime/1.
:- dynamic deter/1.

depth_bound(30).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            CILOG Help
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help_message(['
CILOG Help','

A clause is of the form','
  Head <- Body','
  Head','
Head is an atom.','
Body is an expression made up of','
   Atom','
   ~ B1           negation as finite failure','
   B1 & B2        conjunction B1 and B2','
where B1 and B2 are expressions.','
   bagof(A,Q,L)   true if L is the list of A''s where Q is true.','

***** Basic commands:','

tell CLAUSE.           add the clause to the knowledge base','
askable Atom.          makes Atom askable.','
assumable Atom.        makes atom assumable.','

ask QUERY.             ask if expression QUERY is a consequence of the KB','
whynot QUERY.          find out why QEURY failed.','

bound N.               set search depth-bound to N (default=30)','
listing.               list all clauses in the knowledge base.','
listing H.             list clauses in the knowledge base with head H.','
askables.              list all askables.','
assumables.            list all assumables.','
clear.                 clear the knowledge base','
check.                 check the knowledge base for undefined predicates','
clear H.               clear clauses with head H from knowledge base','
help.                  print this help message','
stats runtime.         give runtime statistics','
quit.                  quit cilog','
prolog.                exit to Prolog. Type "start." to start cilog again.','

***** Input/Output','

load ''Filename''.      load the clauses in file Filename','
prload ''Filename''.    load the clauses in Filename, using Prolog''s syntax.']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            CILOG Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpret(help) :- !,
   help_message(Help),
   writeallonline(Help).

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
   whynotb(Q,DB,_,[],_).

interpret((clear H)) :- !,
   retractall((H <- _)),
   retractall(assumabl(H)),
   retractall(askabl(H)),
   retractall(asked(H,_)).

interpret((stats Cmd)) :- !,
   stats_command(Cmd).

interpret(clear) :- !,
   interpret((clear _)).

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

interpret((askable G)) :- !,
   assertz(askabl(G)).

interpret((assumable G)) :- !,
   assertz(assumabl(G)).

interpret((deterministic G)) :- !,
   assertz(deter(G)).

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

interpret((listing)) :- !,
   interpret((listing _)),
   interpret(askables),
   interpret(assumables).

interpret((listing H)) :-
   (H <- B),
   (B = true 
   -> writeallonline([H,'.'])
   ; writeallonline([H,' <- ',B,'.'])
   ),
   fail.
interpret((listing _)) :- !.

interpret((A <- B)) :- !,
   writeallonline(['Illegal command, ',(A <- B),'. You have to "tell" a clause.']).

interpret(C) :-
   writeallonline(['Unknown Command, ',C,'. Type "help." for help.']).


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
   solve(Q,DB,Res,[wrule(yes,true,Q,true)],[],Ass),
   (answer_found -> true ; assert(answer_found)),
%   numbervars((Q,Res,Ass),0,_),
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
%  Goal
%  Whytree is a list of wrule(Head,LeftTree,Current,RightBody)
solve(true,_,true,_,Ass,Ass) :- !.
solve((A&B),N,(AT&BT),[wrule(H,LT,_,RB)|WT],A0,A2) :- !,
   solve(A,N,AT,[wrule(H,LT,A,(B&RB))|WT],A0,A1),
   solve(B,N,BT,[wrule(H,(LT&AT),B,RB)|WT],A1,A2).
solve(A \= B,_,if(A \= B,builtin),_,A0,A0) :- !,
   differ(A,B).
solve(call(G),_,_,WT,_,_) :- 
   var(G),!,
   writeallonline(['Error: the argument to call must be bound when evaluated.']),
   why_question(WT,_),
   writeallonline(['Failing the call with unbound argument.']),!,
   fail.
solve(call(G),N,T,WT,A0,A1) :- !,
   solve(G,N,T,WT,A0,A1).
solve((~ G),N,if(~G,naf),WT,A0,A0) :- !,
   mywhen( ground(G), failtoprove(G,N,WT)).
solve(bagof(E,Q,L),N,BagTrees,WT,A0,A1) :- !,
   solvebag(bagof(E,Q,L),N,BagTrees,WT,A0,A1).
solve(G,_,if(G,assumed),_,A0,A1) :-
   assumabl(G),
   insert(G,A0,A1).
solve(G,_,if(G,asked),WT,A0,A0) :-
   askabl(G),
   ask_user(G,WT,Ans),
   Ans \== unknown,!,      % fail is Ans=unknown, otherwise don't try clauses
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

report(Q,Res,[]) :- !,
   writeallonline(['Answer: ',Q,'.']),
   report_runtime,
   writel(['  [ok,more,how,help]: ']),
   flush_and_read(Comm),
   interpret_report(Comm,Q,Res,[]).
report(Q,Res,Ass) :-
   writeallonline(['Answer: ',Q,'.']),
   writeallonline(['Assuming: ',Ass,'.']),
   report_runtime,
   writel(['  [more,ok,how,help]: ']),
   flush_and_read(Comm),
   interpret_report(Comm,Q,Res,Ass).

% interpret_report(Comm,Q,Res)
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
   \+ \+ (numbervars(WT,0,_),why_question(WT,_)),
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
   writeallonline(['  Error in built-in predicate: ',H,'.']),
   writel(['     [fail,succeed,why,ok,help]: ']),
   flush_and_read(Comm),
   interpret_builtin_error_command(Comm,H,WT).

interpret_builtin_error_command(help,H,WT) :- !,
   writeallonline([
     '  There is an error in a built-in predicate.',nl,
     '  You can give the following commands:',nl,
     '    fail.       fail this subgoal.',nl,
     '    succeed.    make this subgoal succeed.',nl,
     '    why.        determine why this subgoal was called.',nl,
     '    ok.         return to cilog prompt.',nl,
     '    help.       print this message.']),
   debugging_failure_goal(H,WT).
interpret_builtin_error_command(fail,_,_) :- !,
   fail.
interpret_builtin_error_command(succeed,_,_) :- !.
interpret_builtin_error_command(ok,_,_) :- !,
   throw(prompt).
interpret_builtin_error_command(end_of_file,_,_) :- !,
   throw(prompt).
interpret_builtin_error_command(why,H,WT) :- !,
   \+ \+ (numbervars(WT,0,_),why_question(WT,_)),
   debugging_builtin_error(H,WT).
interpret_builtin_error_command(Comm,H,WT) :- !,
   writeallonline(['  Unknown command: ',Comm,'. Type "help." for help.']),
   debugging_builtin_error(H,WT).

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

ask_user(G,WT,Ans) :-
   ground(G),!,
   writel(['Is ',G,' true? [yes,no,unknown,why,help]: ']),
   flush_and_read(Rep),
   interpret_ask_answer(Rep,G,WT,Ans).
ask_user(G,WT,fail) :-
   writeallonline(['   Error: Askables with free variables not implemented.',nl,
            '   The system wanted to ask ',G,'.',nl,
            '   Entering why interation.']),
   numbervars(WT,0,_),
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
   \+ \+ ( numbervars(WT,0,_),why_question(WT,Rep),Rep \== prompt),
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

% failtoprove(G,N,WT)  G is a ground goal, N is a depth-bound
% The complication here is due to the interaction with the depth-bound

failtoprove(G,N,WT) :-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            BAGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solvebag(bagof(E,Q,L),N,bag(E,Q,BL),WT,A0,A1) :-
   retract(failed(How)),
   assert(failed(naturally)),
   extractExistVars(Q,EVs,QR),
   bagof(anstree(E,T),EVs^solve(QR,N,T,WT,A0,A1),BL),  % answer for each A1
   ( failed(naturally) ->
       firstOfAnstree(BL,L),
       retract(failed(naturally)),
       assert(failed(How))
   ;
       fail
   ).

firstOfAnstree([],[]).
firstOfAnstree([anstree(E,_)|L],[E|R]) :-
   firstOfAnstree(L,R).

extractExistVars(A^B^C,A^Vs,Q) :- !,
	extractExistVars(B^C,Vs,Q).
extractExistVars((A^Q&R),A,(Q&R)) :- !.
extractExistVars(A^Q,A,Q) :- !.
extractExistVars(Q,none,Q) :- !.

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
traverse(if(~G,naf),Rep) :- !,
   writeallonline(['   ',G,' finitely failed. You can examine the search space.']),
   depth_bound(DB),
   whynotb(G,DB,Rep,[],_).
traverse(if(H,B),Rep) :-
    writeallonline(['   ',H,' <-']),
    print_tree_body(B,1,Max),
    writel(['   How? [Number,up,retry,ok,prompt,help]: ']),
    flush_and_read(Comm),
    interpretcommand(Comm,B,Max,if(H,B),Rep).
traverse(bag(E,Q,BL),Rep) :-
    howbag(E,Q,BL,Rep).

% print_tree_body(B,N) is true if B is a body to be printed and N is the 
% count of atoms before B was called.
print_tree_body(true,N,N).
print_tree_body((A&B),N0,N2) :-
   print_tree_body(A,N0,N1),
   print_tree_body(B,N1,N2).
print_tree_body(if(H,_),N,N1) :-
   writeallonline(['      ',N,': ',H]),
   N1 is N+1.
print_tree_body(bag(E,Q,BL),N,N1) :-
   firstOfAnstree(BL,L),
   writeallonline(['      ',N,': ',bagof(E,Q,L)]),
   N1 is N+1.

% interpretcommand(Comm,B) interprets the command Comm on body B
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

% howbag(E,Q,BL,Rep) allows the user to search the bag replies
%  the call was howbag(E,Q,L) and BL is the result, including proof trees
howbag(E,Q,BL,Rep) :-
   writeallonline(['   The call ',bagof(E,Q,V),' returned with ',V,' containing']),
   displaybaglist(BL,0,BLLen),
   writel(['   How? [Number,up,whynot,ok,prompt,help]: ']),
    flush_and_read(Comm),
    interpret_how_bag_command(Comm,E,Q,BL,BLLen,Rep).

%displaybaglist(BagList,NumBefore,TotalNumber)
displaybaglist([],0,0) :- !,
    writeallonline(['no elements.']).
displaybaglist([],N,N) :- !.
displaybaglist([anstree(E,_)|R],N,NN) :-
   N1 is N+1,
   writeallonline(['     ',N1,': ',E]),
   displaybaglist(R,N1,NN).

interpret_how_bag_command(up,_,_,_,_,up) :- !.
interpret_how_bag_command(ok,_,_,_,_,top) :- !.
interpret_how_bag_command(prompt,_,_,_,_,_)  :- !,
    throw(prompt).
interpret_how_bag_command(end_of_file,_,_,_,_,_)  :- !,
   writeallonline(['^D']),
   throw(prompt).

interpret_how_bag_command(help,E,Q,BL,Max,Rep) :- !,
   writeallonline([
     '    Give either (end each command with a period):',nl,
     '       how i.       explain how element i (i=<',Max,') was proved.',nl,
     '       up.          go up the proof tree one level.',nl,
     '       retry.       find another proof.',nl,
     '       ok.          stop traversing the proof tree.',nl,
     '       prompt.      return to the cilog prompt.',nl,
     '       help.        to print this message.']),
    howbag(E,Q,BL,Rep).
interpret_how_bag_command(N,E,Q,BL,BLLen,Rep) :-
   integer(N),!,
   ( N >= 0, N =< BLLen ->
       nthList(N,BL,anstree(_,ET)),
       traverse(ET,Repl),
       ( Repl = up
       -> howbag(E,Q,BL,Rep)
       ; Rep=Repl
       )
   ; writeallonline([' Error: ',N,' must be in range ',[1,BLLen]]),
     howbag(E,Q,BL,Rep)
   ).
interpret_how_bag_command((how N),E,Q,BL,BLLen,Rep) :- !,
   interpret_how_bag_command(N,E,Q,BL,BLLen,Rep).

interpret_how_bag_command(Comm,E,Q,BL,_,Rep) :- !,
    writeallonline(['Unknown command ',Comm,' type "help." for help.']),
    howbag(E,Q,BL,Rep).

% nthList(N,L,E) is true if the Nth element of list L is E
nthList(1,[E|_],E) :- !.
nthList(N,[_|R],E) :-
   N>1,
   N1 is N-1,
   nthList(N1,R,E).

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
   report_whynot_conj(A,Res,B,DB,Rep,Ass1,Ass2).
whynotb((A & _),DB,Rep,Ass0,Ass1) :- !,
   whynotb(A,DB,Rep,Ass0,Ass1).

whynotb(call(A),DB,Rep,Ass0,Ass1) :- !,
   whynotb(A,DB,Rep,Ass0,Ass1).
whynotb((~ A),DB,Rep,Ass0,Ass0) :- !,
   retractall(failed(_)),
   assert(failed(naturally)),
   ( solve(A,DB,Res,[whynot],[],_) ->
     writeallonline(['    ',~A,' failed as ',A,' suceeded. Here is how:']),
     traverse(Res,Rep)
   ; failed(unnaturally) ->
     writeallonline(['    ',~A,' failed because of the depth-bound.']),
     whynotb(A,DB,Rep,[],_)
   ; writeallonline(['    ',~A,' succeeds, as ',A,' finitely fails.']),
     Rep=up
   ).

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


report_whynot_conj(A,Res,B,DB,Rep,Ass0,Ass1) :-
   writeallonline(['  The proof for ',A,' succeeded.']),
   writel(['   Should this answer lead to a successful proof? [yes,no,debug,help]: ']),
   flush_and_read(Comm),
   why_not_conj_interpret(Comm,A,Res,B,DB,Rep,Ass0,Ass1).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            FILE INTERACTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flush_and_read(T) :-
   flush_output,
   read(T).

ci_load(File) :-
   current_input(OldFile),
   open(File,read,Input),
   set_input(Input),
   flush_and_read(T),
   read_all(T),
   set_input(OldFile),
   writeallonline(['CILOG theory ',File,' loaded.']).

read_all(end_of_file) :- !.
read_all((askable G)) :- !,
   assertz(askabl(G)),
   flush_and_read(T2),
   read_all(T2).
read_all((assumable G)) :- !,
   assertz(assumabl(G)),
   flush_and_read(T2),
   read_all(T2).
read_all((H :- B)) :- !,
   writeallonline(['Error: Illegal Implication: ',H,' :- ',B,'. Use <- or prload.']).
read_all(T) :-
   tell_clause(T),
   flush_and_read(T2),
   read_all(T2).

prolog_load(File) :-
   current_input(OldFile),
   open(File,read,Input),
   set_input(Input),
   flush_and_read(T),
   prread_all(T),
   set_input(OldFile),
   writeallonline(['CILOG theory ',File,' consulted.']).

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
   cilog_version(V),
   writeallonline([nl,
     'CILOG Version ',V,'. Copyright 1998-2004, David Poole.',nl,
     'CILOG comes with absolutely no warranty.',nl,
     'All inputs end with a period. Type "help." for help.']),
   start1.

start1 :-
   catch(go,Exc,handle_exception(Exc)).   % for Sicstus Prolog
%   go.                                           % for other Prologs

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
%%            HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% writeallonline(L) writes each element of list L, ends with new line
writeallonline(L) :-
   \+ \+ (numbervars(L,0,_),writel0(L)),
   nl.
% writel(L) writes each element of list L
writel(L) :-
   \+ \+ (numbervars(L,0,_),writel0(L)).

% writel0(L) writes each element of list L
writel0([]) :- !.
writel0([nl|T]) :- !,
   nl,
   writel0(T).
writel0([H|T]) :-
   mywrite(H),
   writel0(T).

insert(A,[],[A]).
insert(A,[A1|R],[A|R]) :- A == A1,!.
insert(A,[B|R],[B|R1]) :-
   insert(A,R,R1).


% different(X,Y) is true if X and Y denote different individuals.
different(X,Y) :-
   \+ (X=Y),!.
different(X,Y) :-
   X \== Y,
   writeallonline(['Warning: ',X\=Y,' failing. Delaying not implemented.']),!,
   fail.

:- initialization(start).