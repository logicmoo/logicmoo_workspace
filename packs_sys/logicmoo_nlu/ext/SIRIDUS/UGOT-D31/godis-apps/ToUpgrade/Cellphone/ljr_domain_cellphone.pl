 
/*************************************************************************

         name: domain_travel.pl 
      version: 
  description: An example domain file
       author: Peter Bohlin, Staffan Larsson, Stina Ericsson
 
*************************************************************************/

:- module( domain_cellphone, [ relevant_to_task/3,
			       relevant_to_tasks/2,
			       relevant_answer/2,
			       all_answers/2,
			       plan/2,
			       reduce/3,
			       revisable/1,
			       sysaction/1,
			       dominates/2,
			       inCountry/2
			 ] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

/*----------------------------------------------------------------------
     relevant_answer( -Question, +Answer )
     -- Returns (if it succeeds) a Question to which
        the Answer is relevant 
----------------------------------------------------------------------*/

%%% WH-questions about task

% full answer
relevant_answer( _^(task(_)), task(Task) ):-
	plan( Task, _ ).

% elliptical answer
relevant_answer( _^(task(_)), Task ):-
	plan( Task, _ ).

%%% Alternative questions about task


% task answer is relevant to task question if given task is one of the
% tasks asked about, or is dominated by one of the tasks asked about
% 
% task(Task) is relevant to [task(T1),...,task(Tn)] if
% 1. Task=T1 or ... or Task=Tn
% 2. Task is dominated by T1 or ... or Tn

% full answer
relevant_answer( TaskList, task(Task) ):-
	member( task(Task), TaskList ),
	plan( Task, _ ). 
%relevant_answer( TaskList, task(Task) ):-
%	member( task(Task0), TaskList ),
%	dominates( Task0, Task ),
%	plan( Task, _ ). 

% elliptical answer
relevant_answer( TaskList, Task ):-
	member( task(Task), TaskList ),
	plan( Task, _ ). 
%relevant_answer( TaskList, Task ):-
%	member( task(Task0), TaskList ),
%	dominates( Task0, Task ),
%	plan( Task, _ ). 



%%% WH-question about name

% full answer
relevant_answer( _^(name(_)), name(Name)) :-
	user:val(lexicon,Lexicon).
%	Lexicon: name( Name ).

% elliptical answer
%relevant_answer( _^(name(_)), Name) :-
%	user:val(lexicon,Lexicon),
%	Lexicon: name( Name ).

relevant_answer([delete,save,dial,next],Ans):-
	member(Ans,[delete,save,dial,next]),
	user:val(lexicon,Lexicon).

relevant_answer(next,Ans):-
	member(Ans, [next, not next]).




%%% WH-question about number

% full answer
relevant_answer( _^(number(_)), number(Number)) :-
	user:val(lexicon,Lexicon).
	% Lexicon: number( Number ).

% elliptical answer
%relevant_answer( _^(number(_)), Number) :-
%	user:val(lexicon,Lexicon),
%	Lexicon: number( Number ).



%%% Yes/no question P?

% yes
relevant_answer( Q, yes) :-
	ynq(Q).

% no
relevant_answer( Q, no) :-
	ynq(Q).

% P
relevant_answer( Q, P ) :-
	ynq(Q),
	P = Q.


%%% Definition of yes/no questions

ynq( YNQ ):-
	YNQ \= _^_,		% whq
	YNQ \= [_|_].		% altq





/*----------------------------------------------------------------------
     relevant_to_task( +Move, -Task, -Plan )
     -- Returns (if it succeeds) a Plan to which Move is relevant
----------------------------------------------------------------------*/

relevant_to_task( Move, Task, Plan ):-
	plan( Task, Plan ),
	Move = answer( A ),
	member( findout( Q ), Plan ), % hela planen måste sökas igenom!!!
	relevant_answer( Q, A ). 

/*----------------------------------------------------------------------
     relevant_to_tasks( +Move, -Tasks )
     -- Returns (if it succeeds) a list of tasks Tasks to which Move is
     relevant; all elements in Task have the form task(T)
----------------------------------------------------------------------*/

relevant_to_task( Move, Task ):-
	plan( Task, Plan ),
	Move = answer( A ),
	member( findout( Q ), Plan ), % hela planen måste sökas igenom!!!
	relevant_answer( Q, A ).

relevant_to_tasks( Move, Tasks ):-
	setof( task(Task), relevant_to_task( Move, Task ), Tasks ).

/*----------------------------------------------------------------------
     plan( ?Name, ?Plan )
     -- Dialogue plan
----------------------------------------------------------------------*/


plan( top, [ findout([task(phonebook), task(messages),task(calls)]),
	     case([ ( task(phonebook), exec(phonebook) ),
		    ( task(messages), exec(messages) ),
		    (task(calls),exec(calls)),
		    forget
		  ])
	   ]).

plan( phonebook, [ findout([task(search_phonebook), task(add_new_number)]),
		   case([ ( task(search_phonebook), exec(search_phonebook) ),
			  ( task(add_new_number), exec(add_new_number) ),
			  forget
		         ])
		 ] ).
		 
plan( search_phonebook,
      [ findout( X^name(X) ),	% "Vem vill du söka?"
	findout( call ),	% "Vill du ringa?"
	if_then( call,
		 if_then( name(N),
			  [ call_name(N),
			    inform(call_name(N)) ] ) ),
	forget,
	exec(phonebook)
      ] ).

plan( add_new_number,
      [ findout( X^name(X) ),	% "Vilket namn?"
	findout( Y^number(Y) ), % "Vilket nummer?"
	findout( call ),	% "Vill du ringa?"
	if_then( call,
		 if_then( name(N),
			  [ call_name(N),
			    inform(call_name(N)) ] ) ),
	forget,
	exec(phonebook)
      ] ).

plan( calls, [ findout([task(missed_calls), task(received_calls), task(made_calls)]),
	       case([(task(missed_calls), exec(missed_calls)),
		      (task(received_calls), exec(received_calls)),
		     (task(made_calls), exec(made_calls))
		    ])]).

plan( missed_calls, [  get_call(X),
		       seq([inform(call(X)),
		     findout([ delete, save, dial, next ])]),
		      case([
			    (delete, delete(X)),
			    (save, save(X)),
			    (dial, call(X)),
			    (next, exec(missed_calls))
			   ]),
		      findout( next ),
		      if_then_else(next, [forget,exec(missed_calls)], [forget,exec(calls)])]).

sysaction( call_name( _ ) ).
sysaction( call_number( _ ) ).
sysaction( save_to_phonebook( _, _ ) ).
sysaction( call(_) ).
sysaction( save(_) ).
sysaction( delete(_) ).
sysaction( get_call(_)).

/*----------------------------------------------------------------------
   dominates( T1, T2 )
   -- Task T1 dominates T2 in the menu hierarchy
----------------------------------------------------------------------*/

dominates0( T1, T2 ):-
	plan( T1, [ findout( Ts ) | _ ] ),
	member( task( T2 ), Ts ).

dominates( T1, T2 ):-
	dominates0( T1, T2 ).

dominates( T1, T3 ):-
	dominates0( T1, T2 ),
	dominates( T2, T3 ).
	

/*----------------------------------------------------------------------
   reduce( +Q, +A, -P )

   -- reduces a quesition and an answer to a proposition

   ** this should perhaps be in the definition of the datatypes "question"
   and "answer", as an operation which takes a question and an answer
   and yields a question
----------------------------------------------------------------------*/

/*
** Y/N-questions
*/

reduce( Q, yes, P ):-
	ynq( Q ),
	P = Q. 

reduce( Q, no, P ):-
	ynq( Q ),
	P = not( Q ).

reduce( Q, P, P ):-
	ynq( Q ),
	P = Q.

/*
** Alt-questions
*/

% findout([ A1, A2, ..., An ]) is answered by A1 or A2 or .... or An

reduce(AltList, A, P):-
	member(A, AltList),!,
	P = A.

% findout([Pred(_),...]) is answered by Pred(_)

reduce(AltList, A, P):-
	AltList = [ A0 | _ ],
	A0 =.. [ Pred | _ ],
	A =.. [ Pred | _ ],!,
	P = A.

/*
** WH-questions
*/

% X^Q is answered by an atom (ellipsis)

reduce(X^P, X, P):-
	atom(X).

% X^Q is answered by full proposition

reduce( X^Q, A, P ):-
	Q =.. [Y,X],
	A =.. [Y,X],
	P = Q,
	!.






%all_answers( Q, As ):-
%	setof( A, relevant_answer( Q, A ), As ).
