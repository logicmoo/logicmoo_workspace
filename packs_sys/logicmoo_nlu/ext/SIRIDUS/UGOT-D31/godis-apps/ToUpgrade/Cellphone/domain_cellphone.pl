 
/*************************************************************************

         name: domain_cellphone.pl 
      version: 28 May, 2001
  description: Mini Mobile Phone domain file
      authors: Peter Bohlin, Staffan Larsson, Stina Ericsson
 
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
			       inCountry/2,
			       voice/1
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
relevant_answer( TaskList, task(Task) ):-
	member( task(Task0), TaskList ),
	dominates( Task0, Task ),
	plan( Task, _ ). 

% elliptical answer
relevant_answer( TaskList, Task ):-
	member( task(Task), TaskList ),
	plan( Task, _ ). 
relevant_answer( TaskList, Task ):-
	member( task(Task0), TaskList ),
	dominates( Task0, Task ),
	plan( Task, _ ). 






%%%%% non-task WH-questions


% wh-question about call

relevant_answer( _^(call(_)), Who) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_name( Who ).

relevant_answer( _^(call(_)), Who) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_number( Who ).

relevant_answer( _^(call(_)), name(Who) ) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_name( Who ).

relevant_answer( _^(call(_)), number(Who) ) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_number( Who ).


% wh-question about current_security_code

relevant_answer( _^(current_security_code(_)), Code) :-
	user:val(lexicon,Lexicon), 
	Lexicon: lex_security_code( Code ).

relevant_answer( _^(current_security_code(_)), number(Code)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_security_code( Code ).

relevant_answer( _^(current_security_code(_)), security_code(Code)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_security_code( Code ).

% wh-question about entry_to_delete

relevant_answer( _^(entry_to_delete(_)), Name) :-
	user:val(lexicon,Lexicon), 
	Lexicon: lex_name( Name ).

relevant_answer( _^(entry_to_delete(_)), name(Name)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_name( Name ).

% wh-question about language

relevant_answer( _^(language(_)), Language) :-
	user:val(lexicon,Lexicon), 
	Lexicon: lex_language( Language ).

relevant_answer( _^(language(_)), language(Language)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_language( Language ).

% wh-question about name_to_add

relevant_answer( _^(name_to_add(_)), Name) :-
	user:val(lexicon,Lexicon), 
	Lexicon: lex_name( Name ).

relevant_answer( _^(name_to_add(_)), name(Name)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_name( Name ).

% wh-question about new_security_code

relevant_answer( _^(new_security_code(_)), Code) :-
	user:val(lexicon,Lexicon), 
	Lexicon: lex_security_code( Code ).

relevant_answer( _^(new_security_code(_)), number(Code)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_security_code( Code ).

relevant_answer( _^(new_security_code(_)), security_code(Code)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_security_code( Code ).

% wh-question about number_to_add

relevant_answer( _^(number_to_add(_)), Number) :-
	user:val(lexicon,Lexicon), 
	Lexicon: lex_number( Number ).

relevant_answer( _^(number_to_add(_)), number(Number)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_number( Number ).

% general wh-questions; overgenerates since no lexicon check
% SL 010531

relevant_answer( _^P, P ).

% wh-question about search_for_name

relevant_answer( _^(search_for_name(_)), Name) :-
	user:val(lexicon,Lexicon), 
	Lexicon: lex_name( Name ).

relevant_answer( _^(search_for_name(_)), name(Name)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_name( Name ).


% wh-question about the_security_code

relevant_answer( _^(the_security_code(_)), Code) :-
	user:val(lexicon,Lexicon), 
	Lexicon: lex_security_code( Code ).

relevant_answer( _^(the_security_code(_)), number(Code)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_security_code( Code ).

relevant_answer( _^(the_security_code(_)), security_code(Code)) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_security_code( Code ).






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

% Mikael & David
% not P, added to avoid relevance requests
relevant_answer( Q, not(P) ) :-
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

default( something_else ).
%default( top ).




% ------------------------------
%%%% TOP
% ------------------------------

plan( top, [ findout( [ task(call), task(phonebook),
			task(settings), task(main_menu)%, task(tone_control)
		      ] )%,
%	     case( [ ( task(call), exec(call) ),
%		     ( task(phonebook), exec(phonebook) ),
%		     ( task(settings), exec(settings) )%,
		     %( task(tone_control), exec(tone_control) ),
%		     forget
	%	   ] )
	   ] ).

plan( main_menu,
      [ findout(main_menu),
	if_then_else(main_menu,
		     [ forget,
		       change_domain(menu) ],
		     [ forget(task(_)),
		       forget(not main_menu),
		       exec(top) ]
		    ) ] ).




% ------------------------------
%%%% CALL
% ------------------------------


plan( call, [ findout( X^call(X) ),
	      if_then( call( NameOrNumber ),
		       [ callSysAct( NameOrNumber ),
			 inform( calling( NameOrNumber ) ) ] ),
	      forget,
	      exec( top )
	    ] ).



% ------------------------------
%%%% PHONEBOOK
% ------------------------------

plan( phonebook, [ findout( [ task(search_phonebook),
			      task(add_new_entry),
			      task(delete_entry) ] )%,
%		   case( [ ( task(search_phonebook), exec(search_phonebook) ),
%			   ( task(add_new_entry), exec(add_new_entry) ),
%			   ( task(delete_entry), exec(delete_entry) )%,
%			   forget
%			 ] )
		 ] ).

plan( search_phonebook, [ findout( X1^search_for_name(X1) ),
			  consultDB( X2^(pb_numberDB(X2)) ),
		          respond( X3^(pb_numberDB(X3)) ),
			  if_then_else( pb_numberDB(fail), % sh.com
					[],
			                [ findout( call ),
			                  if_then( call,
				                   [ if_then( pb_numberDB(Number),
					                      [ callSysAct(Number),
						                inform( calling( Number ) ) ]
							    ) ] ) ] ),
			  forget,
			  exec(top)
			] ).

plan( add_new_entry, [ findout( X1^name_to_add(X1) ),
		       findout( X2^number_to_add(X2) ),
		       updateDB( add_entry ),
		       if_then( name_to_add(X3),
				inform(entry_added(X3)) ),
		       forget,
		       exec(top)
		     ] ).

plan( delete_entry, [ findout( X1^entry_to_delete(X1) ),
		      consultDB( X2^(pb_numberDB(X2)) ), % to check that entry (name) exists in PB
		      if_then( fail,       % pr.bel
				    [ respond( X3^(pb_numberDB(X3)) ) ] ),
		      if_then( pb_numberDB(_X3),  % pr.bel
			       [ updateDB( delete_entry ),
		                 if_then( entry_to_delete(Name),
			                  inform(entry_deleted(Name)) ) ] ),
		      forget,
		      exec(top)
		    ] ).




% ------------------------------
%%%% SETTINGS
% ------------------------------

plan( settings, [ findout( [ task(telephone_settings),
			     task(security_settings),
			     task(reset) ] )%,
%		  case( [ ( task(telephone_settings), exec(telephone_settings) ),
%			  ( task(security_settings), exec(security_settings) ),
%			  ( task(reset), exec(reset) )%,
%			  forget
%			] )
		] ).
		  

plan( telephone_settings, [ findout( [ task(change_language) ] ),
			    case( [ ( task(change_language), exec(change_language) ),
				    forget
				  ] )
			  ] ).

plan( change_language, [ findout( X^language(X) ),
			 if_then( language(L),
				  [ findout( change_language(L) ),
				    if_then( change_language(L),
				             [ change_languageSysAct(L),
				               inform( new_language(L) ) ] ) ] ),
			 forget,
			 exec( top )
		       ] ).

plan( security_settings, [ findout( [ task(change_security_code) ] ),
			   case( [ ( task( change_security_code ), exec( change_security_code ) ),
				   forget
				 ] )
			 ] ).

/*
plan( change_security_code,
      [ findout( X^current_security_code(X) ),
	if_then( current_security_code(UserInputCode),
		 [ consultDB( Z^(security_codeDB(Z)) ),
		   if_then( security_codeDB(UserInputCode),
			    inform( code_correct ) )
		 ] ),
	forget,
	exec(top)
      ] ).
*/

plan( change_security_code,
      [ findout( X1^current_security_code(X1) ),
	consultDB( X2^(security_codeDB(X2)) ),
	if_then( current_security_code(UserInputCode), % sh.com
		 [ if_then( security_codeDB(UserInputCode), % pr.bel % if userCode = dbCode
			    [ findout( X3^new_security_code(X3) ),
			      updateDB( change_security_code ),
			      inform( security_code_changed ) ] ) ] ),
	forget,
	exec(top)
      ] ).

% needed: some way of informing wrong user input code..
plan( reset,
      [ findout( X^the_security_code(X) ),
	consultDB( Z^(security_codeDB(Z)) ),
	if_then( the_security_code(UserInputCode), % sh.com
		 [ if_then( security_codeDB(UserInputCode), % pr.bel % if userCode = dbCode
			    [ reset_phoneSysAct,
			      inform( phone_reset ) ] ) ] ),
	forget,
	exec(top)
      ] ).





/*----------------------------------------------------------------------
   sysaction/1
----------------------------------------------------------------------*/

sysaction( callSysAct(_) ).
sysaction( change_languageSysAct(_) ).
sysaction( change_security_codeSysAct(_) ).
sysaction( check_security_codeSysAct(_) ).
sysaction( reset_phoneSysAct ).

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

% specific questions (Alex)

reduce( X^current_security_code(X), number(X), current_security_code(X) ).
reduce( X^new_security_code(X), number(X), new_security_code(X) ).
reduce( X^the_security_code(X), number(X), the_security_code(X) ).
reduce( X^number_to_add(X), number(X), number_to_add(X) ).

% SE 
% when answer to a question is not the same as the question
%   e.g person_name is the answer to destination

reduce( X^Q, A, P ):-
	Q =.. [Y,X],
	A =.. [Z,X],
	Y \== Z,
	P = Q,
	!.

% Mikael & David ( "X^Q is answered by an atom (ellipsis)" above
%                  doesn't work when X not an atom )
% X^Q is answered by an ellipsis
reduce(X^P, X, P). %% SÅHÄR FUNKAR BÄTTRE; men det blir ett annat problem som beror på update-algoritmen - man måste göra manage_plan igen efter den andra accommodate




%all_answers( Q, As ):-
%	setof( A, relevant_answer( Q, A ), As ).



% abstract(+A,+P,-Q)
%
% Q is a question s.t. reduce(Q,A,P) holds

% abstract( A, P, _^P ).
abstract( A, P, X^Q ):-
	A =.. [Y,_],
	P =.. [Y,_],
	Q =.. [Y,X],
%	P = Q,
	!.

% Voice

voice( 3 ).
