
 
/*************************************************************************

        name: resource_interfaces.pl 
     version: 
 description: InDiTeS Resource interface variables
      author: Peter Bohlin
 
*************************************************************************/

:- module(resource_interfaces, [
				resource_variable_of_type/2,
				is_resource_type/1,
				resource_condition/3,
				resource_operation/3
			       ]).

% :- multifile is_type/1, of_type/2, empty_object/2, operation/4, condition/3.

% dummy so resource_operation is defined somewhere

resource_operation( dummy, dummy, dummy ).

/*----------------------------------------------------------------------
     lexicon -- the dialogue lexicon
----------------------------------------------------------------------*/

is_resource_type( lexicon ).

resource_variable_of_type( lexicon, lexicon ).

of_type( lexicon_homecentre_english, lexicon ).

resource_condition( lexicon, input_form( Phrase, Move ), Lexicon ) :-
	Lexicon : input_form( Phrase, Move ).

resource_condition( lexicon, output_form( Phrase, Move ), Lexicon ) :-
	Lexicon : output_form( Phrase, Move ).

resource_condition( lexicon, yn_answer(A), Lexicon ) :-
	Lexicon : yn_answer( A ).

/*----------------------------------------------------------------------
     domain -- the dialogue domain
----------------------------------------------------------------------*/

is_resource_type( domain ).

resource_variable_of_type( domain, domain ).

of_type( homecentre, domain ).

resource_condition( domain, relevant_to_task(Move, Task, Plan), Domain ) :- 
	Domain : relevant_to_task( Move, Task, Plan ).

resource_condition( domain, relevant_answer(Query,Answer), Domain ) :-
	Domain : relevant_answer( Query, Answer ).

resource_condition( domain, dplan(Task, Goal, stackset(Plan)), Domain ) :-
	Domain : dplan( Task, Goal, Plan ).

resource_condition( domain, tplan(Task, Goal,stackset(Plan)), Domain ) :-
	Domain : tplan( Task, Goal, Plan ).


