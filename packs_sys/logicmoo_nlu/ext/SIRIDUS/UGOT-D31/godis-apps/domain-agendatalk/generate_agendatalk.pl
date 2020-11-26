
 
/*************************************************************************

         name: generate_agendatalk.pl
      version: Jan 18, 2002; Dec 2002
  description: A simple generator
               Assumes condition output_form(Move, WordList) on lexicon,
	       where Output is a list of words (atoms)
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

%%% needs the variable domain of type domain,
%%% with a condition output_form/2

:- module(generate, [generate/0,init/0,quit/0]).

%TRINDIKIT4 test
:- ( current_module(tkit_properties)->
       use_module(trindikit(tkit_tis_access),
		  [check_condition/1, apply_update/1]),
       use_module(trindikit(tkit_operators));
       use_module(library(tis_access),
		  [check_condition/1, apply_update/1]),
       use_module(library(tis_operators))).


:- use_module( library(lists), [ append/3, reverse/2, member/2 ] ).
/*----------------------------------------------------------------------
     TIS access restrictions
----------------------------------------------------------------------*/

read_access( [ next_moves, output, lexicon ] ).
write_access( [ output ] ).

tis_requirements( [ output: string,
		    lexicon: lexicon,
		    next_moves: set(dmove) or
		      latest_moves: oqueue(dmove) or
		      latest_moves: queue(dmove) ] ).

/*----------------------------------------------------------------------
     generate
----------------------------------------------------------------------*/

generate :-
	check_condition( $next_moves = oqueue(MoveList) or $next_moves = set(MoveList) or $next_moves = queue(MoveList) ),
	%reverse(MovesRev,Moves), % hack so ack-moves are done first
	realize_moves( MoveList, String ),
	apply_update( set( output, String ) ).


init.

quit.

/*----------------------------------------------------------------------
     realize_moves( +Moves, -String )
----------------------------------------------------------------------*/

realize_moves( [], "" ).
realize_moves( [Move|Moves], S ) :-
	check_condition($/shared/com = ShCom),
	check_condition( output_formcom( $lexicon, Move, ShCom,WordList ) ),
	wordlist2string( WordList, S1 ),
	(   Moves = []
	->  S = S1
	;   realize_moves( Moves, S2 ),
	    append( S1, [0' |S2], S )
	).

/*----------------------------------------------------------------------
     wordlist2string( +AtomList, -String )
     -- translates a a list of atoms (words) to prolog string
----------------------------------------------------------------------*/

wordlist2string( [], "" ).
% no space before punctuation mark at end of sentence
wordlist2string( [W,Punct], Str ):-
	member(Punct, ['.','?','!']),!,
	name(W, StrW ),
	name(Punct, StrPunct),
	append(StrW, StrPunct, Str ). 
wordlist2string( [W|Ws], Str ):-
	name(W, StrW ),
	wordlist2string(Ws, Str0 ),
	append( StrW, " ", Str1 ),
	append( Str1, Str0, Str ).

init.
quit.
