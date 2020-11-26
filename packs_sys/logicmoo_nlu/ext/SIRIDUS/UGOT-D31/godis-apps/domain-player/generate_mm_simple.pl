
 
/*************************************************************************

         name: generate_simple.pl
      version: Jan 18, 2002; Dec 2002
  description: A simple generator
               Assumes condition output_form(Move, WordList) on lexicon,
	       where Output is a list of words (atoms)
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

%%% needs the variable domain of type domain,
%%% with a condition output_form/2

:- module(generate, [generate/0,init/0,quit/0]).

:- use_module( trindikit(tkit_tis_access), [check_condition/1, apply_update/1] ).

:- use_module( library(lists), [ append/3, reverse/2, member/2 ] ).
:-use_module(library(charsio)).
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
	format('generate called\n',[]),
	check_condition( $next_moves = oqueue(MoveList) or $next_moves = set(MoveList) or $next_moves = queue(MoveList) ),
	%reverse(MovesRev,Moves), % hack so ack-moves are done first
	format('Moves: ~w\n',[MoveList]),
	realize_moves( MoveList, String ),
	format('nl: ~w\n',[String]),
	apply_update( set( output, String ) ),
	realize_gui_moves( MoveList, String2),
	apply_update( set(output_gui, String2) ).
	
	%realize_gui_moves( MoveList, String2 ),
	%format('gui: ~w\n',[String2]),
	%append("[menu([",String2,S3),
	%append(S3,"])]",S),
	%apply_update( set(output_gui, S) ).
%%% godkänd realize_gui_moves...
%%% ask(set([action(handle_player),action(handle_playlist),action(handle_stations)])) 

init.

quit.

/*----------------------------------------------------------------------
     realize_moves( +Moves, -String )
----------------------------------------------------------------------*/

realize_moves( [], "" ).
realize_moves( [Move|Moves], S ) :-
	check_condition( output_form( $lexicon, Move, WordList ) ),
	wordlist2string( WordList, S1 ),
	(   Moves = []
	->  S = S1
	;   realize_moves( Moves, S2 ),
	    append( S1, [0' |S2], S )
	).

%%% %%ask(set([action(handle_player),action(handle_playlist),action(handle_stations)]))
%%% realize_gui_move(_,"").
/*
icm:und*pos:usr*song([stalker])
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
*/
realize_gui_moves(MoveList,Str):-
	(
	  member(ask(set(Alts)),MoveList),
	  realize_altq(Alts,Str);

	  member(ask(Q),MoveList),
	  Q\= _^_,
	  realize_yn(Str);

	  member(icm:und*int:usr*_,MoveList),
	  realize_yn(Str);

	  member(icm:sem*pos:_,MoveList),
	  realize_yn(Str);
	  
	  Str = "[menu([])]"
	).
realize_yn("[menu([item(yes),item(no)])]").
%realize_yn("[menu([item(ja),item(nej)])]").

realize_altq(Alts,Str):-
	ras(Alts,Apa),
	write_term_to_chars([menu(Apa)],Str,[quoted(true)]).

ras([],[]).
ras([Alt|Alts],[Apa|Apor]):-
	ra(Alt,Apa),
	ras(Alts,Apor).

ra(Alt,item(Apa)):-
	check_condition( output_form( $lexicon, Alt, WordList ) ),
	wordlist2string(WordList,Str),
	atom_chars(Apa,Str).
      
	
	


	

%realize_gui_moves( A, B ):-
%	realize_gui_moves( A, [], B ).

%realize_gui_moves( [], T, T ).
realize_gui_moves( [Move|Moves],Tmp, S ) :-
				%VILKET MOOOVEicm:und*int:usr*group([depeche,mode])
				%ILKET MOOOVEicm:und*int:usr*item([halo])
	(
	  %format("\n\n\n        VILKET MOOOVE ~w\n\n\n",[Move]),
	  (
	    %% song
	    Move = icm:und*int:usr*item(List);
	    Move = icm:und*int:usr*song(List);
	    Move = icm:und*int:usr*itemAdd(List);
	    Move = icm:und*int:usr*itemRem(List);
	    %% grupp
	    Move = icm:und*int:usr*group(List);
	    Move = icm:und*int:usr*song_artist(List);
	    Move = icm:und*int:usr*artist(List);
	    
	    Move = icm:und*int:usr*album(List);
	    Move = icm:und*int:usr*restart;
	    Move = icm:und*int:usr*what_to_play(List)
%%% 	    Move = icl:und*int:usr*_
	  ),
%%	  format("\n\n\n        VILKET MOOOVE2 ~w\n\n\n",[Move]),
	  WordList = [ja,nej]
%%% 	;
%%% 	  Move = icm:sem*pos:answer(group(X0)),
%%% 	  fix(X0,X),
%%% 	  WordList = X %%X == [eva,dahlgren]
	;
	 % format("\n\n\n        VILKET MOOOVE~w\n\n\n",[Move]),
	  Move = ask(set(_)),
	  check_condition( output_form( $lexicon, Move, WordList ) )
	;
	  WordList = []
	),
	%format("\n\n\n        WORDLIST  ~w\n\n\n",[WordList]),
	(
	  Moves = []
	->
	  list_to_string(WordList,[],SS),
	  !,
	  append(SS,Tmp,S)
	;
	  (
	    WordList = []
	  ->
	    realize_gui_moves(Moves,Tmp,TT)
	  ;
	    list_to_string(WordList,[],SS),
	    !,
	    append(SS,",",SS2),
	    realize_gui_moves(Moves,Tmp,TT)
	  ),
	  append(SS2,TT,S)
	).
list_to_string([],S,S).
list_to_string([X|Xs],Tmp,S):-
	(
	  X = 'Vill du ',
	  list_to_string(Xs,Tmp,S)
	;
	  X = ',',
	  list_to_string(Xs,Tmp,S)
	;
	  X = 'eller',
	  list_to_string(Xs,Tmp,S)
	;
	  chunk([X|Xs],[Y|Ys]),
	  !,
	  (
	    (Ys = [];Ys = ['?'])
	  ->
	    name(Y,WL),
	    append("'",WL,WLE),
	    append(WLE,"'",WL2),
	    itemize(WL2,WL22),
	    append(Tmp,WL22,S)
	  ;
	    name(Y,WL),
	    append("'",WL,WLE),
	    append(WLE,"'",WL2),
	    itemize(WL2,WL22),
	    append(WL22,",",WL3),
	    list_to_string(Ys,Tmp,T),
	    append(WL3,T,S)
	  )
	).

itemize(String,String2):-
	append("item(",String,S2),
	append(S2,")",String2).

chunk([X|Xs],Y):-
	(
	  'fråga om' = X,
	  chunk([X|Xs],[],Y)
	;
	  Y = [X|Xs]
	).

chunk([],A,A).
chunk([X|Xs], Tmp, Svar):-
	(
	  X = 'fråga om'
	->
	  chunk2(Xs,Ys),
	  chunk(Ys,Tmp,Svar)
	;
	  append(Tmp,[X],TT),
	  chunk(Xs,TT,Svar)
	).

%% once
chunk2(List,NewList):-
	(
	  member('fråga om',List),
	  append(PartList,['fråga om'|Xs],List),
	  fix(PartList,NewListT),
	  append(NewListT,['fråga om'|Xs],NewList)
	;
	  member('?',List),
	  append(PartList,['?'|Xs],List),
	  fix(PartList,NewListT),
	  append(NewListT,Xs,NewList)
	).
	
fix([X],[Y]):-
	(
	  X = eller,
	  Y = '',
	  !
	;
	  Y = X
	).
fix([X|Xs],[S]):-
	(
	  X = eller,
	  !,
	  fix(Xs,[S])
	;
	  fix(Xs,[SS]),
	  atom_concat(X,' ',S2),
	  atom_concat(S2,SS,S)
	).
	


%%% %% fixa komplett sträng...
%%% %%% saker hör ihop mellan ','
%%% list_to_string([],S,S).
%%% list_to_string([X|Xs],Tmp,S):-
%%% 	format("Vad kommer in: ~wdetta var det\n",[X]),
%%% 	(
%%% 	  X = 'Vill du ',
%%% 	  list_to_string(Xs,Tmp,S)
%%% 	;
%%% 	  X = ',',
%%% 	  list_to_string(Xs,Tmp,S)
%%% 	;
%%% 	  X = 'eller',
%%% 	  list_to_string(Xs,Tmp,S)
%%% 	;
%%% 	  chunk([X|Xs],[Y|Ys]),
%%% 	  ( 
%%% 	    Ys = []
%%% 	  ->
%%% 	    name(Y,WL),
%%% 	    append("'",WL,WLE),
%%% 	    append(WLE,"'",WL2),
%%% 	    itemize(WL2,WL22),
%%% 	    append(Tmp,WL22,S)
%%% 	  ;
%%% 	    name(Y,WL),
%%% 	    append("'",WL,WLE),
%%% 	    append(WLE,"'",WL2),
%%% 	    itemize(WL2,WL22),%% WL22 == item('Spela')
%%% 	    append(WL22,",",WL3),
%%% 	    list_to_string(Ys,Tmp,T),
%%% 	    append(WL3,T,S)
%%% 	  )
%%% 	).

%%% itemize(String,String2):-
%%% 	append("item(",String,S2),
%%% 	append(S2,")",String2).

%%% printit([]).
%%% printit([X|Xs]):-
%%% 	format("  ~w\n",[X]),
%%% 	printit(Xs).

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
