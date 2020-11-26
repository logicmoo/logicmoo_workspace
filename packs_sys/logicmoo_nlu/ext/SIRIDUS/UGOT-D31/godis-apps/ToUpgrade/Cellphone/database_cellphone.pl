
 
/*************************************************************************

         name: database_cellphone.pl 
      version: 
  description: Database for MiniMobilePhone
       author: Peter Bohlin, Staffan Larsson, Stina Ericsson
 
*************************************************************************/

:- module( database_cellphone, [ consultDB/3, updateDB/2 ] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- dynamic entry/2, sec_code/1.

/*----------------------------------------------------------------------
     consultDB( +Beliefs, +Query, -Answer )
     -- Returns (if it succeeds) an Answer to a Query given
        background Beliefs and a Domain
----------------------------------------------------------------------*/

consultDB( _^Query, Known, Query ):-
        db( Facts ),
        select( Query, Facts, KnownFacts ), 
        % check that known facts are consistent with db post
        \+ (
             member( Fact, KnownFacts ),
             \+ implies( Known, Fact )
           ).

implies( Bel, Fact ) :-
        member( Fact, Bel ).


updateDB( add_entry, Known ) :-
	member( name_to_add(Name), Known ),
	member( number_to_add(Number), Known ),
	%to_number( Number1, Number ),
	assert( entry(Name,Number) ).

updateDB( delete_entry, Known ) :-
	member( entry_to_delete(Name), Known ),
	entry(Name,Number),
	retract( entry(Name,Number) ).

updateDB( change_security_code, Known ) :-
	member( current_security_code(CurrCode), Known ),
	member( new_security_code(NewCode), Known ),
	%to_number( CurrCode1, CurrCode ),
	%to_number( NewCode1, NewCode ),
	retract( sec_code(CurrCode) ),
	assert( sec_code(NewCode) ).

to_number( N, N ) :-
	number( N ), !.

to_number( A, N ) :-
	atom_chars( A, Cs ),
	number_chars( N, Cs ).

to_atom( N, N ) :-
	atom( N ), !.

to_atom( N, A ) :-
	number_chars( N, Cs ),
	atom_chars( A, Cs ).

%%% db( -Facts )

db( [ task(change_security_code), current_security_code(_UserInputCode), security_codeDB(CodeDB) ] ) :-
	sec_code(Code0),
	CodeDB = Code0. % Alex
%	to_atom(Code0,CodeDB).

db( [ the_security_code(_UserInputCode), security_codeDB(CodeDB) ] ) :-
	sec_code(Code0),
	CodeDB = Code0. % Alex
%	to_atom(Code0,CodeDB).

/*
% NB Binding NewSC does not work
db( [ current_security_code(CurrSC), new_security_code(NewSC),
      security_code_changed_toDB(NewSC) ] ).
*/

/*
db( [ current_security_code(CurrSC), new_security_code(NewSC),
      security_code_changed_toDB(ChangedCode) ] ) :-
	retract(sec_code(CurrSC)),
	assert(sec_code(NewSC)),
	sec_code(ChangedCode).
*/

db( [ search_for_name(Name), pb_numberDB(Number) ] ) :-
	entry(Name,Number).

db( [ entry_to_delete(Name), pb_numberDB(Number) ] ) :-
	entry(Name,Number).

/*
db( [ entry_to_delete(Name), entry_deletedDB(Number) ] ) :-
	entry(Name,Number),
	retract(entry(Name,Number)).

db( [ name_to_add(Name), number_to_add(Number), entry_addedDB(Name) ] ) :-
	assert(entry(Name,Number)).
*/


%%% db facts for the mobile phone domain


%% security code
sec_code('9999').


%% phone book

%entry(Name,Number).
entry(anna,565644).
entry(john,338896).
entry(mia,543912).



%%
security_code_defined(Code) :-
	user:val(lexicon,Lexicon),
	Lexicon: lex_security_code(Code).









