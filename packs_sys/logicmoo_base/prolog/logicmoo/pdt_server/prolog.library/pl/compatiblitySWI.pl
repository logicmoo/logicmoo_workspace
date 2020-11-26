/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- use_module(library(error)). 
:- use_module(library(memfile)).  

:- multifile test/1.

/*
 * SWI Compability
 * specific for SWI Prolog
 */

:- dynamic outdir/1.
:- dynamic file_output/1.
:- dynamic output_to_file/0.
:- dynamic output_to_memory/3.
:- dynamic output_to_memory_key/1.


index_information(Predicate, I) :-
	predicate_property(Predicate, indexed(I)).

output_to_file.

toggle_out :-
    output_to_file,
    print('output console'),
    !,
    retract(output_to_file).
toggle_out :-
    print('output file'),
    assert(output_to_file).

/*
 * open_printf_to_memory(Key) 
 *
 * Use the following pattern to ensure closing of your stream:
 *    call_cleanup(
 *   	  (	    
 *    	open_printf_to_memory(<key>),
 *	  ), close_printf_to_memory(<key>, Content)).
 *
 */
 
open_printf_to_memory(Key) :-
    output_to_memory(Key,Handle,Stream),
    retractall(output_to_memory(Key,_,_)),
    catch((
    close(Stream),
    free_memory_file(Handle)),
    Exception,true),
    format('EXCEPTION: catched Exception in open_printf_to_memory. Possible reason: trying to create an existing stream~nSTREAM: ~w ~n~w ~n', [Key,Exception]),
    fail.

open_printf_to_memory(Key) :-
    !, 
    new_memory_file(Handle),
    open_memory_file(Handle, write, Stream),
    asserta(output_to_memory(Key,Handle,Stream)),
	select_printf(Key).

/*
 * close_printf_to_memory(+Key,-Content) 
 *
 * Closes the current memory stream with key Key. The output is written into Content.
 * After closing, printf is set to the last memory stream that was opened before this one.
 * If Key does not exist, nothing will happen
 *
 */
my_call_cleanup(Goal,Cleanup):-
    catch(Goal,E,true),!,
    Cleanup,
    (	nonvar(E)
    ->	throw(E)
    ;	true
    ).
my_call_cleanup(_,Cleanup):-    
    Cleanup,
    fail.
    
close_printf_to_memory(Key,Content):-
	
    my_call_cleanup(
    	close_and_get_content(Key,Content),
    	delete_printf_to_memory(Key)
    ).
%close_printf_to_memory(_,_) :-
 %   throw('no memory file exists').
 

close_and_get_content(Key,Content):-
    output_to_memory(Key,Handle,Stream),
	close(Stream),
	memory_file_to_atom(Handle,Content).

delete_printf_to_memory(Key):-

    output_to_memory(Key,Handle,_),
    retractall(output_to_memory(Key,_,_)),
	free_memory_file(Handle),
	(  	output_to_memory_key(Key) 
    -> 	retract(output_to_memory_key(Key)),
		select_printf_last
	;  	true
	).

%close_printf_to_memory(Key,Content) :-
%	output_to_memory(Key,Handle,Stream),
%	!,
%    call_cleanup(      	
%    	memory_file_to_atom(Handle,Content),
%		do_close_printf_to_memory(Key,Stream,Handle)		
%	).
%
%do_close_printf_to_memory(Key,Stream,Handle):-
%    close(Stream),
%    free_memory_file(Handle),
%   	retract(output_to_memory(Key, Handle,Stream)),    
%    (  	output_to_memory_key(Key) 
%    -> 	retract(output_to_memory_key(Key)),
%		select_printf_last
%	;  	true
%	).
    

    

close_all_printf_to_memory:-
    close_all_printf_to_memory(_).
    
close_all_printf_to_memory(ContentTemp2):-
    not(output_to_memory(_,_,_)),
    ContentTemp2 = ''.    

close_all_printf_to_memory(Content) :-
    output_to_memory(Key,_,_),   
    !, 
    close_printf_to_memory(Key,ContentTemp),
    close_all_printf_to_memory(ContentTemp2),
    concat(ContentTemp,ContentTemp2,Content).

/*
 * select_printf(+Key)
 *
 * Select current memory stream.
 */

select_printf(Key) :-
	retractall(output_to_memory_key(_)),
	assert(output_to_memory_key(Key)).

select_printf_last :-
	output_to_memory(LastKey,_,_),
	select_printf(LastKey).
	
select_printf_last.

test(memory_file) :-
    open_printf_to_memory(testkey),
    printf(asdf),
    printf(asdf),
    close_printf_to_memory(testkey,Content),
    Content = asdfasdf.

printf(_format, _args) :-
    output_to_memory(_,_,_stream),
    !,
    format(_stream, _format, _args).

%printf(_format, _args) :-
%    output_to_tmp(_stream),
%    !,
%    format(_stream, _format, _args).

printf(_format, _args) :-
    output_to_file,
    file_output(_stream),
    !,
    format(_stream, _format, _args).

printf(_format, _args) :-
    current_output(_stream),
    format(_stream, _format, _args),
    flush_output.


printf(_format) :-
    printf(_format, []).
%    file_output(_stream),
%    current_output(_stream),
%    format(_stream, _format, []).

    

println :-
    printf('~n').


/*
	assert1T(+Term)
	Asserts term Term. If Term already exists it will not be 
	asserted. The predicate always succeeds.
*/
assert1T(Fact) :- call(Fact) -> true ; assert(Fact).

/*
	retractT(+Term)
	Retracts Term. The predicate succeeds also if a fact 
	unifying with Term does not exists when the predicate
	is called.
*/

retractT(Fact) :- call(Fact) -> retract(Fact) ; true.

/* The following, original versions of the above two 
   predicates are BAD :-( !!!

assert1T(_x) :- assert1(_x).
assert1T(_).

retractT(_x) :- retract(_x).
retractT(_).

   They create one useless choicepoint and backtrack even if
   the fact is actually asserted / retracted. That is expensive 
   and might be the cause of spurious follow-up errors in 
   contexts that expect assert not to backtrack or retract 
   not to backtrack once more than the existing number of 
   retractable facts!    -- GK, 27.03.2009
*/

/*
	stringAppend(?Atom1, ?Atom2, Atom3)
	
	Atom3 forms the concatination of Atom1 and Atom2.
	At least two arguments must be instantiated.
	
	Mapped to atom_concat/3. (Needed for ISO-Prolog Compatibility).
*/

stringAppend(S1, _S2, _Ret) :-
	nonvar(S1),
	S1 = unqualified(_,_),
	throw(exception).

stringAppend(_S1, S2, _Ret) :-
	nonvar(S2),
	S2 = unqualified(_,_),
	throw(exception).

stringAppend(_S1, _S2, Ret) :-
	nonvar(Ret),
	Ret = unqualified(_,_),
	throw(exception).

stringAppend(Str1, Str2, Ret) :-
    atom_concat(Str1, Str2, Ret).

/*
stringAppend(_str1, _str2, _Ret) :-
    atomic(_str1),
    atomic(_str2),
    atom_concat(_str1, _str2, _Ret),
    !.
    

stringAppend(_str1, _str2, _Ret) :-
    atomic(_str1),
    atomic(_Ret),
    atom_concat(_str1, _str2, _Ret),
    !.
    

stringAppend(_str1, _str2, _Ret) :-
    atomic(_str2),
    atomic(_Ret),
    atom_concat(_str1, _str2, _Ret),
    !.
    
stringAppend(_str1, _str2, _Ret) :-
    write(_str1),
	user:debugme(_str1, _str2, _Ret).

user:debugme(_str1, _str2, _Ret).
	
	*/
	
test('stringAppend/3#1') :- stringAppend('','','').
test('stringAppend/3#2') :- stringAppend('a','','a').
test('stringAppend/3#3') :- stringAppend('','a','a').
test('stringAppend/3#4') :- stringAppend('a','b','ab').
test('stringAppend/3#4') :- stringAppend('uwe ','tarek bardey','uwe tarek bardey').

list2java(l, S) :-
    concat_atom(l, ', ', S).


mapPredicate(_, _ ,[] ,[]).
mapPredicate(Pred, Arg1 ,[Arg2H | Arg2T] ,[RetH | RetT]) :-
               Q =.. [Pred, Arg1, Arg2H, RetH],
               call(Q),
               mapPredicate(Pred, Arg1, Arg2T, RetT).

sum(Int1, Int2, Int3) :- plus(Int1, Int2, Int3).

int2string(_int, _string) :- swritef(_string, "%d", [_int]).

equals(_term1, _term2) :- _term1 = _term2.
nequals(_term1, _term2) :- _term1 \= _term2.

debugPrint(_str) :- writef(_str).




:- dynamic(output_to_memory/2).

open_print_to_memory :-
	output_to_memory(_,_),
	throw('memory file still open').

open_print_to_memory :-
    !,
    new_memory_file(Handle),
    open_memory_file(Handle, write, Stream),
    current_output(Out),
    assert(output_to_memory(Handle,Out)),
    set_output(Stream).

close_print_to_memory(Content) :-
    output_to_memory(Handle,Out),
    !,
    current_output(MemStream),
    close(MemStream),
    set_output(Out),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle),
    retract(output_to_memory(Handle,Out)).

close_print_to_memory(_) :-
    throw('no memory file exists').

/*
:- redefine_system_predicate(get_single_char(_)).

get_single_char(A) :-
    print(aha),
    system:get_single_char(A).
*/

/*
 * disable_tty_control
 *
 * Disables tty control char-wise read on the windows platform.
 */
:- if(pdt_support:pdt_support(tty_control)).
disable_tty_control :- 
	(	current_prolog_flag(windows, _)
	->	set_prolog_flag(tty_control, false)
	;	true
	). 

:- disable_tty_control.
:- endif.


read_term_atom(Atom,Term,Options):-
	atom_to_memory_file(Atom,Handle),
	open_memory_file(Handle, read, Stream),
	catch(read_term(Stream,Term,Options),
	      Exception,
	      write(Exception)),
	close(Stream),free_memory_file(Handle),
	( nonvar(Exception) ->
	   throw(Exception);
	   true
	).
	
