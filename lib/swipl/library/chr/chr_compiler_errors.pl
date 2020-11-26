/*  Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2011, K.U. Leuven
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(chr_compiler_errors,
		[
			chr_info/3,
			chr_warning/3,
			chr_error/3,
			print_chr_error/1
		]).

:- use_module(chr_compiler_options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_info(+Type,+FormattedMessage,+MessageParameters)

chr_info(_,Message,Params) :-
	( \+verbosity_on ->
		true
	;
		long_line_with_equality_signs,
		format(user_error,'CHR compiler:\n',[]),
		format(user_error,Message,Params),
		long_line_with_equality_signs
	).


%% SWI begin
verbosity_on :-
	current_prolog_flag(verbose,V), V \== silent,
	current_prolog_flag(verbose_load,true).
%% SWI end

%% SICStus begin
%% verbosity_on.  % at the moment
%% SICStus end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_warning(+Type,+FormattedMessage,+MessageParameters)

chr_warning(deprecated(Term),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: deprecated syntax      ~w.\n',[Term]),
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
        format(user_error,'    Support for deprecated syntax will be discontinued in the near future!\n',[]),
	long_line_with_equality_signs.

chr_warning(internal,Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: something unexpected happened in the CHR compiler.\n',[]),
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
        format(user_error,'    Your program may not have been compiled correctly!\n',[]),
        format(user_error,'    Please contact tom.schrijvers@cs.kuleuven.be.\n',[]),
	long_line_with_equality_signs.

chr_warning(unsupported_pragma(Pragma,Rule),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: unsupported pragma ~w in ~@.\n',[Pragma,format_rule(Rule)]),
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
        format(user_error,'    Pragma is ignored!\n',[]),
	long_line_with_equality_signs.
chr_warning(problem_pragma(Pragma,Rule),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: unsupported pragma ~w in ~@.\n',[Pragma,format_rule(Rule)]),
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
	long_line_with_equality_signs.

chr_warning(_,Message,Params) :-
	( chr_pp_flag(verbosity,on) ->
		long_line_with_equality_signs,
		format(user_error,'CHR compiler WARNING:\n',[]),
		format(user_error,'    `--> ',[]),
		format(user_error,Message,Params),
		long_line_with_equality_signs
	;
		true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_error(+Type,+FormattedMessage,+MessageParameters)

chr_error(Type,Message,Params) :-
	throw(chr_error(error(Type,Message,Params))).

print_chr_error(error(Type,Message,Params)) :-
	print_chr_error(Type,Message,Params).

print_chr_error(syntax(Term),Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR: invalid syntax "~w".\n',[Term]),
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
	long_line_with_equality_signs.

print_chr_error(type_error,Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler TYPE ERROR:\n',[]),
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
	long_line_with_equality_signs.

print_chr_error(internal,Message,Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR: something unexpected happened in the CHR compiler.\n',[]),
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
        format(user_error,'    Please contact tom.schrijvers@cs.kuleuven.be.\n',[]),
	long_line_with_equality_signs.

print_chr_error(cyclic_alias(Alias),_Message,_Params) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR: cyclic alias "~w".\n',[Alias]),
	format(user_error,'    `--> Aborting compilation.\n',[]),
	long_line_with_equality_signs.

print_chr_error(_,Message,Params) :-
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR:\n',[]),
	format(user_error,'    `--> ',[]),
	format(user_error,Message,Params),
	long_line_with_equality_signs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- public
	format_rule/1.			% called using format/3 `@'

format_rule(PragmaRule) :-
	PragmaRule = pragma(_,_,Pragmas,MaybeName,N),
	( MaybeName = yes(Name) ->
		write('rule '), write(Name)
	;
		write('rule number '), write(N)
	),
	( memberchk(source_location(SourceLocation),Pragmas) ->
		write(' at '),
		write(SourceLocation)
	;
		true
	).

long_line_with_equality_signs :-
	format(user_error,'================================================================================\n',[]).
