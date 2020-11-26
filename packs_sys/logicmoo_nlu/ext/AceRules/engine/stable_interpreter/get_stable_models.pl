% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(get_stable_models, [
		get_stable_models/4  % +SmRules, +Mode, -Model, +MaxModels
	]).

:- use_module('../op_defs').
:- use_module('../list_utils').

/** <module> Stable model generator

This module wraps the external tools Smodels and Lparse to calculate the answer set(s)
for a rule set using stable model semantics.

The rule set needs to be in poor format, and so are the resulting models. See
stable_interpreter.pl for details about these formats.

---+++ Technical remark:

I do not know how to read and write the same pipe in Prolog (i.e. executing an external
program, writing some input for the program, and reading its output). I did it now like this:

==
open(pipe('(echo "inputline1"; echo "inputline2"; echo "and so on") | program'), read, S)
==

@author Tobias Kuhn
@version 2007-02-09

@see stable_interpreter.pl
*/


%% get_stable_models(+SmRules, +Mode, -Model, +MaxModels)
%
% Calculates the stable models (Model) for the rule set in poor format (SmRules). Mode is
% stable or stable_strong. MaxModels defines the number of models that should be calculated,
% as a maximum.

get_stable_models(SmRules, Mode, Models, MaxModels) :-
    integer(MaxModels),
    MaxModels > 0,
    new_memory_file(MemHandle),
    open_memory_file(MemHandle, write, S),
    write(S, '( echo ""; '),
    echo_rules_command(SmRules, Mode, S),
    (Mode = stable_strong ->
    	format(S, ' ) | lparse --true-negation | smodels ~w | stable_interpreter/postsmod', [MaxModels])
    ;
    	format(S, ' ) | lparse | smodels ~w | stable_interpreter/postsmod', [MaxModels])
    ),
    close(S),
    memory_file_to_atom(MemHandle, Command),
    open(pipe(Command), read, P),
    get_models(P, Models),
    close(P),
    ( Models == [] ->
    	throw(ar_error('stable-interpreter.get-stable-model.NoModel', 'Program has no answer.'))
    ;
    	true
    ).


get_models(S, [Model|ModelsRest]) :-
    read(S, model(Model)),
    !,
    get_models(S, ModelsRest).

get_models(_, []).


echo_rules_command([], _, _).

echo_rules_command([(_, -Fact, [])|Rest], stable_strong, S) :-
    !,
    format(S, 'echo "-~w."; ', [Fact]),
    echo_rules_command(Rest, stable_strong, S).

echo_rules_command([(_, Fact, [])|Rest], Mode, S) :-
    !,
    format(S, 'echo "~w."; ', [Fact]),
    echo_rules_command(Rest, Mode, S).

echo_rules_command([(_, Head, Body)|Rest], Mode, S) :-
    !,
    write(S, 'echo "'),
    write_head(Head, Mode, S),
    write(S, ' :- '),
    write_body(Body, Mode, S),
    write(S, '."; '),
    echo_rules_command(Rest, Mode, S).


write_head(- Head, stable_strong, S) :-
    !,
    write(S, '-'),
    write_term(S, Head, [numbervars(true)]).

write_head(Head, _, S) :-
    write_term(S, Head, [numbervars(true)]).


write_body([B], Mode, S) :-
    !,
    write_body_element(B, Mode, S).

write_body([B|Rest], Mode, S) :-
    write_body_element(B, Mode, S),
    write(S, ', '),
    write_body(Rest, Mode, S).


write_body_element(- B, stable, S) :-
    !,
    write(S, 'not '),
    write_term(S, B, [numbervars(true)]).

write_body_element(~ B, stable, S) :-
    !,
    write(S, 'not '),
    write_term(S, B, [numbervars(true)]).

write_body_element(B, stable, S) :-
    write_term(S, B, [numbervars(true)]).

write_body_element(~ (- B), stable_strong, S) :-
    !,
    write(S, 'not -'),
    write_term(S, B, [numbervars(true)]).

write_body_element(~ B, stable_strong, S) :-
    !,
    write(S, 'not '),
    write_term(S, B, [numbervars(true)]).

write_body_element(- B, stable_strong, S) :-
    !,
    write(S, '-'),
    write_term(S, B, [numbervars(true)]).

write_body_element(B, stable_strong, S) :-
    write_term(S, B, [numbervars(true)]).

