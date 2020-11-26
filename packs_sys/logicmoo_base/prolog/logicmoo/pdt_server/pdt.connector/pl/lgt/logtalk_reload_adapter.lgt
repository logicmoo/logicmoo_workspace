/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- object(logtalk_reload_adapter).

:- multifile(logtalk::message_hook/4).
:- dynamic(logtalk::message_hook/4).

logtalk::message_hook(Term, Kind, core, Tokens) :-
    with_mutex('reloadMutex', (
		{pdt_reload:warning_and_error_tracing},
		(	arg(2, Term, StartLine-_EndLine) ->
			true
		;	logtalk_load_context(term_position, StartLine-_EndLine)
		),
		logtalk_load_context(source, Path),
		functor(Kind, Level, _),
		{pdt_reload:assertz(traced_messages(logtalk, Level, StartLine, Tokens, Path))},
		{pdt_reload:trace_reload(traced_messages(logtalk, Level, StartLine, Tokens, Path))},
	%	assertz(user:am(_Term, Level,Lines)),
		fail
	)).

logtalk::message_hook(loading_file(FullPath, _Options), _, core, _) :-
	with_mutex('reloadMutex', (
		{pdt_reload:warning_and_error_tracing},
		{pdt_reload:assertz(reloaded_file__(FullPath))},
		fail
	)).

logtalk::message_hook(reloading_file(FullPath, _Options), _, core, _) :-
	with_mutex('reloadMutex', (
		{pdt_reload:warning_and_error_tracing},
		{pdt_reload:assertz(reloaded_file__(FullPath))},
		fail
	)).

%	logtalk::message_hook(_Term, _Kind, _Component, _Tokens) :-
%		nonvar(Term),
%		arg(1, Term, Path),
%		is_absolute_file_path(Path),
%		arg(2, Term, Lines),
%		(	integer(Lines)
%			% if integer(Lines), Lines =< 0 -> no line number available
%		;	Lines = Begin - End,
%		 	integer(Begin),
%			integer(End)
%		),
%		fail.

               /*************************************
                * PDT RELOAD                        *
                *************************************/

:- public(pdt_reload/1).
%% pdt_reload(File) is det.
%
% wrapper for consult. Only used to ignore PLEditor triggered consults in the history.

pdt_reload(FullPath) :-
	write(FullPath), nl,
	% ensure that the argument is a path to a Logtalk source file
	(	sub_atom(FullPath, _, 4, 0, '.lgt') ->
		true
	;	sub_atom(FullPath, _, 8, 0, '.logtalk')
	),
	(	logtalk::loaded_file_property(FullPath, flags(Flags)) ->
		% we're reloading a source file; use the same explicit flags as before
		logtalk_load(FullPath, Flags)
	;	% first time; assume only implicit compilation options
		logtalk_load(FullPath)
	).

:- end_object.