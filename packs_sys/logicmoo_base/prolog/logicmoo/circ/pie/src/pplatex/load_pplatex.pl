:- prolog_load_context(directory, Dir),
	assert(library_directory(Dir)),
	concat_atom([Provers, 'pplatex'], Dir),
	concat_atom([Provers, swilib], Swilib),
	asserta(user:file_search_path(swilib, Swilib)).

:- use_module(pplatex).
:- use_module(swilib(options)).

:- op(1200,xfx, user:(=>)).
:- op(1200,xfx, user:(<=>)).
:- op(1160,xfx, user:(<->)).
:- op(1050,xfy, user:(<-)).
:- op(550, fy, user:(~)).

main :-
        catch(process, E, (print_message(error, E), fail)),
        halt.
main :-
        halt(1).

process :-
	current_prolog_flag(argv, Args),
	( select(OptionArg, Args, Files),
	  sub_atom(OptionArg, 0, B, _, '--options='),
	  sub_atom(OptionArg, B, _, 0, OptionsAtom),
	  term_to_atom(Options, OptionsAtom)
	; Files = Args,
	  Options = []
	),
	default_options(Options,
			[format=latex, style=brief, maxpos=50],
			Options1),
	process_files(Files, Options1).

process_files(Files, Options) :-
        ( member(F, Files),
	  read_file_to_codes(F, Codes, []),
	  ( last(Codes, 0'.) ->
	    Codes1 = Codes,
	    Options1 = [finally='.'|Options]
	  ; last(Codes, 0',) ->
	    append(Codes, [0',], Codes2),
	    append(Codes2, [0'.], Codes1),
	    Options1 = [finally=','|Options]
	  ; append(Codes, [0'.], Codes1),
	    Options1 = Options
	  ),
	  read_from_chars(Codes1, Term),
	  format('%% Term: ~q~n', [Term]),
	  pp_form(Term, Options1),
	  fail
	; true
	).
	