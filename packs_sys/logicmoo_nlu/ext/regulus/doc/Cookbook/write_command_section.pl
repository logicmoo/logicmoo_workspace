
:- ['$REGULUS/Prolog/load'].
 
command_section_file('$REGULUS/doc/Cookbook/commands.tex').
config_entry_file('$REGULUS/doc/Cookbook/config-file-entries.tex').
blank_config_entry_file('$REGULUS/doc/blank_config_entries.pl').
regulus_prolog_dir('$REGULUS/Prolog').

write_command_section :-
	command_section_file(File),
	absolute_file_name(File, AbsFile),
	get_all_commands(Commands),
	open(AbsFile, write, S),
	%format(S, '\\section{Commands}~n~n', []),
	write_commands(Commands, S),
	close(S),
	length(Commands, N),
	format('~N--- Written ~d commands to ~w~n', [N, AbsFile]),
	!.

write_blank_config_file_entries :-
	findall(Entry,
		config_file_ref(Entry),
		List0),
	sort(List0, List),
	length(List, N),
	blank_config_entry_file(File),
	safe_absolute_file_name(File, AbsFile),
	write_atom_list_to_file(List, AbsFile),
	format('~N~n--- ~d entries written to ~w~n', [N, AbsFile]),
	!.

write_config_entry_section :-
	config_entry_file(File),
	absolute_file_name(File, AbsFile),
	get_all_config_file_entries(Entries),
	open(AbsFile, write, S),
	%format(S, '\\section{Commands}~n~n', []),
	write_commands(Entries, S),
	close(S),
	length(Entries, N),
	format('~N--- Written ~d config file entries to ~w~n', [N, AbsFile]),
	!.

get_all_commands(List) :-
	findall([PrintFormForCommand, Command, Doc, Section],
		(   loop_command(Command, Doc, Section),
		    print_form_for_term(Command, PrintFormForCommand)
		),
		List0),
	sort(List0, List).

get_all_config_file_entries(List) :-
	findall([PrintFormForEntry, Entry, 'doc irrelevant', Section],
		(   config_file_entry(Entry, _Doc, Section),
		    print_form_for_term(Entry, PrintFormForEntry)
		),
		List0),
	sort(List0, List).

print_form_for_term(Term, PrintFormForTerm) :-
	copy_term(Term, Term1),
	make_ground(Term1),
	format_to_atom('~w', [Term1], PrintFormForTerm).

%------------------------------------------------------------------

config_file_ref(FullEntry) :-
	regulus_prolog_dir(Dir),
	safe_directory_files(Dir, Files),
	!,
	member(File, Files),
	pathname_has_extension(File, 'pl'),
	format_to_atom('~w/~w', [Dir, File], File1),
	safe_absolute_file_name(File1, AbsFile),
	prolog_file_to_list(AbsFile, Contents),
	format('~N--- Searching ~w~n', [AbsFile]),
	config_file_ref(Contents, Entry),
	nonvar(Entry),
	make_ground(Entry),
	format_to_atom('~q.', [config_file_entry(Entry, '', no_section)], FullEntry).

config_file_ref(Var, _) :-
	var(Var),
	!,
	fail.
config_file_ref(regulus_config(Entry, _), Entry) :-
	!.
config_file_ref(get_regulus_config_item(Entry, _), Entry) :-
	!.
config_file_ref(T, Entry) :-
	compound(T),
	functor(T, _F, N),
	config_file_ref_args(N, T, Entry).

config_file_ref_args(I, T, Entry) :-
	I > 0,
	arg(I, T, Arg),
	config_file_ref(Arg, Entry).
config_file_ref_args(I, T, Entry) :-
	I > 1,
	I1 is I - 1,
	config_file_ref_args(I1, T, Entry).

%------------------------------------------------------------------

write_blank_config_file_entries1([]).
write_blank_config_file_entries1([F | R]) :-
	write_blank_config_file_entry(F),
	!,
	write_blank_config_file_entries1(R).

write_blank_config_file_entry(Entry) :-
	format('~N~w~n', [Entry]).

%------------------------------------------------------------------

write_commands([], _S).
write_commands([F | R], S) :-
	write_command(F, S),
	!,
	write_commands(R, S).

write_command([_PrintForm, Command, Doc, Section], S) :-
	format('~N--- Writing out command: ~w~n', [Command]),
	format_command_to_string(Command, CommandString),
	make_command_string_into_section_label(CommandString, CommandLabelString),
	format(S, '~N~n\\section{{\\tt ~s}}~n', [CommandString]),
	%format(S, '~N~n\\section{~s}~n', [CommandString]),
	format(S, '~N\\label{Section:~s}~n~n', [CommandLabelString]),
	atom_codes(Doc, DocString0),
	tex_string(DocString0, DocString),
	(   DocString = "" ->
	    format(S, '~N{\\em [No documentation yet.]}~n', [])
	;
	    DocString = "doc irrelevant" ->
	    true
	;
	    format(S, '~N{\\em [~s.]}~n', [DocString])
	),
	include_or_create_doc_file_for_command(S, Command),
	(   ( DocString = "doc irrelevant", empty_doc_file_for_command(Command) ) ->
	    format(S, '~N{\\em [No documentation yet.]}~n', [])
	;
	    true
	),
	(   Section = no_section ->
	    true
	;
	    otherwise ->
	    format(S, '~N~nSee Section~~\\ref{Section:~w}.~n', [Section])
	),
	!.
write_command(F, S) :-
	format('~N*** Error: bad call: ~w~n', [write_command(F, S)]),
	fail.

%------------------------------------------------------------------

include_or_create_doc_file_for_command(S, Command) :-
	doc_file_for_command(Command, DocFile),
	(   safe_file_exists(DocFile) ->
	    format(S, '~N~n', []),
	    copy_file_to_latex_stream(DocFile, S),
	    format(S, '~N~n~n', [])
	;
	    open(DocFile, write, DummyS),
	    close(DummyS)
	),
	!.
include_or_create_doc_file_for_command(S, Command) :-
	format('~N*** Error: bad call: ~w~n',
	       [include_or_create_doc_file_for_command(S, Command)]),
	fail.
