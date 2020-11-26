
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(doc_utils,
	[doc_file_for_command/2,
	 empty_doc_file_for_command/1,

	 print_doc_for_command_or_config_file_item/1,
	 
	 format_command_to_string/2,
	 
	 copy_file_to_latex_stream/2,
	 make_command_string_into_section_label/2,
	 make_string_into_file_name/2,

	 tex_string/2,
	 detex_string/2
     ]
).

:- use_module('$REGULUS/Prolog/generate').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

doc_file_prefix('$REGULUS/doc/commanddoc').

detex_rules_file('$REGULUS/doc/detex_rules.pl').

tex_rules_file('$REGULUS/doc/tex_rules.pl').

% Construct a unique name in case we have two copies of Regulus loading at once.
% This is in particular important when we have multiple translation servers.

compiled_detex_rules_file(File) :-
	safe_process_id(Id),
	format_to_atom('$REGULUS/doc/compiled_detex_rules_~d.pl', [Id], File).

compiled_tex_rules_file(File) :-
	safe_process_id(Id),
	format_to_atom('$REGULUS/doc/compiled_tex_rules_~d.pl', [Id], File).

%---------------------------------------------------------------

print_doc_for_command_or_config_file_item(Item) :-
	findall([Command, HelpText],
		(   command_matches_item(Item, Command, HelpText)
		;
		    config_file_entry_matches_item(Item, Command, HelpText)
		),
		CommandsAndHelpTexts),
	(   CommandsAndHelpTexts = [] ->
	    format('~N--- Nothing found called "~w"~n', [Item])
	;
	    otherwise ->
	    print_doc_for_commands(CommandsAndHelpTexts)
	).

command_matches_item(Item, Command, HelpText) :-
	Command = [Item | _],
	user:loop_command(Command, HelpText).

config_file_entry_matches_item(Item, Command, HelpText) :-
	Command = Item,
	user:config_file_entry(Command, HelpText, _).
config_file_entry_matches_item(Item, Command, HelpText) :-
	Command =.. [Item, _],
	user:config_file_entry(Command, HelpText, _).

print_doc_for_commands([]).
print_doc_for_commands([F | R]) :-
	print_doc_for_command(F),
	!,
	print_doc_for_commands(R).

print_doc_for_command([Command, HelpText]) :-
	format('~N~n', []),
	print_loop_command_or_config_file_entry(Command),
	(   HelpText = '' ->
	    true
	;
	    otherwise ->
	    format('~N[Brief doc: ~w]~n', [HelpText])
	),
	doc_file_for_command(Command, DocFile),
	(   safe_file_exists(DocFile) ->
	    read_file_to_string(DocFile, String)
	;
	    String = ""
	),
	(   ( HelpText = '', String = "" ) ->
	    format('~N--- No documentation available~n', [])
	;
	    String = "" ->
	    format('~N--- Full documentation not available~n', [])
	;
	    otherwise ->
	    detex_string(String, String1),
	    format('~s', [String1])
	),
	!.
print_doc_for_command(Command) :-
	format('~N*** Error: bad call: ~w~n', [print_doc_for_command(Command)]),
	fail.

print_loop_command_or_config_file_entry(Command) :-
	is_list(Command),
	user:print_loop_command(Command),
	!.
print_loop_command_or_config_file_entry(Config) :-
	atom(Config),
	format('~N~w~n', [Config]),
	!.
print_loop_command_or_config_file_entry(Config) :-
	compound(Config),
	functor(Config, F, 1),
	format('~N~w(<Arg>)~n', [F]),
	!.
print_loop_command_or_config_file_entry(Config) :-
	format('~N*** Error: bad call: ~w~n',
	       [print_loop_command_or_config_file_entry(Config)]),
	fail.

%---------------------------------------------------------------

doc_file_for_command(Command, AbsDocFile) :-
	doc_file_prefix(Prefix),
	replace_vars_by_args(Command, 1, CommandWithArgs),
	(   atom(CommandWithArgs) ->
	    CommandWithArgs = CommandAtom0
	;
	    otherwise ->
	    join_with_spaces(CommandWithArgs, CommandAtom0)
	),
	atom_codes(CommandAtom0, CommandString),
	make_string_into_file_name(CommandString, CommandString1),
	format_to_atom('~w/~s.txt', [Prefix, CommandString1], DocFile),
	safe_absolute_file_name(DocFile, AbsDocFile),
	!.
doc_file_for_command(Command, AbsDocFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [doc_file_for_command(Command, AbsDocFile)]),
	fail.

%------------------------------------------------------------------

empty_doc_file_for_command(Command) :-
	doc_file_for_command(Command, DocFile),
	read_file_to_string(DocFile, String),
	String = "",
	!.

%------------------------------------------------------------------

format_command_to_string(Command, CommandString) :-
	replace_vars_by_args(Command, 1, CommandWithArgs),
	(   atom(CommandWithArgs) ->
	    CommandWithArgs = CommandAtom0
	;
	    otherwise ->
	    join_with_spaces(CommandWithArgs, CommandAtom0)
	),
	atom_codes(CommandAtom0, CommandString0),
	tex_string(CommandString0, CommandString),
	!.
format_command_to_string(Command, CommandString) :-
	format('~N*** Error: bad call: ~w~n',
	       [format_command_to_string(Command, CommandString)]),
	fail.

replace_vars_by_args(V, _I, ['Args']) :-
	var(V),
	!.
replace_vars_by_args(Atom, _, Atom) :-
	atomic(Atom),
	!.
replace_vars_by_args(Config, _, Config1) :-
	compound(Config),
	Config =.. [Entry, Var],
	var(Var),
	format_to_atom('~w(Arg1)', [Entry], Config1),
	!.
replace_vars_by_args([F | R], I, [F1 | R1]) :-
	(   var(F) ->
	    %format_to_atom('$\\langle$Arg ~d$\\rangle$', [I], F1),
	    %format_to_atom('(Arg ~d)', [I], F1),
	    format_to_atom('Arg~d', [I], F1),	    
	    I1 is I + 1
	;
	    otherwise ->
	    F1 = F,
	    I1 = I
	),
	!,
	replace_vars_by_args(R, I1, R1).
replace_vars_by_args(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [replace_vars_by_args(X, Y)]),
	fail.

copy_file_to_latex_stream(InFile, SOut) :-
	open(InFile, read, SIn),
	copy_latex_stream(SIn, SOut),
	close(SIn).	

copy_latex_stream(SIn, SOut) :-
	read_line(SIn, Line),
	copy_latex_stream1(Line, SIn, SOut).

copy_latex_stream1(Line, _SIn, _SOut) :-
	Line = end_of_file,
	!.
copy_latex_stream1(Line, SIn, SOut) :-
	%latex_quote_chars_in_string(Line, Line1),
	Line = Line1,
	format(SOut, '~N~s~n', [Line1]),
	copy_latex_stream(SIn, SOut).

make_command_string_into_section_label([], []).
make_command_string_into_section_label([F | R], R1) :-
	char_unsuitable_for_latex_label(F),
	!,
	make_command_string_into_section_label(R, R1).
make_command_string_into_section_label([F | R], [F1 | R1]) :-
	char_substitution_for_latex_label(F, F1),
	!,
	make_command_string_into_section_label(R, R1).
make_command_string_into_section_label([F | R], [F | R1]) :-
	!,
	make_command_string_into_section_label(R, R1).

make_string_into_file_name([], []).
make_string_into_file_name([F | R], [F1 | R1]) :-
	char_substitution_for_file_name(F, F1),
	!,
	make_string_into_file_name(R, R1).
make_string_into_file_name([F | R], [F | R1]) :-
	!,
	make_string_into_file_name(R, R1).

char_requiring_latex_quote(0'_).

char_unsuitable_for_latex_label(0' ).
char_unsuitable_for_latex_label(0'\\).
char_unsuitable_for_latex_label(0'().
char_unsuitable_for_latex_label(0')).

char_substitution_for_latex_label(0'_, 0'-).

char_substitution_for_file_name(0'(, 0'_).
char_substitution_for_file_name(0'), 0'_).

%---------------------------------------------------------------

compile_tex_rules :-
	tex_rules_file(File),
	compiled_tex_rules_file(CompiledFile),
	compile_orthography_file_or_files([File], CompiledFile),
	safe_compile(tex, CompiledFile).

compile_detex_rules :-
	detex_rules_file(File),
	compiled_detex_rules_file(CompiledFile),
	compile_orthography_file_or_files([File], CompiledFile),
	safe_compile(detex, CompiledFile).

:- compile_tex_rules.
:- compile_detex_rules.

%---------------------------------------------------------------

tex_string(InString, OutString) :-
	fix_orthography_simple_on_string(InString, tex, OutString).

detex_string(InString, OutString) :-
	fix_orthography_simple_on_string(InString, detex, OutString).
 