
:- use_module(library(system)).
:- use_module(library(lists)).

% Note that prefix/2 has its arguments the opposite way around in SICStus 3 and SICStus 4!
sicstus_version(Version) :-
	prolog_flag(version, VersionAtom),
	atom_codes(VersionAtom, VersionChars),
	(   prefix("SICStus 3", VersionChars) ->
	    Version0 = 3
	;
	    prefix(VersionChars, "SICStus 4") ->
	    Version0 = 4
	;
	    otherwise ->
	    format('~N*** Error: unknown version of SICStus: "~w"~n', [VersionAtom]),
	    fail
	),
	!,
	%debug_format('~N--- SICStus version = ~d: ("~w")~n', [Version0, VersionAtom]),
	Version = Version0.

sicstus_sub_version(Version) :-
	prolog_flag(version, VersionAtom),
	atom_codes(VersionAtom, VersionChars),
	(   prefix("SICStus 3", VersionChars) ->
	    Version0 = [3]
	;
	    prefix(VersionChars, "SICStus 4.0.2") ->
	    Version0 = [4,0,2]
	;
	    prefix(VersionChars, "SICStus 4") ->
	    Version0 = [4,0,not_2]
	;
	    otherwise ->
	    format('~N*** Error: unknown version of SICStus: "~w"~n', [VersionAtom]),
	    fail
	),
	!,
	%debug_format('~N--- SICStus version = ~d: ("~w")~n', [Version0, VersionAtom]),
	Version = Version0.

%debug_format(X, Y) :- format(X, Y).
debug_format(_X, _Y).

% Sicstus 3 version
term_expansion('SICSTUS3/4'(Sicstus3, Sicstus4), Result) :-
	(   sicstus_version(3) ->
	    Result = Sicstus3
	;
	    otherwise ->
	    Result = Sicstus4
	),
	debug_format('~N--- Conditional load: "~w"~n', [Result]).

% Sicstus 3 version
term_expansion('SICSTUS3/4.0.2/4.other'(Sicstus3, Sicstus402, Sicstus40not2), Result) :-
	(   sicstus_sub_version([3|_]) ->
	    Result = Sicstus3
	;
	    sicstus_sub_version([4,0,2]) ->
	    Result = Sicstus402
	;
	    otherwise ->
	    Result = Sicstus40not2
	),
	debug_format('~N--- Conditional load: "~w"~n', [Result]).

term_expansion('SICSTUS3ONLY'(Sicstus3), Result) :-
	(   sicstus_version(3) ->
	    Result = Sicstus3
	;
	    otherwise ->
	    Result = ( :- true )
	),
	debug_format('~N--- Conditional load: "~w"~n', [Result]).

term_expansion('SICSTUS4ONLY'(Sicstus4), Result) :-
	(   sicstus_version(4) ->
	    Result = Sicstus4
	;
	    otherwise ->
	    Result = ( :- true )
	),
	debug_format('~N--- Conditional load: "~w"~n', [Result]).	

% Dynamic lexicon support now generally available - previously only for selected users.

term_expansion('LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE', Result) :-
	Result = ( :- use_module('$REGULUS/Prolog/dynamic_lexicon') ).

term_expansion('LOAD_DYNAMIC_LEXICON_RUNTIME_SUPPORT_IF_AVAILABLE', Result) :-
	Result = ( :- use_module('$REGULUS/Prolog/dynamic_lexicon_runtime') ).

% Sicstus 4 version
term_expansion('SICSTUS3/4'(Sicstus3, Sicstus4), Layout, Ids, Result, Layout, [compat | Ids]) :-
	(   sicstus_version(3) ->
	    Result = Sicstus3
	;
	    otherwise ->
	    Result = Sicstus4
	),
	debug_format('~N--- Conditional load: "~w"~n', [Result]).

% Sicstus 3 version
term_expansion('SICSTUS3/4.0.2/4.other'(Sicstus3, Sicstus402, Sicstus40not2), Layout, Ids, Result, Layout, [compat | Ids]) :-
	(   sicstus_sub_version([3|_]) ->
	    Result = Sicstus3
	;
	    sicstus_sub_version([4,0,2]) ->
	    Result = Sicstus402
	;
	    otherwise ->
	    Result = Sicstus40not2
	),
	debug_format('~N--- Conditional load: "~w"~n', [Result]).

term_expansion('SICSTUS3ONLY'(Sicstus3), Layout, Ids, Result, Layout, [compat | Ids]) :-
	(   sicstus_version(3) ->
	    Result = Sicstus3
	;
	    otherwise ->
	    Result = ( :- true )
	),
	debug_format('~N--- Conditional load: "~w"~n', [Result]).

term_expansion('SICSTUS4ONLY'(Sicstus4), Layout, Ids, Result, Layout, [compat | Ids]) :-
	(   sicstus_version(4) ->
	    Result = Sicstus4
	;
	    otherwise ->
	    Result = ( :- true )
	),
	debug_format('~N--- Conditional load: "~w"~n', [Result]).

% Dynamic lexicon support now generally available - previously only for selected users.

term_expansion('LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE', Layout, Ids, Result, Layout, [compat | Ids]) :-
	Result = ( :- use_module('$REGULUS/Prolog/dynamic_lexicon') ).

term_expansion('LOAD_DYNAMIC_LEXICON_RUNTIME_SUPPORT_IF_AVAILABLE', Layout, Ids, Result, Layout, [compat | Ids]) :-
	Result = ( :- use_module('$REGULUS/Prolog/dynamic_lexicon_runtime') ).

