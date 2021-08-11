:- module(create_stubs,
	  [ create_stubs/0
	  ]).

:- use_module(library(check_installation)).
:- use_module(library(dcg/basics)).

create_stubs :-
	forall(check_installation:component(Name, Dict),
	       ( check_installation:issue_url(Dict.put(source, Name), URL),
		 check_installation:issue_base(Base),
		 atom_concat(Base, Local, URL),
		 file_name_extension(FileBase, html, Local),
		 file_name_extension(FileBase, txt, Stub),
		 create_stub(Name, Stub))).

create_stub(_, Stub) :-
	exists_file(Stub), !.
create_stub(Topic, Stub) :-
	phrase_from_file(replace(['TOPIC'=Topic], Codes), 'stub.stub'),
	setup_call_cleanup(
	    open(Stub, write, Out),
	    format(Out, '~s', [Codes]),
	    close(Out)).

replace(Replace, Codes) -->
	"@", string_without("@", VarCodes), "@",
	{ atom_codes(Var, VarCodes),
	  memberchk(Var=Value, Replace), !,
	  format(codes(Codes,Tail), '~w', [Value])
	},
	replace(Replace, Tail).
replace(Replace, [H|T]) -->
	[H],
	replace(Replace, T).
replace(_, []) -->
	[].
