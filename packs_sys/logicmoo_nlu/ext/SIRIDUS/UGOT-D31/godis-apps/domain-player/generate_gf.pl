:- module(generate, [generate/0,init/0,quit/0]).

:- use_module( library(tis_access), [check_conditions/1,
				     check_condition/1,
				     apply_update/1] ).

:-use_module(library(charsio),[write_term_to_chars/4]).
:-use_module(library(terms),[term_variables/2]).
:-use_module(library(oaag),[solve/1]).

init.
quit.

generate:-
	check_condition($next_moves = oqueue(MoveList) or
		        $next_moves = set(MoveList) or
		       $next_moves = queue(MoveList) ),
	generate(MoveList,String),
	apply_update(output:=String).


generate([],""):-!.

generate(MoveList,OutputString):-
	check_conditions([$domain=Domain,$language=Lang]),
	gf_properties_file(Domain,PropFile),
	gf_grammar_file(sys,Domain,Lang,SysNLGrammar),
	gf_grammar_file(sys,Domain,sem,SysSemGrammar),
	moves2atom(MoveList,MoveListAtom),
	solve( translate(PropFile,
			 SysSemGrammar,MoveListAtom,
			 SysNLGrammar,OutputAtom)),
	atom_chars(OutputAtom,OutputString).

%computes name of GF agent's properties from value of $domain
gf_properties_file(Domain,File):-
	join(['sys',Domain],'_',File).

%computes name of GF grammar from value of $domain, $language and participant
gf_grammar_file(Participant,Domain,Language,File):-
	join([Participant,Domain,Language],'_',File).

%joins atoms in a list into one atom, similar to perl's join
join([],_,'').
join([X],_,X).
join([X,Y|Xs],Sep,Joined):-
	atom(X),
	atom(Sep),
	atom_concat(X,Sep,Z),
	join([Y|Xs],Sep,Q),
	atom_concat(Z,Q,Joined).


moves2atom(Moves,Atom):-
	bind_vars_to_X(Moves),
	write_term_to_chars(Moves,Chars,[],[portrayed(true)]),
	
	map_space_to_underscore(Chars,AtomChars),
	atom_chars(Atom,AtomChars).

bind_vars_to_X(Term):-
	term_variables(Term,TermVars),
	bind_var_list_to_X(TermVars).

bind_var_list_to_X([]).
bind_var_list_to_X(['X'|List]):-
	bind_var_list_to_X(List).

map_space_to_underscore("","").
map_space_to_underscore([0' |Xs],[0'_|Ys]):-!,
	map_space_to_underscore(Xs,Ys).
map_space_to_underscore([X|Xs],[X|Ys]):-
	map_space_to_underscore(Xs,Ys).
