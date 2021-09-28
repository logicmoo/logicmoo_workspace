:- module(dg_morph,[
		    parse_word_standalone/4,
		    dg_morph:parse_word/7
		   ]).

:- use_module(word_scanner).
:- use_module(morphophon).
:- use_module(messages).
:- use_module(checkers).

parse_word_standalone(Lang,Word,Analysis,Lex) :-
	append(Word,"#",Word1),
	dg_scan_rest(Lang,start,Word1,parsed("#",[],[]),parsed(_,[45|Lex],_),[],Stack),
	!,
	dg:parse_dg(Lang,Word1,[],Stack,Analysis).

%%	parse_word(+Lang,+Sent,+PrevWord,-Word2,+Stack,-Rest,
%%	-NewStack) is nondet
%
%	This procedure is invoked by the syntactic parsers.
%	Parses the next word in the input string, and updates
%	the parsing stack with the analysis of the new word 
%	(the syntactic parsing procedures pass Stack 
%	and receive the updated stack in NewStack).
%	
%	Rest is unified with the original sentence stripped
%	of the word alalyzed.
dg_morph:parse_word(Lang,Sent,PrevWord,Word2,Stack,Rest,NewStack) :-
	get_word(Sent,Word,Rest),
	check_external_sandhi(Lang,PrevWord,Word,Rest,Word2),
	!,
	dg_morph_parse_word_aux(Lang,Word2,Stack,NewStack).

%%	dg_morph_parse_word_aux(+Lang,+Word,+OldStack,+NewStack) is det
%
%	This is the actual word-parsing procedure. Three cases are
%	distinguished:
%	
%	1. If the word starts with an open paren, it is considered
%	the name of a variable (or another proper name), and it is 
%	analyzed as ct(lit_name,Literal).
%	2. If the word has a lexical entry, there is no need to
%	activate the morphological parsing, and the lexical
%	entry data are put into the stack as the analysis.
%	3. In the elsewhere case, dg_scan_rest/7 is invoked,
%	which performs the morphological analysis of the word.
dg_morph_parse_word_aux(_Lang,[40|Word],Stack,[ct(lit_name,Literal)|Stack]) :-
	append(Literal,[41],Word),
	!.
dg_morph_parse_word_aux(Lang,Word,Stack,[ct(Cat,LF)|Stack]) :-
	grammar:m(Lang,Cat,Word,Exp),
	(   (\+global_par:makedict(no)) 
	; nsm_dict:assert_found_morph(Cat,Word,Exp)
	),
	convert_lexical(Lang,Cat,Exp,LF).
dg_morph_parse_word_aux(Lang,Word2,Stack,NewStack) :-
	append(Word2,"#",Word1),
	dg_scan_rest(Lang,start,Word1,parsed("#",[],[]),_Parsed1,Stack,NewStack).

dg_scan_rest(Lang,Class,[],Parsed,Parsed,Stack,Stack) :-
	grammar:arc(Lang,Class,stop),!.
dg_scan_rest(Lang,Class,"#",Parsed,Parsed,Stack,Stack) :-
	grammar:arc(Lang,Class,stop),!.
dg_scan_rest(Lang,OldClass,RestWord,Parsed,NewParsed,Stack,NewStack) :-
	find_morph(Lang,NewClass,NewExponent,RestWord,NewRestWord,Parsed,Parsed1),
	grammar:arc(Lang,OldClass,NewClass),
	convert_lexical(Lang,NewClass,NewExponent,LF),
	append([ct(NewClass,LF)],Stack,Stack1),
	notify_shift_morph(Stack1),
	dg:try_reduce(Lang,1,Stack1,Stack2,no),
	dg_scan_rest(Lang,NewClass,NewRestWord,Parsed1,NewParsed,Stack2,NewStack).

convert_lexical(Lang,Class,Exp,LF) :-
	grammar:dg_class_macro(Lang,Class,Exp,LF).%,!.
convert_lexical(_Lang,_Class,LF,LF).

check_external_sandhi(Lang,Prev,Surf,Next,Lex) :-
	grammar:allo(Lang,Surf,Lex,_Residue,PredSurf,SuccSurf,[],[],Conditions),
	check_pred(PredSurf,Prev),
	check_succ(SuccSurf,Next),
	check_cond_list_aux(Lang,Conditions,PredSurf,[]),
	!.
check_external_sandhi(_,_,W,_,W).

