:- module(gdoc_examples,[
			 get_examples/3,
			 write_examples/4
			]).


/** <module> Analysis and translation of example sentences

This module enables the grammar formatter to write a table
of examples, with analysis and/or translation, to the printable
grammar.
*/


:- use_module(mark_up).
:- use_module(utils).

%%	get_examples(+Lang, +RuleNumber, -Examples) is det
%
%	Unifies Examples with a PROLOG list containing the example
%	sentences, both for analysis (first clause) and translation
%	(second clause) stored for the rule which has RuleNumber as 
%	identifier. Somewhere in the grammar there will be clauses
%	as:
%	
%	==
%	RuleNumber dr r_a ["example 1 to be analysed", 
%	                   "example 2 to be analysed"].
%	==
%	
%	and
%	
%	==
%	RuleNumber dr r_t ["example 1 to be translated", 
%	                   "example 2 to be translated"].
%	==
%	
get_examples(Lang,RuleNumber,a:List) :-
	grammar:dep_analyze(Lang,RuleNumber,List),
	!.
get_examples(Lang,RuleNumber,t:List) :-
	grammar:dep_transl(Lang,RuleNumber,List),
	!.
get_examples(_Lang,_Rule,_:[]).


%%	write_examples(+L1, +L2, +Format, +Examples) is det
%
%	* L1 is the language code of the language whose grammar
%	we are writing;
%	* L2 is the code of the metalanguage (the language in which the
%	gramamr is written);
%	* Format is a term identifying one of the available formats
%	(see global_par:supported_markup_list/1);
%	* List has the form *a:List* (these sentences will
%	be printed with their NSM-PROLOG analysis) or *t:List*
%	(sentences printed with translation into L2).
%	
write_examples(_,_,_,_:[]) :- !.
write_examples(Lang,L2,Format,a:List) :-
	grammar:auto_text(L2,examples,EXAMPLES),
	write_auto_text(Format,s_with_analysis_prologue(EXAMPLES)),
	ex_analysis(Lang,Format,List),
	write_auto_text(Format,s_with_analysis_epilogue).
write_examples(L1,L2,Format,t:List) :-
	grammar:auto_text(L2,examples,EXAMPLES),	
	write_auto_text(Format,s_with_translation_prologue(EXAMPLES)),
	ex_translation(L1,L2,Format,List).
write_examples(Lang,_,Format,ph:List) :-
	write_auto_text(Format,ph_example_prologue),
	ex_ph_analysis(Lang,Format,List),
	write_auto_text(Format,ph_example_epilogue).

%%	ex_ph_analysis(+Lang,+Format,+ExampleList) is det
%
%	@tbd generalizing for tagmemic grammars
%	
ex_ph_analysis(_,_,[]).
ex_ph_analysis(Lang,Format,[PF|List]) :-
	dg_morph:parse_word_standalone(Lang,PF,_LF,Lex),
	!,
	write_auto_text(Format,pre_ph_example),
	put_string(PF),
	write_auto_text(Format,infra_ph_example),
	write(Lex),
	write_auto_text(Format,post_ph_example),	
	ex_ph_analysis(Lang,Format,List).

%ANALYSIS FAILED:
ex_ph_analysis(Lang,Format,[PF|List]) :-
	write_auto_text(Format,pre_ph_example),
	put_string(PF),
	write_auto_text(Format,infra_ph_example),
	write('??'),
	write_auto_text(Format,post_ph_example),	
	ex_ph_analysis(Lang,Format,List).



ex_analysis(_,_,[]).
ex_analysis(Lang,Format,[Sent|List]) :-
	nsmdalia:parse_sentence(Lang,Sent,LF,_Rest),
	write_auto_text(Format,pre_sent_with_analysis),
	put_string(Sent),
	write_auto_text(Format,infra_sent_with_analysis),
%	rule_formatter:format_formula(Lang,LF,Fo),
%	rule_formatter:pp(Fo,Format),
	write(LF),
	write_auto_text(Format,post_sent_with_analysis),	
	ex_analysis(Lang,Format,List).

ex_translation(_,_,_,[]) :- !.
ex_translation(L1,L2,Format,PF1) :-
	ex_parse(L1,Format,PF1,LF),
	grammar:synt_grammar_type(L2,G),
	ex_gen(L2,G,Format,LF,PF2),
	!,
	ex_write_transl(Format,PF1,PF2).
ex_translation(_,_,Format,_) :-
	write_auto_text(Format,ex_fail).

ex_parse(_,_,[],[]).
ex_parse(Lang,Format,[PF1|PF],[LF1|LF]) :-
	nsmdalia:parse_sentence(Lang,PF1,LF1,_Rest),
	ex_parse(Lang,Format,PF,LF).

ex_gen(_,_,_,[],[]).
ex_gen(Lang,G,Format,[noparse|LF],["** ERROR **"|PF]) :-
	!,
	ex_gen(Lang,G,Format,LF,PF).
ex_gen(Lang,G,Format,[noparse(X)|LF],[X|PF]) :-
	!,
	ex_gen(Lang,G,Format,LF,PF).
ex_gen(Lang,G,Format,[LF1|LF],[PF1|PF]) :-
	nsmdalia:generic_gen_sentence(Lang,G,LF1,PF1),
	ex_gen(Lang,G,Format,LF,PF).

ex_write_transl(Format,A,B) :-	
	write_auto_text(Format,single_line_prologue),	
	ex_write_transl_aux(Format,A,B),
	write_auto_text(Format,single_line_epilogue).

ex_write_transl_aux(_Format,[],[]).
ex_write_transl_aux(Format,[PF1s|PF1],[PF2s|PF2]) :-
	write_auto_text(Format,single_line_before),
	put_string(PF1s),
	write_auto_text(Format,single_line_infra),
	put_string(PF2s),	
	write_auto_text(Format,single_line_after),	
	ex_write_transl_aux(Format,PF1,PF2).
