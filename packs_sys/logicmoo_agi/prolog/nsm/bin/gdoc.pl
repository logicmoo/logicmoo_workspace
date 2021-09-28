:- module(gdoc,[
		print_grammar/0,
		print_formatted_grammar/3
	       ]).
:- include(operators).

:- use_module(rule_formatter).
:- use_module(gdoc_examples).
:- use_module(mark_up).
:- use_module(utils).
:- use_module(global_par).


/** <module> Grammar documentation generator

This module contains routines to generate human-readable version
of the a grammar loaded into NSM-DALIa
*/


%%	print_grammar is det
%
%	After obtaining values for the global parameters current_lang/1,
%	current_l2/1 and current_markup/1, this procedure directs the
%	output to the currently active output file (see 
%       set_output_file/1), and calls the actual grammar formatting
%       pricedure, print_formatted_grammar/3. After a formatted version
%       of the grammar has been written to the output file, this file
%       is closed.
%       
print_grammar :-
	get_current_lang(L1),
	get_current_l2(L2),
	current_markup(Markup),
	switch_output_file,
	print_formatted_grammar(L1,L2,Markup),
	close_output_file.


%%	print_formatted_grammar(+L1,+L2,+Format) is det
%
%	Writes a formatted version of the grammar
%	to the output file (see nsmdalia:set_output_file/1).
%	Lang is the usual language code, while Format can
%	be one of the atoms specified in the List argument of
%	global_par:supported_markup_list(List).
%	
%	Lang refers to the language whose gramamr we are describing,
%	while L2 is the languiage in which the grammar is written.
%	
print_formatted_grammar(Lang,L2,Format) :-
	write_auto_text(Format,grammar_heading(Lang,L2)),
	print_frontmatter(Lang,L2,Format),
	print_abstract(Lang,L2,Format),
	print_prologue(Lang,L2,Format,1,SectNum1),
	print_transcr_tables(Lang,L2,Format,SectNum1,SectNum2),
	print_decl(Lang,L2,Format,SectNum2,SectNum3),
	print_morphology(Lang,L2,Format,SectNum3,SectNum4),
	print_syntax(Lang,L2,Format,SectNum4,SectNum5),
%	print_lexicon(Lang,Format),
	print_epilogue(Lang,L2,Format,SectNum5,_),
	print_backmatter(Lang,Format),
	write_auto_text(Format,grammar_end).

print_frontmatter(Lang,L2,Format) :-
	write_auto_text(Format,pre_grammar_frontmatter),
	print_title(Lang,L2,Format),
	print_version(Lang,L2,Format),
	print_author(Lang,Format),
	print_date(Lang,Format),
	print_acknow(Lang,L2,Format),
	write_auto_text(Format,post_grammar_frontmatter),
	!.	
print_title(Lang,L2,Format) :-	
	grammar:g_title(Lang,L2,Title),
	!,
	write_auto_text(Format,pre_grammar_title),
	put_string_format(Format,Title),
	write_auto_text(Format,post_grammar_title).
print_title(_Lang,_L2,_Format).

print_version(Lang,L2,Format) :-
	grammar:g_version(Lang,V),
	!,
	write_auto_text(Format,pre_grammar_version),
	grammar:auto_text(L2,version,Version),
	append_list(S,[Version," ",V]),
	put_string_format(Format,S),
	write_auto_text(Format,post_grammar_version).
print_version(_,_,_).

print_author(Lang,Format) :-
	grammar:g_author(Lang,Author),	
	!,
	write_auto_text(Format,pre_grammar_author),
	put_string_format(Format,Author),
	write_auto_text(Format,post_grammar_author).
print_author(_,_).

print_date(Lang,Format) :-
	grammar:g_date(Lang,Date),
	!,
	write_auto_text(Format,pre_grammar_date),
	put_string_format(Format,Date),
	write_auto_text(Format,post_grammar_date).
print_date(_,_).



print_acknow(Lang,L2,Format) :-
	grammar:g_ackn(Lang,Akn),
	!,
	grammar:auto_text(L2,acknowledgements,Acknow),
	write_auto_text(Format,pre_grammar_ackn),
	put_string(Acknow),
	write_auto_text(Format,pre_grammar_ackn_body),
	put_string_format(Format,Akn),
	write_auto_text(Format,post_grammar_ackn).
print_acknow(_,_,_).

print_abstract(Lang,L2,Format) :-
	grammar:g_abstract(Lang,L2,Ab),
	!,
	write_auto_text(Format,pre_grammar_abstract_title),
	grammar:auto_text(L2,abstract,Abstract),
	put_string_format(Format,Abstract),
	put_string_format(Format,": "),
	write_auto_text(Format,post_grammar_abstract_title),
	put_string_format(Format,Ab),
	write_auto_text(Format,post_grammar_abstract).
print_abstract(_Lang,_L2,_Format).


print_prologue(Lang,L2,Format,SectNum,SectNum1) :-	
	grammar:auto_text(L2,prologue,SectHeader),
	get_doc_text(g_prologue(Lang,L2,Text)),
	write_auto_text(Format,pre_grammar_prologue),	
	print_section(L2,Format,SectNum,SectHeader,Text),
	write_auto_text(Format,post_grammar_prologue),
	SectNum1 is SectNum + 1,
	!.
print_prologue(_Lang,_L2,_Format,Num,Num).



print_epilogue(Lang,L2,Format,SectNum,SectNum1) :-	
	grammar:auto_text(L2,epilogue,SectHeader),
	get_doc_text(g_epilogue(Lang,L2,Text)),
	write_auto_text(Format,pre_grammar_epilogue),	
	print_section(L2,Format,SectNum,SectHeader,Text),
	write_auto_text(Format,post_grammar_epilogue),
	SectNum1 is SectNum + 1,
	!.
print_epilogue(_Lang,_L2,_Format,Num,Num).

print_decl(Lang,L2,Format,SectNum,NewSectNum) :-
	grammar:morph_threshold(Lang,_MT),
	findall(dep_threshold(Level,Max),
		grammar:dep_threshold(Lang,Level,Max),
		Levels),
	reverse(Levels,Levels1),
	grammar:auto_text(L2,levels,SectHeader),
	get_doc_text(g_levels(Lang,L2,Text)),
	print_section(L2,Format,SectNum,SectHeader,Text),
	write_auto_text(Format,pre_grammar_level_table),
	grammar:auto_text(L2,level,LevelHeader),
	put_string_format(Format,LevelHeader),
	write_auto_text(Format,infra_level_header),
	grammar:auto_text(L2,ends_at,Ends_At),
	put_string_format(Format,Ends_At),
	write_auto_text(Format,post_level_header),
	print_levels(Format,L2,Levels1),
	write_auto_text(Format,post_grammar_level_table),
	NewSectNum is SectNum + 1,
	!.

print_section(L2,Format,SectNum,Header,Text) :-
	name(SectNum,Num),
	write_auto_text(Format,sect_header(Num,Header)),	
	write_auto_text(Format,pre_grammar_sect_body),
	transcr:put_lang_string_format(L2,Format,Text),
%	put_string_format(Format,Text),	
	write_auto_text(Format,post_grammar_sect_body).


print_levels(_,_,[]) :- !.
print_levels(Format,L2,[dep_threshold(Level,Max)|Levels]) :-
	write_auto_text(Format,pre_level_item),
	write(Level),
	write_auto_text(Format,infra_level_item),
	write(Max),
	write_auto_text(Format,post_level_item),
	print_levels(Format,L2,Levels).
	

print_transcr_tables(Lang,L2,Format,SectNum,SectNum1) :-
	grammar:auto_text(L2,transcr,SectHeader),
	get_doc_text(g_transcr(Lang,L2,Text)),
	print_section(L2,Format,SectNum,SectHeader,Text),
	print_transcr_tables_aux(Lang,L2,Format,1),
	SectNum1 is SectNum + 1,
	!.


print_transcr_tables_aux(Lang,L2,Format,N) :-
	grammar:transcr_table(Lang,N,Table),
	!,
	grammar:auto_text(Lang,table,NAME),
	name(N,NN),
	write_auto_text(Format,pre_grammar_transcr_table(NN,NAME)),
	print_table(Format,Table),
	write_auto_text(Format,post_grammar_transcr_table),
	M is N + 1,
	print_transcr_tables_aux(Lang,L2,Format,M).
print_transcr_tables_aux(_Lang,_L2,_Format,_N).


print_table(Format,[A:B|Table]) :-
	write_auto_text(Format,pre_transcr_table_item),
	put_string_format(Format,A),
	put_string(":"),
	put_string_format(Format,B),
	write_auto_text(Format,post_transcr_table_item),
	print_table(Format,Table).
print_table(_,[]).

print_morphology(Lang,L2,Format,SectNum,SectNum1) :-
	grammar:auto_text(L2,morphology,SectHeader),
	get_doc_text(g_morph(Lang,L2,Text)),
	print_section(L2,Format,SectNum,SectHeader,Text),
	print_ph(Lang,L2,Format),
	print_mseq(Lang,L2,Format),
	grammar:morph_threshold(Lang,Max),
	print_gdep_intro(Lang,L2,Format),
	print_dependencies(Lang,L2,Format,SectNum,SectNum1,0,Max,1),
	!.


print_gdep_intro(Lang,L2,Format) :-
	grammar:auto_text(L2,gdep,DepHeader),
	write_auto_text(Format,nonumber_header(DepHeader)),
	get_doc_text(g_dep_intro(Lang,L2,Text)),
	write_auto_text(Format,pre_grammar_sect_body),
	transcr:put_lang_string_format(L2,Format,Text),
	write_auto_text(Format,post_grammar_sect_body).

print_syntax(Lang,L2,Format,SectNum,SectNum1) :-
	grammar:auto_text(L2,syntax,SectHeader),
	get_doc_text(g_synt(Lang,L2,Text)),
	print_section(L2,Format,SectNum,SectHeader,Text),
	grammar:morph_threshold(Lang,StartSynt),
	SS is StartSynt + 1,
	grammar:actual_max_dep(Lang,Max),
	print_dependencies(Lang,L2,Format,SectNum,SectNum1,SS,Max,1),
	!.	
print_syntax(_Lang,_L2,_Format,SectNum,SectNum).


print_ph(Lang,L2,Format) :-
	grammar:auto_text(L2,ph,PH),
	write_auto_text(Format,pre_grammar_ph_header),
	transcr:put_lang_string_format(L2,Format,PH),
%	put_string_format(Format,PH),	
	write_auto_text(Format,post_grammar_ph_header),
	findall(ph(NAME,B,C,D,E,F,G,H,I,J),
		grammar:ph(Lang,NAME,B,C,D,E,F,G,H,I,J),
		PHList),
	!,
	write_auto_text(Format,pre_grammar_ph),
	print_formatted_ph_rules(Lang,Format,PHList),
	write_auto_text(Format,post_grammar_ph).


print_formatted_ph_rules(_,_,[]).
print_formatted_ph_rules(Lang,Format,[Rule|PH]) :-
	print_format_ph_rule(Lang,Format,Rule),
	print_rule_examples(Lang,Format,Rule,ph),
	print_formatted_ph_rules(Lang,Format,PH).


print_mseq(Lang,L2,Format) :-
	grammar:auto_text(L2,mseq,MSEQ),
	write_auto_text(Format,pre_grammar_mseq_header),
	transcr:put_lang_string_format(L2,Format,MSEQ),
%	put_string_format(Format,MSEQ),	
	write_auto_text(Format,post_grammar_mseq_header),
	findall(MS,
		grammar:morph_seq(Lang,MS),
		MSList),
	write_auto_text(Format,pre_grammar_mseq),
	print_mseq_rules(Lang,Format,MSList,1),
	write_auto_text(Format,post_grammar_mseq).


print_mseq_rules(_,_,[],_).
print_mseq_rules(Lang,Format,[MS|List],I) :-
	print_format_mseq_rule(Format,MS,I),
	J is I + 1,
	print_mseq_rules(Lang,Format,List,J).


print_lexicon(_Lang,_Format).



print_backmatter(_Lang,_Format).






print_dependencies(_Lang,_L2,_Format,SectNum,SectNum1,Max1,Max,_Level) :-
	Max1 > Max,
	!,
	SectNum1 is SectNum + 1.

print_dependencies(Lang,L2,Format,SectNum,SectNum1,I,Max,Level) :-
	grammar:dep(Lang,I,_,_),
	!,
	print_format_dep_rule(Lang,I,Format),
	print_rule_doc(Lang,L2,Format,I),
	print_rule_examples(Lang,Format,I,dep),
	check_level(Lang,Level,I,NewLevel),
	J is I + 1,
	print_dependencies(Lang,L2,Format,SectNum,SectNum1,J,Max,NewLevel).
print_dependencies(Lang,L2,Format,SectNum,SectNum1,I,Max,Level) :-
	check_level(Lang,Level,I,NewLevel),
	J is I + 1,
	print_dependencies(Lang,L2,Format,SectNum,SectNum1,J,Max,NewLevel).

check_level(_Lang,0,_I,1). % FARE
check_level(Lang,Level,I,Level) :-
	grammar:dep_threshold(Lang,Level,Max),
	I < Max,
	!.
check_level(Lang,Level,I,NewLevel) :-
	grammar:dep_threshold(Lang,Level,Max),
	I = Max,
	NewLevel is Level + 1,
	print_new_level(Lang,NewLevel).

print_new_level(_,_).
	
print_rule_examples(Lang,Format,Rule,dep) :-
	global_par:current_l2(L2),
	get_examples(Lang,Rule,a:A_List),
	get_examples(Lang,Rule,t:T_List),
	write_examples(Lang,L2,Format,a:A_List),
	write_examples(Lang,L2,Format,t:T_List),
	!.
	
print_rule_examples(Lang,Format,Rule,ph) :-
	get_examples(Lang,ph(Rule),List),
	write_examples(Lang,_L2,Format,ph:List),
	!.

print_rule_examples(_,_,_,_).

print_rule_doc(Lang,L2,Format,RuleNumber) :-
	grammar:dep_doc(Lang,RuleNumber,Text),
	!,
	write_auto_text(Format,pre_rule_doc),
	transcr:put_lang_string_format(L2,Format,Text),
%	put_string_format(Format,Text),	
	write_auto_text(Format,post_rule_doc).
print_rule_doc(_,_,_,_).

get_doc_text(Doc) :-
	call(grammar:Doc),
	!.
get_doc_text(_).
