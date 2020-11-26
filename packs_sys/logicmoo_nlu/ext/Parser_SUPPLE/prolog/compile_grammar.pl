
/* ------------------------------------------------------------------------
 > FILENAME:	compile_grammar
 > PURPOSE:	
 > AUTHORS:	Kevin Humphreys, Mark Hepple
 > NOTES:	
 ------------------------------------------------------------------------ */

:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

cvsid_compile_grammar("$Id: compile_grammar.pl 7085 2005-12-05 16:32:03Z ian_roberts $").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% compile_grammar.pl
% 
% writes out a compiled form of grammar rules or lexical entries
% with features according to the table feature_table/1, and default
% values (currently for lexical entries only) according to the table
% default_table/1. 
%
% e.g.: word('president', n(person:3, number:sing)) will be rewritten as:
%       word(president, n((sem : _1), 
%                         (s_form : president), 
%			  (m_root : president), 
%			  (ne_tag : _2), 
%			  (person : 3), 
%			  (number : sing), 
%			  (tense : _3))).
%
% kwh 10/4/95
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- multifile best_parse_cats/1, filter_chart/0, rule/2, word/2.
:- dynamic best_parse_cats/1, filter_chart/0, rule/2, word/2,
	 best_parse_cats/2, compiled_rule/5, compiled_word/2.

best_parse_cats(dummy). % make this best_parse_cats/1's "home file"
rule(dummy,dummy). % make this rule/2's "home file"


:- op(10,xfy,^).
:- op(505,xfx,:).


%%% table of all features in the order expected by the parser
% for categories in the seeded input chart:
feature_table(list_np, [s_form:_,m_root:_,m_affix:_,text:_,ne_tag:_,ne_type:_,gender:_]) :- !.
%nouns
feature_table(n, [s_form:_,m_root:_,m_affix:_,text:_,person:_,number:_]) :- !.
feature_table(pn, [s_form:_,m_root:_,m_affix:_,text:_,person:_,number:_]) :- !.
%adjectives
feature_table(jj, [s_form:_,m_root:_,m_affix:_,text:_,degree:_]) :- !.
%adverbs
feature_table(rb, [s_form:_,m_root:_,m_affix:_,text:_,degree:_]) :- !.
%verbs
feature_table(v, [s_form:_,m_root:_,m_affix:_,text:_,person:_,number:_,tense:_,vform:_]) :- !.

% for  semantic categories
feature_table(sem_cat,[s_form:_,text:_,type:_,kind:_,name:_]):-!.

% for  semantic categories
feature_table(sem_cat_1,[s_form:_,text:_,type:_,unit:_,count:_,kind:_,name:_]):-!.

feature_table(ne_date,[s_form:_,text:_]):-!.

%other terminals
feature_table(Category,	[s_form:_,m_root:_,m_affix:_,text:_]) :-
	memberchk(Category, [sym,period,comma,pps,wps,sgml,date,cdg,cc,cd,dt,ex,fw,in,ls,md,pdt,pos,prp,rp,to,uh,wdt,wp,wrb,top,bottom,ordinal,char]), !.


% for non-terminal categories in the grammars - assume all features + source
feature_table(tagged_location_np, [edge:_,sem:_,head:_,s_form:_,m_root:_,m_affix:_,text:_,source:_,ne_tag:_,ne_type:_]) :- !.
feature_table(Category,	[edge:_,sem:_,head:_,s_form:_,m_root:_,m_affix:_,text:_,source:_,person:_,number:_,gender:_,tense:_,aspect:_,voice:_,vform:_]).


compile_grammars(Grammars) :-
	compile_grammars(Grammars,_).

write_compiled_grammar(OutFile) :-
	telling(Old),
	tell(OutFile), told, tell(OutFile),
	write_best_parse_cats,
	write_compiled_grammar,
	write_compiled_lexicon,
	told,
	tell(Old).

write_best_parse_cats :-
	best_parse_cats(Grammar,Cats),
	Term =.. [best_parse_cats,Grammar,Cats],
	writeq(Term), write('.'), nl,
	fail.
write_best_parse_cats :-
	filter_grammar(Grammar),
	Term =.. [filter_grammar,Grammar],
	writeq(Term), write('.'), nl,
	fail.
write_best_parse_cats :- nl.
	



compile_grammars(Grammars,GrammarID) :-
	retractall(compiled_rule(_,_,_,_,_)),
	retractall(compiled_word(_,_)),
	retractall(gensymmark(grammar,_)),
	assert(gensymmark(grammar,0)),
	buchart_gensym(grammar,GrammarID),
	retractall(gensymmark(rule,_)),
	assert(gensymmark(rule,0)),
	compile_grammars2(Grammars,GrammarID),!.


compile_grammars2([],_) :- !.
compile_grammars2([File],GrammarID) :-
	retractall(rule(_,_)),
	retractall(word(_,_)),
	consult(File),
	((filter_chart, compile_grammar(GrammarID,_))
	;compile_last_grammar(GrammarID)),
	retractall(rule(_,_)),
	retractall(word(_,_)).
compile_grammars2([File|Rest],GrammarID) :-
	retractall(rule(_,_)),
	retractall(word(_,_)),
	consult(File),
	compile_grammar(GrammarID,NextGrammarID),
	compile_grammars2(Rest,NextGrammarID).


% compile_grammar(InFile)
% reads a grammar/lexicon from InFile and produces a compiled form,
% based on the table given above
%

% record best parse cats and signal end of current grammar
compile_grammar(GrammarID,NextGrammarID) :-
	filter_chart,
	best_parse_cats(BestCats),
	assert(filter_grammar(GrammarID)),
	assert(best_parse_cats(GrammarID,BestCats)),
	compile_grammar3(GrammarID),
	% set up next grammar
	buchart_gensym(grammar,NextGrammarID),
	retractall(gensymmark(rule,_)),
	assert(gensymmark(rule,0)),
	retractall(best_parse_cats(_)),
	retractall(filter_chart).
% add rules to current grammar
compile_grammar(GrammarID,GrammarID) :-
	compile_grammar3(GrammarID),
	retractall(best_parse_cats(_)).
	
% add rules to current grammar and record final best parse cats
compile_last_grammar(GrammarID) :-
	best_parse_cats(BestCats),
	assert(best_parse_cats(GrammarID,BestCats)),
	compile_grammar3(GrammarID),
	retractall(best_parse_cats(_)).




% compile_grammar2(InFile)
% produces a compiled form of all current rule/2 and word/2 clauses
%
compile_grammar2(InFile) :-
	compile_grammar3(InFile),
	compile_lexicon(InFile).


compile_grammar3(GrammarID) :-
	rule(LHS,RHS),
	compile_features([LHS],[CLHS]),
	compile_features(RHS,CRHS),
	reverse(CRHS,[LastRHS|RevRHS]),
	% avoid duplicates
	\+(compiled_rule(LastRHS,RevRHS,CLHS,_,GrammarID)),
	buchart_gensym(rule,RuleNum),
	% use digits only, to allow use of standard sort
	atom_chars(RuleNum,[_,_,_,_|NumChars]),
	number_chars(RNum,NumChars),
	assert(compiled_rule(LastRHS,RevRHS,CLHS,RNum,GrammarID)),
	fail.
compile_grammar3(_) :- !.


compile_features([],[]) :- !.
% multiple categories
compile_features([FL|Fs], Out) :-
	is_list(FL), !,
	member(F,FL), % backtrack
	compile_features([F],CF),
	compile_features(Fs,CFs),
	((CF = [], Out = CFs) % might be optional
	;(CF = [CF1], Out = [CF1|CFs])).
% without single optional category
compile_features([{F}|Fs], CFs) :-
	compile_features(Fs,CFs).
% with single optional category
compile_features([{F}|Fs], Out) :- !,
	compile_features([F],CF),
	compile_features(Fs,CFs),
	((CF = [CF1], Out = [CF1|CFs])
	;append(CF,CFs,Out)).
% single non-optional category
compile_features([F|Fs], [CF|CFs]) :-
	F =.. [Cat|Features],
	feature_table(Cat,Table),
	unify_features(Features,Table),
	CF =.. [Cat|Table], !,
	compile_features(Fs,CFs).

unify_features([], Table) :- !.
unify_features([F|Fs], _) :-
	member(F,Fs),
	error('Duplicated feature in rule', [F]).
unify_features([F|Fs], Table) :-
	member(F, Table),
	unify_features(Fs, Table), !.
unify_features([F|_], _) :-
	error('Feature in rule not defined in feature table', [F]).


write_compiled_grammar :-
	compiled_rule(LRHS,RHS,LHS,RuleNum,GrammarID),
	Term =.. [compiled_rule,LRHS,RHS,LHS,RuleNum,GrammarID],
	writeq(Term), write('.'), nl,
	fail.
write_compiled_grammar.




% compile_lexicon uses the following table to add values for the
% specified features if no explicit value given in an entry
default_table(Word,
	      [s_form:Word,
	       m_root:Word]).


compile_lexicon(InFile) :-
	word(Word,Features),
	compile_features([Features],[CFs]),
	CFs =.. [C|Fs],	
	default_table(Word,Defaults),
	add_default_values(Defaults, Fs),
	assert(compiled_word(Word,CFs)),
	fail.
compile_lexicon(_) :- !.

add_default_values([], Table) :- !.
add_default_values([D|Ds], Table) :-
	member(D, Table),
	add_default_values(Ds, Table), !.
add_default_values([D|Ds], Table) :-
	add_default_values(Ds, Table).

write_compiled_lexicon :-
	compiled_word(Word,Cat),
	Term =.. [word,Word,Cat],
	writeq(Term), write('.'), nl,
	fail.
write_compiled_lexicon.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Revision 1.22  1999/02/25 16:30:15  kwh
% remove possibly incompatible categories in feature table
%
% Revision 1.21  1999/02/18 12:01:30  kwh
% allow optional categories
%
% Revision 1.20  1998/04/01 18:25:15  kwh
% added source feature - preferred by best parse if source=list for ne grammar
%
% Revision 1.19  1998/03/25 18:51:45  kwh
% affixes passed into chart
%
% Revision 1.18  1998/02/24 21:17:37  kwh
% new default ne grammar, using top/bottom
%
% Revision 1.17  1998/02/24 15:24:17  kwh
% new text feature on edges for header/body distinction
%
% Revision 1.16  1998/02/20 16:12:00  kwh
% sicstus runtime version
%
% Revision 1.15  1998/02/07 19:29:21  kwh
% use number_chars for rule ids so that ordering check works as intended
%
% Revision 1.14  1998/02/05 15:28:24  kwh
% translate all adverbs to RB
%
% Revision 1.13  1998/01/08 14:11:45  kwh
% use simple digits as rule id numbers, rather than ruleX format, to make comparison by sort correct
%
% Revision 1.12  1997/12/01 15:55:48  kwh
% rationalised top level control, merge subgrammars during compilation, pass child edges through for syntax output, and revised docs
%
% Revision 1.11  1997/11/12 13:29:10  huyck
% Added changes for top and bottom and kludged the comma problem.
%
% Revision 1.10  1997/09/30  17:24:39  kwh
% merge buchart_cascade changes back in
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

