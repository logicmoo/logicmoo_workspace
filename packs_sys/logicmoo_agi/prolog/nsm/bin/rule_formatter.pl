:- module(rule_formatter,[
			  print_format_ph_rule/3,
			  print_format_dep_rule/3,
			  format_arglist/4,
			  print_expanded_cond_list/3,
			  formatted_formula/5,
			  pp/3,
			  print_format_mseq_rule/3
			 ]).


:- use_module(utils).
:- use_module(mark_up).

:- include('operators.pl'). % importante, per leggere bene dep

:- dynamic(formatted_formula/5).

/** <module> Grammar rules pretty-printer

This module contains routines that produce a pretty_printed
version of dependency and allomorph-construction rules.
*/

%%	print_format_ph_rule(+Lang,+Format,+PHRule) is det
%
%	
print_format_ph_rule(_Lang,Format,
		  ph(Name,Morpheme,Morph,Residue,PredSurf,SuccSurf,
		     PredLex,SuccLex,Conditions,_PreConditions)) :-
	write_auto_text(Format,pre_ph_rule),
	put_string_format(Format,Name),
	write_auto_text(Format,pre_ph_body),
	format_ph_cond_list(Conditions,PredSurf,SuccSurf,PredLex,SuccLex,1,Var7),
	format_ph_string(Morpheme,M,[Morph,PredSurf,SuccSurf,PredLex,SuccLex],Var7,Var1),
	put_string_format(Format,M),
	write_auto_text(Format,longrightarrow),
	format_ph_string(Morph,Mo,[Morpheme,PredSurf,SuccSurf,PredLex,SuccLex],Var1,Var2),	
	put_string_format(Format,Mo),
	print_residue(Format,Residue),
	format_ph_string(PredSurf,PS,[Morpheme,Morph,SuccSurf,PredLex,SuccLex],Var2,Var3),
	format_ph_string(SuccSurf,SS,[Morpheme,Morph,PredSurf,PredLex,SuccLex],Var3,Var4),
	format_ph_string(PredLex,PL,[Morpheme,Morph,PredSurf,SuccSurf,SuccLex],Var4,Var5),
	format_ph_string(SuccLex,SL,[Morpheme,Morph,PredSurf,SuccSurf,PredLex],Var5,_),
	print_context(Format,PS,SS),
	print_lex_context(Format,PL,SL),
%	write_auto_text(Format,pre_ph_cond_list),
%	pprint_list(0,Format,Conditions),
%	write_auto_text(Format,pre_ph_precond_list),
%	pprint_list(0,Format,PreConditions),
%	write_auto_text(Format,post_ph_cond_list),
%	print_expanded_cond_list(Lang,Format,ph(Name,Conditions,PreConditions)),
	write_auto_text(Format,post_ph_rule),
	!.
print_format_ph_rule(_Lang,_Format,_PH).


format_ph_string(Var,[63|Var],_List,Num,NewNum) :-
	var(Var),
	!,
	name(Num,Var),
	NewNum is Num + 1.
format_ph_string(S1+S2,Formatted,List,Num,NewNum) :-
	!,
	format_ph_string(S1,S1f,List,Num,Num1),
	format_ph_string(S2,S2f,List,Num1,NewNum),
	append_list(Formatted,[S1f,"+",S2f]).
format_ph_string(S,Formatted,_,Num,Num) :-
	is_list(S),
	flatten(S,Formatted).
format_ph_string(Item,Item,_,Num,Num).

format_ph_cond_list([],_PS,_SS,_PL,_SL,Num,Num).
format_ph_cond_list([Cond|CondList],PS,SS,PL,SL,Num,NewNum) :-
	format_ph_cond(Cond,PS,SS,PL,SL,Num,NewNum1),	
	format_ph_cond_list(CondList,PS,SS,PL,SL,NewNum1,NewNum).


format_ph_cond(Var << Class,_PredSurf,_SuccSurf,_PredLex,_SuccLex,Num,Num) :-
	% importante passarli, così Var viene istanziata anche lì
	var(Var),
	!,
	name(Class,X),
	append([63],X,Var).
format_ph_cond(Functor,PredSurf,SuccSurf,PredLex,SuccLex,Num,NewNum) :-
	Functor =.. [_|Args],
	!,
	inst_variables(Args,[_,PredSurf,SuccSurf,PredLex,SuccLex],Num,NewNum).
format_ph_cond(_Cond,_PredSurf,_SuccSurf,_PredLex,_SuccLex,Num,Num).

inst_variables([],_,Num,Num).
inst_variables([Var|List],InstList,Num,NewNum) :-
	format_ph_string(Var,Var,InstList,Num,NewNum1),
	inst_variables(List,InstList,NewNum1,NewNum).




print_residue(_,[]) :- !.
print_residue(Format,Res) :-
	put_string_format(Format,"-"),
	put_string_format(Format,Res),
	put_string_format(Format," ").

print_lex_context(_Format,[],[]) :- !.
print_lex_context(Format,Left,Right) :-
	put_string_format(Format,"*: "),
	print_context(Format,Left,Right).

print_context(_Format,[],[]) :- !.
print_context(Format,Left,Right) :-
	write_auto_text(Format,pre_ph_context),
	put_string_format(Format,Left),
	put_string_format(Format,[95,95]),
	put_string_format(Format,Right),
% 	put_format(Format,93),
	write_auto_text(Format,post_ph_context).


print_format_mseq_rule(Format,MS,I) :-
	name(I,II),
	write_auto_text(Format,pre_grammar_mseq_rule(II)),
	print_mseq_rule(MS),
	write_auto_text(Format,post_grammar_mseq_rule).

print_mseq_rule([]).

print_mseq_rule([Item]) :-
	var(Item),
	!,
	inst_mseq_var(Item),
	write(Item).
print_mseq_rule([Item|Rule]) :-
	var(Item),
	!,
	inst_mseq_var(Item),
	write(Item),
	write(' + '),
	print_mseq_rule(Rule).

print_mseq_rule([Item]) :-
	Item =..[_|Args],
	!,
	inst_mseq_var_list(Args),
	write(Item).
print_mseq_rule([Item|Rule]) :-
	Item =..[_|Args],
	!,
	inst_mseq_var_list(Args),
	write(Item),
	write(' + '),
	print_mseq_rule(Rule).
	

check_directional(Lang,DR,"A + B = C   //   B + A = C") :-
	grammar:non_directional(Lang,DR),
	!.
check_directional(_Lang,_DR,"A + B = C").



print_format_dep_rule(Lang,DR,Format) :-
	grammar:dep(Lang,DR,Dep,Cond1),
	check_directional(Lang,DR,Direct),
	format_dep(Lang,Dep,CtA,CtB,CtC,A,B,C),
%	format_cond_list(Cond,Cond1),
	name(DR,RuleNum),
	write_auto_text(Format,pre_dep_rule(RuleNum,Direct)),
	write_auto_text(Format,pre_cat_a),
	write_cat(CtA),
	write_auto_text(Format,pre_dep_a),
	pp(A,Format,"_A"),
	write_auto_text(Format,post_dep_a),
	write_auto_text(Format,pre_cat_b),
	write_cat(CtB),
	write_auto_text(Format,pre_dep_b),
	pp(B,Format,"_B"),
	write_auto_text(Format,post_dep_b),	
	write_auto_text(Format,pre_cat_c),
	write_cat(CtC),
	write_auto_text(Format,pre_dep_c),
	pp(C,Format,"_C"),
	write_auto_text(Format,post_dep_c),
	write_auto_text(Format,post_dep_rule),
	print_conditions(Cond1,DR,Format),
	!.
print_format_dep_rule(Lang,DR,Format) :-
	grammar:dep(Lang,DR,ct(CtA,AA)+ct(CtB,BB)==>ct(CtC,CC),Cond1),
	trace,
	check_directional(Lang,DR,Direct),
	name(DR,RuleNum),
	write_auto_text(Format,pre_dep_rule(RuleNum,Direct)),
	write_auto_text(Format,pre_cat_a),
	write_cat(CtA),
	write_auto_text(Format,pre_dep_a),
	write(AA),
	write_auto_text(Format,post_dep_a),
	write_auto_text(Format,pre_cat_b),
	write_cat(CtB),
	write_auto_text(Format,pre_dep_b),
	write(BB),
	write_auto_text(Format,post_dep_b),	
	write_auto_text(Format,pre_cat_c),
	write_cat(CtC),
	write_auto_text(Format,pre_dep_c),
	write(CC),
	write_auto_text(Format,post_dep_c),
	write_auto_text(Format,post_dep_rule),
	print_conditions(Cond1,DR,Format),
	!.
print_format_dep_rule(_Lang,_Dr,Format) :-
	write_auto_text(Format,format_dep_rule_fail).


print_conditions([],_,_) :- !.
print_conditions(Cond1,DR,Format) :-
	grammar:auto_text(Lang,conditions,CONDITIONS),
	write_auto_text(Format,pre_dep_cond_list(CONDITIONS)),
	pp(Cond1,Format,""),
	write_auto_text(Format,post_dep_cond_list),	
	print_expanded_cond_list(Lang,Format,dep(DR,Cond1)).


print_expanded_cond_list(Lang,Format,ph(NAME,Cond,_PreCond)) :-
	grammar:ph_print_cond_list(Lang,NAME,CondList),
	!,
	grammar:auto_text(paradigms,Paradigms),
	write_auto_text(Format,pre_exp_cond_list(Paradigms)),
	print_exp_cond_list_aux(Lang,Format,Cond,CondList),
	write_auto_text(Format,post_exp_cond_list).
print_expanded_cond_list(Lang,Format,dep(DEP,CondList)) :-
	grammar:dep_print_cond_list(Lang,DEP,PrintingList),
	!,
	grammar:auto_text(paradigms,Paradigms),	
	write_auto_text(Format,pre_exp_cond_list(Paradigms)),	
	print_exp_cond_list_aux(Lang,Format,CondList,PrintingList),
	write_auto_text(Format,post_exp_cond_list).	
print_expanded_cond_list(_,_,_).


print_exp_cond_list_aux(_Lang,_Format,[],_CondList).
print_exp_cond_list_aux(Lang,Format,[Cond|List],PrintingList) :-
	Cond =.. [Functor|_Args],
	member(Functor,PrintingList),
	!,
	expand_condition(Lang,Cond,Cond1),
	write_paradigm_members(Format,Cond1),
	print_exp_cond_list_aux(Lang,Format,List,PrintingList).
print_exp_cond_list_aux(Lang,Format,[_Cond|List],PrintingList) :-
	print_exp_cond_list_aux(Lang,Format,List,PrintingList).


write_paradigm_members(_,[]).
write_paradigm_members(Format,[Item|List]) :-
	write_auto_text(Format,pre_exp_par_item),
	write(Item),
	write_auto_text(Format,pre_exp_par_item),
	write_paradigm_members(Format,List).

% format_cond_list(C,C).

expand_condition(Lang,C,C2) :-
	findall(C,
		grammar:paradigm(Lang,C),
		C2),
	inst_paradigm_var_list(C2).

inst_paradigm_var_list([]).
inst_paradigm_var_list([P|L]) :-
	P =.. [_|ArgList],
	inst_var_list_num(ArgList,L,1,_),
	inst_paradigm_var_list(L).

inst_var_list_num([],_,N,N).
inst_var_list_num([A|List],L,N,N1) :-
	var(A),
	!,
	name(N,NN),
	A = [63|NN],
	N2 is N + 1,
	inst_var_list_num(List,L,N2,N1).
inst_var_list_num([_|List],L,N,N1) :-
	inst_var_list_num(List,L,N,N1).


format_dep(Lang,ct(CtA,A) + ct(CtB,B) ==> ct(CtC,C),CtA,CtB,CtC,A1,B1,C1) :-
	format_formula(Lang,A,A1,"_A"),
	format_formula(Lang,B,B1,"_B"),
	format_formula(Lang,C,C1,"_C").

format_formula(_,F,F,_) :-
	var(F),!.
format_formula(_,F,F,_) :-
	is_list(F),!.
format_formula(Lang,F,Fo,ABC) :-
	formatted_formula(Lang,ABC,F,Fo,Actions),
	do_actions(rule_formatter,Actions),
	!.
format_formula(_,F,Fo,_ABC) :-
	F =.. Fo1,
	Fo1 = [Head|Fo2],
	Fo = [Head:Fo2].

get_role_fullname(X,'?role') :-
	var(X),
	!.
get_role_fullname(R,Rfo) :-
	role_fullname(R,Rfo),!.
get_role_fullname(X,X).


role_fullname(a,agent).
role_fullname(o,object).
role_fullname(d,dative).
role_fullname(b,benefactive).
role_fullname(i,instrument).
role_fullname(c,comitative).
role_fullname(top,argument).
role_fullname(l,locative).
role_fullname(j,theme).


formatted_formula(_AnyLang,
		  ABC,
		  sp(A,B,C,D,E),
		  [ substantive:
		    [ det:A, 
		      same_or_other:B,
		      number:C,
		      attributes:D,
		      head_noun:E]],
		  [inst_var_list(ABC,[det:A,same_or_other:B,number:C,attributes:D,head_noun:E])
		  ]).

formatted_formula(Lang,
		  ABC,
		  p(Pred,ArgList),
		    [ head : Pred,
		      arguments : ArgList_fo],
		    [inst_var(pred:Pred,ABC),
		     format_arglist(Lang,ArgList,ArgList_fo,ABC),
		     inst_var_list(ABC,ArgList_fo)]).

formatted_formula(Lang,
		  ABC,
		  s(A,B,C,D,E,P,F,G),
		  [ sentence:
		    [ neg : A,
		      time : B,
		      can_or_will : C,
		      times : D,
		      how_long : E,
		      predicate : Pfo,
		      loc : Ffo,
		      manner : Gfo]],
		 [format_formula(Lang,P,Pfo,ABC),
		  format_formula(Lang,F,Ffo,ABC),
		  format_formula(Lang,G,Gfo,ABC),
		  inst_var_list(ABC,[neg:A,time:B,can:C,
				 times:D,how_long:E,predicate:Pfo,
				 loc:Ffo,manner:Gfo])
		 ]).

formatted_formula(_Lang,
		  ABC,
		  because(S,N),
		  [ because_sentence: 
		         [cause:N1,
			  consequence:S1
			 ]
		  ],
		  [format_formula(Lang,S,S1,ABC),
		   format_formula(Lang,N,N1,ABC),
		   inst_var_list(ABC,[sent1:S1,sent2:N1])]).
pp(List,Format,ABC) :-
	write_auto_text(Format,pre_formula),
	pprint_list(0,Format,List,ABC),
	write_auto_text(Format,post_formula).

pprint_list(_,_,[],_).
pprint_list(Indent,Format,[Tag:Item|List],ABC) :-
	is_list(Item),
	!,
	write_auto_text(Format,pre_formula_item(Indent)),
	write(Tag),
	write_auto_text(Format,post_formula_item),
	NewIndent is Indent + 2,
	pprint_list(NewIndent,Format,Item,ABC),
	pprint_list(Indent,Format,List,ABC).
pprint_list(Indent,Format,[Tag:Item|List],ABC) :-
	!,
	write_auto_text(Format,pre_formula_item(Indent)),
	write(Tag),
	write_auto_text(Format,tab_formula_item(Tag)),
	inst_var(Tag:Item,ABC),
	write(' :: '),
	write(Item),
	write_auto_text(Format,post_formula_item),
	pprint_list(Indent,Format,List,ABC).
pprint_list(Indent,Format,[Item|List],ABC) :-
	is_list(Item),
	!,
	NewIndent is Indent + 2,
	pprint_list(NewIndent,Format,Item,ABC),
	pprint_list(Indent,Format,List,ABC).
pprint_list(Indent,Format,[Item|List],ABC) :-
	!,
	write_auto_text(Format,pre_formula_item(Indent)),
	write(Item),
	write_auto_text(Format,post_formula_item),
	pprint_list(Indent,Format,List,ABC).
pprint_list(Indent,Format,Atom,_ABC) :-
	write_auto_text(Format,pre_formula_item(Indent)),
	write(Atom),
	write_auto_text(Format,post_formula_item).

/* NON TOGLIERLI ! CHIAMATI DA UN CALL */
inst_var_list(_ABC,[]).
inst_var_list(ABC,[Var|List]) :-
	inst_var(ABC,Var),
	inst_var_list(ABC,List).

format_arglist(_,[],[],_).
format_arglist(Lang,[Role:Arg|List],[Role_fo:Arg_fo|List_fo],ABC) :-
	get_role_fullname(Role,Role_fo),
	inst_var(Role:Arg,ABC),
	format_formula(Lang,Arg,Arg_fo,ABC),
	format_arglist(Lang,List,List_fo,ABC).


inst_var(Tag:Var,ABC) :- 
	atom(Tag),
	var(Var),
	!,
	name(Tag,Name),
	append(Name,ABC,Name1),
	name(Var,[63|Name1]).

inst_var('?',_) :- !.
inst_var(_,_).


inst_mseq_var('*') :- !.
inst_mseq_var(_).

inst_mseq_var_list([]).
inst_mseq_var_list([A|B]) :-
	inst_mseq_var(A),
	inst_mseq_var_list(B).


write_cat(Cat) :-
	Cat =.. [_Main,Sub],
	!,
	inst_var(subcat:Sub,""),
	write(Cat).
write_cat(Cat) :-
	var(Cat),
	!,
	write('?cat').
write_cat(Cat) :-
	write(Cat).

