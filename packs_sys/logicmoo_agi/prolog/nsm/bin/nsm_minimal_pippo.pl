:- module(nsm_minimal,[
		       nsm2univ/2,
		       adjust_lf_version/4,
		       pp_univ/2,
		       build_dr/5,
		       univ/2
		      ]).

:- include('operators.pl').

univ(n,
[
				  det::e,
				  alt::e,
				  q::e,
				  pers::e,				 
				  a::[
				      eval::e,
				      size::e,
				      length::e,
				      height::e,
				      speed::e,
				      width::e,
				      weight::e,
				      temp::e,
				      age::e,
				      shape::e,
				      colour::e,
				      origin::e,
				      material::e
				     ],
				  dem::e,
				  poss::e,
				  class::e,
				  n::e 
				 ]
    ).

univ(v,	 [ c::[
	       compl::e,
	       top::e,
	       int::e,
	       top2::e,
	       foc::e,
	       pol::e
	      ],
	   mod::[
		 speech_act::e,
		 eval::e,
	         evid::e,
		 epist::e
		],
	   f::[ 
		top3::e,
	        finite::e
	      ],
           t::[
	       past::e,
	       fut::e
	      ],
	   m::[
	       irrealis::e,
	       necess::e,
	       possib::e,
	       vol::e,
	       oblig::e,
	       allow::e
	      ],
	   asp::[
		 hab::e,
		 rep::e,
		 freq::e,
		 celer::e,
		 ant::e,
		 term::e,
		 cont::e,
		 perf::e,
		 retro::e,
		 pross::e,
		 dur::e,
		 prog::e,
		 prosp::e,
		 compl_sg::e,
		 compl_pl::e
		],
           vo::[
		v_1::e,
		v_2::e,
		v_3::e
	       ],
	   ak::[
	       celer::e,
	       comp::e,
	       rep::e,
	       freq::e
	      ],
	   pred::[
		  v::e,
		  a::e,
		  o::e,
		  d::e,
		  e::e,
		  c::e,
		  i::e,
		  b::e,
		  l::e,
		  m::e 
		  ]]).

nsm2univ(ct(Cat,Sem),[Cat::X]) :-
	!,
	nsm2univ(Sem,X).

nsm2univ(sp(DetDim,Alt,Q,AttList,H),[
				  det::Det,
				  alt::Alt,
				  q::Q,
				  pers::P,				 
				  a::[
				      eval::Eval,
				      size::Size,
				      length::e,
				      height::e,
				      speed::e,
				      width::e,
				      weight::e,
				      temp::e,
				      age::e,
				      shape::e,
				      colour::e,
				      origin::e,
				      material::e
				     ],
				  dem::Dem,
				  poss::e,
				  class::e,
				  n::H1 
				 ]) :-
	np_det_dim(DetDim,Det,Dem),
%	np_class(H,Class),
	np_person(H,P,H1),
	nsm2univ_attlist(AttList,Eval,Size),
	!.

nsm2univ(s(Pol,Time,Can,Times,Dur,p(PRED,ARGS),Loc,Manner),
	 [ c::[
	       compl::e,
	       top::e,
	       int::e,
	       top2::e,
	       foc::e,
	       pol::Pol
	      ],
	   mod::[
		 speech_act::e,
		 eval::e,
	         evid::e,
		 epist::e
		],
	   f::[ 
		top3::e,
	        finite::e
	      ],
           t::[
	       past::TPAST,
	       fut::TFUT
	      ],
	   m::[
	       irrealis::e,
	       necess::e,
	       possib::Can,
	       vol::e,
	       oblig::e,
	       allow::e
	      ],
	   asp::[
		 hab::e,
		 rep::Times,
		 freq::e,
		 celer::e,
		 ant::ANT,
		 term::e,
		 cont::e,
		 perf::PERF,
		 retro::e,
		 pross::e,
		 dur::Dur,
		 prog::Prog,
		 prosp::e,
		 compl_sg::e,
		 compl_pl::e
		],
           vo::[
		v_1::e,
		v_2::e,
		v_3::e
	       ],
	   ak::[
	       celer::e,
	       comp::e,
	       rep::e,
	       freq::e
	      ],
	   pred::[
		  v::V,
		  a::A,
		  o::O,
		  d::D,
		  e::E,
		  c::C,
		  i::I,
		  b::B,
		  l::L,
		  m::M 
		  ]]) :-
	nsm2univ_loc(Loc,L),
	nsm2univ_man(Manner,M),
	nsm2univ_tense(Time,TPAST,TFUT,ANT),
	nsm2univ_pred(PRED,V,Prog,PERF),
	nsm2univ_arglist(V,ARGS,[a::A,o::O,d::D,e::E,c::C,i::I,b::B]),
	!.

nsm2univ(adv(ADV),adv(ADV)).
nsm2univ(e,e).


nsm2univ_loc(e,e) :- !.
nsm2univ_loc(loc(REL,LOC),[rel::REL,loc::X]) :-
	!,nsm2univ(LOC,X).

nsm2univ_man(e,e) :- !.
nsm2univ_man(man(MANNER),X) :-
	!,nsm2univ(MANNER,X).



nsm2univ_pred(i(V),V,prog,e) :- !.
nsm2univ_pred(p(i(V)),V,prog,ant) :- !.
nsm2univ_pred(p(V),V,e,ant) :- !.
nsm2univ_pred(V,V,e,e) :- !.

nsm2univ_tense(time(Extent,before,d(this)),
	      [extent::Extent,ref::this],e,e).
nsm2univ_tense(time(Extent,after,d(this)),
	      e,[extent::Extent,ref::this],e).
nsm2univ_tense(time(Extent,before,d(anaf)),
	      e,e,[extent::Extent,ref::anaf]).
nsm2univ_tense(time(Extent,after,d(anaf)),
	      e,[extent::Extent,ref::this],e).
nsm2univ_tense(time(e,now,e),e,e,e).
nsm2univ_tense(e,e,e,e).


nsm2univ_arglist(V,A,B) :-
	var(A),
	!,
	univ2arglist(V,A,B).
nsm2univ_arglist(_V,A,B) :-
	arglist2univ(A,B).

arglist2univ([],[a::A,o::O,d::D,e::E,c::C,i::I,b::B]) :-
	inst_empty_list([A,D,O,E,C,I,B]),!.
arglist2univ([R:A|List],ARGLIST) :-
	nsm2univ(A,A1),
	member(R::A1,ARGLIST),
	!,
	arglist2univ(List,ARGLIST).


univ2arglist(V,ArgList,UnivArgList) :-
	arg_frame(V,ArgList),
	instantiate_args(ArgList,UnivArgList).

instantiate_args([],_).
instantiate_args([Slot:Filler|ArgList],UnivArgList) :-
	member(Slot::Filler1,UnivArgList),
	nsm2univ(Filler,Filler1),
	instantiate_args(ArgList,UnivArgList).
	

arg_frame(do,[a:_,o:_,d:_,c:_,i:_,b:_]).
arg_frame(happen,[o:_,b:_]).
arg_frame(move,[a:_]).
arg_frame(think,[e:_]).
arg_frame(know,[e:_]).
arg_frame(feel,[e:_,o:_,t:_]).
arg_frame(hear,[e:_,o:_,t:_]).
arg_frame(say,[a:_,o:_,d:_,t:_]).


	
	

nsm2univ_attlist(A,B,C) :-
	var(A),
	!,
	univ2attlist(A,B,C).
nsm2univ_attlist(A,B,C) :-
	attlist2univ(A,B,C).


univ2attlist([],e,e) :- !.
univ2attlist([Eval|List],[int::INT,a::EVAL1],Size) :-
	add_attr(Eval,INT,EVAL1),
	!,
	univ2attlist(List,e,Size).
univ2attlist([Size|List],e,[int::INT,a::SIZE1]) :-
	add_attr(Size,INT,SIZE1),
	!,
	univ2attlist(List,e,e).


add_attr(very(Attr),very,Attr) :- !.
add_attr(Attr,e,Attr).
	

attlist2univ([],Eval,Size) :-
	inst_empty_list([Eval,Size]).
attlist2univ([good|A],[int::e,a::good],Size) :-
	attlist2univ(A,[int::e,a::good],Size).
attlist2univ([bad|A],[int::e,a::bad],Size) :-
	attlist2univ(A,[int::e,a::bad],Size).
attlist2univ([very(good)|A],[int::very,a::good],Size) :-
	attlist2univ(A,[int::very,a::good],Size).
attlist2univ([very(bad)|A],[int::very,a::bad],Size) :-
	attlist2univ(A,[int::very,a::bad],Size).
attlist2univ([big|A],Eval,[int::e,a::big]) :-
	attlist2univ(A,Eval,[int::e,a::big]).
attlist2univ([small|A],Eval,[int::e,a::small]) :-
	attlist2univ(A,Eval,[int::e,a::small]).
attlist2univ([very(big)|A],Eval,[int::very,a::big]) :-
	attlist2univ(A,Eval,[int::very,a::big]).
attlist2univ([very(small)|A],Eval,[int::very,a::small]) :-
	attlist2univ(A,Eval,[int::very,a::small]).
	


np_det_dim(e,e,e).
np_det_dim(ref(unkn),ref(unkn),e).
np_det_dim(this,ref(_),this).

np_class(H,H).

np_person(d(P),loc(P),P).
np_person(someone(X),e,someone(X)).
np_person(something(X),e,something(X)).
np_person(somewhere(X),e,somewhere(X)).
np_person(sometime(X),e,sometime(X)).


inst_empty_list([]).
inst_empty_list([e|List]) :-
	!,
	inst_empty_list(List).
inst_empty_list([_|List]) :-
	inst_empty_list(List).



adjust_lf_version(L1,L2,P1,NewP1) :-
	lf_version(L1,V1),
	lf_version(L2,V2),
	!,
	adjust_version(V1,V2,P1,NewP1).

adjust_version(_,e,P,P).
adjust_version(nsmpl,nsmpl,P,P).
adjust_version(univ,univ,P,P).
adjust_version(nsmpl,univ,P,NewP) :-
	nsm2univ(P,NewP).
adjust_version(univ,nsmpl,P,NewP) :-
	nsm2univ(NewP,P).

lf_version(L,univ) :-
	grammar:g_version(L,[85|_]),!.
lf_version(L,univ) :-
	grammar:g_version(L,[117|_]),!.
lf_version(_,nsmpl).

pp_univ(List,FullOrMin) :-
	pp_univ(List,"",FullOrMin).

pp_univ([],_,_).
pp_univ([Tag::Var|List],Indent,Full) :-
	var(Var),
	!,
	write_item(Indent,Tag,'UNINSTANTIATED'),
	pp_univ(List,Indent,Full).
pp_univ([_Tag::[A|B]|List],Indent,minimal) :-
	all_empty([A|B]),
	!,
	pp_univ(List,Indent,minimal).
pp_univ([Tag::[A|B]|List],Indent,Full) :-
	!,
	append(Indent,". ",NewIndent),
	write_item(Indent,Tag, ' _____ '),
	pp_univ([A|B],NewIndent,Full),
	pp_univ(List,Indent,Full).
pp_univ([_Tag::e|List],Indent,minimal) :-
	!,
	pp_univ(List,Indent,minimal).
pp_univ([Tag::Item|List],Indent,Full) :-
	write_item(Indent,Tag,Item),
	pp_univ(List,Indent,Full).


write_item(Indent,Tag,Item) :-
	write_indent(Indent),
	write(Tag:Item),
	nl.


write_indent([]) :- !.
write_indent([A|B]) :-
	name(X,[A|B]),
	write(X).
	

all_empty([]).
all_empty([_::e|List]) :-
	all_empty(List).


full_univ(p,
      [path,
       place,
       p
      ]).


build_dr(Lang,Num,Rule,DR,Cond) :-
	get_dependants(Rule,H,D,Order),
	get_formula(Lang,H,H_Formula),
	get_formula(Lang,D,DF),
	inst_dependants(Rule,H_Formula,HF),
	get_paradigms(Rule,DF,DF1,DF2,HF,RF,Cond),
	inst_dependants_aux(DF2,RF,RF1),
	build_dr_rule(Lang,Order,Num,DF1,HF,RF1,Cond,DR).

get_formula(_Lang,F,F) :-
	is_list(F),
	!.
get_formula(Lang,Cat,F) :-
	grammar:dg_class_macro(Lang,Cat,_,F),
	!.
get_formula(_Lang,Cat,F) :-
	univ(Cat,F).

build_dr_rule(Lang,h+d,Num,DF1,HF,RF,Cond,
	     dep(Lang,Num,HF+DF1==>RF,Cond)).
build_dr_rule(Lang,h-d,Num,DF1,HF,RF,Cond,
	     dep(Lang,Num,HF-DF1==>RF,Cond)).
build_dr_rule(Lang,d+h,Num,DF1,HF,RF,Cond,
	     dep(Lang,Num,DF1+HF==>RF,Cond)).
build_dr_rule(Lang,d-h,Num,DF1,HF,RF,Cond,
	     dep(Lang,Num,DF1-HF==>RF,Cond)).

get_dependants(Rule,H,D,Order) :-
	member(r::Dep,Rule),
	get_dependants_aux(Dep,H,D,Order).

get_dependants_aux(r::h(H)+D,H,D,h+d).
get_dependants_aux(r::h(H)-D,H,D,h-d).
get_dependants_aux(r::D+h(H),H,D,d+h).
get_dependants_aux(r::D-h(H),H,D,d-h).


get_paradigms(Rule,DF,DF1,DF2,HF,RF,Cond) :-
	member(p::Par,Rule),
	!,
	get_paradigms_aux(Par,DF,DF1,DF2,HF,RF,Cond).
get_paradigms(_Rule,DF,DF,_DF2?,HF,HF,[]).

get_paradigms_aux([],DF,DF,_DF2?,HF,HF,[]).
get_paradigms_aux([Par|List],DF,DF1,DF2,HF,RF,[Par1|Cond]) :-
	build_paradigm(Par,DF,DF1,DF2,HF,NewHF,Par1),
	get_paradigms_aux(List,DF,DF1,DF2,NewHF,RF,Cond).


build_paradigm(Par,DF,DF1,DF2,HF,NewHF,Par1) :-
	Par =.. [Name|Args],
	build_par_slots(Args,DF,DF1,DF2,HF,NewHF,NewArgs),
	Par1 =..[Name|NewArgs].

build_par_slots([],DF,DF,_DF2?,HF,HF,[]).
build_par_slots([h::Arg|Args],DF,DF1a,DF2,HF,HF2,[Arg1|NewArgs]) :-
	inst_dependant(Arg,HF,HF1,Arg1),
	build_par_slots(Args,DF,DF1a,DF2,HF1,HF2,NewArgs).
build_par_slots([d::Arg|Args],DF,DF1a,DF2,HF,HF2,[Arg1|NewArgs]) :-
	inst_dependant(Arg,DF,DF1,Arg1),
	build_par_slots(Args,DF1,DF1a,DF2,HF,HF2,NewArgs).


150 dar [
	 r::h(v)+infl,
	 inst::[],
	 p::[]
	].


inst_dependants(Rule,HF1,HF2) :-
	member(inst::Inst,Rule),
	!,
	inst_dependants_aux(Inst,HF1,HF2).
inst_dependants(_Rule,HF,HF).

inst_dependants_aux([],HF,HF).
inst_dependants_aux([Item|Inst],HF1,HF2) :-
	inst_dependant(Item,HF1,HF3,_),
	inst_dependants_aux(Inst,HF3,HF2).
	
inst_dependant(_,[],[],_).
inst_dependant(Tag::Path,[Tag::SubList|L1],[Tag::NewSubList|L2],ITEM) :-
	!,
	inst_dependant(Path,SubList,NewSubList,ITEM),
	inst_dependant(Tag::Path,L1,L2,_).
inst_dependant(Tag:::Val,[Tag::e|L1],[Tag::Val|L2],Val) :-
	!,
	inst_dependant(Tag:::Val,L1,L2,_).
inst_dependant(Tag,[Tag::e|L1],[Tag::Variable|L2],Variable) :-
	!,
	inst_dependant(Tag,L1,L2,_).
inst_dependant(Tag,[I|L1],[I|L2],Item) :-
	inst_dependant(Tag,L1,L2,Item).




