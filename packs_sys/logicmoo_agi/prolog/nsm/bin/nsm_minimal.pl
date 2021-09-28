:- module(nsm_minimal,[
		       nsm2univ/2,
		       univ2nsm/2,
		       adjust_lf_version/5,
		       lf_version/2,
		       pp_univ/2,
		       build_dr/4,
		       univ/3,
		       univ/1,
		       inst_dependants_aux/4,
		       close_slots/1
		      ]).

:- include('operators.pl').

:- dynamic(class_formula/3).

univ(nsm).

univ(n,nsm,[
	det::e,
	alt::e,
	q::e,
	pers::e,				 
	att::[
	      eval::e,
	      size::e
           ],
	dem::e,
        poss::e,
	class::e,
	n::e]).

	
univ(v,nsm,
	 [ c::[
	       compl::e
	      ],
	   mod::[
               pol::e
		],
           t::[
	       past::e, fut::e
	      ],
	   m::[
	       necess::e, possib::e, vol::e, oblig::e, allow::e
	      ],
	   asp::[
		 rep::e, ant::e, perf::e, dur::e, prog::e
		],
           vo::[
		v_1::e,	v_2::e,	v_3::e
	       ],
	   vp::[
		  v::e, arg1::e,arg2::e, arg3::e 
	       ],
	    args:: [a::e, o::e, d::e, e::e, c::e, i::e, b::e, l::e, m::e 
		  ]]).


univ(n, full,
[
				  det::e,
				  alt::e,
				  q::e,
				  pers::e,				 
				  att::[
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

univ(v,	full, [ c::[
	       compl::e,
	       top::e,
	       int::e,
	       top2::e,
	       foc::e
	      ],
	   mod::[
		 speech_act::e,
   	         pol::e,
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
	   vp::[
		  v::e,
		  arg1 :: e,
		  arg2 :: e,
		  arg3 :: e
	       ],
           args :: [ 		
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

univ(p,_,
      [path,
       place,
       p
      ]).

nsm2univ(NSM,UNIV) :-
	univ(FullOrNSM),
	!,
	nsm2univ(FullOrNSM,NSM,UNIV).

nsm2univ(Full,ct(Cat,Sem),[Cat::X]) :-
	!,
	nsm2univ(Full,Sem,X).

nsm2univ(Full,sp(DetDim,Alt,Q,AttList,H),List) :-
	univ(n,Full,List1),
	np_det_dim(DetDim,Det,Dem),
%	np_class(H,Class),
	np_person(H,P,H1),
	nsm2univ_attlist(AttList,Eval,Size),
	inst_dependants_aux([det:::Det,alt:::Alt,q:::Q,pers:::P,
			    att::eval:::Eval,att::size:::Size,
			    dem:::Dem,n:::H1],
			    List1,List,no),
	!.


nsm2univ(Full,if(S1,S2),List) :-
	nsm2univ(Full,S1,L1),
	nsm2univ(Full,S2,L2),
	inst_dependant(c::compl:::[if,L1],L2,List,_,no),
	!.

nsm2univ(Full,s(Pol,Time,Can,Times,Dur,p(PRED,ARGS),Loc,Manner),List) :-
	nsm2univ_loc(Full,Loc,L),
	nsm2univ_man(Full,Manner,M),
	nsm2univ_tense(Time,TPAST,TFUT,ANT),
	nsm2univ_pred(PRED,V,Prog,PERF),
	arglist2univ(Full,ARGS,[a::A,o::O,d::D,e::E,c::C,i::I,b::B]),
	univ(v,Full,List1),
	inst_dependants_aux(
	    [mod::pol:::Pol, t::past:::TPAST, t::fut:::TFUT,
	     m::possib:::Can, asp::rep:::Times, asp::ant:::ANT,
	     asp::perf:::PERF, asp::dur:::Dur, asp::prog:::Prog,
	     vp::v:::V, 
	     args::a:::A, args::o:::O, args::d:::D,
	     args::e:::E, args::c:::C, args::i:::I, args::b:::B,
	     args::l:::L, args::m:::M 
	    ],List1,List,no),
	!.

nsm2univ(_Full,X,X).


nsm2univ_loc(_,e,e) :- !.
nsm2univ_loc(Full,loc(REL,LOC),[rel::REL,loc::X]) :-
	!,nsm2univ(Full,LOC,X).

nsm2univ_man(_,e,e) :- !.
nsm2univ_man(Full,man(MANNER),X) :-
	!,nsm2univ(Full,MANNER,X).



nsm2univ_pred(i(V),V,prog,e) :- !.
nsm2univ_pred(p(i(V)),V,prog,ant) :- !.
nsm2univ_pred(p(V),V,e,ant) :- !.
nsm2univ_pred(V,V,e,e) :- !.

nsm2univ_tense(time(Extent,before,this),
	      [extent::Extent,ref::this],e,e).
nsm2univ_tense(time(Extent,after,this),
	      e,[extent::Extent,ref::this],e).
nsm2univ_tense(time(Extent,before,anaf),
	      e,e,[extent::Extent,ref::anaf]).
nsm2univ_tense(time(Extent,after,anaf),
	      e,[extent::Extent,ref::this],e).
nsm2univ_tense(time(e,now,e),e,e,e).
nsm2univ_tense(now,e,e,e).
nsm2univ_tense(before(now), [extent::e,ref::this],e,e).
nsm2univ_tense(after(now),e,[extent::e,ref::this],e).
nsm2univ_tense(before(before), [extent::e,ref::this],e,[extent:e,ref::anaf]).
nsm2univ_tense(after(before),e,[extent::e,ref::this],[extent:e,ref::anaf]).
nsm2univ_tense(e,e,e,e).



arglist2univ(_,[],[a::A,o::O,d::D,e::E,c::C,i::I,b::B]) :-
	inst_empty_list([A,D,O,E,C,I,B]),!.
arglist2univ(Full,[R:A|List],ARGLIST) :-
	nsm2univ(Full,A,A1),
	member(R::A1,ARGLIST),
	!,
	arglist2univ(Full,List,ARGLIST).

	

nsm2univ_attlist(A,B,C) :-
	var(A),
	!,
	univ2attlist(A,B,C).
nsm2univ_attlist(A,B,C) :-
	attlist2univ(A,B,C).


univ2attlist([],e,e) :- !.
univ2attlist([Eval|List],[int::INT,att::EVAL1],Size) :-
	add_attr(Eval,INT,EVAL1),
	!,
	univ2attlist(List,e,Size).
univ2attlist([Size|List],e,[int::INT,att::SIZE1]) :-
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
np_det_dim(this,ref(known),this).


np_person(d(P),loc(P),P).
np_person(X,e,X).


inst_empty_list([]).
inst_empty_list([e|List]) :-
	!,
	inst_empty_list(List).
inst_empty_list([_|List]) :-
	inst_empty_list(List).





univ2nsm(n::Univ,sp(D,Alt,NUM,AttList,H)) :-
	get_value(det,Univ,Det),
	get_value(dem,Univ,Dim),
	get_value(alt,Univ,Alt),
	get_value(q,Univ,NUM),
	get_value(n,Univ,H),
	get_value(att::eval,Univ,Eval),
	get_value(att::size,Univ,Size),
	attlist2univ(AttList,Eval,Size),
	np_det_dim(D,Det,Dim),
	!.
	
univ2nsm(v::Univ,s(Pol,Time,Can,Times,Dur,p(PRED,ARGS),Loc,Manner)) :-
	get_value(vp::v,Univ,V),
	get_value(args::a,Univ,A),
	get_value(args::o,Univ,O),
	get_value(args::d,Univ,D),
	get_value(args::e,Univ,E),
	get_value(args::c,Univ,C), 
	get_value(args::i,Univ,I),
	get_value(args::b,Univ,B),
	get_value(args::l,Univ,L),
	get_value(args::m,Univ,M), 
	get_value(mod::pol,Univ,Pol),
	get_value(t::past,Univ,TPAST), 
	get_value(t::fut,Univ,TFUT),
	get_value(m::possib,Univ,Can),
	get_value(asp::rep,Univ,Times),
	get_value(asp::ant,Univ,ANT),
	get_value(asp::perf,Univ,PERF),
	get_value(asp::dur,Univ,Dur), 
	get_value(asp::prog,Univ,Prog),
	univ2arglist(V,ARGS,[a::A,o::O,d::D,e::E,c::C,i::I,b::B]),
	univ2nsm_loc(Loc,L),
	univ2nsm_man(Manner,M),
	nsm2univ_tense(Time,TPAST,TFUT,ANT),
	nsm2univ_pred(PRED,V,Prog,PERF),
	!.
	

univ2nsm_loc(e,e) :- !.
univ2nsm_loc(loc(REL,LOC),[rel::REL,loc::X]) :-
	!,
	univ2nsm(LOC,X).

univ2nsm_man(e,e) :- !.
univ2nsm_man(man(MANNER),X) :-
	!,
	univ2nsm(MANNER,X).
	


univ2arglist(V,ArgList,UnivArgList) :-
	arg_frame(V,ArgList),
	instantiate_args(ArgList,UnivArgList).

instantiate_args([],_).
instantiate_args([Slot:Filler|ArgList],UnivArgList) :-
	member(Slot::Filler1,UnivArgList),
	univ2nsm(n::Filler1,Filler),
	instantiate_args(ArgList,UnivArgList).
	

arg_frame(do,[a:_,o:_,d:_,c:_,i:_,b:_]).
arg_frame(happen,[o:_,b:_]).
arg_frame(move,[a:_]).
arg_frame(think,[e:_]).
arg_frame(know,[e:_]).
arg_frame(feel,[e:_,o:_,t:_]).
arg_frame(hear,[e:_,o:_,t:_]).
arg_frame(say,[a:_,o:_,d:_,t:_]).






adjust_lf_version(L1,L2,Full,P1,NewP1) :-
	lf_version(L1,V1),
	lf_version(L2,V2),
	!,
	adjust_version(V1,V2,Full,P1,NewP1).

adjust_version(_,e,_,P,P).
adjust_version(nsmpl,nsmpl,_,P,P).
adjust_version(univ,univ,_,P,P).
adjust_version(nsmpl,univ,Full,P,NewP) :-
	nsm2univ(Full,P,NewP).
adjust_version(univ,nsmpl,Full,P,NewP) :-
	nsm2univ(Full,NewP,P).


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
	name(Tag,TagName),
	name(VarName,[63|TagName]),
	write_item(Indent,Tag,VarName),
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





build_dr(Lang,Num,Rule,DR) :-
	get_dependants(Rule,H,D,Order),
	get_formula(Lang,H,H_Formula),
	get_formula(Lang,D,DF),
	inst_d(Rule,DF,NewDF),
	inst_dependants(Rule,H_Formula,HF,no),
	get_paradigms(Rule,NewDF,DF1,HF,RF,Cond),
	bind_features(Rule,DF1,DF2,RF,RF1,Cond,Cond1),
	attach_dep(Rule,DF2,RF1,RF2,Cond1,Cond2),
	build_dr_rule(Lang,Order,Num,ct(D,DF2),ct(H,HF),ct(H,RF2),Cond2,DR).

attach_dep(Rule,DF2,RF1,RF2,Cond,Cond1) :-
	member(ac::AttachTo,Rule),
	!,
	attach_dependant(AttachTo,RF1,RF2,DF2),
	append(Cond,(prolog_pred (nsm_minimal:close_slots(DF2))),Cond1).
attach_dep(Rule,DF2,RF1,RF2,Cond,Cond) :-
	member(a::AttachTo,Rule),
	!,
	attach_dependant(AttachTo,RF1,RF2,DF2).
attach_dep(_Rule,_DF2,RF,RF,Cond,Cond).
	
inst_d(Rule,DF,DF1) :-
	try_all(Rule,DF,DF1),
	!.
inst_d(Rule,DF,DF1) :-
	try_list(Rule,DF,DF1),
	!.
inst_d(_Rule,DF,DF).

try_all(Rule,DF,DF2) :-
	member(d::all,Rule),
	!,
	open_slots(DF,DF2).

try_list(Rule,DF,DF2) :-
	member(d::INSTD,Rule),
	!,
	inst_dependants_aux(INSTD,DF,DF2,no).




open_slots([],[]).
open_slots([Tag::List|DF],[Tag::NewList|DF1]) :-
	is_list(List),
	!,
	open_slots(List,NewList),
	open_slots(DF,DF1).
open_slots([Tag::Var|DF],[Tag::Var|DF1]) :-
	var(Var),
	!,
	open_slots(DF,DF1).
open_slots([Tag::e|DF],[Tag::_|DF1]) :-
	!,
	open_slots(DF,DF1).
open_slots([Tag::Val|DF],[Tag::Val|DF1]) :-
	open_slots(DF,DF1).


bind_features(Rule,DF,DF1,RF,RF1,Cond,Cond1) :-
	member(bind::Bindings,Rule),
	!,
	bind_features_aux(Bindings,DF,DF1,RF,RF1,Cond,Cond1).
bind_features(_,DF,DF,RF,RF,Cond,Cond).


bind_features_aux([],DF,DF,RF,RF,Cond,Cond).
bind_features_aux([Dep=>Head|Features],DF,DF1,RF,RF1,Cond,C1) :-
	inst_dependant(Dep,DF,DF2,Value,yes),
	inst_dependant(Head,RF,RF2,Value,yes),
	bind_features_aux(Features,DF2,DF1,RF2,RF1,[leftrec(Value)|Cond],C1).


get_formula(_Lang,F,F) :-
	is_list(F),
	!.
get_formula(Lang,Cat,F) :-
	class_formula(Lang,Cat,F),
	!.
get_formula(_Lang,Cat,F) :-
	univ(Cat,nsm,F),
	!.
get_formula(_Lang,Cat,[Cat::e]).

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
	get_dependants_aux(Dep,H,D,Order),
	!.


get_dependants_aux(h(H)+D,H,D,h+d).
get_dependants_aux(h(H)-D,H,D,h-d).
get_dependants_aux(D+h(H),H,D,d+h).
get_dependants_aux(D-h(H),H,D,d-h).


get_paradigms(Rule,DF,DF1,HF,RF,Cond) :-
	member(p::Par,Rule),
	!,
	get_paradigms_aux(Par,DF,DF1,HF,RF,Cond).
get_paradigms(_Rule,DF,DF,HF,HF,[]).

get_paradigms_aux([],DF,DF,HF,HF,[]).
get_paradigms_aux([Par|List],DF,DF1,HF,RF,[Par1|Cond]) :-
	build_paradigm(Par,DF,DF1,HF,NewHF,Par1),
	get_paradigms_aux(List,DF,DF1,NewHF,RF,Cond).


build_paradigm(Par,DF,DF1,HF,NewHF,Par1) :-
	Par =.. [Name|Args],
	build_par_slots(Args,DF,DF1,HF,NewHF,NewArgs),
	Par1 =..[Name|NewArgs].

build_par_slots([],DF,DF,HF,HF,[]).
build_par_slots([h::Arg|Args],DF,DF2,HF,HF2,[Arg1|NewArgs]) :-
	inst_dependant(Arg,HF,HF1,Arg1,yes),
	build_par_slots(Args,DF,DF2,HF1,HF2,NewArgs).
build_par_slots([d::Arg|Args],DF,DF2,HF,HF2,[Arg1|NewArgs]) :-
	inst_dependant(Arg,DF,DF1,Arg1,yes),
	build_par_slots(Args,DF1,DF2,HF,HF2,NewArgs).



inst_dependants(Rule,HF1,HF2,BIND_VARS) :-
	try_inst_list(Rule,HF1,HF2,BIND_VARS),
	!.
inst_dependants(Rule,HF1,HF2,BIND_VARS) :-
	try_lower(Rule,HF1,HF2,BIND_VARS),
	!.
inst_dependants(_Rule,HF,HF,_BIND_VARS).

try_inst_list(Rule,HF1,HF2,BIND_VARS) :-
	member(h::Inst,Rule),
	!,
	inst_dependants_aux(Inst,HF1,HF2,BIND_VARS).

try_lower(Rule,HF1,HF2,_BIND_VARS) :-
	member(l::Lower,Rule),
	!,
	inst_lower(Lower,HF1,HF2,no).


inst_dependants_aux([],HF,HF,_).
inst_dependants_aux([Item|Inst],HF1,HF2,BIND_VARS) :-
	inst_dependant(Item,HF1,HF3,_,BIND_VARS),
	inst_dependants_aux(Inst,HF3,HF2,BIND_VARS).



 
% se manca b nella lista a, a::b istanzia comunque dopo a
 
inst_lower(_,[],[],_) :- !.
inst_lower(Tag::Path,[Tag::SubList|L1],[Tag::NewSubList|L2],YES) :-
	is_list(SubList),
	!,
	inst_lower(Path,SubList,NewSubList,YES),
	inst_lower(Tag::Path,L1,L2,yes).
inst_lower(Tag,[Tag1::SubList|L1],[Tag1::NewSubList|L2],yes) :-
	is_list(SubList),
	!,
	inst_lower(Tag,SubList,NewSubList,yes),
	inst_lower(Tag,L1,L2,yes).
inst_lower(Tag,[Tag::Var|L1],[Tag::Var|L2],_) :-
	!,
	inst_lower(Tag,L1,L2,yes).
inst_lower(Tag,[Tag1::e|L1],[Tag1::_|L2],yes) :-
	!,
	inst_lower(Tag,L1,L2,yes).
inst_lower(Tag,[I|L1],[I|L2],YES) :-
	inst_lower(Tag,L1,L2,YES).



attach_dependant(_,[],[],_) :- !.
attach_dependant(Tag::Path,[Tag::SubList|L1],[Tag::NewSubList|L1],DF2) :-
	!,
	attach_dependant(Path,SubList,NewSubList,DF2).
attach_dependant(Tag:::Val,[Tag::Val|L1],[Tag::DF2|L1],DF2) :- !.
attach_dependant(Tag,[Tag::e|L1],[Tag::DF2|L1],DF2) :- !.
attach_dependant(Tag,[I|L1],[I|L2],Item) :-
	attach_dependant(Tag,L1,L2,Item).

	
inst_dependant(_,[],[],_,_) :- !.
inst_dependant(Tag::Path,[Tag::SubList|L1],[Tag::NewSubList|L2],ITEM,INST_VARS) :-
	!,
	inst_dependant(Path,SubList,NewSubList,ITEM,INST_VARS),
	inst_dependant(Tag::Path,L1,L2,_,INST_VARS).
inst_dependant(Tag:::Val,[Tag::Var|L1],[Tag::Var|L2],Variable,no) :-
	var(Var),
	!,
	inst_dependant(Tag:::Val,L1,L2,Variable,no).
inst_dependant(Tag,[Tag::Var|L1],[Tag::Var|L2],Variable,no) :-
	var(Var),
	!,
	inst_dependant(Tag,L1,L2,Variable,no).
inst_dependant(Tag:::Val,[Tag::Var|L1],[Tag::Var|L2],Val,yes) :-
	var(Var),
	!,
	Var = Val,
	inst_dependant(Tag:::Val,L1,L2,_,yes).
inst_dependant(Tag,[Tag::Var|L1],[Tag::Var|L2],Val,yes) :-
	var(Var),
	!,
	Val = Var,
	inst_dependant(Tag,L1,L2,_,yes).
inst_dependant(Tag:::Val,[Tag::e|L1],[Tag::Val|L2],Val,INST_VARS) :-
	!,
	inst_dependant(Tag:::Val,L1,L2,_,INST_VARS).
inst_dependant(Tag,[Tag::e|L1],[Tag::Variable|L2],Variable,INST_VARS) :-
	!,
	inst_dependant(Tag,L1,L2,_,INST_VARS).
inst_dependant(Tag,[I|L1],[I|L2],Item,INST_VARS) :-
	inst_dependant(Tag,L1,L2,Item,INST_VARS).




close_slots([]) :- !.
close_slots([_Item::List|Rest]) :-
	is_list(List),
	!,
	close_slots(List),
	close_slots(Rest).
close_slots([_Item::e|Rest]) :-
	!,
	close_slots(Rest).
close_slots([_::_|Rest]) :-
	close_slots(Rest).


get_value(Tag::Path,List,Value) :-
	member(Tag::SubList,List),
	is_list(SubList),
	!,
	get_value(Path,SubList,Value).
get_value(Tag,List,Value) :-
	member(Tag::Value,List),
	!.




/*
150 dar [
	 r::h(v)+infl,
	 h::[],          % restriction on head feature values
	 d::[],          % restriction on dep  feature values. Can be 'all'.
	 l::lower,       % from that level down, all can be <> e (in h)
	 a::slot,        % h-slot to which dep is to be attached
	 ac::slot,       % like a, but with empty-slot filling by 'e'.
	 bind::[df=>hf]  % lega slots di h e dep	 
	].
*/
