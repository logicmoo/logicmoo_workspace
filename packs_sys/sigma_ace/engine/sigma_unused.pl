% ===================================================================
% File 'sigma_unused.pl' 
% Authors: Douglas Miles; Jay Halcomb
% Contact: dmiles@teknowledge.com ; jhalcomb@teknowledge.com ; apease@teknowledge.com
% Version: 'sigma_unused.pl' 1.0.0 
% Purpose: A holder for unused predicates
% Created - 2000/12/10 dmiles@teknowledge.com
% ===================================================================

end_of_file.

% =====================================================%
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% =====================================================%

% ==========================================================
% HL Types
% ===================================================================

% sort_constraints(Contstrained,Sorted)
% ===================================================================

sort_constraints((Cont:-Strained),(Cont:-LessStrained)):-sort_antesceeds(Strained,LessStrained),!.
sort_constraints(Sorted,Sorted):-!.

sort_antesceeds(true,true).
sort_antesceeds(Term,NewTems):-
		term_to_list(Term,TermList),
		sort_ant1(TermList,TermList1),
		sort_ant2(TermList1,TermList2),!,
		reorderAnteceedants(TermList2,NewTems),!.

sort_ant1(Var,Var):-isSlot(Var),!.
sort_ant1([],[]):-!.
sort_ant1([not(A),B],[B,not(A)]):-!.
sort_ant1([not(A),B,C],[B,C,not(A)]):-!.
sort_ant1([not(A),B,C,D],[B,C,D,not(A)]):-!.
sort_ant1(A,A):-!.
sort_ant2(Var,Var):-isSlot(Var),!.
sort_ant2([],[]):-!.
sort_ant2(['equal'(X,Y),B],[B,'equal'(X,Y)]):-!.
sort_ant2(['equal'(X,Y),B,C],[B,C,'equal'(X,Y)]):-!.
sort_ant2(['equal'(X,Y),B,C,D],[B,C,D,'equal'(X,Y)]):-!.
sort_ant2(A,A):-!.

% ===================================================================
% convert WFS-Prolog                  (can_to_rule((A,B),(AA,BB)))
% ===================================================================

can_to_rule(PROLOG,true,true,KB,Ctx,TN,Cost):-isSlot(PROLOG),!.

can_to_rule(PROLOG,true,true,KB,Ctx,TN):-member(PROLOG,[true,end_of_file,surf]),!.
                                                                           
can_to_rule((WFS:-true),Arg1,Arg2,KB,Ctx,TN):-!,can_to_rule(WFS,Arg1,Arg2,KB,Ctx,TN).


%can_to_rule(resolve_skolem(A,B),skolem,(A=BB),KB,Ctx,TN):- !,krlog_to_prolog(B,BB).
can_to_rule(WFS,WFS,predicate,KB,Ctx,TN):-
		functor(WFS,F,A),predicates_PTTP_leaves_alone(F,A),!.
can_to_rule((WFS:-BODY),WFS,[inline_rule(Cost)|BODY],KB,Ctx,TN):-
		functor(WFS,F,A),predicates_PTTP_leaves_alone(F,A),!.
can_to_rule((WFS:-BODY),WFS,BODY,KB,Ctx,TN):-!.	
can_to_rule(WFS,WFS,fact,KB,Ctx,TN):-!.

can_to_wfs_proc(X,end_of_file):-memberchk(X,[end_of_file,surf,true,false]),!.
can_to_wfs_proc((X:-true),XX):-!,can_to_wfs_proc(X,XX),!.
can_to_wfs_proc(entails(Antecedant,Consequent),O):- !,
	can_to_wfs_proc((Consequent:-Antecedant),O).

can_to_wfs_proc((A,B),(AA,BB)):-!,
      can_to_wfs_proc(A,AA),!,
      can_to_wfs_proc(B,BB),!.

can_to_wfs_proc((A;B),(AA;BB)):-!,
      can_to_wfs_proc(A,AA),!,
      can_to_wfs_proc(B,BB),!.


can_to_wfs_proc((Consequent:-Antecedant),(Proto:-AProto)):- !,
         logOnFailure(pterm_to_bt_cons(Consequent,Proto)),
         logOnFailure(build_ante(Antecedant,AProto)).

can_to_wfs_proc((Consequent),((Proto))):- !,
         logOnFailure(pterm_to_bt_cons(Consequent,Proto)).

build_ante(true,true):-!.
build_ante((Ante,Cedant),(APro,ToO)):-!,
		build_ante(Ante,APro),
		build_ante(Cedant,ToO).
build_ante(';'(Ante,Cedant),','(APro,ToO)):-!,
		build_ante(Ante,APro),
		build_ante(Cedant,ToO).
build_ante(Antecedant,AProtoO):-
		logOnFailure(pterm_to_bt_ante(Antecedant,AProtoO)).

      
% ===================================================================
% pterm_to_bt_ante(Term,2ndOrderTerm,Cost)
% ===================================================================

pterm_to_bt_ante(A,A):-atomic(A),!.

pterm_to_bt_ante(','(X,Y),(XX,YY)):-
           pterm_to_bt_ante(X,XX),
           pterm_to_bt_ante(Y,YY).

pterm_to_bt_ante(';'(X,Y),(XX;YY)):-
           pterm_to_bt_ante(X,XX),
           pterm_to_bt_ante(Y,YY).

pterm_to_bt_ante(not(T),TT):-predicates_PTTP_leaves_alone(T),!,tilde_t(T,TT).

pterm_to_bt_ante(T,T):-predicates_PTTP_leaves_alone(T),!.


pterm_to_bt_ante(not(not(Consequent)),Proto):-!,pterm_to_bt_cons(Consequent,Proto).

pterm_to_bt_ante(not(Term),not(BTA)):-!,logOnFailure(pterm_to_bt_ante(Term,BTA)).

%pterm_to_bt_ante(T,T,0):-predicates_declared_formula_wrappers(T),!.

/*
pterm_to_bt_ante(In,Out,C):-
           nonvar(In),In=..[P|Args],predicates_declared_formula_wrappers(P),!,
           conv_each_ante_l(Args,ArgsOut,C),Out=..[P|ArgsOut],!.
*/


pterm_to_bt_ante(Term,BTP):-
			logOnFailure(Term=..[B|T]),!,
			logOnFailure(encode_hash([B|T],[Hash,F|A])),
			logOnFailure(get_rtype(F,Rtype)),
			logOnFailure(BTP=..[Rtype,Hash,F|A]).

encode_hash([holds,B|T],[Proto|[B|T]]):-isSlot(B),!,length(T,Arity),
	entity_make_hash(Proto,Arity),request_wrap(Proto),sendNote('(hilog)').
encode_hash([holds,B|T],[HA|[B|T]]):-!,length(T,Arity),get_formula_hash(B,Arity,T,HA).
encode_hash([B|T],[HA|[B|T]]):-!,length(T,Arity),get_formula_hash(B,Arity,T,HA).


tilde_t(T,TT):-T=..[F|A],atom_codes(F,FC),atom_codes(NF,[126|FC]),TT=..[NF|A],!.



wrap_the_ants(true,true,KB,Context):-!.
wrap_the_ants(Before,Wrapped,KB,Context):-build_ante(Before,AfterL,_),!,conv(AfterL,After),!,wrap_in_byrd(After,Wrapped,KB,Context).

%wrap_in_byrd(After,Wrapped,KB,Context):-!,byrd_query_compile(After,Wrapped,KB,Context,Vars).


gen_occurs(A,B) :- A == B. 
gen_occurs(A,B) :- nonvar(B), functor(B,F,N), gen_occurs(A,B,N). 
gen_occurs(A,B,N) :- N > 0, arg(N,B,AN), gen_occurs(A,AN),!. % RED 
gen_occurs(A,B,M) :- M > 0, N is M - 1, gen_occurs(A,B,N).

pterm_to_bt_cons(not(not(Consequent)),Proto):-!,pterm_to_bt_cons(Consequent,Proto).
pterm_to_bt_cons(Consequent,Proto):-logOnFailure(pterm_to_bt_ante(Consequent,Proto)).


% ===================================================================
% krlog_to_proquery(KRProlog,Prolog)  Converts And/Or/Implies terms to Proper Prolog
% ====================================================================

krlog_to_proquery(A,A):-isSlot(A),!.

krlog_to_proquery(ThisKR,That):-
	logOnFailure(krlog_to_prolog(ThisKR,This)),
	logOnFailure(can_to_wfs_proc(This,That)).


compile_w_add(_):-fail.
%compile_w_add('equal').

convert_rule_to_wfsc(WFS,Fact,WFSC,Fact):-member(Fact,[fact,inline,predicate]),!,
	head_transpose(WFS,WFSC),!.	

convert_rule_to_wfsc(WFS,C,WFSC,CC):-
	head_transpose(WFS,WFSC),
	body_transpose(C,CC),!.
	
convert_rule_to_wfsc(WFS,C,WFS,C):-!.

head_transpose(A,A):-var(A),!.

head_transpose((A,B),(TA,TB)):-!,
	head_transpose((A),(TA)),
	head_transpose((B),(TB)).
head_transpose((A;B),(TA;TB)):-!,
	head_transpose((A),(TA)),
	head_transpose((B),(TB)).
head_transpose(A,B):-is_list(A),!,body_transpose(A,B),!.

head_transpose(u(A,B,C),(u(A,B,C))):-!.
head_transpose('~u'(A,B,C),('~u'(A,B,C))):-!.
head_transpose('t_instance'(A,B),('t_instance'(A,B))):-!.

/*

head_transpose(not(Before),not(After)):-nonvar(Before)
	head_transpose1(Before,Mid,Predicate),!,
	((ground(Predicate),surface_knows(singleValued(Predicate,ArgNum),_,_,_,_)) ->
			   (After = sv(Mid,ArgNum),!) ; (After = Mid,!) ).
*/

head_transpose(Before,After):-
	head_transpose1(Before,Mid,Predicate),!,
	add_sv_if_needed(Mid,Predicate,After),!.

	
add_sv_if_needed(Mid,Predicate,sv(Mid,ArgNum)):-
	nonvar(Predicate),
	surface_knows(singleValued(Predicate,ArgNum),_,_,_,_),!.
add_sv_if_needed(Mid,Predicate,Mid):-!.
	
% Programatic Types
head_transpose1(not(FACT),(not(FACT)),Predicate):-
	nonvar(FACT),
	FACT=..[RType,Type,Predicate|ArgS].

head_transpose1(FACT,(FACT),Predicate):-
	nonvar(FACT),
	FACT=..[RType,Type,Predicate|ArgS].


% ==========================================================
		
body_transpose([],[]):-!.
body_transpose([A|NTE],[AN|TES]):-
		head_transpose(A,AN),!,
		body_transpose(NTE,TES).
	


C:\jdk1.3\jre\lib;
C:\jdk1.3\lib;
C:\jakarta-tomcat\webapps\sigma\WEB-INF\lib\xerces.jar;
C:\jakarta-tomcat\webapps\sigma\WEB-INF\lib\jakarta-regexp-1.2.jar;
C:\jakarta-tomcat\webapps\sigma\WEB-INF\lib\cpj.jar;C:\jakarta-tomcat\lib\jasper.jar;C:\jakarta-tomcat\lib\webserver.jar;C:\jakarta-tomcat\lib\servlet.jar;C:\jakarta-tomcat\webapps\sigma\WEB-INF\classes;C:\jdk1.3\src.jar

%This is a Sicstus version of P-Progol 2.7.4
%Original YAP version written by Ashwin Srinivasan
%This Sicstus 3 version adapted from YAP version by James Cussens

%Sicstus 3 modules
   /*
:- use_module(library(random)). %for random/1.
:- use_module(library(lists)). %for reverse, member, etc
:- use_module(library(terms)). %term_hash/2. only loaded if doing posonly
:- use_module(library(system)). %for system/1
     */
% Features/ requirements of **original YAP** version
% 	Assumes Prolog compiler can do depth-checking
% 	Pos-only learning that includes pruning -- no inflate parameter
% 	Lazy evaluation of bottom clause during reduction
%		use: set(construct_bottom,reduction)
% 	Automatic generation of a refinement operator from modes
%		use: set(refine,auto)
%	Probabilistic refinement operators
%		selection of refinements that previously led to good answers
%		use: set(refine,probabilistic)
%		this is still being tested

% Features/ requirements of **this Sicstus** version
%       After pre-processing (which is basically indexing)
%          positive and negative examples are saved to a file,
%          with ".exs" backchain and then loaded
%          If this file is already there then it is simply loaded.
%          fcompiling this file can save compilation time if the equal
%          examples are to be used on many runs.	
% 	No depth checking is done!
%       Have to write false(Clause) :- .. rather than false :-
%           for constraints.
%       For pos-only learning, you must declare your type predicates *and* forAll
%             predicates which they call as dynamic.
%             **this could cause slowness if any of these are also background preds**
%             (the random sample is not compiled as in YAP)
%       Can't generate random examples after reading in examples. 
% 	Pos-only learning that includes pruning -- no inflate parameter
% 	Lazy evaluation of bottom clause during reduction
%		use: set(construct_bottom,reduction)
% 	Automatic generation of a refinement operator from modes
%		use: set(refine,auto)
%	Probabilistic refinement operators
%		selection of refinements that previously led to good answers
%		use: set(refine,probabilistic)
%		this is still being tested



%:- source.
%:- system_predicate(false,false), hide(false).

%Some predicates may or may not be defined by the user.
%If not defined then forAll calls should just fail
:- unknown(_,fail).
%If can't load eg example files, the just fail.
%:- nofileerrors.

progol_version('2.7.4 (Sicstus)').
progol_manual('http://www.comlab.ox.ac.uk/oucl/groups/machlearn/PProgol/ppman_toc.html').

sicstus_statistics(runtime,[X,_]):-statistics(cputime,X).

:-
	nl, nl,
	print('P - P R O G O L'), nl,
	progol_version(Version), print('Version '), print(Version), nl,
	progol_manual(Man),
	print('On-line manual at: '),
	print(Man), nl.


:- op(500,fy,#).
:- op(500,fy,*).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C O N S T R U C T


% layered generation of atoms to add to clause body
get_atoms([],_,_,Last,Last):- !.
get_atoms(Preds,Depth,MaxDepth,Last,LastLit):-
	Depth =< MaxDepth,
	Depth0 is Depth - 1,
	recorded(terms,terms(_,Depth0,_,_),_),% new terms generate ?
	add_types(Depth0),		% add types to new terms
	!,
	get_atoms1(Preds,Depth,MaxDepth,Last,Last1),
	Depth1 is Depth + 1,
	get_atoms(Preds,Depth1,MaxDepth,Last1,LastLit).
get_atoms(_,_,_,Last,Last).

get_atoms1([],_,_,Last,Last).
get_atoms1([PSym/NSucc/I/O/C|Preds],Depth,MaxDepth,Last,LastLit):-
	gen_layer(PSym/NSucc/I/O/C,Depth),
	flatten(Depth,MaxDepth,I/O/C,Last,Last1),
	get_atoms1(Preds,Depth,MaxDepth,Last1,LastLit).

flatten(Depth,MaxDepth,In/Out/Const,Last,_):-
	retractall(progol_dyn,flatten_num(_)),
	recorda(progol_dyn,flatten_num(Last),_),
	get_next_atom(Lit1),
	recorded(progol_dyn,flatten_num(LastSoFar),DbRef1),
	(Lit1 = not(Lit) -> Negated = true; Lit = Lit1, Negated = false),
	functor(Lit,Name,Arity),
	flatten_atom(Name/Arity,Depth,MaxDepth,Lit,Negated,In,Out,Const,LastSoFar,Last1),
	erase(DbRef1),
	recorda(progol_dyn,flatten_num(Last1),_),
	fail.
flatten(_,_,_,_,Last):-
	recorded(progol_dyn,flatten_num(Last),DbRef2),
	erase(DbRef2), !.

get_next_atom(Lit1):-
	recorded(atoms,Lit1,DbRef), 
	erase(DbRef).

flatten_atom('='/2,Depth,_,Lit,Negated,In,Out,Const,Last,Last1):-
	!,
	integrate_args(Depth,Lit,Out,_),	% recursively unwrap terms in o/p
	integrate_args(Depth,Lit,Const),
	flatten_eq(Lit,Negated,In,Out,Const,Last,Last1).
flatten_atom(Name/Arity,Depth,Depth,Lit,Negated,In,Out,Const,Last,LitNum):-
	!,
	integrate_args(Depth,Lit,Out),
	integrate_args(Depth,Lit,Const),
	functor(FAtom,Name,Arity),
	flatten_lit(Lit,Arity,In,Out,Const,FAtom),
	get_argterms(FAtom,Out,[],OTerms),
	(in_path(OTerms)->
		get_argterms(FAtom,In,[],ITerms),
		add_lit(Last,Negated,FAtom,In,Out,ITerms,OTerms,LitNum);
		LitNum = Last).
flatten_atom(Name/Arity,Depth,_,Lit,Negated,In,Out,Const,Last,LitNum):-
	integrate_args(Depth,Lit,Out),
	integrate_args(Depth,Lit,Const),
	functor(FAtom,Name,Arity),
	flatten_lit(Lit,Arity,In,Out,Const,FAtom),
	get_argterms(FAtom,Out,[],OTerms),
	get_argterms(FAtom,In,[],ITerms),
	add_lit(Last,Negated,FAtom,In,Out,ITerms,OTerms,LitNum).


flatten_eq(Lit,Negated,In,[],Const,Last,LitNum):-
	!,
	functor(FAtom,'=',2),
	flatten_lit(Lit,2,In,[],Const,FAtom),
	get_argterms(FAtom,In,[],ITerms),
	add_lit(Last,Negated,FAtom,In,[],ITerms,[],LitNum).
flatten_eq(Lit,Negated,In,Out,[],Last,Last1):-
	get_argterms(Lit,Out,[],OTerm),
	get_eqs(OTerm,Negated,In,Out,Last,Last1).

% get a list of equalities for output terms produced by a literal
% also update dependency graph for the equalities
get_eqs([],_,_,_,Last,Last).
get_eqs([Term|Terms],Neg,In,Out,Last,LastLit):-
	Term =.. [Name|Args],
	(Args = [] -> get_eqs(Terms,Neg,In,Out,Last,LastLit);
			flatten_terms(Args,NewArgs),
			recorded(terms,terms(TNo,_,Term,_),_),
			recorded(getPrologVars,getPrologVars(VarNum,TNo,_,_),_),
			FlatTerm =.. [Name|NewArgs],
			add_lit(Last,Neg,(VarNum=FlatTerm),In,Out,[VarNum],NewArgs,LitNum)),
	get_eqs(Terms,Neg,In,Out,LitNum,LastLit).

flatten_terms([],[]).
flatten_terms([Term|Terms],[Var|Vars]):-
	recorded(terms,terms(TNo,_,Term,_),_),
	recorded(getPrologVars,getPrologVars(Var,TNo,_,_),_),
	flatten_terms(Terms,Vars).

% return a flattened literal with variable numbers replacing
% ground terms
flatten_lit(_,0,_,_,_,_):-  !.
flatten_lit(Lit,ArgNo,In,Out,Const,FAtom):-
	%member1(ArgNo/_,Const), !,
	memberchk(ArgNo/_,Const), !,
	arg(ArgNo,Lit,Term),
	arg(ArgNo,FAtom,progol_const(Term)),
	NextArg is ArgNo - 1,
	flatten_lit(Lit,NextArg,In,Out,Const,FAtom).
flatten_lit(Lit,ArgNo,In,Out,Const,FAtom):-
	%member1(ArgNo/Type,In), !,
	memberchk(ArgNo/Type,In), !,
	arg(ArgNo,Lit,Term),
	recorded(terms,terms(TNo,_,Term,Type),_),
	recorded(getPrologVars,getPrologVars(Var,TNo,_,_),_),
	arg(ArgNo,FAtom,Var),
	NextArg is ArgNo - 1,
	flatten_lit(Lit,NextArg,In,Out,Const,FAtom).
flatten_lit(Lit,ArgNo,In,Out,Const,FAtom):-
	%member1(ArgNo/Type,Out), !,
	memberchk(ArgNo/Type,Out), !,
	arg(ArgNo,Lit,Term),
	recorded(terms,terms(TNo,_,Term,Type),_),
	recorded(getPrologVars,getPrologVars(Var,TNo,_,_),_),
	arg(ArgNo,FAtom,Var),
	NextArg is ArgNo - 1,
	flatten_lit(Lit,NextArg,In,Out,Const,FAtom).
	

% check to avoid generating useless literals in the last i layer
in_path(OVars):-
	recorded(sat,head_ovars(Vars),_), !,
	(Vars=[];OVars=[];intersects(Vars,OVars)).
in_path(_).

% modify the literal database: check if performing lazy evaluation
% of bottom clause, and update input and output terms in literal
add_lit(Last,Negated,FAtom,I,O,_,_,Last):-
	(recorded(progol,set(lazy_bottom,true),_);
		recorded(progol,set(construct_bottom,false),_)),
	(Negated = true -> Lit = not(FAtom); Lit = FAtom),
	recorded(lits,lit_info(_,0,Lit,I,O,_),_), !.
add_lit(Last,Negated,FAtom,In,Out,ITerms,OTerms,LitNum):-
	LitNum is Last + 1,
	update_iterms(LitNum,ITerms),
	update_oterms(LitNum,OTerms,[],Dependents),
	add_litinfo(LitNum,Negated,FAtom,In,Out,Dependents).

% add a literal to lits database without checking
add_litinfo(LitNum,true,FAtom,I,O,D):-
	!,
	recordz(lits,lit_info(LitNum,0,not(FAtom),I,O,D),_),
	get_vars(FAtom,I,IVars),
	get_vars(FAtom,O,OVars),
	recordz(ivars,ivars(LitNum,IVars),_),
	recordz(ovars,ovars(LitNum,OVars),_).
add_litinfo(LitNum,_,FAtom,I,O,D):-
	recordz(lits,lit_info(LitNum,0,FAtom,I,O,D),_),
	get_vars(FAtom,I,IVars),
	get_vars(FAtom,O,OVars),
	recordz(ivars,ivars(LitNum,IVars),_),
	recordz(ovars,ovars(LitNum,OVars),_).

% update lits database after checking that the atom does not 'exists '
% used during updates of lit database by lazy evaluation
update_litinfo(LitNum,true,FAtom,I,O,D):-
	recorded(lits,lit_info(LitNum,0,not(FAtom),I,O,D),_), !.
update_litinfo(LitNum,false,FAtom,I,O,D):-
	recorded(lits,lit_info(LitNum,0,FAtom,I,O,D),_), !.
update_litinfo(LitNum,Negated,FAtom,I,O,D):-
	gen_lit(LitNum),
	add_litinfo(LitNum,Negated,FAtom,I,O,D), !.

	
% update database with input terms of literal
update_iterms(_,[]).
update_iterms(LitNum,[VarNum|Vars]):-
	recorded(getPrologVars,getPrologVars(VarNum,TNo,I,O),DbRef),
	erase(DbRef),
	update(LitNum,I,NewI),
	recorda(getPrologVars,getPrologVars(VarNum,TNo,NewI,O),_),
	update_dependents(LitNum,O),
	update_iterms(LitNum,Vars).

% update database with output terms of literal
% return list of dependent literals
update_oterms(_,[],Dependents,Dependents).
update_oterms(LitNum,[VarNum|Vars],DSoFar,Dependents):-
	recorded(getPrologVars,getPrologVars(VarNum,TNo,I,O),DbRef),
	erase(DbRef),
	update(LitNum,O,NewO),
	recorda(getPrologVars,getPrologVars(VarNum,TNo,I,NewO),_),
	update_list(I,DSoFar,D1),
	update_oterms(LitNum,Vars,D1,Dependents).

% update Dependent list of literals with LitNum
update_dependents(_,[]).
update_dependents(LitNum,[Lit|Lits]):-
	recorded(lits,lit_info(Lit,Depth,Atom,ITerms,OTerms,Dependents),DbRef),
	erase(DbRef),
	update(LitNum,Dependents,NewD),
	recorda(lits,lit_info(Lit,Depth,Atom,ITerms,OTerms,NewD),_),
	update_dependents(LitNum,Lits).

% recursively mark literals with minimum depth to bind output getPrologVars in head
mark_lits([],_).
mark_lits(Lits,Depth):-
	mark_lits(Lits,Depth,true,[],Predecessors),
	delete_list(Lits,Predecessors,P1),
	Depth1 is Depth + 1,
	mark_lits(P1,Depth1).

mark_lits([],_,_,P,P).
mark_lits([Lit|Lits],Depth,GetPreds,PSoFar,P):-
	recorded(progol_dyn,marked(Lit/Depth0),DbRef), !,
	(Depth < Depth0 ->
		erase(DbRef),
		mark_lit(Lit,Depth,GetPreds,P1),
		update_list(P1,PSoFar,P2),
		mark_lits(Lits,Depth,GetPreds,P2,P);
		mark_lits(Lits,Depth,GetPreds,PSoFar,P)).
mark_lits([Lit|Lits],Depth,GetPreds,PSoFar,P):-
	mark_lit(Lit,Depth,GetPreds,P1), !,
	update_list(P1,PSoFar,P2),
	mark_lits(Lits,Depth,GetPreds,P2,P).
mark_lits([_|Lits],Depth,GetPreds,PSoFar,P):-
	mark_lits(Lits,Depth,GetPreds,PSoFar,P).

mark_lit(Lit,Depth,GetPreds,P1):-
	recorded(lits,lit_info(Lit,_,Atom,I,O,D),DbRef),
	erase(DbRef),
	recorda(progol_dyn,marked(Lit/Depth),_),
	recorda(lits,lit_info(Lit,Depth,Atom,I,O,D),_),
	(GetPreds = false ->
		P1 = [];
		get_predicates(D,D1),
		mark_lits(D1,Depth,false,[],_),
		get_vars(Atom,I,V1),
		get_predecessors(V1,[],P1)).
	
get_predicates([],[]).
get_predicates([Lit|Lits],[Lit|T]):-
	recorded(lits,lit_info(Lit,_,_,_,_,[]),_), !,
	get_predicates(Lits,T).
get_predicates([_|Lits],T):-
	get_predicates(Lits,T).

get_predecessors([],P,P).
get_predecessors([Var|Vars],PSoFar,P):-
	recorded(getPrologVars,getPrologVars(Var,_,_,O),_),
	update_list(O,PSoFar,P1),
	get_predecessors(Vars,P1,P).

% removal of literals that are repeated because of mode differences
rm_moderepeats(_,_):-
	sicstus_workround_recorded(lits,lit_info(Lit1,_,Pred1,_,_,_),_),
	sicstus_workround_recorded(lits,lit_info(Lit2,_,Pred1,_,_,_),DbRef),
	Lit1 > 1, Lit2 > Lit1,
	sicstus_workround_erase(DbRef),
	recorda(progol_dyn,marked(Lit2/0),_),
	fail.
rm_moderepeats(Last,N):-
	sicstus_workround_cleanup,
	recorded(progol_dyn,marked(_),_), !,
	get_marked(1,Last,Lits),
	length(Lits,N),
	p1_message('repeated literals'), p_message(N/Last),
	remove_lits(Lits).
rm_moderepeats(_,0).

% removal of symmetric literals
rm_symmetric(_,_):-
	recorded(progol,set(check_symmetry,true),_),
	sicstus_workround_recorded(lits,lit_info(Lit1,_,Pred1,[I1|T1],_,_),_),
	is_symmetric(Pred1,Name,Arity),
	get_vars(Pred1,[I1|T1],S1),
	sicstus_workround_recorded(lits,lit_info(Lit2,_,Pred2,[I2|T2],_,_),DbRef),
	\+ Lit1=Lit2,
	is_symmetric(Pred2,Name,Arity),
	get_vars(Pred2,[I2|T2],S2),
	%equal_set(S1,S2),
	permutation(S1,S2),
	recorda(progol_dyn,marked(Lit2/0),_),
	sicstus_workround_erase(DbRef),
	fail.
rm_symmetric(Last,N):-
	sicstus_workround_cleanup,
	recorded(progol_dyn,marked(_),_), !,
	get_marked(1,Last,Lits),
	length(Lits,N),
	p1_message('symmetric literals'), p_message(N/Last),
	remove_lits(Lits).
rm_symmetric(_,0).

is_symmetric(not(Pred),not(Name),Arity):-
	!,
	functor(Pred,Name,Arity),
	recorded(progol,symmetric(Name/Arity),_).
is_symmetric(Pred,Name,Arity):-
	functor(Pred,Name,Arity),
	recorded(progol,symmetric(Name/Arity),_).
	
% removal of literals that are repeated because of commutativity
rm_commutative(_,_):-
	recorded(progol,commutative(Name/Arity),_),
	p1_message('checking commutative literals'), p_message(Name/Arity),
	functor(Pred,Name,Arity), functor(Pred1,Name,Arity),
	sicstus_workround_recorded(lits,lit_info(Lit1,_,Pred,[I1|T1],_,_),_),
	get_vars(Pred,[I1|T1],S1),
	sicstus_workround_recorded(lits,lit_info(Lit2,_,Pred1,[I2|T2],_,_),DbRef),
	\+ Lit1=Lit2,
	get_vars(Pred1,[I2|T2],S2),
	%equal_set(S1,S2),
	permutation(S1,S2),
	recorda(progol_dyn,marked(Lit2/0),_),
	sicstus_workround_erase(DbRef),
	fail.
rm_commutative(Last,N):-
	sicstus_workround_cleanup,
	recorded(progol_dyn,marked(_),_), !,
	get_marked(1,Last,Lits),
	length(Lits,N),
	p1_message('commutative literals'), p_message(N/Last),
	remove_lits(Lits).
rm_commutative(_,0).

% recursive marking of literals that do not contribute to establishing
% variable chains to output getPrologVars in the head
rm_uselesslits(_,0):-
	recorded(sat,head_ovars([]),_), !.
rm_uselesslits(Last,N):-
	recorded(sat,head_ovars(OVars),_),
	get_predecessors(OVars,[],P),
	% delete_list([1],P,P1),
	% mark_lits(P1,0),
	mark_lits(P,0),
	get_unmarked(1,Last,Lits),
	length(Lits,N),
	p1_message('useless literals'), p_message(N/Last),
	remove_lits(Lits).

% implication map of body literals based on rewrites in
% background knowledge
get_implied:-
	recorded(progol,check_implication(Name/Arity),_),
	functor(Pred1,Name,Arity),
	functor(Pred2,Name,Arity),
	recorded(lits,lit_info(Lit1,_,Pred1,_,_,_),_),
	get_flatatom(Pred1,[],Atom1,TV1),
	skolemize(Atom1/TV1,SAtom/TV),
	asserta(SAtom),
	get_implied(Lit1,TV,Pred2),
	retract(SAtom),
	fail.
get_implied.

%depth-bound calling turned off in the Sicstus version
get_implied(Lit1,TV,Pred2):-
	%recorded(progol,set(depth,Depth),_),
	recorded(lits,lit_info(Lit2,_,Pred2,_,_,_),_),
	\+ Lit1=Lit2,
	get_flatatom(Pred2,TV,Atom2,_),
	skolemize(Atom2,SAtom2),
	%once(depth_bound_call(SAtom2,Depth)),
	once(SAtom2), %replacement for above literal
	update_implied(Lit2,Lit1),
	fail.
get_implied(_,_,_).

update_implied(Lit2,Lit1):-
	(recorded(sat,implied_by(Lit2,L2),DbRef1)->
		erase(DbRef1),
		recorda(sat,implied_by(Lit2,[Lit1|L2]),_);
		recorda(sat,implied_by(Lit2,[Lit1]),_)),
	(recorded(sat,=>(Lit1,L1),DbRef2)->
		erase(DbRef2),
		recorda(sat,=>(Lit1,[Lit2|L1]),_);
		recorda(sat,=>(Lit1,[Lit2]),_)).

% get a list of unmarked literals
get_unmarked(Lit,Last,[]):-
	Lit > Last, !.
get_unmarked(Lit,Last,Lits):-
	recorded(progol_dyn,marked(Lit/_),DbRef), !,
	erase(DbRef),
	Next is Lit + 1,
	get_unmarked(Next,Last,Lits).
get_unmarked(Lit,Last,[Lit|Lits]):-
	recorded(lits,lit_info(Lit,_,_,_,_,_),DbRef), !,
	erase(DbRef),
	Next is Lit + 1,
	get_unmarked(Next,Last,Lits).
get_unmarked(Lit,Last,Lits):-
	Next is Lit + 1,
	get_unmarked(Next,Last,Lits).

% get a list of marked literals
get_marked(Lit,Last,[]):-
	Lit > Last, !.
get_marked(Lit,Last,[Lit|Lits]):-
	recorded(progol_dyn,marked(Lit/_),DbRef), !,
	erase(DbRef),
	(recorded(lits,lit_info(Lit,_,_,_,_,_),DbRef1)->
		erase(DbRef1);
		true),
	Next is Lit + 1,
	get_marked(Next,Last,Lits).
get_marked(Lit,Last,Lits):-
	Next is Lit + 1,
	get_marked(Next,Last,Lits).

% update descendent lists of literals by removing useless literals
remove_lits(L):-
	recorded(lits,lit_info(Lit,Depth,A,I,O,D),DbRef),
	erase(DbRef),
	delete_list(L,D,D1),
	recorda(lits,lit_info(Lit,Depth,A,I,O,D1),_),
	fail.
remove_lits(_).

% get terms at arg positions specified 
get_argterms(not(Literal),Args,TermsSoFar,Terms):-
	!,
	get_argterms(Literal,Args,TermsSoFar,Terms).
get_argterms(_,[],Terms,Terms).
get_argterms(Literal,[ArgNo|Args],TermsSoFar,Terms):-
	val(ArgNo,Pos),
	arg(Pos,Literal,Term),
	update(Term,TermsSoFar,T1),
	get_argterms(Literal,Args,T1,Terms).

% get getPrologVars at arg positions specified
get_argvars(not(Literal),Args,Vars):-
	!,
	get_argvars(Literal,Args,Vars).
get_argvars(_,[],[]).
get_argvars(Literal,[ArgNo|Args],Vars):-
	val(ArgNo,Pos), arg(Pos,Literal,Term),
	get_termvars([Term],TV1),
	get_argvars(Literal,Args,TV2),
	append(TV1,TV2,Vars).

get_termvars([],[]).
get_termvars([Var|Terms],[Var|TVars]):-
	integer(Var), !,
	get_termvars(Terms,TVars).
get_termvars([Term|Terms],TVars):-
	Term =.. [_|Terms1],
	get_termvars(Terms1,TV1),
	get_termvars(Terms,TV2),
	append(TV1,TV2,TVars).

% generate a new literal at depth Depth: forced backtracking will give forAll lits
gen_layer(PSym/NSucc/Input/_/_,Depth):-
	(PSym = not(Pred) ->
		functor(Pred,Name,Arity),
		functor(Lit1,Name,Arity),
		Lit = not(Lit1);
		functor(PSym,Name,Arity),
		functor(Lit,Name,Arity)),
	%delete(Arg/Type,Input,OtherInputs),
	select(Arg/Type,Input,OtherInputs),
	Depth1 is Depth - 1,
	construct_incall(Lit,Depth1,[Arg/Type],Call1),
	construct_call(Lit,Depth,OtherInputs,Call2),
	Call1,
	Call2,
	get_successes(Lit,NSucc),
	fail.
gen_layer(_,_).

%no depth bound call in Sicstus
get_successes(Literal,1):-
	%depth_bound_call(Literal), 
	Literal,
	update_atoms(Literal), !.
get_successes(Literal,*):-
	%depth_bound_call(Literal), 
	Literal,
	update_atoms(Literal).
get_successes(Literal,N):-
	integer(N),
	N > 1,
	reset_succ,
	get_nsuccesses(Literal,N).

% get at most N matches for a literal
get_nsuccesses(Literal,N):-
	%depth_bound_call(Literal), 
	Literal,
	recorded(progol_dyn,last_success(Succ0),DbRef),
	erase(DbRef),
	Succ0 < N,
	Succ1 is Succ0 + 1,
	update_atoms(Literal),
	recorda(progol_dyn,last_success(Succ1),_),
	(Succ1 >= N -> !; true).

update_atoms(Atom):-
	recorded(atoms,Atom,_), !.
update_atoms(Atom):-
	recorda(atoms,Atom,_).

% call with input term that is an ouput of a previous literal
construct_incall(_,_,[],true):- !.
construct_incall(not(Lit),Depth,Args,Call):-
	!,
	construct_incall(Lit,Depth,Args,Call).
construct_incall(Lit,Depth,[Arg/Type],Call):-
	!,
	Call = (recorded(terms,terms(TNo,Depth,Term,Type),_),
			recorded(getPrologVars,getPrologVars(_,TNo,_,[_|_]),_)),
	arg(Arg,Lit,Term).
construct_incall(Lit,Depth,[Arg/Type|Args],(Call,Calls)):-
	arg(Arg,Lit,Term),
	Call = (recorded(terms,terms(TNo,Depth,Term,Type),_),
			recorded(getPrologVars,getPrologVars(_,TNo,_,[_|_]),_)),
	(var(Depth)-> construct_incall(Lit,_,Args,Calls);
		construct_incall(Lit,Depth,Args,Calls)).

construct_call(_,_,[],true):- !.
construct_call(not(Lit),Depth,Args,Call):-
	!,
	construct_call(Lit,Depth,Args,Call).
construct_call(Lit,Depth,[Arg/Type],Call):-
	!,
	Call = (recorded(terms,terms(TNo,Depth1,Term,Type),_),
			Depth1 < Depth,
			recorded(getPrologVars,getPrologVars(_,TNo,_,[_|_]),_)),
	arg(Arg,Lit,Term).
construct_call(Lit,Depth,[Arg/Type|Args],(Call,Calls)):-
	arg(Arg,Lit,Term),
	Call = (recorded(terms,terms(TNo,Depth1,Term,Type),_),
			Depth1 < Depth,
			recorded(getPrologVars,getPrologVars(_,TNo,_,[_|_]),_)),
	construct_call(Lit,Depth,Args,Calls).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utilities used to generate most specific inverse resolvent

% integrate a list of arguments of a Literal: returns terms integrated
% terms in argument are recursively unwrapped and integrated
integrate_args(_,_,[],[]).
integrate_args(Depth,Literal,[ArgPos/Type|T],TermList):-
        arg(ArgPos,Literal,Term),
        integrate_term(Depth,Term,TL1),
        (recorded(terms,terms(TNo,Depth,Term,unknown),DbRef)->
                        erase(DbRef),
                        recorda(terms,terms(TNo,Depth,Term,Type),_);
                        true),
        integrate_args(Depth,Literal,T,TL2),
        update_list(TL2,TL1,TermList).


% integrate a list of arguments of a Literal
% terms in argument are not recursively unwrapped
integrate_args(_,_,[]).
integrate_args(Depth,Literal,[ArgPos/Type|T]):-
        arg(ArgPos,Literal,Term),
        integrate_term(Depth,Term/Type),
        (recorded(terms,terms(TNo,Depth,Term,unknown),DbRef)->
                        erase(DbRef),
                        recorda(terms,terms(TNo,Depth,Term,Type),_);
                        true),
        integrate_args(Depth,Literal,T).


% integrate list of terms into database of terms: return terms integrated
integrate_terms(_,[],[]).
integrate_terms(Depth,[Term|Terms],TermList):-
	integrate_term(Depth,Term,TL1),
	integrate_terms(Depth,Terms,TL2),
	append(TL1,TL2,TermList).

% integrate a term into database of terms: return terms integrated
% recursively unwraps terms
%
% term is the output at current level
integrate_term(Depth,Term,[]):-
	recorded(terms,terms(TNo,Depth,Term,_),_),
	recorded(getPrologVars,getPrologVars(_,TNo,_,[_|_]),_), !.
% term is not the output of any literal
integrate_term(Depth,Term,[TNo|Terms]):-
	recorded(terms,terms(TNo,Depth1,Term,Type),DbRef),
	(Type = unknown ; recorded(getPrologVars,getPrologVars(_,TNo,_,[]),_)), !,
	(Depth1<Depth -> erase(DbRef), recorda(terms,terms(TNo,Depth,Term,Type),_);
			true),
	Term =.. [_|Args],
	integrate_terms(Depth,Args,Terms).
integrate_term(Depth,Term,Terms):-
	recorded(terms,terms(_,_,Term,Type),_),
	\+ Type = unknown,
	Term =.. [_|Args],
	integrate_terms(Depth,Args,Terms),
	!.
integrate_term(Depth,Term,[TNo|Terms]):-
	recorded(sat,last_term(Num),DbRef),
	erase(DbRef),
	recorded(sat,last_var(Var0),DbRef1),
	erase(DbRef1),
	TNo is Num + 1,
	Var is Var0 + 1,
	recorda(terms,terms(TNo,Depth,Term,unknown),_),
	recorda(getPrologVars,getPrologVars(Var,TNo,[],[]),_),
	Term =.. [_|Args],
	recorda(sat,last_term(TNo),_),
	recorda(sat,last_var(Var),_),
	integrate_terms(Depth,Args,Terms).

% integrate a term without recursive unwrapping 
integrate_term(Depth,Term/Type):-
        recorded(terms,terms(TNo,Depth,Term,Type),_),
        recorded(getPrologVars,getPrologVars(_,TNo,_,[_|_]),_), !.
integrate_term(Depth,Term/Type):-
        recorded(terms,terms(TNo,Depth1,Term,Type),DbRef),
        (Type = unknown ; recorded(getPrologVars,getPrologVars(_,TNo,_,[]),_)), !,
        (Depth1<Depth -> erase(DbRef), recorda(terms,terms(TNo,Depth,Term,Type),_);
                        true).
integrate_term(_,Term/Type):-
        recorded(terms,terms(_,_,Term,Type),_),
        \+ Type = unknown,
        !.
integrate_term(Depth,Term/Type):-
	recorded(sat,last_term(Num),DbRef),
	erase(DbRef),
	recorded(sat,last_var(Var0),DbRef1),
	erase(DbRef1),
	TNo is Num + 1,
	Var is Var0 + 1,
	recorda(sat,last_term(TNo),_),
	recorda(sat,last_var(Var),_),
	recorda(getPrologVars,getPrologVars(Var,TNo,[],[]),_),
	recorda(terms,terms(TNo,Depth,Term,Type),_).
	% recorda(terms,terms(TNo,Depth,Term,unknown),_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% obtain types required from mode declarations
update_modetypes([],[]).
update_modetypes([PSym/NSucc|Preds],[PSym/NSucc/Input/Output/Constants|T]):-
	(PSym = not(Pred) -> true; Pred = PSym),
	functor(Pred,_,Arity),
	split_args1(Pred,Arity,Input,Output,Constants),
	update_argtypes(Input),
	update_argtypes(Output),
	update_argtypes(Constants),
	update_modetypes(Preds,T).

update_argtypes([]).
update_argtypes([_/Type|T]):-
	update_types(type(Type)),
	update_argtypes(T).

% update types required
update_types(Type):-
	recorded(progol_dyn,Type,_), !.
update_types(Type):-
	recorda(progol_dyn,Type,_).

% split argument positions into +/-/#
split_args(Lit,Input,Output,Constants):-
	functor(Lit,Psym,Arity),
	functor(Pred,Psym,Arity),
	recorded(progol,mode(_,Pred),_),
	split_args1(Pred,Arity,Input,Output,Constants).

split_args1(_,0,[],[],[]):- !.
split_args1(Literal,Argno,[Argno/Type|Input1],Output,Constants):-
	arg(Argno,Literal,+Type), !,
	Argno1 is Argno - 1,
	split_args1(Literal,Argno1,Input1,Output,Constants).
split_args1(Literal,Argno,Input,[Argno/Type|Output1],Constants):-
	arg(Argno,Literal,-Type), !,
	Argno1 is Argno - 1,
	split_args1(Literal,Argno1,Input,Output1,Constants).
split_args1(Literal,Argno,Input,Output,[Argno/Type|Constants1]):-
	arg(Argno,Literal,#Type), !,
	Argno1 is Argno - 1,
	split_args1(Literal,Argno1,Input,Output,Constants1).
split_args1(Literal,Argno,Input,Output,Constants):-
	Argno1 is Argno - 1,
	split_args1(Literal,Argno1,Input,Output,Constants).

% add type info for terms
add_types(Depth):-
	recorded(terms,terms(TNo,Depth,Term,unknown),DbRef),
	recorded(progol_dyn,type(Type),_),
	Fact =.. [Type,Term],
	Fact,
	erase(DbRef),
	recorda(terms,terms(TNo,Depth,Term,Type),_),
	fail.
add_types(_).

get_determs(PSym/Arity,L):-
	findall(Lit/NSucc,
		(recorded(progol,determination(PSym/Arity,PSym1/Arity1),_),
		functor(Lit,PSym1,Arity1),recorded(progol,mode(NSucc,Lit),_)),L).

get_modes(PSym/Arity,L):-
	functor(Lit,PSym,Arity),
	findall(Lit,recorded(progol,mode(_,Lit),_),L).

% combine forAll mode declarations for a literal
% any argument that is uniformly an input, output or constant remains as such
% forAll other arguments are made outputs
combine_modes(Lit,I,O,C):-
        get_locs(Lit,input,I,O1),
        get_locs(Lit,output,O2,O3),
        get_locs(Lit,hashed,C,O4),
        append(O2,O1,O12),
        append(O12,O3,O123),
        append(O123,O4,O1234),
        sort(O1234,O5),
        merge_vlist(O5,O).

 
get_locs(Lit,M,ArgTypes,Rest):-
        functor(Lit,Name,Arity),
        functor(Mode,Name,Arity),
        findall(Mode,recorded(progol,mode(_,Mode),_),Modes),
        (M = input ->
                MType = +Type;
                (M = output -> MType = -Type; MType = #Type)
        ),
        get_locs(Arity,Modes,MType,Type,ArgTypes,Rest).
 
get_locs(0,_,_,_,[],[]):- !.
get_locs(Arg,Modes,MType,Type,Locs,Rest):-
        findall(Type,(member(Mode,Modes),arg(Arg,Mode,MType)),Types),
        length(Modes,NModes), length(Types,NTypes),
        Arg0 is Arg - 1,
        %sort(Types,STypes), %not used
        (NModes = NTypes ->
                Locs = [Arg/any|T],
                get_locs(Arg0,Modes,MType,Type,T,Rest);
                (NTypes = 0 ->
                        get_locs(Arg0,Modes,MType,Type,Locs,Rest);
                        Rest = [Arg/any|T],
                        get_locs(Arg0,Modes,MType,Type,Locs,T)
                )
        ).
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% S E A R C H

search(S,Nodes):-
	next_node(_,_), !,
	arg(23,S,LazyPreds),
        arg(3,S,RefineOp),
        repeat,
	next_node(Node,DbRef),
	erase(DbRef),
        once(recorded(search,current(LastE,Last,BestSoFar),DbRef1)),
        print_node_count(Last,50),
        expand(S,Node,Path,MinLength,Succ,PosCover,NegCover,OVars,
		PrefixClause,PrefixTV,PrefixLength),
        lazy_evaluate(Succ,LazyPreds,Path,PosCover,NegCover,Succ1),
	NextE is LastE + 1,
        get_gains(S,Last,BestSoFar,Path,PrefixClause,PrefixTV,PrefixLength,
                MinLength,Succ1,PosCover,NegCover,OVars,NextE,Last0,NextBest0),
	(RefineOp = true -> Last1 = Last0, NextBest = NextBest0;
        	get_sibgains(S,Node,Last0,NextBest0,Path,PrefixClause,
			PrefixTV,PrefixLength,MinLength,PosCover,NegCover,
			OVars,NextE,Last1,NextBest)),
        recorda(search,current(NextE,Last1,NextBest),_),
        (continue_search(S,NextBest,Last1) ->
                erase(DbRef1),
		recorda(nodes,expansion(NextE,Last,Last1),_), 
                prune_open(S,BestSoFar,NextBest),
                get_nextbest(Next),
		Next = none,
                recorded(search,current(_,Nodes,_),_);
                recorded(search,current(_,Nodes,_),_)), !.
search(_,Nodes):-
	recorded(search,current(_,Nodes,_),_).

next_node(Node,DbRef):-
	once(recorded(search,nextnode(Node),DbRef)), !.

get_search_settings(S):-
        functor(S,set,32),
	(setting(nodes,MaxNodes)-> arg(1,S,MaxNodes); arg(1,S,0)),
	(setting(explore,Explore)-> arg(2,S,Explore); arg(2,S,false)),
	(setting(refineop,RefineOp)-> arg(3,S,RefineOp); arg(3,S,false)),
	(setting(search,Search)->true; Search=bf),
	(setting(evalfn,EvalFn)->true; EvalFn=coverage),
	arg(4,S,Search/EvalFn),
	(setting(greedy,Greedy)-> arg(5,S,Greedy); arg(5,S,false)),
	(setting(verbosity,Verbose)-> arg(6,S,Verbose); arg(6,S,1)),
	(setting(clauselength,CLength)-> arg(7,S,CLength); arg(7,S,4)),
	(setting(caching,Cache)-> arg(8,S,Cache); arg(8,S,false)),
	(setting(prune_defs,Prune)-> arg(9,S,Prune); arg(9,S,false)),
	(setting(lazy_on_cost,LCost)-> arg(10,S,LCost); arg(10,S,false)),
	(setting(lazy_on_contradiction,LContra)-> arg(11,S,LContra); arg(11,S,false)),
	(setting(lazy_negs,LNegs)-> arg(12,S,LNegs); arg(12,S,false)),
	(setting(minpos,MinPos)-> arg(13,S,MinPos); arg(13,S,1)),
	(setting(depth,Depth)-> arg(14,S,Depth); arg(14,S,10)),
	(setting(cache_clauselength,CCLim) -> arg(15,S,CCLim); arg(15,S,3)),
        (recorded(progol,size(pos,PSize),_)-> arg(16,S,PSize); arg(16,S,0)),
	(setting(noise,Noise)-> arg(17,S,Noise); arg(17,S,false)),
	(setting(minacc,MinAcc)-> arg(18,S,MinAcc); arg(18,S,false)),
        (recorded(progol_dyn,base(Base),_)-> arg(19,S,Base); arg(19,S,1000)),
        (recorded(progol,size(rand,RSize),_)-> arg(20,S,RSize); arg(20,S,0)),
	(setting(lazy_bottom,LBot)-> arg(21,S,LBot); arg(21,S,false)),
	(setting(refine,Refine)-> arg(22,S,Refine); arg(22,S,false)),
	findall(PN/PA,recorded(progol,lazy_evaluate(PN/PA),_),LazyPreds),
	arg(23,S,LazyPreds),
        (recorded(progol,size(neg,NSize),_)-> arg(24,S,NSize); arg(24,S,0)),
	(setting(openlist,OSize)-> arg(25,S,OSize); arg(25,S,inf)),
        (recorded(progol,check_implication(_),_)-> arg(26,S,true); arg(26,S,false)),
        (recorded(sat,set(eq,true),_)-> arg(27,S,true); arg(27,S,false)),
        (recorded(sat,head_ovars(HOVars),_)-> arg(28,S,HOVars); arg(28,S,HOVars)),
	(setting(store_cover,true) -> arg(29,S,true); arg(29,S,false)),
	(setting(construct_bottom,CBott) -> arg(30,S,CBott); arg(30,S,saturation)),
	(get_ovars1(1,HIVars) ->  arg(31,S,HIVars); arg(31,S,[])),
	(setting(language,Lang) -> arg(32,S,Lang); arg(32,S,false)).

continue_search(S,_,Nodes):-
        arg(1,S,MaxNodes),
        Nodes >= MaxNodes, !,
	p_message('node limit exceeded'),
        fail.
continue_search(S,_,_):-
        arg(2,S,Explore),
        Explore = true, !.
continue_search(S,_,_):-
	arg(4,S,_/Evalfn),
        (Evalfn = cost; Evalfn = posonly), !.
continue_search(S,Best,_):-
	Best = [P|_]/_,
        arg(16,S,P1),
	P < P1.


update_max_head_count(N,0):-
	retractall(progol_dyn,max_head_count(_)),
	recorda(progol_dyn,max_head_count(N),_), !.
update_max_head_count(Count,Last):-
	recorded(nodes,node(Last,LitNum,_,_,PosCover,_,_,_),_), !,
	recorda(progol_dyn,head_lit(LitNum),_),
	interval_count(PosCover,N),
	Next is Last - 1,
	(N > Count -> update_max_head_count(N,Next);
		update_max_head_count(Count,Next)).
update_max_head_count(Count,Last):-
	Next is Last - 1,
	update_max_head_count(Count,Next).



expand(S,NodeNum,Path,Length,[LitNums],PosCover,NegCover,OVars,LitNums,[],CL):-
        arg(3,S,RefineOp),
        RefineOp = true, !,
	recorded(nodes,node(NodeNum,LitNums,Path,Length/CL,_,_,OVars,_E),DbRef),
        erase(DbRef),
        arg(5,S,Greedy),
        (Greedy = true ->
                recorded(progol,atoms_left(pos,PosCover),_);
                arg(16,S,PSize),
                PosCover = [1-PSize]),
        arg(4,S,_/Evalfn),
	(Evalfn = posonly -> 
                arg(20,S,RSize),
                NegCover = [1-RSize];
                arg(24,S,NSize),
                NegCover = [1-NSize]).
expand(S,NodeNum,Path1,Length,Descendents,PCover,NCover,OVars,C,TV,CL):-
        recorded(nodes,node(NodeNum,LitNum,Path,Length/_,PCover,NCover,OVars,_E),_DbRef),
        append(Path,[LitNum],Path1),
	get_pclause(Path1,[],C,TV,CL,_),
        arg(26,S,ICheck),
        recorded(lits,lit_info(LitNum,_,_Atom,_,_,Dependents),_),
	intersect1(Dependents,Path1,_,Succ),
        check_parents(Succ,OVars,D1,_),
        (ICheck = true ->
	    (recorded(sat,=>(LitNum,Implied),_)->
                        delete_list(Implied,D1,Descendents);
                        Descendents = D1);
			Descendents = D1).

get_user_refinement([],Type,Template,Id):-
	!,
	find_refinements(Type,false,Template,Id).
get_user_refinement(Lits,Type,Template,Id):-
        get_pclause(Lits,[],Clause1,_,_,_),
	((Clause1 = (Head:-true)) -> Clause = Head; Clause = Clause1),
        find_refinements(Type,Clause,Template,Id).

find_refinements(Flag,Clause,Template,0):-
	\+ Flag = probabilistic,
	!,
	refine(Clause,Template).
find_refinements(probabilistic,Clause,Template,Id):-
	findall(P/R,(refine(Clause,R),find_beta_prob(refine(Clause,R),P),P>0),R1),
	probabilistic_extensions(Clause,R2),
	append(R2,R1,Refinements),
	quicksort(descending,Refinements,Sorted),
	member(_/Template,Sorted),
	(Template= (Head:-true) -> Clause1 = Head; Clause1 = Template),
	get_refine_id(refine(Clause,Clause1),Id).

find_beta_prob(Refinement,P):-
	beta(Refinement,A,B), !,
	P is A/(A+B).
find_beta_prob(_,0.5).

% find forAll clauses that can be reached using 
% refinements that can be reached from the current clause
% using beta counts only. These are available from previous runsa.
probabilistic_extensions(C,L):-
	findall(P/C1,(beta(refine(C,C1),A,B),nonvar(C1),P is A/(A+B),P>0),L1),
	probabilistic_extend(L1,L1,L).

% find extensions of forAll clauses in a list of prob/clause pairs
probabilistic_extend([],L,L).
probabilistic_extend([_/Clause|T],LSoFar,L):-
	probabilistic_extensions(Clause,L1),
	append(LSoFar,L1,L2),
	probabilistic_extend(T,L2,L).

get_refine_id(Refinement,Id):-
	recorded(refine,refine_id(Refinement,Id),_), !.
get_refine_id(Refinement,Id):-
	gen_refine_id(Id),
	recorda(refine,refine_id(Refinement,Id),_).

match_bot_lits(false,Clause,SoFar,Lits):-
	once(match_virtual_bottom(Clause,SoFar,Lits)).
match_bot_lits(reduction,Clause,SoFar,Lits):-
	match_lazy_bottom(Clause,SoFar,Lits).
match_bot_lits(saturation,Clause,SoFar,Lits):-
	once(get_progol_clause(Clause,ProgolClause)),
	match_bot_lits(ProgolClause,SoFar,Lits).

match_bot_lits((Lit,Lits),SoFar,[LitNum|LitNums]):-
	!,
	match_bot_lit(Lit,LitNum),
	\+ member(LitNum,SoFar),
	match_bot_lits(Lits,[LitNum|SoFar],LitNums).
match_bot_lits(Lit,SoFar,[LitNum]):-
	match_bot_lit(Lit,LitNum),
	\+ member(LitNum,SoFar).

match_bot_lit(Lit,LitNum):-
	recorded(lits,lit_info(LitNum,_,Lit,_,_,_),_), 
	recorded(sat,bot_size(Last),_),
	LitNum =< Last.

match_virtual_bottom(Clause,SoFar,SLits):-
	once(get_progol_clause(Clause,ProgolClause)),
	copy_term(Clause,CClause),
	split_clause(CClause,CHead,CBody),
	example_saturated(CHead),
	numbervars(CBody,0,_),
	match_body_modes(true,CBody),
	match_bot_lits(ProgolClause,SoFar,Lits),
	quicksort(ascending,Lits,SLits).

match_lazy_bottom(Clause,SoFar,SLits):-
	once(get_progol_clause(Clause,ProgolClause)),
	copy_term(Clause,CClause),
	split_clause(CClause,CHead,CBody),
	example_saturated(CHead),
	store(stage),
	set(stage,saturation),
	match_lazy_bottom1(CBody),
	reinstate(stage),
	match_bot_lits(ProgolClause,SoFar,Lits),
	quicksort(ascending,Lits,SLits).


match_lazy_bottom1(Body):-
	Body,
	match_body_modes(false,Body),
	fail.
match_lazy_bottom1(_).

match_body_modes(IgnoreModes,(CLit,CLits)):-
        !,
        match_mode(IgnoreModes,body,CLit),
        match_body_modes(IgnoreModes,CLits).
match_body_modes(IgnoreModes,CLit):-
        match_mode(IgnoreModes,body,CLit).

match_mode(_,_,true):- !.
match_mode(IgnoreModes,Loc,CLit):-
        (IgnoreModes = true ->
                        combine_modes(CLit,I,O,C);
                        functor(CLit,Name,Arity),
                        functor(Mode,Name,Arity),
                        recorded(progol,mode(_,Mode),_),
                        split_args1(Mode,Arity,I,O,C)),
        (recorded(sat,bot_size(BSize),DbRef)-> erase(DbRef); BSize = 0),
        (recorded(sat,last_lit(Last),DbRef1)-> erase(DbRef1); Last = 0),
        recorda(atoms,CLit,_),
        set(i,IVal),
        (Loc = head ->
                flatten(0,IVal,O/I/C,BSize,BSize1);
                flatten(0,IVal,I/O/C,BSize,BSize1)),
        recorda(sat,bot_size(BSize1),_),
        recorda(sat,last_lit(BSize1),_).

% integrate head literal into lits database
% used during lazy evaluation of bottom clause
% if no bottom is constructed then outputvars is set to empty
%       and forAll mode declararations are combined into a single one
integrate_head_lit(false,_,[]):-
	!,
	example_saturated(Example),
	match_mode(true,head,Example),
	recorda(progol_dyn,head_ovars([]),_).
integrate_head_lit(_,true,HeadOVars):-
	!,
	example_saturated(Example),
	match_mode(false,head,Example),
	get_ivars1(1,HeadOVars),
	recorda(progol_dyn,head_ovars(HeadOVars),_).
integrate_head_lit(_,_,_).


get_progol_clause((Lit:-true),PLit):-
	!,
	get_progol_lit(Lit,PLit).
get_progol_clause((Lit:-Lits),(PLit,PLits)):-
	!,
	get_progol_lit(Lit,PLit),
	get_progol_lits(Lits,PLits).
get_progol_clause(Lit,PLit):-
	get_progol_lit(Lit,PLit).

get_progol_lits((Lit,Lits),(PLit,PLits)):-
	!,
	get_progol_lit(Lit,PLit),
	get_progol_lits(Lits,PLits).
get_progol_lits(Lit,PLit):-
	get_progol_lit(Lit,PLit).

get_progol_lit(Lit,PLit):-
	functor(Lit,Name,Arity),
	functor(PLit,Name,Arity),
	get_progol_lit(Lit,PLit,Arity).

get_progol_lit(_,_,0):- !.
get_progol_lit(Lit,PLit,Arg):-
	arg(Arg,Lit,Term),
	(var(Term) -> arg(Arg,PLit,Term);arg(Arg,PLit,progol_const(Term))),
	NextArg is Arg - 1,
	get_progol_lit(Lit,PLit,NextArg), !.

get_ovars([],V,V).
get_ovars([LitNum|Lits],VarsSoFar,Vars):-
	get_ovars1(LitNum,OVars),
	append(OVars,VarsSoFar,Vars1),
	get_ovars(Lits,Vars1,Vars).

get_ovars1(LitNum,OVars):-
	recorded(ovars,ovars(LitNum,OVars),_), !.
get_ovars1(LitNum,OVars):-
	recorded(lits,lit_info(LitNum,_,Atom,_,O,_),_),
	get_vars(Atom,O,OVars).


% get set of getPrologVars at arg positions specified
get_vars(not(Literal),Args,Vars):-
	!,
	get_vars(Literal,Args,Vars).
get_vars(_,[],[]).
get_vars(Literal,[ArgNo|Args],Vars):-
	val(ArgNo,Pos), arg(Pos,Literal,Term),
	get_vars_in_term([Term],TV1),
	get_vars(Literal,Args,TV2),
	update_list(TV2,TV1,Vars).

get_vars_in_term([],[]).
get_vars_in_term([Var|Terms],[Var|TVars]):-
	integer(Var), !,
	get_vars_in_term(Terms,TVars).
get_vars_in_term([Term|Terms],TVars):-
	Term =.. [_|Terms1],
	get_vars_in_term(Terms1,TV1),
	get_vars_in_term(Terms,TV2),
	update_list(TV2,TV1,TVars).

get_ivars([],V,V).
get_ivars([LitNum|Lits],VarsSoFar,Vars):-
	get_ivars1(LitNum,IVars),
	append(IVars,VarsSoFar,Vars1),
	get_ivars(Lits,Vars1,Vars).

get_ivars1(LitNum,IVars):-
	recorded(ivars,ivars(LitNum,IVars),_), !.
get_ivars1(LitNum,IVars):-
	recorded(lits,lit_info(LitNum,_,Atom,I,_,_),_),
	get_vars(Atom,I,IVars).

check_parents([],_,[],[]).
check_parents([LitNum|Lits],OutputVars,[LitNum|DLits],Rest):-
	get_ivars1(LitNum,IVars),
	%subset1(IVars,OutputVars), !,
	sublist(IVars,OutputVars), !,
	check_parents(Lits,OutputVars,DLits,Rest).
check_parents([LitNum|Lits],OutputVars,DLits,[LitNum|Rest]):-
	check_parents(Lits,OutputVars,DLits,Rest), !.


get_gains(S,Last,Best,_,_,_,_,_,_,_,_,_,_,Last,Best):-
        \+ continue_search(S,Best,Last), !.
get_gains(_,Last,Best,_,_,_,_,_,[],_,_,_,_,Last,Best):- !.
get_gains(S,Last,Best,Path,C,TV,L,Min,[L1|Succ],Pos,Neg,OVars,E,Last1,NextBest):-
        get_gain(S,upper,Last,Best,Path,C,TV,L,Min,L1,Pos,Neg,OVars,E,Best1,Node1), !,
        get_gains(S,Node1,Best1,Path,C,TV,L,Min,Succ,Pos,Neg,OVars,E,Last1,NextBest).
get_gains(S,Last,BestSoFar,Path,C,TV,L,Min,[_|Succ],Pos,Neg,OVars,E,Last1,NextBest):-
        get_gains(S,Last,BestSoFar,Path,C,TV,L,Min,Succ,Pos,Neg,OVars,E,Last1,NextBest),
	!.

get_sibgains(S,Node,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,Last1,NextBest):-
        recorded(nodes,node(Node,LitNum,_,_,_,_,_,OldE),_DbRef),
	recorded(nodes,expansion(OldE,_,LastSib),_),
	Node1 is Node + 1,
        arg(31,S,HIVars),
	delete_list(HIVars,OVars,LVars),
        get_sibgain(S,LVars,LitNum,Node1,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,
			E,NextBest,Last1).

get_sibgain(S,_,_,Node,Node1,Last,Best,_,_,_,_,_,_,_,_,_,Best,Last):-
	(Node > Node1;
	not(continue_search(S,Best,Last))), !.
get_sibgain(S,LVars,LitNum,Node,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,LBest,LNode):-
        arg(23,S,Lazy),
        arg(26,S,ICheck),
        get_sibpncover(Lazy,Node,Pos,Neg,Sib1,PC,NC), 
       	lazy_evaluate([Sib1],Lazy,Path,PC,NC,[Sib]),
        (ICheck = true ->
                (recorded(sat,=>(LitNum,Implied),_)->
			not(member(Sib,Implied));	
                        true);
                true),
	get_ivars1(Sib,SibIVars),
	(intersects(SibIVars,LVars) -> Flag = upper;
		get_ovars1(Sib,SibOVars),
		(intersects(SibOVars,LVars) -> Flag = upper; Flag = exact)),
        get_gain(S,Flag,Last,Best,Path,C,TV,L,Min,Sib,PC,NC,OVars,E,Best1,Node1), !,
	NextNode is Node + 1,
	get_sibgain(S,LVars,LitNum,NextNode,LastSib,Node1,Best1,Path,C,TV,L,Min,Pos,Neg,
			OVars,E,LBest,LNode), !.
get_sibgain(S,LVars,LitNum,Node,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,Best1,Node1):-
	NextNode is Node + 1,
	get_sibgain(S,LVars,LitNum,NextNode,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,
			OVars,E,Best1,Node1), !.

get_sibpncover(Lazy,NodeNum,Pos,Neg,Sib,PC,NC):-
        recorded(nodes,node(NodeNum,Sib,_,_,Pos1,Neg1,_,_),_),
        recorded(lits,lit_info(Sib,_,Atom,_,_,_),_),
	functor(Atom,Name,Arity),
	(member1(Name/Arity,Lazy) ->
		PC = Pos, NC = Neg;
		calc_intersection(Pos,Pos1,PC),
		calc_intersection(Neg,Neg1,NC)).

calc_intersection(A1/[B1-L1],A2/[B2-L2],A/[B-L]):-
	!,
	intervals_intersection(A1,A2,A),
	B3 is max(B1,B2),
	(intervals_intersects(A1,[B2-L2],X3-_) -> true; X3 = B3),
	(intervals_intersects(A2,[B1-L1],X4-_) -> true; X4 = B3),
	B4 is min(X3,B3),
	B is min(X4,B4),
	L is max(L1,L2).
calc_intersection(A1/_B1,A2,A):-
	!,
	intervals_intersection(A1,A2,A).
calc_intersection(A1,A2/_B2,A):-
	!,
	intervals_intersection(A1,A2,A).
calc_intersection(A1,A2,A):-
	intervals_intersection(A1,A2,A).
	

get_gain(S,_,Last,Best/Node,Path,_,_,_,MinLength,L1,Pos,Neg,OVars,E,Best1,NewLast):-
        arg(3,S,RefineOp),
        RefineOp = true ,
        arg(22,S,RefineType),
        arg(23,S,LazyPreds),
        retractall(progol_dyn,best_refinement(_)),
        retractall(progol_dyn,last_refinement(_)),
	recorda(progol_dyn,best_refinement(Best/Node),_),
	recorda(progol_dyn,last_refinement(Last),_),
	arg(21,S,LazyBottom),
	arg(28,S,HOVars),
	arg(30,S,ConstructBottom),
	integrate_head_lit(ConstructBottom,LazyBottom,HOVars),
	get_user_refinement(L1,RefineType,Template,Id),
	match_bot_lits(ConstructBottom,Template,[],Refinement),
	lazy_evaluate_refinement(Refinement,LazyPreds,Pos,Neg,Refinement1),
        arg(29,S,CCheck),
        (CCheck = true ->
                retractall(progol_dyn,covers(_,_)),
                retractall(progol_dyn,coversn(_,_));
                true),
	get_pclause(Refinement1,[],Clause,_,CLength,LastD),
	length_ok(S,MinLength,CLength,LastD,EMin,ELength),
	split_clause(Clause,Head,Body),
	recordz(pclause,pclause(Head,Body),DbRef),
	recorded(progol_dyn,best_refinement(OldBest),DbRef1),
	recorded(progol_dyn,last_refinement(OldLast),DbRef2),
        arg(6,S,Verbosity),
	(Verbosity >= 1 ->
	    p_message('new refinement'),
	    pp_dclause(Clause);
	    true),
	get_gain1(S,upper,DbRef,Clause,CLength,EMin/ELength,OldLast,OldBest,
	[Id|Path],Refinement1,Refinement1,Pos,Neg,OVars,E,Best1),
	erase(DbRef),
	erase(DbRef2),
	NewLast is OldLast + 1,
	recorda(progol_dyn,last_refinement(NewLast),DbRef3),
	erase(DbRef1),
	recorda(progol_dyn,best_refinement(Best1),DbRef4),
	(continue_search(S,Best1,NewLast) ->
		fail;
		erase(DbRef3),
		erase(DbRef4)), !.
get_gain(S,_,_,_,_,_,_,_,_,_,_,_,_,_,Best,Last):-
	arg(3,S,RefineOp),
        RefineOp = true , !,
	recorded(progol_dyn,best_refinement(Best),DbRef),
	recorded(progol_dyn,last_refinement(Last),DbRef1),
	erase(DbRef),
	erase(DbRef1).
get_gain(S,Flag,Last,Best/Node,Path,C,TV,Len1,MinLen,L1,Pos,Neg,OVars,E,Best1,Last1):-
        arg(29,S,CCheck),
        (CCheck = true ->
                retractall(progol_dyn,covers(_,_)),
                retractall(progol_dyn,coversn(_,_));
                true),
        update(L1,Path,Path1),
        get_pclause([L1],TV,Lit1,_,Len2,LastD),
        extend_clause(C,Lit1,Clause),
        CLen is Len1 + Len2,
        length_ok(S,MinLen,CLen,LastD,EMin,ELength),
        split_clause(Clause,Head,Body),
        recordz(pclause,pclause(Head,Body),DbRef),
        arg(6,S,Verbosity),
        (Verbosity >= 1 -> pp_dclause(Clause); true),
        get_gain1(S,Flag,DbRef,Clause,CLen,EMin/ELength,Last,Best/Node,
                        Path,Path1,L1,Pos,Neg,OVars,E,Best1),
        erase(DbRef),
        Last1 is Last + 1.

get_gain1(S,_,DbRef,_,CL,_,Last,Best,Path,_,L1,Pos,Neg,_,E,Best):-
        abandon_branch(S,DbRef), !,
        Node1 is Last + 1,
        arg(7,S,ClauseLength),
        (ClauseLength = CL -> true;
                arg(3,S,RefineOp),
                (RefineOp = true  -> true;
                        recorda(nodes,node(Node1,L1,0,0,Pos,Neg,[],E),_))),
	arg(22,S,Refine),
	(Refine = probabilistic -> inc_beta_counts(Path,beta); true).
get_gain1(S,_,DbRef,_,_,_,_,Best,Path,_,_,_,_,_,_,Best):-
        arg(8,S,Caching),
        Caching = true,
        'instance'(DbRef,pclause(Head,Body)),
        skolemize((Head:-Body),SHead,SBody,_),
        recorded(prune_cache,prune([SHead|SBody]),_), !,
        p_message('in prune cache'),
	arg(22,S,Refine),
	(Refine = probabilistic -> inc_beta_counts(Path,beta); true), !.
get_gain1(S,Flag,DbRef,C,CL,EMin/EL,Last,Best/Node,Path,_Path1,L1,Pos,Neg,OVars,E,Best1):-
        (false(C) ->
                p_message('constraint violated'),
                Contradiction = true;
                Contradiction = false),
        arg(8,S,Caching),
	(Caching = true ->
	    arg(15,S,CCLim),
	    get_cache_entry(CCLim,C,Entry);
	    Entry = false),
        arg(3,S,RefineOp),
	(RefineOp = false -> true ; refinement_ok(Entry,RefineOp)),
	'instance'(DbRef,pclause(Head,Body)),
	arg(32,S,Lang),
	(Lang = false -> true; lang_ok((Head:-Body),Lang)),
        progol_prove_examples(S,Flag,Path,Contradiction,Entry,Best,CL,EL,(Head:-Body),Pos,Neg,
				PCvr,NCvr,Label),
	arg(4,S,Search/Evalfn),
	complete_clause_label(Evalfn,C,Label,Label1),
	compression_ok(Evalfn,Label1),
       	get_search_keys(Search,Label1,SearchKeys),
        arg(6,S,Verbosity),
        (Verbosity >= 1 -> Label = [A,B|_], p_message(A/B); true),
        Node1 is Last + 1,
        arg(7,S,ClauseLength),
        (ClauseLength = CL -> true;
		(RefineOp = true ->
		    get_ovars(L1,[],OVars2),
		    recorda(nodes,node(Node1,L1,Path,EMin/EL,[],[],OVars2,E),_);
			get_ovars1(L1,OVars1),
			append(OVars,OVars1,OVars2),
                	recorda(nodes,node(Node1,L1,Path,EMin/EL,PCvr,
			NCvr,OVars2,E),_)),
		arg(19,S,Base),
                update_open_list(Base,SearchKeys,Label1,Node1)),
	arg(28,S,HOVars),
        (clause_ok(Contradiction,HOVars,OVars2) ->
                update_best(S,C,PCvr,NCvr,Best/Node,Label1/Node1,Best1);
                Best1=Best/Node),
	update_probabilistic_refinement(S,Path,Best/Node,Best1,Label1,ClauseLength,CL), !.
get_gain1(_,_,_,_,_,_,_,Best,_,_,_,_,_,_,_,Best).


abandon_branch(S,DbRef):-
        arg(9,S,PruneDefined),
        PruneDefined = true,
        'instance'(DbRef,pclause(Head,Body)),
        prune((Head:-Body)), !,
        arg(6,S,Verbosity),
        (Verbosity >= 1 -> p_message(pruned); true).
abandon_branch(S,_):-
        abandon, !,
        arg(6,S,Verbosity),
        (Verbosity >= 1 -> p_message(abandoned); true).


clause_ok(false,V1,V2):-
        %subset1(V1,V2).
	sublist(V1,V2).

refinement_ok(false,_):- !.
refinement_ok(_,false):- !.
refinement_ok(Entry,true):-
	(check_cache(Entry,pos,_); check_cache(Entry,neg,_)), !,
	p_message('redundant refinement'),
	fail.
refinement_ok(_,_).

lang_ok((Head:-Body),N):-
	get_psyms((Head,Body),PSymList),
	(lang_ok1(PSymList,N) -> true;
		p_message('outside language bound'),
		fail).

get_psyms((L,B),[N/A|Syms]):-
	!,
	functor(L,N,A),
	get_psyms(B,Syms).
get_psyms(true,[]):- !.
get_psyms(L,[N/A]):-
	functor(L,N,A).

lang_ok1([],_).
lang_ok1([Pred|Preds],N):-
        length(Preds,N0),
        delete_all(Pred,Preds,Preds1),
        length(Preds1,N1),
        PredOccurs is N0 - N1 + 1,
	PredOccurs =< N,
	lang_ok1(Preds1,N).


% update hyperparameters of beta distrib for probabilistic refinement
update_probabilistic_refinement(S,_,_,_,_,_,_):-
	arg(22,S,RefineType),
	\+ RefineType = probabilistic, !.
update_probabilistic_refinement(_,Path,Best/_,Best1/_,Best1,_,_):-
	\+ Best = Best1,
	inc_beta_counts(Path,alpha), !.
update_probabilistic_refinement(_S,Path,Label/_,Label/_,Label1,LMax,L):-
	Label = [_,_,_,Gain|_],
	Label1 = [_,_,_,Gain1|_],
        (Gain1 = Gain ->
	    inc_beta_counts(Path,alpha);
		(LMax = L -> inc_beta_counts(Path,beta); true)).
	

% increment hyperparameters of beta distribution for each refinement
% only used if performing probabilistic refinements
inc_beta_counts([],_):- !.
inc_beta_counts([R1|R],Parameter):-
	inc_beta_count(R1,Parameter),
	inc_beta_counts(R,Parameter), !.

inc_beta_count(RefineId,Parameter):-
	recorded(refine,beta(RefineId,A,B),DbRef), !,
	erase(DbRef),
	(Parameter = beta -> A1 is A, B1 is B+1; A1 is A+1, B1 is B),
	recorda(refine,beta(RefineId,A1,B1),_).
inc_beta_count(RefineId,Parameter):-
	recorded(refine,refine_id(Clause,RefineId),_),
	beta(Clause,A,B), !,
	(Parameter = beta -> A1 is A, B1 is B+1; A1 is A+1, B1 is B),
	recorda(refine,beta(RefineId,A1,B1),_).
inc_beta_count(RefineId,Parameter):-
	(Parameter = beta -> A1 is 1, B1 is 2; A1 is 2, B1 is 1),
	recorda(refine,beta(RefineId,A1,B1),_), !.


progol_prove_examples(S,Flag,_,_,Entry,Best,CL,L2,Clause,Pos,Rand,PCover,RCover,[P,B,CL,I,G]):-
	arg(4,S,_/Evalfn),
	Evalfn = posonly, !,
        progol_prove_pos(S,Flag,Entry,Best,[PC,L2],Clause,Pos,PCover,PC),
	progol_prove_rand(S,Flag,Entry,Clause,Rand,RCover,RC),
	find_posgain(PCover,P),
	arg(16,S,M), arg(20,S,N),
	GC is (RC+1.0)/(N+2.0), % Laplace correction for small numbers
	A is log(P),
	B is log(GC),
	G is GC*M/P,
	C is CL/P,
	% Sz is CL*M/P,
	% D is M*G,
	%  I is M - D - Sz,
	I is A - B - C.
progol_prove_examples(S,_,_,_,Entry,_,CL,_,_,Pos,Neg,Pos,Neg,[PC,NC,CL]):-
        arg(10,S,LazyOnCost),
        LazyOnCost = true, !,
        progol_prove_lazy_cached(S,Entry,Pos,Neg,Pos1,Neg1),
        interval_count(Pos1,PC),
        interval_count(Neg1,NC).
progol_prove_examples(S,_,_,true,Entry,_,CL,_,_,Pos,Neg,Pos,Neg,[PC,NC,CL]):-
        arg(11,S,LazyOnContra),
        LazyOnContra = true, !,
        progol_prove_lazy_cached(S,Entry,Pos,Neg,Pos1,Neg1),
        interval_count(Pos1,PC),
        interval_count(Neg1,NC).
progol_prove_examples(S,Flag,Path,_,Ent,Best,CL,L2,Clause,Pos,Neg,PCover,NCover,[PC,NC,CL]):-
        arg(7,S,ClauseLength),
        ClauseLength = CL,
	arg(22,S,Refine),
	interval_count(Pos,MaxPCount),
        (progol_prove_neg(S,Flag,Ent,Best,[MaxPCount,CL],Clause,Neg,NCover,NC)-> true;
		(Refine = probabilistic ->
			\+ Best = [MaxPCount|_],
			inc_beta_counts(Path,beta);
			true),
		fail),
        arg(17,S,Noise), arg(18,S,MinAcc),
        (maxlength_neg_ok(Noise/MinAcc,Ent,MaxPCount,NC)-> true;
		arg(22,S,Refine),
		(Refine=probabilistic -> inc_beta_counts(Path,beta); true),
		fail), 
        progol_prove_pos(S,Flag,Ent,Best,[PC,L2],Clause,Pos,PCover,PC),
        (maxlength_neg_ok(Noise/MinAcc,Ent,PC,NC)-> true;
		(Refine=probabilistic -> inc_beta_counts(Path,beta); true),
		fail), !.
progol_prove_examples(S,Flag,_,_,Ent,Best,CL,L2,Clause,Pos,Neg,PCover,NCover,[PC,NC,CL]):-
        arg(7,S,ClauseLength),
        ClauseLength > CL,
        progol_prove_pos(S,Flag,Ent,Best,[PC,L2],Clause,Pos,PCover,PC),
        progol_prove_neg(S,Flag,Ent,Best,[PC,CL],Clause,Neg,NCover,NC),
	!.

progol_prove_lazy_cached(S,Entry,Pos,Neg,Pos1,Neg1):-
        arg(8,S,Caching),
	Caching = true, !,
	(check_cache(Entry,pos,Pos1)->
		true;
		add_cache(Entry,pos,Pos),
		Pos1 = Pos),
	(check_cache(Entry,neg,Neg1)->
		true;
		add_cache(Entry,neg,Neg),
		Neg1 = Neg).
progol_prove_lazy_cached(_,_,Pos,Neg,Pos,Neg).

complete_clause_label(posonly,_,L,L):- !.
complete_clause_label(user,Clause,[P,N,L],[P,N,L,Val]):-
	cost(Clause,[P,N,L],Cost),!,
        (Cost = inf -> Val is -10000; (Cost = -inf -> Val is 10000; Val is -Cost)).
complete_clause_label(EvalFn,_Clause,[P,N,L],[P,N,L,Val]):-
	evalfn(EvalFn,[P,N,L],Val), !.
complete_clause_label(_,_,_,_):-
	p_message1('error'), p_message('incorrect evaluation/cost function'),
	fail.

get_search_keys(bf,[_P,_N,L,F|_T],[L1,F]):-
	L1 is -1*L.
get_search_keys(df,[_P,_N,L,F|_T],[L,F]).
get_search_keys(heuristic,[_P,_N,L,F|_T],[F,L1]):-
	L1 is -1*L.

progol_prove_pos(_,_,_,_,_,_,[],[],0):- !.
progol_prove_pos(S,_Flag,Entry,BestSoFar,PosSoFar,Clause,_,PCover,PCount):-
        arg(29,S,CCheck),
        CCheck = true,
        recorded(progol_dyn,covers(PCover,PCount),_), 
        pos_ok(S,Entry,BestSoFar,PosSoFar,Clause,PCover), !.
progol_prove_pos(S,Flag,Entry,BestSoFar,PosSoFar,Clause,Pos,PCover,PCount):-
        progol_prove_cache(Flag,S,pos,Entry,Clause,Pos,PCover,PCount),
        pos_ok(S,Entry,BestSoFar,PosSoFar,Clause,PCover), !.

progol_prove_neg(S,_,Entry,_,_,_,[],[],0):-
	arg(8,S,Caching),
	(Caching = true -> add_cache(Entry,neg,[]); true), !.
progol_prove_neg(S,_,_,_,_,_,_,NCover,NCount):-
	arg(29,S,CCheck),
        CCheck = true,
        recorded(progol_dyn,coversn(NCover,NCount),_), !.
progol_prove_neg(S,Flag,Entry,BestSoFar,PosSoFar,Clause,Neg,NCover,NCount):-
        arg(12,S,LazyNegs),
        LazyNegs = true, !,
        lazy_progol_prove_neg(S,Flag,Entry,BestSoFar,PosSoFar,Clause,Neg,NCover,NCount).
progol_prove_neg(S,Flag,Entry,[P,0,L1|_],[P,L2],Clause,Neg,[],0):-
	arg(4,S,bf/coverage),
        L2 is L1 - 1,
	!,
        progol_prove_cache(Flag,S,neg,Entry,Clause,Neg,0,[],0), !.
progol_prove_neg(S,Flag,Entry,[P,N|_],[P,L1],Clause,Neg,NCover,NCount):-
	arg(4,S,bf/coverage),
        !,
        arg(7,S,ClauseLength),
        (ClauseLength = L1 ->
		arg(2,S,Explore),
		(Explore = true -> MaxNegs is N; MaxNegs is N - 1),
                MaxNegs >= 0,
                progol_prove_cache(Flag,S,neg,Entry,Clause,Neg,MaxNegs,NCover,NCount),
		NCount =< MaxNegs;
                progol_prove_cache(Flag,S,neg,Entry,Clause,Neg,NCover,NCount)),
        !.
progol_prove_neg(S,Flag,Entry,_,[P1,L1],Clause,Neg,NCover,NCount):-
        arg(7,S,ClauseLength),
        ClauseLength = L1, !,
        arg(17,S,Noise), arg(18,S,MinAcc),
        get_max_negs(Noise/MinAcc,P1,N1),
        progol_prove_cache(Flag,S,neg,Entry,Clause,Neg,N1,NCover,NCount),
	NCount =< N1,
        !.
progol_prove_neg(S,Flag,Entry,_,_,Clause,Neg,NCover,NCount):-
        progol_prove_cache(Flag,S,neg,Entry,Clause,Neg,NCover,NCount),
        !.

progol_prove_rand(S,Flag,Entry,Clause,Rand,RCover,RCount):-
        progol_prove_cache(Flag,S,rand,Entry,Clause,Rand,RCover,RCount),
        !.

lazy_progol_prove_neg(S,Flag,Entry,[P,N|_],[P,_],Clause,Neg,NCover,NCount):-
        arg(4,S,bf/coverage),
	!,
        MaxNegs is N + 1,
	progol_prove_cache(Flag,S,neg,Entry,Clause,Neg,MaxNegs,NCover,NCount),
        !.
lazy_progol_prove_neg(S,Flag,Entry,_,[P1,_],Clause,Neg,NCover,NCount):-
        arg(17,S,Noise), arg(18,S,MinAcc),
        get_max_negs(Noise/MinAcc,P1,N1),
        MaxNegs is N1 + 1,
        progol_prove_cache(Flag,S,neg,Entry,Clause,Neg,MaxNegs,NCover,NCount),
        !.

get_max_negs(N/_,_,N):- number(N), !.
get_max_negs(false/A,_,0):-
        \+ A = false,
        A >= 0.999, !.
get_max_negs(false/MinAcc,P1,N):-
        \+ MinAcc = false,
        number(P1),
        !,
        %NR is (1-MinAcc)*P1/MinAcc,
        %floor_real(NR,N1),
	%N is N1 + 1.
	N is integer((1-MinAcc)*P1/MinAcc) + 1.
        
get_max_negs(_,_,0).

print_node_count(NodeNum,B):-
        N is NodeNum mod B,
        (N = 0-> p1_message('clauses constructed'),p_message(NodeNum); true).


update_open_list(Base,[K1,K2],Label,Node):-
	GainVal is K1*Base + K2,
	recordz(gains,gain(GainVal,Label,Node),_),
	recorded(openlist,OpenList,DbRef),
	erase(DbRef),
	uniq_insert(descending,GainVal,OpenList,List1),
	recorda(openlist,List1,_).


fix_base(LastPos,LastNeg):-
	recorded(progol,set(clauselength,L),_),
        B1 is LastNeg + L + 1,
        ((LastPos =< B1) -> Base = B1; Base = LastPos),
        recorda(progol_dyn,base(Base),_).


pos_ok(S,_,_,_,_,_):-
	arg(4,S,_/Evalfn),
        not((Evalfn = coverage; Evalfn = compression)), !.
pos_ok(S,Entry,_,[P,_],_,_):-
        arg(13,S,MinPos),
        P < MinPos, !,
        arg(8,S,Caching),
        (Caching = true ->
                add_prune_cache(Entry);
                true),
        fail.
pos_ok(S,_,[_,_,_,C1|_],[P,L],_,_):-
        arg(4,S,_/Evalfn),
        evalfn(Evalfn,[P,0,L],C2),
        (C2 > C1;
	(arg(2,S,Explore),Explore=true,explore_eq(C1,C2))), !.



maxlength_neg_ok(Noise/false,Entry,_,N):-
        !,
        (N =< Noise -> true; add_prune_cache(Entry), fail).
maxlength_neg_ok(false/Acc,Entry,P,N):-
        !,
        A is P/(P+N),
        (A >= Acc -> true; add_prune_cache(Entry), fail).
maxlength_neg_ok(_,_,_,_).

compression_ok(compression,[P,_,L|_]):-
	!,
	P - L + 1 > 0.
compression_ok(_,_).

length_ok(S,MinLen,ClauseLen,LastD,ExpectedMin,ExpectedCLen):-
        arg(3,S,RefineOp),
        (RefineOp = true  -> L1 = 0; L1 = LastD),
        (L1 < MinLen->ExpectedMin = L1;ExpectedMin = MinLen),
        ExpectedCLen is ClauseLen + ExpectedMin,
        arg(7,S,CLength),
        ExpectedCLen =< CLength, !.

update_best(S,_,_,_,Best,[_,N|_]/_,Best):-
        arg(17,S,Noise),
        \+ Noise = false,
        N > Noise, !.
update_best(S,_,_,_,Best,[P,N|_]/_,Best):-
        arg(18,S,MinAcc),
        \+ MinAcc = false,
        Accuracy is P/(P + N),
        Accuracy < MinAcc, !.
update_best(S,_,_,_,Best,[_,_,_,Compr|_]/_,Best):-
        arg(4,S,_/compression),
        Compr =< 0, !.
update_best(_S,Clause,PCover,NCover,Label/_,Label1/Node1,Label1/Node1):-
	Label = [_,_,_,Gain|_],
	Label1 = [_,_,_,Gain1|_],
	Gain1 > Gain, !,
        recorded(search,selected(_,_,_,_),DbRef),
        erase(DbRef),
        recorda(search,selected(Label1,Clause,PCover,NCover),_),
        show_clause(Label1,Clause,Node1,false),
        record_clause(Label1,Clause,Node1,false).
update_best(S,Clause,_,_,Label/Node,Label1/Node1,Label/Node):-
        arg(2,S,Explore),
        Explore = true,
        Label = [_,_,_,Gain|_],
	Label1 = [_,_,_,Gain1|_],
	explore_eq(Gain1,Gain), !,
        show_clause(Label1,Clause,Node1,explore),
        record_clause(Label1,Clause,Node1,explore).
update_best(_,_,_,_,Best,_,Best).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% P R U N I N G

get_node([Gain|_],DbRef,Gain,Node):-
        recorded(gains,gain(Gain,_,Node),DbRef).
get_node([_|Gains],DbRef,Gain,Node):-
	get_node(Gains,DbRef,Gain,Node).

prune_open(S,_,_):-
	arg(25,S,OSize),
	\+ OSize = inf,
        retractall(progol_dyn,in_beam(_)),
        recorda(progol_dyn,in_beam(0),_),
        recorded(openlist,Gains,_),
        get_node(Gains,DbRef,Gain,NodeNum),
        recorded(progol_dyn,in_beam(N),DbRef1),
        (N < OSize->
                erase(DbRef1),
                N1 is N + 1,
                recorda(progol_dyn,in_beam(N1),_);
                erase(DbRef),
                p1_message('not-admissible removal'), p_message(NodeNum),
		recorded(gains,gain(Gain,_,NodeNum),DbRef3),
                erase(DbRef3)),
         fail.
prune_open(S,_,_):-
        arg(4,S,Search),
        arg(2,S,Explore),
	(
		Explore = true;
		not((Search = bf/coverage;
			Search = df/coverage;
			Search = heuristic/compression;
			Search = heuristic/posonly))
	), !.
prune_open(_,_/N,_/N):- !.
prune_open(S,_,[_,_,L,Best|_]/_):-
        arg(4,S,_/Evalfn),
	Evalfn = coverage, !,
	arg(27,S,Eq),
        (Eq = true -> MaxLength is L; MaxLength is L - 1),
        remove1(MaxLength,Best),
        !.
prune_open(S,_,[_,_,_,Best|_]/_):-
        arg(4,S,heuristic/Evalfn),
        (Evalfn = compression ; Evalfn = posonly), !,
        remove2(S,Best).
% pruning for laplace and m-estimates devised by James Cussens
prune_open(S,_,[_,_,L,Best|_]/_):-
        arg(4,S,_/Evalfn),
        Evalfn = laplace, !,
        arg(27,S,Eq),
        (Eq = true -> MaxLength is L; MaxLength is L - 1),
        MinPos is (Best/(1-Best))-1,
        remove1(MaxLength,MinPos).
prune_open(S,_,[_,_,L,Best|_]/_):-
        arg(4,S,_/Evalfn),
        (Evalfn = auto_m; Evalfn = mestimate), !,
        arg(27,S,Eq),
        (Eq = true -> MaxLength is L; MaxLength is L - 1),
	setting(prior,Prior),
        ((Evalfn = mestimate,setting(m,M)) ->
            MinPos is M*(Best-Prior)/(1-Best);
            MinPos is ((Best-Prior)/(1-Best))^2),
        remove1(MaxLength,MinPos).
prune_open(_,_,_).


remove1(MaxLength,Best):-
        recorded(gains,gain(_,[P,_,L1|_],_Node1),DbRef),
        (P < Best; (P=Best,L1 >= MaxLength)),
        erase(DbRef),
        fail.
remove1(_,_).

remove2(S,Best):-
	arg(4,S,_/Evalfn),
        recorded(gains,gain(_,[P,_,L|_],_Node1),DbRef),
	(Evalfn = posonly ->
		arg(20,S,RSize),
		Max is log(P) + log(RSize+2.0) - (L+1)/P;
		Max is P - L + 1),
	Best >= Max,
        erase(DbRef),
        fail.
remove2(_,_).

get_nextbest(Node):-
        recorded(openlist,[Gain|_T],_DbRef),
        recorded(gains,gain(Gain,_,Node),DbRef1), !,
        erase(DbRef1),
        recordz(search,nextnode(Node),_).
get_nextbest(Node):-
        recorded(openlist,[_|T],DbRef),
        erase(DbRef),
        recorda(openlist,T,_),
        get_nextbest(Node), !.
get_nextbest(none).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% P R O V E

% progol_prove with caching
% if entry exists in cache, then return it
% otherwise find and cache cover 
% if ``exact'' flag is set then only check proof for examples
% in the part left over due to lazy theorem-proving

progol_prove_cache(exact,S,Type,Entry,Clause,Intervals,IList,Count):-
	!,
	(Intervals = Exact/Left ->
        	%arg(14,S,Depth),
        	%progol_prove(_Depth,Type,Clause,Left,IList1,Count1),
	        progol_prove2(Left,Type,Clause,0,IList1,Count1),
		append(Exact,IList1,IList),
		interval_count(Exact,Count0),
		Count is Count0 + Count1;
		IList = Intervals,
		interval_count(IList,Count)),
        arg(8,S,Caching),
        (Caching = true -> add_cache(Entry,Type,IList); true).
progol_prove_cache(upper,S,Type,Entry,Clause,Intervals,IList,Count):-
        arg(8,S,Caching),
        Caching = true, !,
        %arg(14,S,Depth),
        (check_cache(Entry,Type,Cached)->
                progol_prove_cached(S,Type,Entry,Cached,Clause,Intervals,IList,Count);
                progol_prove_intervals(_Depth,Type,Clause,Intervals,IList,Count),
                add_cache(Entry,Type,IList)).
progol_prove_cache(upper,_S,Type,_,Clause,Intervals,IList,Count):-
        %arg(14,S,Depth),
	(Intervals = Exact/Left ->
		append(Exact,Left,IList1),
        	%progol_prove(_Depth,Type,Clause,IList1,IList,Count);
		progol_prove2(IList1,Type,Clause,0,IList,Count);
        	%progol_prove(_Depth,Type,Clause,Intervals,IList,Count)).
		progol_prove2(Intervals,Type,Clause,0,IList,Count)).

%change this <-
progol_prove_intervals(_Depth,Type,Clause,I1/Left,IList,Count):- 
	!,
	append(I1,Left,Intervals),
	%progol_prove(_Depth,Type,Clause,Intervals,IList,Count).
	progol_prove2(Intervals,Type,Clause,0,IList,Count).
progol_prove_intervals(_Depth,Type,Clause,Intervals,IList,Count):- 
	%progol_prove(_Depth,Type,Clause,Intervals,IList,Count).
	progol_prove2(Intervals,Type,Clause,0,IList,Count).

progol_prove_cached(S,Type,Entry,I1/Left,Clause,Intervals,IList,Count):-
        !,
        %arg(14,S,Depth),
        %progol_prove(_Depth,Type,Clause,Left,I2,_),
        progol_prove2(Left,Type,Clause,0,I2,_),
	append(I1,I2,I),
        (Type = pos ->
                arg(5,S,Greedy),
                (Greedy = true ->
                        intervals_intersection(I,Intervals,IList);
                        IList = I);
                IList = I),
        interval_count(IList,Count),
        update_cache(Entry,Type,IList).
progol_prove_cached(S,Type,Entry,I1,_,Intervals,IList,Count):-
	(Type = pos -> arg(5,S,Greedy),
		(Greedy = true ->
			intervals_intersection(I1,Intervals,IList);
			IList = I1);
		IList = I1),
	interval_count(IList,Count),
	update_cache(Entry,Type,IList).

% progol_prove at most Max atoms
progol_prove_cache(exact,S,Type,Entry,Clause,Intervals,Max,IList,Count):-
	!,
	(Intervals = Exact/Left ->
		interval_count(Exact,Count0),
		Max1 is Max - Count0,
        	arg(12,S,LNegs),
        	%arg(14,S,Depth),
        	%progol_prove(LNegs/false,Depth,Type,Clause,Left,Max1,IList1,Count1),
		progol_prove2(Left,LNegs/false,Type,Clause,Max1,0,IList1,Count1),
		append(Exact,IList1,Exact1),
		find_lazy_left(S,Type,Exact1,Left1),
		IList = Exact1/Left1,
		Count is Count0 + Count1;
		IList = Intervals,
		interval_count(Intervals,Count)),
        arg(8,S,Caching),
        (Caching = true -> add_cache(Entry,Type,IList); true).
progol_prove_cache(upper,S,Type,Entry,Clause,Intervals,Max,IList,Count):-
        arg(8,S,Caching),
        Caching = true, !,
        (check_cache(Entry,Type,Cached)->
                progol_prove_cached(S,Type,Entry,Cached,Clause,Intervals,Max,IList,Count);
                (progol_prove_intervals(S,Type,Clause,Intervals,Max,IList1,Count)->
                        find_lazy_left(S,Type,IList1,Left1),
                        add_cache(Entry,Type,IList1/Left1),
			IList = IList1/Left1,
                        retractall(progol_dyn,example_cache(_));
                        collect_example_cache(IList),
                        add_cache(Entry,Type,IList),
                        fail)).
progol_prove_cache(upper,S,Type,_,Clause,Intervals,Max,IList/Left1,Count):-
        arg(8,S,Caching),
        arg(12,S,LNegs),
        %arg(14,S,Depth),
	(Intervals = Exact/Left ->
		append(Exact,Left,IList1),
        	%progol_prove(LNegs/Caching,Depth,Type,Clause,IList1,Max,IList,Count);
		progol_prove2(IList1,LNegs/Caching,Type,Clause,Max,0,IList,Count);
        	%progol_prove(LNegs/Caching,Depth,Type,Clause,Intervals,Max,IList,Count)),
		progol_prove2(Intervals,LNegs/Caching,Type,Clause,Max,0,IList,Count)),
	find_lazy_left(S,Type,IList,Left1).


progol_prove_intervals(S,Type,Clause,I1/Left,Max,IList,Count):-
        !,
        arg(8,S,Caching),
        arg(12,S,LNegs),
        %arg(14,S,Depth),
        append(I1,Left,Intervals),
        %progol_prove(LNegs/Caching,_Depth,Type,Clause,Intervals,Max,IList,Count).
	progol_prove2(Intervals,LNegs/Caching,Type,Clause,Max,0,IList,Count).
progol_prove_intervals(S,Type,Clause,Intervals,Max,IList,Count):-
        arg(8,S,Caching),
        arg(12,S,LNegs),
        %arg(14,S,Depth),
        %progol_prove(LNegs/Caching,_Depth,Type,Clause,Intervals,Max,IList,Count).
	progol_prove2(Intervals,LNegs/Caching,Type,Clause,Max,0,IList,Count).


progol_prove_cached(S,Type,Entry, I1/Left,Clause,_,Max,IList/Left1,Count):-
        !,
        arg(8,S,Caching),
        arg(12,S,LNegs),
        %arg(14,S,Depth),
        interval_count(I1,C1),
        Max1 is Max - C1,
        Max1 >= 0,
        %(progol_prove(LNegs/Caching,_Depth,Type,Clause,Left,Max1,I2,C2)->
	(progol_prove2(Left,LNegs/Caching,Type,Clause,Max1,0,I2,C2)->
                append(I1,I2,IList),
                Count is C2 + C1,
                find_lazy_left(S,Type,IList,Left1),
                update_cache(Entry,Type,IList/Left1),
                retractall(progol_dyn,example_cache(_));
                collect_example_cache(I2/Left1),
                append(I1,I2,IList),
                update_cache(Entry,Type,IList/Left1),
                fail).
progol_prove_cached(_,neg,_, I1/L1,_,_,_,I1/L1,C1):-
	!,
	interval_count(I1,C1).
progol_prove_cached(S,_,_,I1,_,_,Max,I1,C1):-
	interval_count(I1,C1),
	arg(12,S,LNegs),
	(LNegs = true ->true; C1 =< Max).

collect_example_cache(Intervals/Left):-
	recorded(progol_dyn,example_cache([Last|Rest]),DbRef), 
	erase(DbRef),
	reverse([Last|Rest],IList),
	list_to_intervals1(IList,Intervals),
	Next is Last + 1,
	recorded(progol,size(neg,LastN),_),
	(Next > LastN -> Left = []; Left = [Next-LastN]).

find_lazy_left(S,_,_,[]):-
        arg(12,S,LazyNegs),
        LazyNegs = false, !.
find_lazy_left(_,_,[],[]).
find_lazy_left(S,Type,[_-F],Left):-
        !,
        F1 is F + 1,
	(Type = pos -> arg(16,S,Last);
		(Type = neg -> arg(24,S,Last);
			(Type = rand -> arg(20,S,Last); Last = F))),
        (F1 > Last -> Left = []; Left = [F1-Last]).
find_lazy_left(S,Type,[_|T1],Left):-
        find_lazy_left(S,Type,T1,Left).


% progol_prove atoms specified by Type and index set using Clause.
% dependent on data structure used for index set:
% currently index set is a list of intervals
% return atoms progol_proved and their count
% added depth argument from Progol 2.4 onwards

progol_prove(_,_,_,[],[],0).
progol_prove(_Depth,Type,Clause,[Interval|Intervals],IList,Count):-
	index_progol_prove(_Depth,Type,Clause,Interval,I1,C1),
	progol_prove(_Depth,Type,Clause,Intervals,I2,C2),
	append(I1,I2,IList),
	Count is C1 + C2.

%no depth for Sicstus
%when we know that Sofar is a variable.
progol_prove2([],_,_,Count,[],Count).
progol_prove2([Current-Finish|Intervals],Type,(Head:-Body),InCount,Sofar,OutCount) :-
	\+ ((example(Current,Type,Head),Body)), %uncovered
	!,
	(Current>=Finish ->
	    progol_prove2(Intervals,Type,(Head:-Body),InCount,Sofar,OutCount);
	    Next is Current+1,!,
	    progol_prove2([Next-Finish|Intervals],Type,(Head:-Body),InCount,Sofar,OutCount)
	).
progol_prove2([Current-Finish|Intervals],Type,Clause,InCount,Sofar,OutCount) :-
	(Current>=Finish ->
	    Sofar=[Current-Current|Rest],
	    MidCount is InCount+1,!,
	    progol_prove2(Intervals,Type,Clause,MidCount,Rest,OutCount);
	    Next is Current+1,
	    Sofar=[Current-_Last|_Rest],!,
	    progol_prove3([Next-Finish|Intervals],Type,Clause,InCount,Sofar,OutCount)
	).


%when Sofar is not a variable
progol_prove3([Current-Finish|Intervals],Type,(Head:-Body),InCount,Sofar,OutCount) :-
	\+ ((example(Current,Type,Head),Body)), %uncovered
	!,
	Last is Current-1, %found some previously
	Sofar=[Start-Last|Rest], %complete found interval
	MidCount is InCount+Current-Start,
	(Current>=Finish ->
	    progol_prove2(Intervals,Type,(Head:-Body),MidCount,Rest,OutCount);
	    Next is Current+1,!,
	    progol_prove2([Next-Finish|Intervals],Type,(Head:-Body),MidCount,Rest,OutCount)
	).
progol_prove3([Current-Finish|Intervals],Type,Clause,InCount,Sofar,OutCount) :-
	(Current>=Finish ->
	    Sofar=[Start-Finish|Rest],
	    MidCount is InCount+Finish-Start+1,!,
	    progol_prove2(Intervals,Type,Clause,MidCount,Rest,OutCount);
	    Next is Current+1,!,
	    progol_prove3([Next-Finish|Intervals],Type,Clause,InCount,Sofar,OutCount)
	).

index_progol_prove(_,_,_,Start-Finish,[],0):-
	Start > Finish, !.
index_progol_prove(_Depth,Type,Clause,Start-Finish,IList,Count):-
	index_progol_prove1(_Depth,Type,Clause,Start,Finish,Last),
	Last0 is Last - 1 ,
	Last1 is Last + 1,
	(Last0 >= Start->
		index_progol_prove(_Depth,Type,Clause,Last1-Finish,Rest,Count1),
		IList = [Start-Last0|Rest],
		Count is Last - Start + Count1;
		index_progol_prove(_Depth,Type,Clause,Last1-Finish,IList,Count)).

progol_prove1(G):-
	%depth_bound_call(G), !.
	G, !.
progol_prove2([],_,_,_,_,Count,[],Count).
progol_prove2([Current-Finish|Intervals],Flags,Type,(Head:-Body),Max,InCount,Sofar,OutCount) :-
		\+ ((example(Current,Type,Head),Body)), %uncovered
	!,
	(Current>=Finish ->
	    progol_prove2(Intervals,Flags,Type,(Head:-Body),Max,InCount,Sofar,OutCount);
	    Next is Current+1,!,
	    progol_prove2([Next-Finish|Intervals],Flags,Type,(Head:-Body),Max,InCount,Sofar,OutCount)
	).
progol_prove2([Current-Finish|Intervals],LNegs/Caching,Type,Clause,Max,InCount,Sofar,OutCount) :-
	MidCount is InCount+1,             %progol_proved Current
	(LNegs==false -> MidCount=<Max),  %progol_proved too many?
	((LNegs==true,MidCount==Max) ->   %lazily progol_proved as many as required ..
	    OutCount=MidCount,
	    Sofar=[Current-Current];
	    (Caching == true ->
		(recorded(progol_dyn,example_cache(L),DbRef) ->
		    erase(DbRef),
		    recorda(progol_dyn,example_cache([Current-Current|L]),_);
		    recorda(progol_dyn,example_cache([Current-Current]),_)
		) 
	    ),
	    Max1 is Max-1,
	    (Current>=Finish ->
		Sofar=[Current-Current|Rest],!,
		progol_prove2(Intervals,LNegs/Caching,Type,Clause,Max1,MidCount,Rest,OutCount);
		Next is Current+1,
		Sofar=[Current-_Last|_Rest],!,
		progol_prove3([Next-Finish|Intervals],LNegs/Caching,Type,Clause,Max1,MidCount,Sofar,OutCount)
	    )
	).
    
progol_prove3([Current-Finish|Intervals],Flags,Type,(Head:-Body),Max,InCount,Sofar,OutCount) :-
	\+ ((example(Current,Type,Head),Body)), %uncovered
	!,
	Last is Current-1, %found some previously
	Sofar=[_Start-Last|Rest], %construct
	(Current>=Finish ->
	    progol_prove2(Intervals,Flags,Type,(Head:-Body),Max,InCount,Rest,OutCount);
	    Next is Current+1,!,
	    progol_prove2([Next-Finish|Intervals],Flags,Type,(Head:-Body),Max,InCount,Rest,OutCount)
	).
progol_prove3([Current-Finish|Intervals],LNegs/Caching,Type,Clause,Max,InCount,Sofar,OutCount) :-
	MidCount is InCount+1,             %progol_proved Current
	(LNegs==false -> MidCount=<Max),  %progol_proved too many?
	((LNegs==true,MidCount==Max) ->   %lazily progol_proved as many as required ..
		OutCount=MidCount,
		Sofar=[_Start-Current];    %so stop
		(Caching == true ->
		    (recorded(progol_dyn,example_cache(L),DbRef) ->
			erase(DbRef),
			recorda(progol_dyn,example_cache([Current-Current|L]),_);
			recorda(progol_dyn,example_cache([Current-Current]),_)
		    ) 
		),
		Max1 is Max-1,
		(Current>=Finish ->
		    Sofar=[_Start-Finish|Rest],!,
		    progol_prove2(Intervals,LNegs/Caching,Type,Clause,Max1,MidCount,Rest,OutCount);
		    Next is Current+1,!,
		    progol_prove3([Next-Finish|Intervals],LNegs/Caching,Type,Clause,Max1,MidCount,Sofar,OutCount)
		)
	    ).


	
index_progol_prove1(_,_,_,Num,Last,Num):-
	Num > Last, !.
index_progol_prove1(_Depth,Type,(Head:-Body),Num,Finish,Last):-
	%\+((\+((example(Num,Type,Head),depth_bound_call(Body,Depth))))), !,
	\+((\+((example(Num,Type,Head),Body)))), !,
	Num1 is Num + 1,
	index_progol_prove1(_Depth,Type,(Head:-Body),Num1,Finish,Last).
index_progol_prove1(_,_,_,Last,_,Last).



% progol_proves at most Max atoms using Clause.

progol_prove(_,_,_,_,[],_,[],0).
progol_prove(Flags,_Depth,Type,Clause,[Interval|Intervals],Max,IList,Count):-
        index_progol_prove(Flags,_Depth,Type,Clause,Interval,Max,I1,C1), !,
        Max1 is Max - C1,
        progol_prove(Flags,_Depth,Type,Clause,Intervals,Max1,I2,C2),
        append(I1,I2,IList),
        Count is C1 + C2.


index_progol_prove(_,_,_,_,Start-Finish,_,[],0):-
        Start > Finish, !.
index_progol_prove(Flags,_Depth,Type,Clause,Start-Finish,Max,IList,Count):-
        index_progol_prove1(Flags,_Depth,Type,Clause,Start,Finish,0,Max,Last),
        Last0 is Last - 1 ,
        Last1 is Last + 1,
        (Last0 >= Start->
                Max1 is Max - Last + Start,
		((Max1 = 0, Flags = true/_) ->
                        Rest = [], Count1 = 0;
                	index_progol_prove(Flags,_Depth,Type,Clause,Last1-Finish,
					Max1,Rest,Count1)),
                IList = [Start-Last0|Rest],
                Count is Last - Start + Count1;
                index_progol_prove(Flags,_Depth,Type,Clause,Last1-Finish,Max,IList,Count)).

index_progol_prove1(false/_,_,_,_,_,_,Proved,Allowed,_):-
        Proved > Allowed, !, fail.
index_progol_prove1(_,_,_,_,Num,Last,_,_,Num):-
        Num > Last, !.
index_progol_prove1(true/_,_,_,_,Num,_,Allowed,Allowed,Num):- !.
index_progol_prove1(LNegs/Caching,_Depth,Type,(Head:-Body),Num,Finish,Proved,Allowed,Last):-
        %\+((\+((example(Num,Type,Head),depth_bound_call(Body,Depth))))), !,
	\+((\+((example(Num,Type,Head),Body)))), !,
        Num1 is Num + 1,
        Proved1 is Proved + 1,
        (Caching = true ->
                (recorded(progol_dyn,example_cache(L),DbRef)->
                        erase(DbRef),
                        recorda(progol_dyn,example_cache([Num-Num|L]),_);
                        recorda(progol_dyn,example_cache([Num-Num]),_));
                true),
        index_progol_prove1(LNegs/Caching,_Depth,Type,(Head:-Body),Num1,Finish,Proved1,Allowed,Last).
index_progol_prove1(_,_,_,_,Last,_,_,_,Last).

% general progol_prove at least Min atoms using Clause.
progol_prove_at_least(Type,Clause,Min,Cover,C):-
        split_clause(Clause,Head,Body),
        recordz(pclause,pclause(Head,Body),DbRef),
        recorded(progol,atoms(Type,Atoms),_),
        %recorded(progol,set(depth,Depth),_),
        %progol_prove(_Depth,Type,(Head:-Body),Atoms,Cover,C),
	progol_prove2(Atoms,Type,(Head:-Body),0,Cover,C),
        erase(DbRef),
        C >= Min.

% general progol_prove at most Max atoms using Clause.
progol_prove_at_most(Type,Clause,Max,Cover,C):-
        split_clause(Clause,Head,Body),
        recordz(pclause,pclause(Head,Body),DbRef),
        recorded(progol,atoms(Type,Atoms),_),
        N1 is Max + 1,
        %recorded(progol,set(depth,Depth),_),
        progol_prove(_Depth,Type,(Head:-Body),Atoms,N1,Cover,C),
        erase(DbRef),
        C =< Max.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C A C H I N G

clear_cache:-
	retractall(cache,_),
	retractall(prune_cache,_).


check_cache(Entry,Type,I):-
	\+ Entry = false,
	recorded(cache,Entry,_), !,
        functor(Entry,_,Arity),
        (Type = pos -> Arg is Arity - 1; Arg is Arity),
        arg(Arg,Entry,I),
        nonvar(I),
	p_message('found in cache').

add_cache(false,_,_):- !.
add_cache(Entry,Type,I):-
        (recorded(cache,Entry,DbRef)-> erase(DbRef); true),
        functor(Entry,_,Arity),
        (Type = pos -> Arg is Arity - 1; Arg is Arity),
        (arg(Arg,Entry,I)-> recorda(cache,Entry,_);
                        true), !.


update_cache(Entry,Type,I):-
	\+ Entry = false,
	functor(Entry,Name,Arity),
	(Type = pos -> Arg is Arity - 1; Arg is Arity),
	arg(Arg,Entry,OldI),
	OldI = _/_,
	recorded(cache,Entry,DbRef), 
	erase(DbRef),
	functor(NewEntry,Name,Arity),
	Arity1 is Arity - 1,
	copy_args(Entry,NewEntry,1,Arity1),
	arg(Arity,NewEntry,I), 
	recorda(cache,NewEntry,_), !.
update_cache(_,_,_).

	
add_prune_cache(false):- !.
add_prune_cache(Entry):-
	(recorded(progol,set(caching,true),_)->
		functor(Entry,_,Arity),
		A1 is Arity - 2,
		arg(A1,Entry,Clause),
		recorda(prune_cache,prune(Clause),_);
		true).

get_cache_entry(Max,Clause,Entry):-
        skolemize(Clause,Head,Body,_),
	length(Body,L1),
	Max >= L1 + 1,
	hash_term([Head|Body],Entry), !.
get_cache_entry(_,_,false).

% upto 3-argument indexing using predicate names in a clause
%hash_term([L0,L1,L2,L3,L4|T],Entry):-
%        !,
%        functor(L1,P1,_), functor(L2,P2,_),
%        functor(L3,P3,_), functor(L4,P4,_),
%        functor(Entry,P4,6),
%        arg(1,Entry,P2), arg(2,Entry,P3),
%        arg(3,Entry,P1), arg(4,Entry,[L0,L1,L2,L3,L4|T]).
%hash_term([L0,L1,L2,L3],Entry):-
%        !,
%        functor(L1,P1,_), functor(L2,P2,_),
%        functor(L3,P3,_),
%        functor(Entry,P3,5),
%        arg(1,Entry,P2), arg(2,Entry,P1),
%        arg(3,Entry,[L0,L1,L2,L3]).
%hash_term([L0,L1,L2],Entry):-
%        !,
%        functor(L1,P1,_), functor(L2,P2,_),
%        functor(Entry,P2,4),
%        arg(1,Entry,P1), arg(2,Entry,[L0,L1,L2]).
%hash_term([L0,L1],Entry):-
%        !,
%        functor(L1,P1,_),
%        functor(Entry,P1,3),
%        arg(1,Entry,[L0,L1]).

hash_term([L0,L1|T],Entry):-
	functor(L1,P1,_),
	functor(Entry,P1,4),
	term_hash([L1|T],HashInt),
	arg(1,Entry,HashInt),
	arg(2,Entry,[L0,L1|T]).
	
hash_term([L0],Entry):-
        functor(L0,P0,_),
        functor(Entry,P0,3),
        arg(1,Entry,[L0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C O M M A N D S

sat(Num):-
	example(Num,pos,_),
	sat(pos,Num), !.

sat(Type,Num):-
        setting(construct_bottom,false), !,
        sat_prelims,
        recorda(sat,sat(Num,Type),_).
sat(Type,Num):-
	set(stage,saturation),
	sat_prelims,
	example(Num,Type,Example),
	p1_message('sat'), p_message(Num), p_message(Example),
	record_sat_example(Num),
	recorda(sat,sat(Num,Type),_),
	split_args(Example,Input,Output,Constants),
	integrate_args(0,Example,Output),
	\+ recorded(progol,set(lazy_bottom,true),_), !,
	%StartClock is cputime,
	sicstus_statistics(runtime,[StartClock,_]),
	recordz(atoms,Example,_),
	recorded(progol,set(i,Ival),_),
	flatten(0,Ival,Output/Input/Constants,0,Last1),
	recorded(lits,lit_info(1,_,Atom,_,_,_),_),
	get_vars(Atom,Output,HeadOVars),
	recorda(sat,head_ovars(HeadOVars),_),
	functor(Example,Name,Arity), 
	get_determs(Name/Arity,L),
	(recorded(progol,determination(Name/Arity,'='/2),_)->
		recorda(sat,set(eq,true),_);
		true),
	update_modetypes(L,L1),
	get_atoms(L1,1,Ival,Last1,Last),
	%StopClock is cputime,
	sicstus_statistics(runtime,[StopClock,_]),
	Time is StopClock - StartClock,
	recorda(sat,last_lit(Last),_),
	recorda(sat,bot_size(Last),_),
	rm_moderepeats(Last,Repeats),
	rm_uselesslits(Last,NotConnected),
	rm_commutative(Last,Commutative),
	rm_symmetric(Last,Symmetric),
	get_implied,
	TotalLiterals is Last - Repeats - NotConnected - Commutative - Symmetric,
	show(bottom),
	p1_message('literals'), p_message(TotalLiterals),
	p1_message('saturation time'), p_message(Time),
	noset(stage).
sat(_,_):-
	noset(stage).

reduce:-
	setting(search,Search), 
	reduce(Search), !.

% iterative beam search as described in Quinlan + Cameron-Jones, IJCAI-95
reduce(ibs):-
	!,
	retractall(ibs,_),
	setting(evalfn,Evalfn),
	store(search),
	store(openlist),
	store(caching),
	store(explore),
	set(openlist,1),
	set(explore,true),
	set(caching,true),
	set(search,bf),
	recorda(ibs,rval(1.0),_),
	recorda(ibs,nodes(0),_),
	recorded(sat,sat(Num,Type),_),
	example(Num,Type,Example),
	get_start_label(Evalfn,Label),
	recorda(ibs,selected(Label,(Example:-true),[Num-Num],[]),_),
	Start is cputime,
	repeat,
	setting(openlist,OldOpen),
	p1_message('ibs beam width'), p_message(OldOpen),
	reduce(bf),
	recorded(search,current(_,Nodes0,[PC,NC|_]/_),_),
	N is NC + PC,
	estimate_error_rate(Nodes0,0.5,N,NC,NewR),
	p1_message('ibs estimated error'), p_message(NewR),
	recorded(ibs,rval(OldR),DbRef1),
	recorded(ibs,nodes(Nodes1),DbRef2),
        recorded(search,selected(BL,RCl,PCov,NCov),_),
	erase(DbRef1),
	erase(DbRef2),
	NewOpen is 2*OldOpen,
	Nodes2 is Nodes0 + Nodes1,
	set(openlist,NewOpen),
	recorda(ibs,rval(NewR),_),
	recorda(ibs,nodes(Nodes2),_),
	((NewR >= OldR ; NewOpen > 512)-> true;
		recorded(ibs,selected(_,_,_,_),DbRef3),
		erase(DbRef3),
		recorda(ibs,selected(BL,RCl,PCov,NCov),_),
		fail),
	!,
	Stop is cputime,
	Time is Stop - Start,
	recorded(ibs,nodes(Nodes),_),
        recorded(ibs,selected(BestLabel,RClause,PCover,NCover),_),
	add_hyp(BestLabel,RClause,PCover,NCover),
	p1_message('ibs clauses constructed'), p_message(Nodes),
	p1_message('ibs search time'), p_message(Time),
	p_message('ibs best clause'),
	pp_dclause(RClause),
	show_stats(Evalfn,BestLabel),
	record_search_stats(RClause,Nodes,Time),
	reinstate(search),
	reinstate(openlist),
	reinstate(caching),
	reinstate(explore).

% iterative language search as developed by Camacho, 1996
reduce(ils):-
	retractall(ils,_),
	setting(evalfn,Evalfn),
	store(search),
	store(caching),
	store(language),
	set(language,1),
	set(search,bf),
	set(caching,true),
	recorda(ils,nodes(0),_),
	recorded(sat,sat(Num,Type),_),
	example(Num,Type,Example),
	get_start_label(Evalfn,Label),
	recorda(ils,selected(Label,(Example:-true),[Num-Num],[]),_),
	Start is cputime,
	repeat,
	setting(language,OldLang),
	p1_message('ils language setting'), p_message(OldLang),
	reduce(bf),
	recorded(search,current(_,Nodes0,_),_),
	recorded(ils,nodes(Nodes1),DbRef1),
        recorded(search,selected([P,N,L,F|T],RCl,PCov,NCov),_),
	recorded(ils,selected([_,_,_,F1|_],_,_,_),DbRef2),
	erase(DbRef1),
	NewLang is OldLang + 1,
	Nodes2 is Nodes0 + Nodes1,
	set(language,NewLang),
	recorda(ils,nodes(Nodes2),_),
	(F1 >= F -> true;
		erase(DbRef2),
		recorda(ils,selected([P,N,L,F|T],RCl,PCov,NCov),_),
		set(best,[P,N,L,F|T]),
		fail),
	!,
	Stop is cputime,
	Time is Stop - Start,
	recorded(ils,nodes(Nodes),_),
        recorded(ils,selected(BestLabel,RClause,PCover,NCover),_),
	add_hyp(BestLabel,RClause,PCover,NCover),
	p1_message('ils clauses constructed'), p_message(Nodes),
	p1_message('ils search time'), p_message(Time),
	p_message('ils best clause'),
	pp_dclause(RClause),
	show_stats(Evalfn,BestLabel),
	record_search_stats(RClause,Nodes,Time),
	reinstate(search),
	reinstate(language),
	reinstate(caching).

reduce(_):-
	set(stage,reduction),
	p_message('reduce'),
	reduce_prelims(L,P,N),
	recorda(openlist,[],_),
	get_search_settings(S),
	arg(4,S,_Search/Evalfn),
	get_start_label(Evalfn,Label),
	recorded(sat,sat(Num,Type),_),
	example(Num,Type,Example),
	recorda(search,selected(Label,(Example:-true),[Num-Num],[]),_),
	arg(13,S,MinPos),
	interval_count(P,PosLeft),
	PosLeft >= MinPos, !,
	add_hyp(Label,(Example:-true),[Num-Num],[]),
        (recorded(progol,max_set(Type,Num,Label1,ClauseNum),_)->
		BestSoFar = Label1/ClauseNum;
		(recorded(progol,set(best,Label2),_)->
			BestSoFar = Label2/0;
			BestSoFar = Label/0)),
        recorda(search,best_label(BestSoFar),_),
	p1_message('best label so far'), p_message(BestSoFar),
        arg(3,S,RefineOp),
	%StartClock is cputime,
	sicstus_statistics(runtime,[StartClock,_]),
        (RefineOp = true ->
		clear_cache,
		interval_count(P,MaxPC),
		recorda(progol_dyn,max_head_count(MaxPC),_),
                get_gains(S,0,BestSoFar,[],false,[],0,L,[[]],P,N,[],1,Last,NextBest);
                get_gains(S,0,BestSoFar,[],false,[],0,L,[1],P,N,[],1,Last,NextBest),
		update_max_head_count(0,Last)),
		recorda(nodes,expansion(1,1,Last),_),
	get_nextbest(_),
	recorda(search,current(1,Last,NextBest),_),
	search(S,Nodes),
	%StopClock is cputime,
	sicstus_statistics(runtime,[StopClock,_]),
	Time is StopClock - StartClock,
        recorded(search,selected(BestLabel,RClause,PCover,NCover),_),
	recorded(openlist,_,DbRef),
	erase(DbRef),
	add_hyp(BestLabel,RClause,PCover,NCover),
	p1_message('clauses constructed'), p_message(Nodes),
	p1_message('search time'), p_message(Time),
	p_message('best clause'),
	pp_dclause(RClause),
	show_stats(Evalfn,BestLabel),
	record_search_stats(RClause,Nodes,Time),
	noset(stage),
	!.
reduce(_):-
        recorded(search,selected(BestLabel,RClause,PCover,NCover),_),
	recorded(openlist,_,DbRef),
	erase(DbRef),
	add_hyp(BestLabel,RClause,PCover,NCover),
	p_message('best clause'),
	pp_dclause(RClause),
	(setting(evalfn,Evalfn) -> true; Evalfn = coverage),
	show_stats(Evalfn,BestLabel),
	noset(stage),
	!.

estimate_error_rate(H,Del,N,E,R):-
	TargetProb is 1-exp(log(1-Del)/H),
	estimate_error(1.0/0.0,0.0/1.0,TargetProb,N,E,R).

estimate_error(L/P1,U/P2,P,N,E,R):-
	M is (L+U)/2,
	binom_lte(N,M,E,P3),
	ADiff is abs(P - P3),
	(ADiff < 0.001 ->
		R is M;
		(P3 > P ->
			estimate_error(L/P1,M/P3,P,N,E,R);
			estimate_error(M/P3,U/P2,P,N,E,R)
		)
	).
		
		
	

sat_prelims:-
	clean_up_sat,
	clean_up_reduce,
	clear_hyp,
	reset_counts,
	set_up_builtins.

reduce_prelims(L,P,N):-
	clean_up_reduce,
	check_posonly(_),   
	check_auto_refine,
	(recorded(sat,last_lit(L),_)-> true;
		L = 0, recorda(sat,last_lit(L),_)),
	(recorded(sat,bot_size(B),_)-> true;
		B = 0, recorda(sat,bot_size(B),_)),
        ((recorded(progol,lazy_evaluate(_),_);setting(greedy,true))->
                recorded(progol,atoms_left(pos,P),_);
                recorded(progol,atoms(pos,P),_)),
	recorded(progol,size(pos,PSize),_),
	set(evalfn,E),
	(E = posonly -> NType = rand; NType = neg),
	recorded(progol,size(NType,NSize),_),
	recorded(progol,atoms_left(NType,N),_),
	fix_base(PSize,NSize).

set_up_builtins:-
	recorda(lits,lit_info(-1,0,'!',[],[],[]),_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C O N T R O L

% this finds the unique max cover set solution
induce_max:-
	recorded(progol,atoms(pos,PosSet),_),
	\+ PosSet = [],
	%StartClock is cputime,
	sicstus_statistics(runtime,[StartClock,_]),
	set(maxcover,true),
	induce_max(PosSet),
	%StopClock is cputime,
	sicstus_statistics(runtime,[StopClock,_]),
	Time is StopClock - StartClock,
	show(theory),
	record_theory(Time),
	noset(maxcover),
	p1_message('time taken'), p_message(Time), !.
induce_max.

induce_max([]).
induce_max([Start-Finish|Intervals]):-
	recorda(progol_dyn,counter(Start),_),
	induce_max1(Finish),
	induce_max(Intervals).

induce_max1(Finish):-
        recorded(progol_dyn,counter(S),_),
        S =< Finish, !,
        repeat,
        recorded(progol_dyn,counter(Start),DbRef),
        erase(DbRef),
        recorda(progol,example_selected(pos,Start),DbRef1),
        sat(Start),
        reduce,
        update_coverset(pos,Start),
        erase(DbRef1),
        Next is Start+1,
        recordz(progol_dyn,counter(Next),DbRef2),
        Next > Finish, !,
        erase(DbRef2).
induce_max1(_).

% this implements induction by random sampling
% does not perform greedy cover removal after each reduction
induce_cover:-
	recorded(progol,atoms_left(pos,PosSet),_),
	\+ PosSet = [],
	setting(samplesize,S),
	%StartClock is cputime,
	sicstus_statistics(runtime,[StartClock,_]),
        repeat,
	gen_sample(pos,S),
	recorda(progol,besthyp([-10000,0,1,-10000],0,(false),[],[]),_),
	get_besthyp,
	rm_seeds,
        recorded(progol,atoms_left(pos,[]),_),
	%StopClock is cputime,
	sicstus_statistics(runtime,[StopClock,_]),
	Time is StopClock - StartClock,
	show(theory), 
	record_theory(Time),
	p1_message('time taken'), p_message(Time), !.
induce_cover.

% this implements induction by random sampling
% performs greedy cover removal after each reduction
induce:-
        set(greedy,true),
        recorded(progol,atoms_left(pos,PosSet),_),
        \+ PosSet = [],
	setting(samplesize,S),
        %StartClock is cputime,
	sicstus_statistics(runtime,[StartClock,_]),
        repeat,
        gen_sample(pos,S),
	retractall(progol,besthyp(_,_,_,_,_)),
	recorda(progol,besthyp([-10000,0,1,-10000],0,(false),[],[]),_),
        get_besthyp,
        rm_seeds,
        recorded(progol,atoms_left(pos,[]),_),
        %StopClock is cputime,
        sicstus_statistics(runtime,[StopClock,_]),
	Time is StopClock - StartClock,
	show(theory),
        record_theory(Time),
	noset(greedy),
        p1_message('time taken'), p_message(Time), !.
induce.

rsat:-
        recorded(progol,atoms_left(pos,PosSet),_),
        \+ PosSet = [],
        gen_sample(pos,1),
	recorded(progol,example_selected(pos,Num),DbRef),
	erase(DbRef),
	sat(Num).


get_besthyp:-
	recorded(progol,example_selected(pos,Num),DbRef),
	erase(DbRef),
	sat(Num),
	reset_best_label,	 % set-up target to beat
	reduce,
	update_besthyp(Num),
	fail.
get_besthyp:-
        recorded(progol,besthyp(L,Num,H,PC,NC),DbRef),
        erase(DbRef),
	\+ H = false, !,
	((setting(samplesize,S),S>1)->
		set(nodes,Nodes),
		show_clause(L,H,Nodes,sample),
		record_clause(L,H,Nodes,sample);
		true),
        add_hyp(L,H,PC,NC),
        recorda(progol,example_selected(pos,Num),_), !.
get_besthyp.


reset_best_label:-
	recorded(progol,besthyp(Label1,_,Clause,P,N),_),
	recorded(search,best_label(Label/_),DbRef),
	Label = [_,_,_,Gain|_],
	Label1 = [_,_,_,Gain1|_],
	Gain1 > Gain, !,
	erase(DbRef),
	recorda(search,best_label(Label1/0),_),
	recorded(search,selected(_,_,_,_),DbRef2),
	erase(DbRef2),
	recorda(search,selected(Label1,Clause,P,N),_).
reset_best_label.


update_besthyp(Num):-
	recorded(progol,hypothesis(Label,H,PCover,NCover),_),
	recorded(progol,besthyp(Label1,_,_,_,_),DbRef),
	Label = [_,_,_,Gain|_],
	Label1 = [_,_,_,Gain1|_],
	Gain > Gain1, !,
	erase(DbRef),
	recordz(progol,besthyp(Label,Num,H,PCover,NCover),_).
update_besthyp(_).

get_performance:-
	(setting(train_pos,PFile) ->
		test(PFile,noshow,Tp,TotPos),
		Fn is TotPos - Tp;
		TotPos = 0, Tp = 0, Fn = 0),
	(setting(train_neg,NFile) ->
		test(NFile,noshow,Fp,TotNeg),
		Tn is TotNeg - Fp;
		TotNeg = 0, Tn = 0, Fp = 0),
	TotPos + TotNeg > 0,
	p_message('Training set performance'),
	write_cmatrix([Tp,Fp,Fn,Tn]),
	fail.
get_performance:-
	(setting(test_pos,PFile) ->
		test(PFile,noshow,Tp,TotPos),
		Fn is TotPos - Tp;
		TotPos = 0, Tp = 0, Fn = 0),
	(setting(test_neg,NFile) ->
		test(NFile,noshow,Fp,TotNeg),
		Tn is TotNeg - Fp;
		TotNeg = 0, Tn = 0, Fp = 0),
	TotPos + TotNeg > 0,
	p_message('Test set performance'),
	write_cmatrix([Tp,Fp,Fn,Tn]),
	fail.
get_performance.

write_cmatrix([Tp,Fp,Fn,Tn]):-
        P is Tp + Fn, N is Fp + Tn,
        PP is Tp + Fp, PN is Fn + Tn,
        Total is PP + PN,
        (Total = 0 -> Accuracy is 0.5; Accuracy is (Tp + Tn)/Total),
        find_max_width([Tp,Fp,Fn,Tn,P,N,PP,PN,Total],0,W1),
        W is W1 + 2,
        tab(5), write(' '), tab(W), write('Actual'), nl,
        tab(5), write(' '), write_entry(W,'+'), tab(6), write_entry(W,'-'), nl,
        tab(5), write('+'),
        write_entry(W,Tp), tab(6), write_entry(W,Fp), tab(6), write_entry(W,PP), nl,
        write('Pred '), nl,
        tab(5), write('-'),
        write_entry(W,Fn), tab(6), write_entry(W,Tn), tab(6), write_entry(W,PN), nl, nl,
        tab(5), write(' '), write_entry(W,P), tab(6), write_entry(W,N),
        tab(6), write_entry(W,Total), nl, nl,
        write('Accuracy = '), write(Accuracy), nl.

 
find_max_width([],W,W).
find_max_width([V|T],W1,W):-
        name(V,VList),
        length(VList,VL),
        (VL > W1 -> find_max_width(T,VL,W);
                find_max_width(T,W1,W)).
 
write_entry(W,V):-
        name(V,VList),
        length(VList,VL),
        Y is integer((W-VL)/2),
        tab(Y), write(V), tab(Y).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% L A Z Y  E V A L U A T I O N


% lazy evaluation of literals in a refinement operation
lazy_evaluate_refinement(Refinement,LazyPreds,PosCover,NegCover,NewRefinement):-
	\+ LazyPreds = [], !,
	lazy_evaluate_refinement(Refinement,LazyPreds,[],PosCover,NegCover,NewRefinement).
lazy_evaluate_refinement(Refinement,_,_,_,Refinement).

lazy_evaluate_refinement([],_,Refine,_,_,Refine):- !.
lazy_evaluate_refinement([LitNum|Lits],LazyPreds,Path,PosCover,NegCover,Refine):-
	lazy_evaluate([LitNum],LazyPreds,Path,PosCover,NegCover,[LitNum1]), !,
	update(LitNum1,Path,Path1),
	lazy_evaluate_refinement(Lits,LazyPreds,Path1,PosCover,NegCover,Refine), !.


% lazy evaluation of specified literals
% forAll #'d arguments of these literals are evaluated at reduction-time

lazy_evaluate(Lits,[],_,_,_,Lits):- !.
lazy_evaluate([],_,_,_,_,[]):- !.
lazy_evaluate([[Lit|Lits]],_,_,_,_,[[Lit|Lits]]):- !.
lazy_evaluate([LitNum|LitNums],LazyPreds,Path,PosCover,NegCover,Lits):-
	recorded(lits,lit_info(LitNum,Depth,Atom,I,O,D),_),
	functor(Atom,Name,Arity),
	%member1(Name/Arity,LazyPreds), !,
	memberchk(Name/Arity,LazyPreds), !,
	get_pclause([LitNum|Path],[],(Lit:-(Goals)),_,_,_),
	goals_to_clause(Goals,Clause),
	lazy_progol_prove(pos,Lit,Clause,PosCover),
	(recorded(progol,positive_only(Name/Arity),_)->
		lazy_progol_prove(neg,Lit,Clause,[]);
		lazy_progol_prove_negs(Lit,Clause,NegCover)),
	functor(LazyLiteral,Name,Arity),
	collect_args(I,LazyLiteral),
	lazy_evaluate1(Atom,Depth,I,O,D,LazyLiteral,NewLits),
	retractall(progol_dyn,lazy_evaluate(_,_)),
	lazy_evaluate(LitNums,LazyPreds,Path,PosCover,NegCover,NewLits1),
	update_list(NewLits1,NewLits,Lits).
lazy_evaluate([LitNum|LitNums],LazyPreds,Path,PosCover,NegCover,[LitNum|Lits]):-
	lazy_evaluate(LitNums,LazyPreds,Path,PosCover,NegCover,Lits).

lazy_progol_prove_negs(Lit,Clause,_):-
	recorded(progol,set(lazy_negs,true),_), !,
	recorded(progol,atoms(neg,NegCover),_),
	lazy_progol_prove(neg,Lit,Clause,NegCover).
lazy_progol_prove_negs(Lit,Clause,NegCover):-
	lazy_progol_prove(neg,Lit,Clause,NegCover).

collect_args([],_).
collect_args([Argno/_|Args],Literal):-
	findall(Term,(recorded(progol_dyn,lazy_evaluate(pos,Lit),_),arg(Argno,Lit,Term)),PTerms),
	findall(Term,(recorded(progol_dyn,lazy_evaluate(neg,Lit),_),arg(Argno,Lit,Term)),NTerms),
	arg(Argno,Literal,[PTerms,NTerms]),
	collect_args(Args,Literal).

lazy_evaluate1(Atom,Depth,I,O,D,Lit,NewLits):-
	recorded(sat,last_lit(_),_),
	call_library_pred(Atom,Depth,Lit,I,O,D),
	findall(LitNum,(recorded(progol_dyn,lazy_evaluated(LitNum),DbRef),erase(DbRef)),NewLits).

call_library_pred(OldLit,Depth,Lit,I,O,D):-
	functor(OldLit,Name,Arity),
	recorded(progol,lazy_recall(Name/Arity,Recall),_),
	recorda(progol_dyn,callno(1),_),
	p1_message('lazy evaluation'), p_message(Name),
	repeat,
	evaluate(OldLit,Depth,Lit,I,O,D),
	recorded(progol_dyn,callno(CallNo),DbRef),
	erase(DbRef),
	NextCall is CallNo + 1,
	recorda(progol_dyn,callno(NextCall),DbRef1),
	NextCall > Recall,
	!,
	p_message('completed'),
	erase(DbRef1).
	 
evaluate(OldLit,_,Lit,I,O,D):-
	functor(OldLit,Name,Arity),
	functor(NewLit,Name,Arity),
	Lit,
	copy_args(OldLit,NewLit,I),
	copy_args(OldLit,NewLit,O),
	copy_consts(Lit,NewLit,Arity),
	update_litinfo(LitNum,false,NewLit,I,O,D),
	\+ recorded(progol_dyn,lazy_evaluated(LitNum),_),
	recorda(progol_dyn,lazy_evaluated(LitNum),_), !.
evaluate(_,_,_,_,_,_).

copy_args(_,_,[]).
copy_args(Old,New,[Arg/_|T]):-
	arg(Arg,Old,Term),
	arg(Arg,New,Term),
	copy_args(Old,New,T), !.

copy_consts(_,_,0):- !.
copy_consts(Old,New,Arg):-
	arg(Arg,Old,Term),
	arg(Arg,New,Term1),
	var(Term1), !,
	Term1 = progol_const(Term),
	Arg0 is Arg - 1,
	copy_consts(Old,New,Arg0).
copy_consts(Old,New,Arg):-
	Arg0 is Arg - 1,
	copy_consts(Old,New,Arg0).

% theorem-progol_prover for lazy evaluation of literals

lazy_progol_prove(_,_,_,[]).
lazy_progol_prove(Type,Lit,Clause,[Interval|Intervals]):-
        lazy_index_progol_prove(Type,Lit,Clause,Interval),
        lazy_progol_prove(Type,Lit,Clause,Intervals).

lazy_index_progol_prove(_,_,_,Start-Finish):-
        Start > Finish, !.
lazy_index_progol_prove(Type,Lit,Clause,Start-Finish):-
        lazy_index_progol_prove1(Type,Lit,Clause,Start),
        Start1 is Start + 1,
        lazy_index_progol_prove(Type,Lit,Clause,Start1-Finish).

% bind input args of lazy literal
% each example gives an input binding
lazy_index_progol_prove1(Type,Lit,Clause,Num):-
        (Clause = (Head:-Body)->true;Head=Clause,Body=true),
        progol_prove1((example(Num,Type,Head),Body)),
        recorda(progol_dyn,lazy_evaluate(Type,Lit),_),
        fail.
lazy_index_progol_prove1(_,_,_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% S L P


condition_target:-
	recorded(progol,set(condition,true),_),
	recorded(progol,modeh(_,Pred),_),
	add_generator(Pred,SPred),
	p_message('conditioning'),
	SPred =.. [_|Args],
	functor(Pred,Name,Arity),
	functor(Fact,Name,Arity),
	example(_,_,Fact),
	Fact =.. [_|Args], 
	condition(SPred),
	fail.
condition_target:-
	\+ recorded(progol,set(condition,true),_),
	recorded(progol,modeh(_,Pred),_),
	add_generator(Pred,_), !.
condition_target.


add_generator(Pred,SPred):-
	functor(Pred,Name,Arity),
	make_sname(Name,SName),
	functor(SPred,SName,Arity),
	(clause(SPred,_)-> 
		true;
		range_restrict(Pred,Arity,SPred,Body),
		asserta((SPred:-Body)),
		p1_message('included generator'), p_message(SName/Arity)).

make_sname(Name,SName):-
	concat(['*',Name],SName).

range_restrict(_,0,_,true):- !.
range_restrict(Pred,1,SPred,Lit):-
	!,
	range_restriction(Pred,1,SPred,Lit).
range_restrict(Pred,Arg,SPred,(Lit,Lits)):-
	range_restriction(Pred,Arg,SPred,Lit),
	Arg1 is Arg - 1,
	range_restrict(Pred,Arg1,SPred,Lits).

range_restriction(Pred,Arg,SPred,Lit):-
	arg(Arg,Pred,Type),
	get_type_name(Type,TName),
	functor(Lit,TName,1),
	arg(Arg,SPred,X),
	arg(1,Lit,X).

get_type_name(+Name,Name):- !.
get_type_name(-Name,Name):- !.
get_type_name(#Name,Name):- !.


condition(Fact):-
	slprogol_prove(condition,Fact), !.
condition(_).

sample(_,0,[]):- !.
sample(Name/Arity,N,S):-
	functor(Pred,Name,Arity),
	retractall(slp,samplenum(_)),
	retractall(slp,sample(_)),
	recorda(slp,samplenum(1),_),
	repeat,
	slprogol_prove(stochastic,Pred),
	recorda(slp,sample(Pred),_),
	recorded(slp,samplenum(N1),DbRef),
	erase(DbRef),
	N2 is N1 + 1,
	recorda(slp,samplenum(N2),DbRef1),
	N2 > N,
	!,
	erase(DbRef1),
	functor(Fact,Name,Arity),
	findall(Fact,(recorded(slp,sample(Fact),DbRef2),erase(DbRef2)),S).

gsample(Name/Arity,_,_):-
        make_sname(Name,SName),
        functor(SPred,SName,Arity),
        clause(SPred,true),
        ground(SPred), !,
        update_gsample(Name/Arity,_).
gsample(_,0,_):- !.
gsample(Name/Arity,N,ExsStream):-
	functor(Pred,Name,Arity),
	make_sname(Name,SName),
	functor(SPred,SName,Arity),
	Pred =.. [_|Args],
	retractall(slp,samplenum(_)),
	recorda(slp,samplenum(0),_),
	repeat,
	slprogol_prove(stochastic,SPred),
	SPred =..[_|Args],
	recorded(slp,samplenum(N1),DbRef),
	erase(DbRef),
	N2 is N1 + 1,
	recorda(slp,samplenum(N2),DbRef1),
	%assertz(example(N2,rand,Pred)),
	portray_clause(ExsStream,example(N2,rand,Pred)),
	N2 >= N,
	!,
	erase(DbRef1),
	portray_goal(ExsStream,recorda(progol,size(rand,N),_)),
	portray_goal(ExsStream,recorda(progol,atoms(rand,[1-N]),_)),
	portray_goal(ExsStream,recorda(progol,atoms_left(rand,[1-N]),_)).


update_gsample(Name/Arity,_):-
        functor(Pred,Name,Arity),
        make_sname(Name,SName),
        functor(SPred,SName,Arity),
        retractall(progol,gsample(_)),
        retractall(slp,samplenum(_)),
        recorda(slp,samplenum(0),_),
        SPred =.. [_|Args],
        Pred =.. [_|Args],
        clause(SPred,true),
        ground(SPred),
        recorded(slp,samplenum(N1),DbRef),
        erase(DbRef),
        N2 is N1 + 1,
        recorda(slp,samplenum(N2),_),
        assertz(example(N2,rand,Pred)),
        fail.
update_gsample(_,N):-
        recorded(slp,samplenum(N),DbRef),
        N > 0, !,
        erase(DbRef),
        set(gsamplesize,N),
	recorda(progol,size(rand,N),_),
	recorda(progol,atoms(rand,[1-N]),_),
	recorda(progol,atoms_left(rand,[1-N]),_).
update_gsample(_,_).

	
slprogol_prove(_,true):-
	!.
slprogol_prove(Mode,not(Goal)):-
	slprogol_prove(Mode,Goal),
	!,
	fail.
slprogol_prove(Mode,(Goal1,Goal2)):-
	!,
	slprogol_prove(Mode,Goal1),
	slprogol_prove(Mode,Goal2).
slprogol_prove(Mode,(Goal1;Goal2)):-
	!,
	slprogol_prove(Mode,Goal1);
	slprogol_prove(Mode,Goal2).
%if can't find user-defined predicate with equal name and arity..
slprogol_prove(_,Goal):-  
	functor(Goal,Name,Arity),
	\+ (current_predicate(Name,MostGenGoal),functor(MostGenGoal,Name,Arity)),!,
	Goal.
slprogol_prove(stochastic,Goal):-
	findall(Count/Clause,
		(clause(Goal,Body),Clause=(Goal:-Body),find_count(Clause,Count)),
		ClauseCounts),
	renormalise(ClauseCounts,Normalised),
	%X is random,
	random(X),
	rselect_clause(X,Normalised,(Goal:-Body)),
	slprogol_prove(stochastic,Body).
slprogol_prove(condition,Goal):-
	functor(Goal,Name,Arity),
	functor(Head,Name,Arity),
	clause(Head,Body),
	\+ \+ (Head=Goal,slprogol_prove(condition,Body)),
	inc_count((Head:-Body)).

renormalise(ClauseCounts,Normalised):-
	sum_counts(ClauseCounts,L),
	L > 0,
	renormalise(ClauseCounts,L,Normalised).

sum_counts([],0).
sum_counts([N/_|T],C):-
	sum_counts(T,C1),
	C is N + C1.

renormalise([],_,[]).
renormalise([Count/Clause|T],L,[Prob/Clause|T1]):-
	Prob is Count/L,
	renormalise(T,L,T1).

rselect_clause(X,[P/C|_],C):- X =< P, !.
rselect_clause(X,[P/_|T],C):-
	X1 is X - P,
	rselect_clause(X1,T,C).


find_count(Clause,N):-
	%copy(Clause,Clause1),
	copy_term(Clause,Clause1),
	recorded(slp,count(Clause1,N),_), !.
find_count(_,1).
	
inc_count(Clause):-
	recorded(slp,count(Clause,N),DbRef), !,
	erase(DbRef),
	N1 is N + 1,
	recorda(slp,count(Clause,N1),_).
inc_count(Clause):-
	recorda(slp,count(Clause,2),_).

find_posgain(PCover,P):-
	recorded(progol,set(greedy,true),_), !,
	interval_count(PCover,P).
find_posgain(PCover,P):-
	recorded(progol,atoms_left(pos,PLeft),_),
	intervals_intersection(PLeft,PCover,PC),
	interval_count(PC,P).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% S E A R C H  I / O 

record_clause(Label,Clause,Nodes,Flag):-
	recorded(progol,set(record,true),_),
	recorded(progol,set(recordfile,File),_), !,
	open(File,append,Stream),
	set_output(Stream),
	show_clause(Label,Clause,Nodes,Flag),
	close(Stream),
	set_output(user_output).
record_clause(_,_,_,_).

record_sat_example(N):-
	recorded(progol,set(record,true),_),
	recorded(progol,set(recordfile,File),_), !,
	open(File,append,Stream),
	set_output(Stream),
	p1_message('sat'), p_message(N),
	close(Stream),
	set_output(user_output).
record_sat_example(_).

record_search_stats(Clause,Nodes,Time):-
	recorded(progol,set(record,true),_),
	recorded(progol,set(recordfile,File),_), !,
	open(File,append,Stream),
	set_output(Stream),
	p1_message('clauses constructed'), p_message(Nodes),
	p1_message('search time'), p_message(Time),
	p_message('best clause'),
	pp_dclause(Clause),
	% show(hypothesis),
	close(Stream),
	set_output(user_output).
record_search_stats(_,_,_).

record_theory(Time):-
        recorded(progol,set(record,true),_),
        recorded(progol,set(recordfile,File),_), !,
        open(File,append,Stream),
        set_output(Stream),
        show(theory),
	p1_message('time taken'), p_message(Time),
        nl,
        (recorded(progol,set(maxcover,true),_)->
                show(progol,theory/5), nl,
                show(progol,max_set/4), nl,
                show(progol,rules/1);
                true),
        close(Stream),
        set_output(user_output).
record_theory(_).

record_settings:-
        recorded(progol,set(record,true),_),
        recorded(progol,set(recordfile,File),_), !,
	record_date(File),
	record_machine(File),
        open(File,append,Stream),
        set_output(Stream),
	show(settings),
	close(Stream),
        set_output(user_output).
record_settings.

% Unix specific date command
record_date(File):-
	concat([date,' >> ', File],Cmd),
	execute(Cmd).

% Unix specific machine name
record_machine(File):-
	concat([hostname,' >> ', File],Cmd),
	execute(Cmd).

show_clause(Label,Clause,Nodes,Flag):-
	p_message('-------------------------------------'),
	(Flag=explore -> p_message('exploratory clause');
		(Flag=sample-> p_message('selected from sample');
			p_message('found clause'))),
	pp_dclause(Clause),
	(setting(evalfn,Evalfn)-> true; Evalfn = coverage),
	show_stats(Evalfn,Label),
	p1_message('clause label'), p_message(Label),
	p1_message('clauses explored'), p_message(Nodes),
	p_message('-------------------------------------').


show_stats(Evalfn,[_P,_N,_L,F|_]):-
	print_eval(Evalfn,F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A U T O  -- R E F I N E

% built-in refinement operator

gen_refine:-
	set(refineop,true),
	\+ clause(refine(_,_),_), !,
	Refine = (refine(Clause1,Clause2):- progol_refine(Clause1,Clause2)),
	asserta(Refine),
	p_message('included automatic definition of refinement operator').
gen_refine.

progol_refine(false,Head):-
	!,
	once(recorded(progol,determination(Name/Arity,_),_)),
	functor(Head,Name,Arity).
progol_refine((H:-B),(H1:-B1)):-
	!,
	goals_to_list((H,B),LitList),
	set(clauselength,L),
	length(LitList,ClauseLength),
	ClauseLength < L,
	progol_get_lit(Lit,LitList),	
	append([LitList],Lit,LitList1),
	list_to_goals(LitList1,(H1,B1)).
progol_refine(Head,Clause):-
	progol_refine((Head:-true),Clause).

progol_get_lit(Lit,[H|Lits]):-
	functor(H,Name,Arity),
	progol_get_lit(Lit,Name/Arity),
	progol_link_vars(Lit,[H|Lits]).

progol_get_lit(Lit,Target):-
	forAll(N/A,recorded(progol,determination(Target,N/A),_),PSyms),
	member(Name/Arity,PSyms),
	functor(Lit,Name,Arity).

progol_link_vars(Lit,Lits):-
	progol_get_ilocs(Lit,IVars),
	progol_has_ovars(IVars,Lit,Lits).

progol_get_ilocs(Lit,IVars):-
	functor(Lit,Name,Arity),
	functor(Mode,Name,Arity),
	findall(Mode,recorded(progol,mode(_,Mode),_),Modes),
	progol_get_ilocs(Arity,Modes,IVars).

progol_get_ilocs(0,_,[]):- !.
progol_get_ilocs(Arg,Modes,[Arg/STypes|T]):-
	findall(Type,(member(Mode,Modes),arg(Arg,Mode,+Type)),Types),
	length(Modes,NModes), length(Types,NModes), !,
	sort(Types,STypes),
	Arg0 is Arg - 1,
	progol_get_ilocs(Arg0,Modes,T).
progol_get_ilocs(Arg,Modes,T):-
	Arg0 is Arg - 1,
	progol_get_ilocs(Arg0,Modes,T).

progol_has_ovars([],_,_).
progol_has_ovars([Arg/Types|Vars],Lit,[Head|Lits]):-
	member(Type,Types),
	arg(Arg,Lit,Var),
	(progol_has_ivar(Var,Type,Head);
		(member(BodyLit,Lits),
		progol_has_ovar(Var,Type,BodyLit))),
	progol_has_ovars(Vars,Lit,[Head|Lits]).

progol_has_ovar(Var,Type,Lit):-
	functor(Lit,Name,Arity),
	functor(Mode,Name,Arity),
	recorded(progol,mode(_,Mode),_),
	interval_to_list(1-Arity,Args),
	member(Arg,Args),
	arg(Arg,Mode,-Type),
	arg(Arg,Lit,Var).

progol_has_ivar(Var,Type,Lit):-
	functor(Lit,Name,Arity),
	functor(Mode,Name,Arity),
	recorded(progol,mode(_,Mode),_),
	interval_to_list(1-Arity,Args),
	member(Arg,Args),
	arg(Arg,Mode,+Type),
	arg(Arg,Lit,Var).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% R A N D O M   C L A U S E   G E N E R A T O R
% still being worked on
 
% obtain estimate of distribution of number of clauses
% at each clause length
clause_distribution(_,_):-
	retractall(progol,clause_distribution(_,_)),
	retractall(progol,total_clauses(_)),
	retractall(progol,lit_variants(_,_)),
	set(refine,auto),
	forAll(C,refine(false,C),Clauses),
	update_estimate_nclauses(Clauses),
	fail.
clause_distribution(L,N):-
	recorded(progol,clause_distribution(L,N),_).

update_estimate_nclauses([]):- !.
update_estimate_nclauses(Clauses):-
	update_estimate_variants(Clauses),
	forAll(C,(member(C1,Clauses),refine(C1,C)),NextClauses), !,
	update_estimate_nclauses(NextClauses).
update_estimate_nclauses(_).
	
update_estimate_variants([]):- !.
update_estimate_variants([Clause|Clauses]):-
	find_hashed_variants(Clause,1,N),
	update_distribution(Clause,N),
	update_estimate_variants(Clauses), !.

% number of variants of a template -- estimated from the first
% example left ungeneralised
estimate_variants(Clause,N):-
	recorded(progol,atoms_left(pos,[ExampleNum-_|_]),_),
	example(ExampleNum,pos,H),
	(Clause = (H:-B) -> true; B = true),
	retractall(progol_dyn,variant_count(_)),
	recorda(progol_dyn,variant_count(0),_),
	B,
	recorded(progol_dyn,variant_count(N),DbRef),
	erase(DbRef),
	N1 is N + 1,
	recorda(progol_dyn,variant_count(N1),_),
	fail.
estimate_variants(_,N):-
	(recorded(progol_dyn,variant_count(N),DbRef)->erase(DbRef); N=0).

update_distribution(Clause,N):-
	(Clause=(H:-B) -> CLits = (H,B); CLits = Clause),
	nlits(CLits,L),
	(recorded(progol,clause_distribution(L,N1),DbRef)->
		erase(DbRef),
		N2 is N1 + N;
		N2 is N),
	recorda(progol,clause_distribution(L,N2),_),
	(recorded(progol,total_clauses(N3),DbRef2)->
		erase(DbRef2),
		N4 is N3 + N;
		N4 is N),
	recorda(progol,total_clauses(N4),_).


		
find_hashed_variants(Clause,VSoFar,N):-
	clause_to_list(Clause,CList),
	hashed_variants(CList,VSoFar,N).

hashed_variants([],V,V).
hashed_variants([Lit|Lits],VSoFar,V):-
	hashed_variant(Lit,VSoFar,V1),
	hashed_variants(Lits,V1,V).

hashed_variant(Lit,VSoFar,V1):-
	functor(Lit,Name,Arity),
	functor(Template,Name,Arity),
        (recorded(lits,lit_info(_,_,Template,_,_,_),_) ->
		hashed_combinations(Lit,M);
		ub_hashed_combinations(Arity,Template,1,M)),
	V1 is VSoFar*M,
	recorda(progol,lit_variants(Name/Arity,M),_), !.

hashed_combinations(Lit,M):-
	functor(Lit,Name,Arity),
	recorded(progol,lit_variants(Name/Arity,M),_), !.
hashed_combinations(Lit,M):-
	functor(Lit,Name,Arity),
	functor(Lit1,Name,Arity),
	forAll(C,(recorded(lits,lit_info(_,_,Lit1,_,_,_),_),has_hashed(Lit1,Arity,[],C)),L),
	\+ L = [[]], !,
	length(L,M).
hashed_combinations(_Lit,1).

has_hashed(_Lit,0,L,L):- !.
has_hashed(Lit,Arg,CSoFar,C):-
	arg(Arg,Lit,progol_const(C1)), !,
	Arg1 is Arg - 1,
	has_hashed(Lit,Arg1,[C1|CSoFar],C).
has_hashed(Lit,Arg,CSoFar,C):-
	Arg1 is Arg - 1,
	has_hashed(Lit,Arg1,CSoFar,C).

ub_hashed_combinations(0,_,V,V):- !.
ub_hashed_combinations(Arg,Mode,VSoFar,V):-
	forAll(Type,(recorded(progol,mode(_,Mode),_),arg(Arg,Mode,#Type)),HashedTypes),
	\+ HashedTypes = [], !,
	find_max_values(HashedTypes,1,M),
	V1 is VSoFar*M,
	Arg1 is Arg - 1,
	ub_hashed_combinations(Arg1,Mode,V1,V).
ub_hashed_combinations(Arg,Mode,V1,V):-
	Arg1 is Arg - 1,
	ub_hashed_combinations(Arg1,Mode,V1,V).

find_max_values([],M,M).
find_max_values([Type|Types],MaxSoFar,M):-
	functor(Fact,Type,1),
	findall(T,(Fact,arg(1,Fact,T)),TypeVals),
	length(TypeVals,M1),
	(M1 >= MaxSoFar ->
		find_max_values(Types,M1,M);
		find_max_values(Types,MaxSoFar,M)).

has_hashed_loc(Arg,Mode):-
	Arg > 0,
	arg(Arg,Mode,#_), !.
has_hashed_loc(Arg,Mode):-
	Arg > 1,
	Arg1 is Arg - 1,
	has_hashed_loc(Arg1,Mode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% U T I L I T I E S


%for Sicstus since erased 'instance's are not immediately erased :-(
sicstus_workround_erase(Ref) :-
  erase(Ref),
  recorda(erased,Ref,_).

sicstus_workround_recorded(Key,Term,Ref) :-
  recorded(Key,Term,Ref),
  \+ recorded(erased,Ref,_).

sicstus_workround_cleanup :-
	retractall(erased,_).

% used when saving examples which require skolemisation on loading
portray_goal(Stream,Clause) :-
	write(Stream,':- '),
	portray_clause(Stream,Clause).

% concatenate elements of a list into an atom

concat([Atom],Atom):- !.
concat([H|T],Atom):-
        concat(T,AT),
        name(AT,L2),
        name(H,L1),
        append(L1,L2,L),
        name(Atom,L).


split_clause((Head:-true),Head,true):- !.
split_clause((Head:-Body1),Head,Body2):- \+ Body1 = true, !, Body1 = Body2.
split_clause([Head|T],Head,T):- !.
split_clause([Head],Head,[true]):- !.
split_clause(Head,Head,true).

strip_true((Head:-true),Head):- !.
strip_true(Clause,Clause).

% pretty print a definite clause
pp_dclause(Clause):-
        (recorded(progol,set(pretty_print,true),_)->
                pp_dclause(Clause,true);
                pp_dclause(Clause,false)).
 
pp_dclause((H:-true),Pretty):-
        !,
        pp_dclause(H,Pretty).
pp_dclause((H:-B),Pretty):-
        !,
        %copy((H:-B),(Head:-Body)),
	copy_term((H:-B),(Head:-Body)),
        numbervars((Head:-Body),0,_),
        progol_portray(Pretty,Head),
        (Pretty = true ->
                write(' if:');
                write(' :-')),
        nl,
        recorded(progol,set(print,N),_),
        print_lits(Body,Pretty,1,N).

pp_dclause((Lit),Pretty):-
        %copy(Lit,Lit1),
	copy_term(Lit,Lit1),
        numbervars(Lit1,0,_),
        progol_portray(Pretty,Lit1),
        write('.'), nl.
 
% pretty print a definite clause list: head of list is + literal
pp_dlist([]):- !.
pp_dlist(Clause):-
        (recorded(progol,set(pretty_print,true),_)->
                pp_dlist(Clause,true);
                pp_dlist(Clause,false)).
 
pp_dlist(Clause,Pretty):-
        %copy(Clause,[Head1|Body1]),
	copy_term(Clause,[Head1|Body1]),
        numbervars([Head1|Body1],0,_),
        progol_portray(Pretty,Head1),
        (Body1 = [] ->
                print('.'), nl;
                (Pretty = true ->
                        write(' if:');
                        write(' :-')),
        nl,
        recorded(progol,set(print,N),_),
        print_litlist(Body1,Pretty,1,N)).
 
print_litlist([],_,_,_).
print_litlist([Lit],Pretty,LitNum,_):-
        !,
        print_lit(Lit,Pretty,LitNum,LitNum,'.',_).
print_litlist([Lit|Lits],Pretty,LitNum,LastLit):-
        print_lit(Lit,Pretty,LitNum,LastLit,', ',NextLit),
        print_litlist(Lits,Pretty,NextLit,LastLit).
 
print_lits((Lit,Lits),Pretty,LitNum,LastLit):-
        !,
        (Pretty = true ->
                Sep = ', and ';
                Sep = ', '),
        print_lit(Lit,Pretty,LitNum,LastLit,Sep,NextLit),
        print_lits(Lits,Pretty,NextLit,LastLit).
print_lits((Lit),Pretty,LitNum,_):-
        print_lit(Lit,Pretty,LitNum,LitNum,'.',_).

print_lit(Lit,Pretty,LitNum,LastLit,Sep,NextLit):-
        (LitNum = 1 -> tab(3);true),
        progol_portray(Pretty,Lit), write(Sep),
        (LitNum=LastLit-> nl,NextLit=1; NextLit is LitNum + 1).
 

print_text([]).
print_text([Atom]):-
        !,
        write(Atom).
print_text([Atom|T]):-
        write(Atom), write(' '),
        print_text(T).


p1_message(Mess):-
	print('['), print(Mess), print('] ').

p_message(Mess):-
	print('['), print(Mess), print(']'), nl.

delete_all(_,[],[]).
delete_all(X,[Y|T],T1):-
        X == Y, !,
        delete_all(X,T,T1).
delete_all(X,[Y|T],[Y|T1]):-
        delete_all(X,T,T1).

delete_list([],L,L).
delete_list([H1|T1],L1,L):-
	%delete(H1,L1,L2), !,
	select(H1,L1,L2),!, %Sicstus list library predicate
	delete_list(T1,L2,L).
delete_list([_|T1],L1,L):-
	delete_list(T1,L1,L).

%this is called select/3 in the lists module of Sicstus
%delete(H,[H|T],T).
%delete(H,[H1|T],[H1|T1]):-
	%delete(H,T,T1).

%use select/3 instead
%delete1(H,[H|T],T):- !.
%delete1(H,[H1|T],[H1|T1]):-
%	delete1(H,T,T1).
delete1(X,L1,L2) :- select(X,L1,L2),!.

delete0(_,[],[]).
delete0(H,[H|T],T):- !.
delete0(H,[H1|T],[H1|T1]):-
	delete0(H,T,T1).

%This append has first two args swapped, so forAll append calls have been altered
%to use Sicstus append/3
%append(A,[],A).
%append(A,[H|T],[H|T1]):-
%	append(A,T,T1).

dappend(Z1-Z2,A1-Z1,A1-Z2).

%replaced by memberchk/2 from Sicstus lists library
%which works (unlike what manual says) with uninstantiated 1st arg
%member1(H,[H|_]):- !.
%member1(H,[_|T]):-
%	member1(H,T).


%in lists module
%member(X,[X|_]).
%member(X,[_|T]):-
%	member(X,T).

%in lists module
%reverse(L1, L2) :- revzap(L1, [], L2).

%revzap([X|L], L2, L3) :- revzap(L, [X|L2], L3).
%revzap([], L, L).

merge_vlist([],[]).
merge_vlist([V/L|T],Merged):-
        delete1(V/L1,T,T1), !,
        append(L1,L,L2),
        merge_vlist([V/L2|T1],Merged).
merge_vlist([V/L|T],[V/L|Merged]):-
        merge_vlist(T,Merged).


goals_to_clause((Head,Body),(Head:-Body)):- !.
goals_to_clause(Head,Head).

clause_to_list((Head:-true),[Head]):- !.
clause_to_list((Head:-Body),[Head|L]):-
        !,
        goals_to_list(Body,L).
clause_to_list(Head,[Head]).

extend_clause(false,Lit,(Lit)):- !.
%extend_clause(Clause,Lit,Clause):-
%        functor(Lit,'=',2), !,
%        Lit.
extend_clause((Head:-Body),Lit,(Head:-Body1)):-
        !,
        app_lit(Lit,Body,Body1).
extend_clause(Head,Lit,(Head:-Lit)).
 
app_lit(L,(L1,L2),(L1,L3)):-
        !,
        app_lit(L,L2,L3).
app_lit(L,L1,(L1,L)).



nlits((_,Lits),N):-
	!,
	nlits(Lits,N1),
	N is N1 + 1.
nlits(_,1).

list_to_clause([Goal],(Goal:-true)):- !.
list_to_clause([Head|Goals],(Head:-Body)):-
	list_to_goals(Goals,Body).

list_to_goals([Goal],Goal):- !.
list_to_goals([Goal|Goals],(Goal,Goals1)):-
	list_to_goals(Goals,Goals1).

goals_to_list((true,Goals),T):-
	!,
	goals_to_list(Goals,T).
goals_to_list((Goal,Goals),[Goal|T]):-
	!,
	goals_to_list(Goals,T).
goals_to_list(true,[]):- !.
goals_to_list(Goal,[Goal]).

get_clause(LitNum,Last,_,[]):-
        LitNum > Last, !.
get_clause(LitNum,Last,TVSoFar,[FAtom|FAtoms]):-
        recorded(lits,lit_info(LitNum,_,Atom,_,_,_),_), !,
        get_flatatom(Atom,TVSoFar,FAtom,TV1),
        NextLit is LitNum + 1,
        get_clause(NextLit,Last,TV1,FAtoms).
get_clause(LitNum,Last,TVSoFar,FAtoms):-
        NextLit is LitNum + 1,
        get_clause(NextLit,Last,TVSoFar,FAtoms).

get_flatatom(not(Atom),TVSoFar,not(FAtom),TV1):-
        !,
        get_flatatom(Atom,TVSoFar,FAtom,TV1).
get_flatatom(Atom,TVSoFar,FAtom,TV1):-
        functor(Atom,Name,Arity),
        functor(FAtom,Name,Arity),
        flatten_args(Arity,Atom,FAtom,TVSoFar,TV1).

get_pclause([LitNum],TVSoFar,Clause,TV,Length,LastDepth):-
        !,
        get_pclause1([LitNum],TVSoFar,TV,Clause,Length,LastDepth).
get_pclause([LitNum|LitNums],TVSoFar,Clause,TV,Length,LastDepth):-
        get_pclause1([LitNum],TVSoFar,TV1,Head,Length1,_),
        get_pclause1(LitNums,TV1,TV,Body,Length2,LastDepth),
        (Length2 = 0 ->
                Clause = Head;
                Clause = (Head:-Body)),
        Length is Length1 + Length2.

get_pclause1([LitNum],TVSoFar,TV1,Lit,Length,LastDepth):-
        !,
        recorded(lits,lit_info(LitNum,LastDepth,Atom,_,_,_),_),
        get_flatatom(Atom,TVSoFar,Lit,TV1),
        functor(Lit,Name,_),
        % (Name = '='-> Lit, Length = 0; Length = 1).
        (Name = '='-> Length = 0; Length = 1).
get_pclause1([LitNum|LitNums],TVSoFar,TV2,Lits,Length,LastDepth):-
        recorded(lits,lit_info(LitNum,_,Atom,_,_,_),_),
        get_flatatom(Atom,TVSoFar,Lit,TV1),
        functor(Lit,Name,_),
        (Name = '='->
                % Lit,
                get_pclause1(LitNums,TV1,TV2,Lits,Length,LastDepth);
                get_pclause1(LitNums,TV1,TV2,Lits1,Length1,LastDepth),
                (Length1=0-> Lits=Lit;Lits=(Lit,Lits1)),
                Length is Length1 + 1).

flatten_args(0,_,_,TV,TV):- !.
flatten_args(Arg,Atom,FAtom,TV,TV1):-
        arg(Arg,Atom,Term),
        Arg1 is Arg - 1,
        (Term = progol_const(Const) ->
                arg(Arg,FAtom,Const),
                flatten_args(Arg1,Atom,FAtom,TV,TV1);
                (integer(Term) ->
                        update(Term/Var,TV,TV0),
                        arg(Arg,FAtom,Var),
                        flatten_args(Arg1,Atom,FAtom,TV0,TV1);
                        (functor(Term,Name,Arity),
                         functor(FTerm,Name,Arity),
                         arg(Arg,FAtom,FTerm),
                         flatten_args(Arity,Term,FTerm,TV,TV0),
                         flatten_args(Arg1,Atom,FAtom,TV0,TV1)
                        )
                )
        ).


% returns intersection of S1, S2 and S1-Intersection
intersect1(Elems,[],[],Elems):- !.
intersect1([],_,[],[]):- !.
intersect1([Elem|Elems],S2,[Elem|Intersect],ElemsLeft):-
	%member1(Elem,S2), !,
	memberchk(Elem,S2), !,
	intersect1(Elems,S2,Intersect,ElemsLeft).
intersect1([Elem|Elems],S2,Intersect,[Elem|ElemsLeft]):-
	intersect1(Elems,S2,Intersect,ElemsLeft).


%replaced by sublist/2 from lists module
%subset1([],_).
%subset1([Elem|Elems],S):-
%	member1(Elem,S), !,
%	subset1(Elems,S).

% two sets are equal

%replaced by permutation/2 from lists module
%equal_set([],[]).
%equal_set([H|T],S):-
% 	delete1(H,S,S1),
%	equal_set(T,S1), !.


uniq_insert(_,X,[],[X]).
uniq_insert(descending,H,[H1|T],[H,H1|T]):-
	H > H1, !.
uniq_insert(ascending,H,[H1|T],[H,H1|T]):-
	H < H1, !.
uniq_insert(_,H,[H|T],[H|T]):- !.
uniq_insert(Order,H,[H1|T],[H1|T1]):-
	uniq_insert(Order,H,T,T1).

explore_eq(X1,X2):-
	(setting(explore_tolerance,Epsilon) -> true; Epsilon = 0.0),
	ADiff is abs(X1 - X2),
	ADiff =< Epsilon.

quicksort(_,[],[]).
quicksort(Order,[X|Tail],Sorted):-
	val(X,Xval),
	partition(Xval,Tail,Small,Big),
	quicksort(Order,Small,SSmall),
	quicksort(Order,Big,SBig),
        (Order=ascending-> append(SSmall,[X|SBig],Sorted);
                append(SBig,[X|SSmall],Sorted)).
	

partition(_,[],[],[]).
partition(X,[Y|Tail],[Y|Small],Big):-
	val(Y,Yval),
	X > Yval, !,
	partition(X,Tail,Small,Big).
partition(X,[Y|Tail],Small,[Y|Big]):-
	partition(X,Tail,Small,Big).

update_list([],L,L).
update_list([H|T],L,Updated):-
	update(H,L,L1),
	update_list(T,L1,Updated).

update(H,[H|T],[H|T]):- !.
update(H,[H1|T],[H1|T1]):-
	update(H,T,T1).
update(H,[],[H]).



val(A/_,A):- !.
val(A,A).

val1(_/B,B):- !.
val1(A,A).


% checks if 2 sets intersect
intersects(S1,S2):-
	%member(Elem,S1), member1(Elem,S2), !.
	member(Elem,S1), memberchk(Elem,S2), !.

% checks if bitsets represented as lists of intervals intersect
intervals_intersects([L1-L2|_],I):-
	intervals_intersects1(L1-L2,I), !.
intervals_intersects([_|I1],I):-
	intervals_intersects(I1,I).

intervals_intersects1(L1-_,[M1-M2|_]):-
	L1 >= M1, L1 =< M2, !.
intervals_intersects1(L1-L2,[M1-_|_]):-
	M1 >= L1, M1 =< L2, !.
intervals_intersects1(L1-L2,[_|T]):-
	intervals_intersects1(L1-L2,T).
% checks if bitsets represented as lists of intervals intersect
% returns first intersection
intervals_intersects([L1-L2|_],I,I1):-
	intervals_intersects1(L1-L2,I,I1), !.
intervals_intersects([_|I1],I,I1):-
	intervals_intersects(I1,I,I1).

intervals_intersects1(I1,[I2|_],I):-
	interval_intersection(I1,I2,I), !.
intervals_intersects1(I1,[_|T],I):-
	intervals_intersection(I1,T,I).

interval_intersection(L1-L2,M1-M2,L1-L2):-
	L1 >= M1, L2 =< M2, !.
interval_intersection(L1-L2,M1-M2,M1-M2):-
	M1 >= L1, M2 =< L2, !.
interval_intersection(L1-L2,M1-M2,L1-M2):-
	L1 >= M1, M2 >= L1, M2 =< L2, !.
interval_intersection(L1-L2,M1-M2,M1-L2):-
	M1 >= L1, M1 =< L2, L2 =< M2, !.

intervals_intersection([],_,[]).
intervals_intersection([A-B|T1],[C-D|T2],X) :-
        A >= C, !,
        (B >= D ->
            (D >= A -> 
                X=[A-D|Y],
                intervals_intersection([A-B|T1],T2,Y);
                intervals_intersection([A-B|T1],T2,X)
            );
            X=[A-B|Y],
            intervals_intersection(T1,[C-D|T2],Y)
        ).
intervals_intersection([A-B|T1],[C-D|T2],X) :-
        (D >= B ->
            (B >= C ->
                X=[C-B|Y],
                intervals_intersection(T1,[C-D|T2],Y);
                intervals_intersection(T1,[C-D|T2],X)
            );
            X=[C-D|Y],
            intervals_intersection([A-B|T1],T2,Y)
        ).
intervals_intersection(_,[],[]).


% finds length of intervals in a list
interval_count([],0).
interval_count([L1-L2|T],N):-
	N1 is L2 - L1 + 1,
	interval_count(T,N2),
	N is N1 + N2.
interval_count(I/_L,N):-
	interval_count(I,N).

% convert list to intervals
list_to_intervals(List,Intervals):-
        quicksort(ascending,List,List1),
        list_to_intervals1(List1,Intervals).

list_to_intervals1([],[]).
list_to_intervals1([Start|T],[Start-Finish|I1]):-
        list_to_interval(Start,T,Finish,T1),
        list_to_intervals1(T1,I1).

list_to_interval(Finish,[],Finish,[]).
list_to_interval(Finish,[Next|T],Finish,[Next|T]):-
        Next - Finish > 1,
        !.
list_to_interval(_,[Start|T],Finish,Rest):-
        list_to_interval(Start,T,Finish,Rest).

% converts intervals into a list
intervals_to_list([],[]).
intervals_to_list([Interval|Intervals],L):-
        interval_to_list(Interval,L1),
        intervals_to_list(Intervals,L2),
        append(L1,L2,L), !.

% converts an interval into a list
interval_to_list(Start-Finish,[]):-
	Start > Finish, !.
interval_to_list(Start-Finish,[Start|T]):-
	Start1 is Start+1,
	interval_to_list(Start1-Finish,T).

interval_subsumes(Start1-Finish1,Start2-Finish2):-
	 Start1 =< Start2,
	 Finish1 >= Finish2.

interval_subtract(Start1-Finish1,Start1-Finish1,[]):- !.
interval_subtract(Start1-Finish1,Start1-Finish2,[S2-Finish1]):-
	!,
	S2 is Finish2 + 1.
interval_subtract(Start1-Finish1,Start2-Finish1,[Start1-S1]):-
	!,
	S1 is Start2 - 1.
interval_subtract(Start1-Finish1,Start2-Finish2,[Start1-S1,S2-Finish1]):-
	S1 is Start2 - 1,
	S2 is Finish2 + 1,
	S1 >= Start1, Finish1 >= S2, !.

% compiler instructions
declare_dynamic(Pred/Arity):-
	dynamic Pred/Arity.

clean_up:-
	clean_up_sat,
	clean_up_reduce.

clean_up_sat:-
	retractall(getPrologVars,_),
	retractall(terms,_),
	retractall(lits,_),
	retractall(ivars,_),
	retractall(ovars,_),
	retractall(split,_),
	retractall(sat,_),
	retractall(progol_dyn,_),
	gc.

clean_up_reduce:-
	retractall(search,_),
	retractall(pclause,_),
	retractall(nodes,_),
	retractall(gains,_),
	retractall(progol_dyn,_),
	gc.


retractall(Area,Fact):-
	recorded(Area,Fact,DbRef),
	erase(DbRef),
	fail.
retractall(_,_).

binom_lte(_,_,O,0.0):- O < 0, !.
binom_lte(N,P,O,Prob):-
        binom(N,P,O,Prob1),
        O1 is O - 1,
        binom_lte(N,P,O1,Prob2),
        Prob is Prob1 + Prob2, !.

binom(N,_,O,0.0):- O > N, !.
binom(N,P,O,Prob):-
        choose(N,O,C),
        E1 is P^O,
        P2 is 1 - P,
        O2 is N - O,
        E2 is P2^O2,
        Prob is C*E1*E2, !.
 
choose(N,I,V):-
        NI is N-I,
        (NI > I -> pfac(N,NI,I,V) ; pfac(N,I,NI,V)).

pfac(0,_,_,1).
pfac(1,_,_,1).
pfac(N,N,_,1).
pfac(N,I,C,F):-
        N1 is N-1,
        C1 is C-1,
        pfac(N1,I,C1,N1F),
        F1 is N/C,
        F is N1F*F1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% i/o stuff

read_all(Pred):-
	clean_up,
	reset,
	read_rules(Pred),
	recorda(progol_dyn,set(updatebacks,true),DbRef),
	read_examples(Pred), 	
	record_targetpred, 	
	check_prune_defs,
	%check_user_search, % user has to explicitly set lazy_on_cost if required.
	do_precomputation,
	check_posonly(Pred),
	check_auto_refine,
	erase(DbRef),
	show(settings),
	record_settings.


read_rules(Pred):-
	retractall(progol,mode(_,_)),
	retractall(progol,determination(_,_)),
	construct_name(Pred,rules,File),
	%reconsult(File).
	ensure_loaded(File). %Sicstus compile

%write out and then compile examples
%if examples file (possibly compiled) already there just load it
%example file can contain random examples
read_examples(Pred):-
	construct_name(Pred,exs,ExsFile),
	(ensure_loaded(ExsFile) -> 
	    recorded(progol,size(pos,P),_),
	    recorded(progol,size(neg,N),_);
	    open(ExsFile,write,ExsStream),
	    read_examples(pos,Pred,ExsStream),
	    recorded(progol,size(pos,P),_),
	    portray_goal(ExsStream,recorda(progol,size(pos,P),_)),
	    read_examples(neg,Pred,ExsStream),
	    recorded(progol,size(neg,N),_),
	    portray_goal(ExsStream,recorda(progol,size(neg,N),_)),
	    close(ExsStream),  %now have examples saved
	    (recorded(progol,set(search,posonly),_) -> true; %delay compilation
		compile(ExsFile))), %now have examples compiled
	(P > 0 -> PosE = [1-P]; PosE = []),
	(N > 0 -> NegE = [1-N]; NegE = []),
	recorda(progol,atoms(pos,PosE),_),
	recorda(progol,atoms_left(pos,PosE),_),
	recorda(progol,atoms(neg,NegE),_),
	recorda(progol,atoms_left(neg,NegE),_),
	set_lazy_recalls,
	set_lazy_on_contradiction(P,N),
        Prior is P / (P + N),
	(setting(prior,_) -> true; Prior is P/(P+N), set(prior,Prior)),
	reset_counts,
	recorda(progol,last_clause(0),_).

read_examples(Type,Pred,ExsStream):-
	retractall(progol,size(Type,_)),
	recorda(progol,size(Type,0),_),
	% retractall(progol,example(Type,_,_)),
	construct_name(Pred,Type,File),
	(Type = pos -> Flag = train_pos; Flag = train_neg),
	set(Flag,File),
	(open(File,read,Stream) ->
		%p1_message(consulting),
	    p1_message('indexing and saving'),
		concat([Type, ' examples'],Mess),
		p_message(Mess);
		p1_message('cannot open'), p_message(File),
		fail),
	repeat,
	read(Stream,Example),
	(Example=end_of_file-> close(Stream);
	recorded(progol,size(Type,N),DbRef), erase(DbRef),
	N1 is N + 1,
	recorda(progol,size(Type,N1),_),
	(Type = pos ->
		skolemize(Example,Fact,Body,SkolemVars),
		record_skolemized(Type,N1,SkolemVars,Fact,Body,ExsStream);
		split_clause(Example,Head,Body),
		record_nskolemized(Type,N1,Head,Body,ExsStream)),
	fail),
	!.
%read_examples(_,_).
read_examples(_,_,_).

construct_name(Prefix,Type,Name):-
	name(Prefix,PString),
	file_suffix(Type,SString),
	append(PString,SString,FString),
	name(Name,FString).

construct_prolog_name(Name,Name):-
	name('.pll',Suffix),
	name(Name,Str),
	%append(_,Suffix,Str), !.
	file_suffix(Suffix,Str),!.
construct_prolog_name(Name,Name1):-
	name(Name,Str),
	name('.pll',Suffix),
	append(Str,Suffix,Name1Str),
	name(Name1,Name1Str).

file_suffix(pos,Suffix):- name('.f',Suffix).
file_suffix(neg,Suffix):- name('.n',Suffix).
file_suffix(rules,Suffix):- name('.b',Suffix).
file_suffix(exs,Suffix):- name('.exs',Suffix). %for re-reading examples

record_targetpred:-
	recorded(progol_dyn,backpred(Name/Arity),DbRef),
	erase(DbRef),
	recorda(progol,targetpred(Name/Arity),_),
	record_testclause(Name/Arity),
	fail.
record_targetpred.

check_posonly(_):-
	recorded(progol,size(rand,N),_), 
	N > 0, 
	!. 
check_posonly(Pred):-
	retractall(slp,_),
	setting(evalfn,posonly),
	setting(gsamplesize,S),
	condition_target,
	recorded(progol,targetpred(Name/Arity),_),
	(var(Pred) -> p_message('will not generate random sample - exiting!'), halt ; true),
	construct_name(Pred,exs,ExsFile), %NEW sicstus
	open(ExsFile,append,ExsStream),   %NEW sicstus, append since have pos's
	gsample(Name/Arity,S,ExsStream),  %rands saved to file
	close(ExsStream),                 %NEW sicstus
	compile(ExsFile),                 %now compile
	!. 
check_posonly(_).

check_prune_defs:-
	%clause(prune(_),_), !,
	current_predicate(prune,prune(_)),!,
	set(prune_defs,true).
check_prune_defs.

check_auto_refine:-
	setting(lazy_bottom,true),
	setting(refineop,false), !,
	set(refine,auto).
check_auto_refine.


%check_user_search:-
%	setting(evalfn,user),
%	\+ cost_cover_required,
%	set(lazy_on_cost,true), !.
%check_user_search.

%cost_cover_required:-
%	clause(cost(_,Label,Cost),Body),
%	vars_in_term([Label],[],Vars),
%	(occurs_in(Vars,p(Cost)); occurs_in(Vars,Body)), !.

%vars_in_term([],Vars,Vars):- !.
%vars_in_term([Var|T],VarsSoFar,Vars):-
%        var(Var), !,
%        vars_in_term(T,[Var|VarsSoFar],Vars).
%vars_in_term([Term|T],VarsSoFar,Vars):-
%        Term =.. [_|Terms], !,
%        vars_in_term(Terms,VarsSoFar,V1),
%        vars_in_term(T,V1,Vars).
%vars_in_term([_|T],VarsSoFar,Vars):-
%        vars_in_term(T,VarsSoFar,Vars).


%occurs_in(Vars,(Lit,_)):-
%	occurs_in(Vars,Lit), !.
%occurs_in(Vars,(_,Lits)):-
%	!,
%	occurs_in(Vars,Lits).
%occurs_in(Vars,Lit):-
%	functor(Lit,_,Arity),
%	occurs1(Vars,Lit,1,Arity).

%occurs1(Vars,Lit,Argno,MaxArgs):- 
%	Argno =< MaxArgs,
%	arg(Argno,Lit,Term),
%	vars_in_term([Term],[],Vars1),
%	member(X,Vars), member(Y,Vars1), 
%	X == Y, !.
%occurs1(Vars,Lit,Argno,MaxArgs):- 
%	Argno < MaxArgs,
%	Next is Argno + 1,
%	occurs1(Vars,Lit,Next,MaxArgs).

do_precomputation:-
	pre_compute(Rule),
	split_clause(Rule,Head,Body),
	(clause(Head,Body) -> true;
		asserta(Rule),
		p_message('pre-computation'),
		p_message(Rule)),
	fail.
do_precomputation.

set_lazy_negs(_):-
	recorded(progol,set(lazy_negs,false),_), !.
set_lazy_negs(N):-
	N >= 100, !,
	recorda(progol,set(lazy_negs,true),_).
set_lazy_negs(_).

set_lazy_recalls:-
	recorded(progol,lazy_evaluate(Name/Arity),_),
	functor(Pred,Name,Arity),
	recorda(progol,lazy_recall(Name/Arity,0),_),
	recorded(progol,mode(Recall,Pred),_),
	recorded(progol,lazy_recall(Name/Arity,N),DbRef),
	(Recall = '*' -> RecallNum = 100; RecallNum = Recall),
	RecallNum > N,
	erase(DbRef),
	recorda(progol,lazy_recall(Name/Arity,RecallNum),_),
	fail.
set_lazy_recalls.

set_lazy_on_contradiction(_,_):-
	recorded(progol,set(lazy_on_contradiction,false),_), !.
set_lazy_on_contradiction(P,N):-
	Tot is P + N,
	Tot >= 100, !,
	set(lazy_on_contradiction,true).
set_lazy_on_contradiction(_,_).

% clause for testing partial clauses obtained in search
%Sicstus has no depth checking
record_testclause(Name/Arity):-
	functor(Head,Name,Arity),
	Clause = (Head:-
			recorded(pclause,pclause(Head,Body),_),
			Body, !),
	assertz(Clause).




skolemize((Head:-Body),SHead,SBody,SkolemVars):-
	!,
	%copy((Head:-Body),(SHead:-Body1)),
	copy_term((Head:-Body),(SHead:-Body1)),
	numbervars((SHead:-Body1),0,SkolemVars),
	goals_to_list(Body1,SBody).
skolemize(UnitClause,Lit,[],SkolemVars):-
	%copy(UnitClause,Lit),
	copy_term(UnitClause,Lit),
	numbervars(Lit,0,SkolemVars).
skolemize(UnitClause,Lit):-
	skolemize(UnitClause,Lit,[],_).

%We have to write out example/3 facts and then compile the predicate!
record_nskolemized(Type,N1,Head,true,ExsStream):-
	!,
	%assertz(example(N1,Type,Head)).
	portray_clause(ExsStream,example(N1,Type,Head)).
record_nskolemized(Type,N1,Head,Body,ExsStream):-
	%assertz((example(N1,Type,Head):-Body)).
	portray_clause(ExsStream,(example(N1,Type,Head):-Body)).


%Replace assertion by writing clause
%Replace goal X by writing goal X, no linking so OK
record_skolemized(Type,N1,SkolemVars,Head,Body,ExsStream):-
	%assertz(example(N1,Type,Head)),
	portray_clause(ExsStream,example(N1,Type,Head)),
	functor(Head,Name,Arity),
	%update_backpreds(Name/Arity),
	(recorded(progol_dyn,backpred(Name/Arity),_) -> true;
	    recordz(progol_dyn,backpred(Name/Arity),_),
	    portray_goal(ExsStream,update_backpreds(Name/Arity))),
	%add_backs(Body),
	(Body = [] -> true;
	portray_goal(ExsStream,add_backs(Body))),
	%add_skolem_types(SkolemVars,Head,Body).
	(SkolemVars = 0 -> true;
	portray_goal(ExsStream,add_skolem_types(SkolemVars,Head,Body))).


add_backs([]).
add_backs([Lit|Lits]):-
	recorda(progol,back(Lit),_),
	functor(Lit,Name,Arity),
	declare_dynamic(Name/Arity),
	assertz(Lit),
	add_backs(Lits).

add_skolem_types(0,_,_):- !.
add_skolem_types(_,Head,Body):-
	add_skolem_types([Head]),
	add_skolem_types(Body).

add_skolem_types([]).
add_skolem_types([Lit|Lits]):-
	functor(Lit,PSym,Arity),
	get_modes(PSym/Arity,L),
	add_skolem_types1(Lit,L),
	add_skolem_types(Lits).

add_skolem_types1(_,[]):- !.
add_skolem_types1(Fact,[Lit|Lits]):-
	split_args(Lit,I,O,C),
	add_skolem_types2(Fact,I),
	add_skolem_types2(Fact,O),
	add_skolem_types2(Fact,C),
	add_skolem_types1(Fact,Lits).

add_skolem_types2(_,[]).
add_skolem_types2(Literal,[ArgNo/Type|Rest]):-
	ArgNo2 is ArgNo + 2,
	arg(ArgNo2,Literal,Arg),
	SkolemType =.. [Type,Arg],
	(recorded(progol,back(SkolemType),_)-> true;
		recorda(progol,back(SkolemType),_),
		asserta(SkolemType)),
	add_skolem_types2(Literal,Rest).


copy_args(_,_,Arg,Arity):-
	Arg > Arity, !.
copy_args(Lit,Lit1,Arg,Arity):-
	arg(Arg,Lit,T),
	arg(Arg,Lit1,T),
	NextArg is Arg + 1,
	copy_args(Lit,Lit1,NextArg,Arity).


index_clause((Head:-true),NextClause,(Head)):-
	!,
	recorded(progol,last_clause(ClauseNum),DbRef1),
	erase(DbRef1),
	NextClause is ClauseNum + 1,
	recorda(progol,last_clause(NextClause),_).
index_clause(Clause,NextClause,Clause):-
	recorded(progol,last_clause(ClauseNum),DbRef1),
	erase(DbRef1),
	NextClause is ClauseNum + 1,
	recorda(progol,last_clause(NextClause),_).

update_backpreds(Name/Arity):-
	recorded(progol_dyn,backpred(Name/Arity),_), !.
update_backpreds(Name/Arity):-
	recordz(progol_dyn,backpred(Name/Arity),_).
	
reset_counts:-
	retractall(sat,last_term(_)),
	retractall(sat,last_var(_)),
	recorda(sat,last_term(0),_),
	recorda(sat,last_var(0),_), !.

% reset the number of successes for a literal: cut to avoid useless backtrack
reset_succ:-
        retractall(progol_dyn,last_success(_)),
        recorda(progol_dyn,last_success(0),_), !.

skolem_var(Var):-
	atomic(Var), !,
	name(Var,[36|_]).
skolem_var(Var):-
	gen_var(Num),
	name(Num,L),
	name(Var,[36|L]).

gen_var(Var1):-
	recorded(sat,last_var(Var0),DbRef), !,
	erase(DbRef),
        Var1 is Var0 + 1,
	recorda(sat,last_var(Var1),_).
gen_var(0):-
	recorda(sat,last_var(0),_).

gen_lit(Lit1):-
	recorded(sat,last_lit(Lit0),DbRef), !,
	erase(DbRef),
        Lit1 is Lit0 + 1,
	recorda(sat,last_lit(Lit1),_).
gen_lit(0):-
	recorda(sat,last_lit(0),_).

gen_refine_id(R1):-
	recorded(refine,last_refine(R0),DbRef), !,
	erase(DbRef),
	R1 is R0 + 1,
	recorda(refine,last_refine(R1),_).
gen_refine_id(0):-
	recorda(refine,last_refine(0),_).

gen_lits([],[]).
gen_lits([Lit|Lits],[LitNum|Nums]):-
	gen_lit(LitNum),
	recorda(lits,lit_info(LitNum,0,Lit,[],[],[]),_),
	gen_lits(Lits,Nums).

update_theory(ClauseIndex):-
        recorded(progol,hypothesis(Label,Hypothesis,PCover,NCover),DbRef), 
        erase(DbRef),
	index_clause(Hypothesis,ClauseIndex,Clause),
        (recorded(progol,example_selected(_,Seed),_)-> true;
                PCover = [Seed-_|_]),
        recordz(progol,theory(ClauseIndex,Label/Seed,Clause,PCover,NCover),_),
        (recorded(progol,rules(Rules),DbRef3)->
                erase(DbRef3),
                recorda(progol,rules([ClauseIndex|Rules]),_);
                recorda(progol,rules([ClauseIndex]),_)),
        assertz(Clause), !.


rm_seeds:-
	update_theory(ClauseIndex), !,
	recorded(progol,theory(ClauseIndex,_,_,PCover,_),_),
	rm_seeds(pos,PCover),
	recorded(progol,atoms_left(pos,PLeft),_),
	interval_count(PLeft,PL),
	p1_message('atoms left'), p_message(PL),
	!.
rm_seeds.

rm_seeds(Type,RmIntervals) :-
        recorded(progol,atoms_left(Type,OldIntervals),DbRef),
        erase(DbRef),
        rm_seeds1(RmIntervals,OldIntervals,NewIntervals),
        recordz(progol,atoms_left(Type,NewIntervals),_).
 
rm_seeds1([],Done,Done).
rm_seeds1([Start-Finish|Rest],OldIntervals,NewIntervals) :-
        rm_interval(Start-Finish,OldIntervals,MidIntervals),!,
        rm_seeds1(Rest,MidIntervals,NewIntervals).


% update lower estimate on maximum size cover set for an atom
update_coverset(Type,_):-
        recorded(progol,hypothesis(Label,_,PCover,_),_),
	Label = [_,_,_,Gain|_],
        worse_coversets(PCover,Type,Gain,Worse),
        (Worse = [] -> true;
                update_theory(NewClause),
                update_coversets(Worse,NewClause,Type,Label)).

% revise coversets of previous atoms
worse_coversets(_,_,_,[]):-
	\+ recorded(progol,set(maxcover,true),_), !.
worse_coversets([],_,_,[]).
worse_coversets([Interval|Intervals],Type,Gain,Worse):-
	worse_coversets1(Interval,Type,Gain,W1),
	worse_coversets(Intervals,Type,Gain,W2),
	append(W1,W2,Worse), !.

worse_coversets1(Start-Finish,_,_,[]):-
        Start > Finish, !.
worse_coversets1(Start-Finish,Type,Gain,Rest):-
        recorded(progol,max_set(Type,Start,Label1,_),_),
	Label1 = [_,_,_,Gain1|_],
        Gain1 >= Gain, !,
        Next is Start + 1,
        worse_coversets1(Next-Finish,Type,Gain,Rest), !.
worse_coversets1(Start-Finish,Type,Gain,[Start|Rest]):-
        Next is Start + 1,
        worse_coversets1(Next-Finish,Type,Gain,Rest), !.

update_coversets([],_,_,_).
update_coversets([Atom|Atoms],ClauseNum,Type,Label):-
	(recorded(progol,max_set(Type,Atom,_,_),DbRef)->
		erase(DbRef);
		true),
	recorda(progol,max_set(Type,Atom,Label,ClauseNum),_),
	update_coversets(Atoms,ClauseNum,Type,Label), !.

rm_intervals([],I,I).
rm_intervals([I1|I],Intervals,Result):-
	rm_interval(I1,Intervals,Intervals1), 
	rm_intervals(I,Intervals1,Result).

rm_interval(_,[],[]).
rm_interval(I1,[Interval|Rest],Intervals):-
	interval_intersection(I1,Interval,I2), !,
	interval_subtract(Interval,I2,I3),
	rm_interval(I1,Rest,I4),
	append(I3,I4,Intervals).
rm_interval(I1,[Interval|Rest],[Interval|Intervals]):-
	rm_interval(I1,Rest,Intervals).

% select N random samples from pos/neg set
% if N = 0 returns first example in pos/neg set
gen_sample(Type,0):-
	!,
	recorded(progol,atoms_left(Type,[ExampleNum-_|_]),_),
	retractall(progol,example_selected(_,_)),
	p1_message('select example'), p_message(ExampleNum),
	recordz(progol,example_selected(Type,ExampleNum),_).
gen_sample(Type,SampleSize):-
	recorded(progol,size(Type,_),_),
	recorded(progol,atoms_left(Type,Intervals),_),
	% p1_message('select from'), p_message(Intervals),
	interval_count(Intervals,AtomsLeft),
	N is min(AtomsLeft,SampleSize),
	recordz(progol_dyn,sample_num(0),_),
	retractall(progol,example_selected(_,_)),
	repeat,
	recorded(progol_dyn,sample_num(S1),DbRef),
	S is S1 + 1,
	(S =< N ->
		get_random(AtomsLeft,INum),
		select_example(INum,0,Intervals,ExampleNum),
		\+ recorded(progol,example_selected(Type,ExampleNum),_),
		p1_message('select example'), p_message(ExampleNum),
		erase(DbRef),
		recordz(progol_dyn,sample_num(S),_),
		recordz(progol,example_selected(Type,ExampleNum),_),
		fail;
		erase(DbRef)), !.

select_example(Num,NumberSoFar,[Start-Finish|_],ExampleNum):-
	Num =< NumberSoFar + Finish - Start + 1, !,
	ExampleNum is Num - NumberSoFar + Start - 1.
select_example(Num,NumberSoFar,[Start-Finish|Rest],ExampleNum):-
	N1 is NumberSoFar + Finish - Start + 1,
	select_example(Num,N1,Rest,ExampleNum).
	
get_random(Last,INum):-
	%X is random,
	random(X),
	INum1 is integer(round(X*Last)),
	%Num is X*Last,
	%real_to_int(Num,INum1),
	(INum1 = 0 -> INum = 1; INum = INum1).



once(P):- P, !.

% dummy pre-defined example to force compilation
% this doesn't work for Sicstus
%example('$$dummy','$$dummy','$$dummy').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% L A B E L S
%
% calculation on labels

label_create(Clause,Label):-
        split_clause(Clause,Head,Body),
	nlits((Head,Body),Length),
        recordz(pclause,pclause(Head,Body),DbRef),
        recorded(progol,atoms(pos,Pos),_),
        recorded(progol,atoms(neg,Neg),_),
        %recorded(progol,set(depth,Depth),_),
        %progol_prove(_Depth,pos,(Head:-Body),Pos,PCover,_),
	progol_prove2(Pos,pos,(Head:-Body),0,PCover,_),
        %progol_prove(_Depth,neg,(Head:-Body),Neg,NCover,_),
	progol_prove2(Neg,neg,(Head:-Body),0,NCover,_),
        erase(DbRef),
        assemble_label(PCover,NCover,Length,Label), !.

label_create(pos,Clause,Label):-
        split_clause(Clause,Head,Body),
        recordz(pclause,pclause(Head,Body),DbRef),
        recorded(progol,atoms(pos,Pos),_),
        %recorded(progol,set(depth,Depth),_),
        %progol_prove(_Depth,pos,(Head:-Body),Pos,PCover,_),
	progol_prove2(Pos,pos,(Head:-Body),0,PCover,_),
        erase(DbRef),
        assemble_label(PCover,unknown,unknown,Label).
label_create(neg,Clause,Label):-
        split_clause(Clause,Head,Body),
        recordz(pclause,pclause(Head,Body),DbRef),
        recorded(progol,atoms(neg,Neg),_),
        %recorded(progol,set(depth,Depth),_),
        %progol_prove(_Depth,neg,(Head:-Body),Neg,NCover,_),
	progol_prove2(Neg,neg,(Head:-Body),0,NCover,_),
        erase(DbRef),
        assemble_label(unknown,NCover,unknown,Label).

label_pcover(Label,P):-
	extract_cover(pos,Label,P).
label_ncover(Label,N):-
	extract_cover(neg,Label,N).

label_union([],Label,Label):- !.
label_union(Label,[],Label):- !.
label_union(Label1,Label2,Label):-
        extract_cover(pos,Label1,Pos1),
        extract_cover(pos,Label2,Pos2),
        extract_cover(neg,Label1,Neg1),
        extract_cover(neg,Label2,Neg2),
        extract_length(Label1,L1),
        extract_length(Label2,L2),
        update_list(Pos2,Pos1,Pos),
        update_list(Neg2,Neg1,Neg),
        Length is L1 + L2,
        list_to_intervals(Pos,PCover),
        list_to_intervals(Neg,NCover),
        assemble_label(PCover,NCover,Length,Label).


label_print([]):- !.
label_print(Label):-
	(setting(evalfn,Eval)->true;Eval=coverage),
	evalfn(Eval,Label,Val),
	print_eval(Eval,Val).

print_eval(Evalfn,Val):-
	evalfn_name(Evalfn,Name),
	p1_message(Name), p_message(Val).


eval_rule(0,Label):-
	recorded(progol,hypothesis(_,Clause,_,_),_), !,
	label_create(Clause,Label),
	p_message('Rule 0'),
	pp_dclause(Clause),
	extract_count(pos,Label,PC),
	extract_count(neg,Label,NC),
	extract_length(Label,L),
	label_print([PC,NC,L]),
	nl.
eval_rule(ClauseNum,Label):-
	integer(ClauseNum),
	ClauseNum > 0,
	recorded(progol,theory(ClauseNum,_,Clause,_,_),_),
	!,
	label_create(Clause,Label),
	extract_count(pos,Label,PC),
	extract_count(neg,Label,NC),
	extract_length(Label,L),
	concat(['Rule ',ClauseNum],RuleTag),
	concat(['Pos cover = ',PC,' Neg cover = ',NC],CoverTag),
	p1_message(RuleTag), p_message(CoverTag),
	pp_dclause(Clause),
	label_print([PC,NC,L]),
	nl.
eval_rule(_,_).


evalfn(Label,Val):-
	(setting(evalfn,Eval)->true;Eval=coverage),
	evalfn(Eval,Label,Val).

evalfn_name(compression,'compression').
evalfn_name(coverage,'pos-neg').
evalfn_name(laplace,'laplace estimate').
evalfn_name(pbayes,'pseudo-bayes estimate').
evalfn_name(auto_m,'m estimate').
evalfn_name(mestimate,'m estimate').
evalfn_name(posonly,'posonly bayes estimate').

evalfn(compression,[P,N,L],Val):-
	(P = -inf -> Val = -10000.0;
        	Val is P - N - L + 1), !.
evalfn(coverage,[P,N,_L],Val):-
	(P = -inf -> Val = -10000;
		Val is P - N), !.
evalfn(laplace,[P,N,_L],Val):-
	(P = -10000 -> Val = 0.5;
		Val is (P + 1) / (P + N + 2)), !.
% the evaluation functions below are due to James Cussens
evalfn(pbayes,[P,N,_L],Val):-
        (P = -10000 -> Val = 0.5;
                Acc is P/(P+N),
                setting(prior,Prior),
                K is (Acc*(1 - Acc)) / ((Prior-Acc)^2 ),
                Val is (P + K*Prior) / (P + N + K)), !.
evalfn(MEst,[P,N,_L],Val):-
        (MEst = auto_m; MEst = mestimate),
        (P = -10000 -> Val = 0.5;
                Cover is P + N,
                setting(prior,Prior),   
                (MEst = auto_m -> K is sqrt(Cover);
                        (setting(m,M) -> K = M; K is sqrt(Cover))),
                Val is (P + K*Prior) / (Cover+K)), !.
evalfn(_,_,-10000).


assemble_label(P,N,L,[P,N,L]).

extract_cover(pos,[P,_,_],P1):-
        intervals_to_list(P,P1), !.
extract_cover(neg,[_,N,_],N1):-
        intervals_to_list(N,N1),!.
extract_cover(_,[]).

extract_count(pos,[P,_,_],P1):-
	interval_count(P,P1), !.
extract_count(neg,[_,N,_],N1):-
	interval_count(N,N1), !.
extract_count(neg,_,0).


extract_pos([P,_,_],P).
extract_neg([_,N,_],N).
extract_length([_,_,L],L).

get_start_label(user,[1,0,2,-10000]):- !.
get_start_label(posonly,[1,0,2,-1,0]):- !.
get_start_label(Evalfn,[1,0,2,Val]):-
	evalfn(Evalfn,[1,0,2],Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% L I B R A R Y

progol_help:-
        progol_help(help).
progol_help(Topic):-
        !,
        progol_help_dir(HelpDir),
        concat(['more ',HelpDir,'/',Topic],X),
        nl,
        execute(X),
        nl.

progol_portray(hypothesis):-
	hypothesis(Head,Body,_),
	Body,
	portray((Head:-Body)),
	write('Continue portray [y. or n.])? '), read(n), !.
progol_portray(hypothesis):- !.

progol_portray(X):-
	recorded(progol,set(pretty_print,true),_),
	recorded(progol,text(X,Text),_), !,
        print_text(Text).
progol_portray(X):-
        writeq(X).

progol_portray(Pretty,X):-
        (Pretty = true ->
                recorded(progol,text(X,Text),_),
                print_text(Text);
                writeq(X)).



execute(C):-
	system(C), !.
execute(_).

store(Variable):-
	(recorded(progol,set(Variable,Value),_) -> true; Value = unknown),
	retractall(progol,save(Variable,_)),
	recorda(progol,save(Variable,Value),_).

reinstate(Variable):-
	recorded(progol,save(Variable,Value),DbRef), !,
	erase(DbRef),
	(Value = unknown -> noset(Variable); set(Variable,Value)).
reinstate(_).

set(Variable,Value):-
	var(Value), !,
	recorded(progol,set(Variable,Value),_).
set(Variable,Value):-
	noset(Variable),
	recordz(progol,set(Variable,Value),_),
	special_consideration(Variable,Value).

setting(Variable,Value):-
	recorded(progol,set(Variable,Value),_).

noset(Variable):-
        recorded(progol,set(Variable,Value),DbRef), !,
	store(Variable),
	erase(DbRef), 
	rm_special_consideration(Variable,Value).
noset(_).

text(Literal,Text):-
	retractall(progol,text(Literal,_)),
	recordz(progol,text(Literal,Text),_).

man(M):-
	progol_manual(M).

determination(Pred1,Pred2):-
	recordz(progol,determination(Pred1,Pred2),_).

commutative(Pred):-
	recordz(progol,commutative(Pred),_).

symmetric(Pred):-
	set(check_symmetry,true),
	recordz(progol,symmetric(Pred),_).

lazy_evaluate(Name/Arity):-
        recordz(progol,lazy_evaluate(Name/Arity),_).

check_implication(Name/Arity):-
	recordz(progol,check_implication(Name/Arity),_),
	declare_dynamic(Name/Arity).

positive_only(Pred):-
	recordz(progol,positive_only(Pred),_).

mode(Recall,Pred):-
	recordz(progol,mode(Recall,Pred),_).
modeh(Recall,Pred):-
	recordz(progol,modeh(Recall,Pred),_),
	recordz(progol,mode(Recall,Pred),_).
modeb(Mode,Pred):-
	recordz(progol,mode(Mode,Pred),_).

show(settings):-
	p_message('settings'),
	findall(P-V,setting(P,V),L),
	sort(L,L1),
	member(Parameter-Value,L1),
        tab(8), write(Parameter=Value), nl,
        fail.
show(determinations):-
	p_message('determinations'),
	show1(progol,determination(_,_)).
show(modes):-
	p_message('modes'),
	show1(progol,mode(_,_)).
show(sizes):-
	p_message('sizes'),
	show1(progol,size(_,_)).
show(bottom):-
	p_message('bottom clause'),
	set(verbosity,V),
	V > 0,
	recorded(sat,last_lit(Last),_),
	get_clause(1,Last,[],FlatClause),
	pp_dlist(FlatClause).
show(hypothesis):-
	p_message('hypothesis'),
        recorded(progol,hypothesis(_,Clause,_,_),_),
	pp_dclause(Clause).
show(theory):-
        nl,
        p_message('theory'),
        nl,
        recorded(progol,rules(L),_),
        reverse(L,L1),
        member(ClauseNum,L1),
	recorded(progol,theory(ClauseNum,_,_,_,_),_),
	eval_rule(ClauseNum,_),
	% pp_dclause(Clause),
        fail.
show(theory):-
	get_performance.
show(pos):-
	p_message('positives'),
	store(greedy),
	examples(pos,_),
	reinstate(greedy),
	fail.
show(neg):-
	p_message('negatives'),
	store(greedy),
	examples(neg,_),
	reinstate(greedy),
	fail.
show(rand):-
	p_message('random'),
	examples(rand,_),
	fail.
show(imap):-
	p_message('implication map'),
	recorded(sat,implied_by(Lit,Lits),_),
	member(Lit1,Lits),
	get_pclause([Lit,Lit1],[],Clause,_,_,_),
	pp_dclause(Clause),
	fail.
show(prior):-
	p_message('refinement priors'),
	beta(Refine,A,B),
	%copy(Refine,Refine1),
	%numbervars(Refine1,0,_),
	%write(beta(Refine1,A,B)), write('.'), nl,
	portray_clause(beta(Refine,A,B)),
	fail.
show(posterior):-
	p_message('refinement posterior'),
	recorded(refine,beta(R,A,B),_),
	recorded(refine,refine_id(Refine,R),_),
	%copy(Refine,Refine1),
	%numbervars(Refine1,0,_),
	%write(beta(Refine1,A,B)), write('.'), nl,
	portray_clause(beta(Refine,A,B)),
	fail.
show(_).

settings:-
	show(settings).


examples(Type,List):-
        example(Num,Type,Atom),
        %member1(Num,List),
	memberchk(Num,List),
        %write(Atom), write('.'), nl,
	portray_clause(Atom),
        fail.
examples(_,_).

write_rules(File):-
        open(File,write,Stream),
        set_output(Stream),
	show(theory),
        close(Stream),
        set_output(user_output).
write_rules(_).

best_hypothesis(Head1,Body1,[P,N,L]):-
	recorded(search,selected([P,N,L|_],Clause,_,_),_),
	split_clause(Clause,Head2,Body2), !,
	Head1 = Head2, Body1 = Body2.

hypothesis(Head1,Body1,Label):-
	recorded(pclause,pclause(Head2,Body2),_), !,
	Head1 = Head2, Body1 = Body2,
	get_hyp_label((Head2:-Body2),Label).
hypothesis(Head1,Body1,Label):-
        recorded(progol,hypothesis(_,Clause,_,_),_),
	split_clause(Clause,Head2,Body2), !,
	Head1 = Head2, Body1 = Body2,
	get_hyp_label((Head2:-Body2),Label).

get_hyp_label(_,Label):- var(Label), !.
get_hyp_label((_:-Body),[P,N,L]):-
	nlits(Body,L1),
	L is L1 + 1,
	(recorded(progol_dyn,covers(_,P),_)-> true;
			covers(_),
			recorded(progol_dyn,covers(_,P),_)),
	(recorded(progol_dyn,coversn(_,N),_)-> true;
			coversn(_),
			recorded(progol_dyn,coversn(_,N),_)).

rdhyp:-
	retractall(pclause,pclause(_,_)),
	retractall(progol_dyn,covers(_)),
        read(Clause),
        add_hyp(Clause),
        nl,
        show(hypothesis).

addhyp:-
        recorded(progol,hypothesis(Label,_,PCover,_),_), !,   
        rm_seeds,
        worse_coversets(PCover,pos,Label,Worse),
        recorded(progol,last_clause(NewClause),_),
        (Worse = [] -> true;
                update_coversets(Worse,NewClause,pos,Label)), !.

addhyp:-
        recorded(search,selected(Label,RClause,PCover,NCover),_), !,
        add_hyp(Label,RClause,PCover,NCover),
        rm_seeds,
        worse_coversets(PCover,pos,Label,Worse),
        recorded(progol,last_clause(NewClause),_),
        (Worse = [] -> true;
                update_coversets(Worse,NewClause,pos,Label)), !.
	
covers:-
        get_hyp(Hypothesis),
        label_create(Hypothesis,Label),
        extract_cover(pos,Label,P),
        examples(pos,P),
	length(P,PC),
	p1_message('examples covered'),
	p_message(PC),
	retractall(progol_dyn,covers(_,_)),
	recorda(progol_dyn,covers(P,PC),_).
coversn:-
        get_hyp(Hypothesis),
        label_create(Hypothesis,Label),
        extract_cover(neg,Label,N),
        examples(neg,N),
	length(N,NC),
	p1_message('examples covered'),
	p_message(NC),
	retractall(progol_dyn,coversn(_,_)),
	recorda(progol_dyn,coversn(N,NC),_).


covers(P):-
	get_hyp(Hypothesis),
	label_create(pos,Hypothesis,Label),
	retractall(progol_dyn,covers(_,_)),
	extract_pos(Label,PCover),
	interval_count(PCover,P),
	recorda(progol_dyn,covers(PCover,P),_).

coversn(N):-
	get_hyp(Hypothesis),
	label_create(neg,Hypothesis,Label),
	retractall(progol_dyn,coversn(_,_)),
	extract_neg(Label,NCover),
	interval_count(NCover,N),
	recorda(progol_dyn,coversn(NCover,N),_).

mincovers(Min):-
	get_hyp(Hypothesis),
	retractall(progol_dyn,covers(_,_)),
	progol_prove_at_least(pos,Hypothesis,Min,PCover,P),
	recorda(progol_dyn,covers(PCover,P),_).
mincoversn(Min):-
	get_hyp(Hypothesis),
	retractall(progol_dyn,coversn(_,_)),
	progol_prove_at_least(neg,Hypothesis,Min,NCover,N),
	recorda(progol_dyn,coversn(NCover,N),_).
maxcovers(Max):-
	get_hyp(Hypothesis),
	retractall(progol_dyn,covers(_,_)),
	progol_prove_at_most(pos,Hypothesis,Max,PCover,P),
	recorda(progol_dyn,covers(PCover,P),_).
maxcoversn(Max):-
	get_hyp(Hypothesis),
	retractall(progol_dyn,coversn(_,_)),
	progol_prove_at_most(neg,Hypothesis,Max,NCover,N),
	recorda(progol_dyn,coversn(NCover,N),_).

example_saturated(Example):-
	recorded(sat,sat(Num,Type),_),
	example(Num,Type,Example).

reset:-
        clean_up,
        retractall(cache,_),
        retractall(prune_cache,_),
        set(stage,command),
	set(construct_bottom,saturation),
	set(refineop,false),
        set(lazy_on_cost,false),
        set(nodes,5000),
        set(samplesize,1),
        set(minpos,1),
        set(gsamplesize,100),
        set(clauselength,4),
        set(explore,false),
        set(caching,false),
        set(greedy,false),
        set(refine,false),
        set(search,bf),
        set(prune_defs,false),
        set(evalfn,coverage),
        set(depth,5),
        set(verbosity,1),
        set(i,2),
        set(noise,0),
        set(print,4).


% Auxilliary definitions needed for above

special_consideration(noise,_):-
        noset(minacc), !.
special_consideration(minacc,_):-
        noset(noise), !.
% the following needed for compatibility with earlier versions
special_consideration(search,ida):-
	set(search,bf), set(evalfn,coverage), !.
special_consideration(search,compression):-
	set(search,heuristic), set(evalfn,compression), !.
special_consideration(search,posonly):-
	set(search,heuristic), set(evalfn,posonly), !.
special_consideration(search,user):-
	set(search,heuristic), set(evalfn,user), !.

special_consideration(caching,true):-
	(setting(cache_clauselength,_)->true;set(cache_clauselength,3)).

special_consideration(search,bf):-
	(setting(evalfn,_) -> true; set(evalfn,coverage)), !.
special_consideration(search,df):-
	(setting(evalfn,_) -> true; set(evalfn,coverage)), !.
special_consideration(search,heuristic):-
	(setting(evalfn,_)->true; set(evalfn,compression)), !.
special_consideration(evalfn,coverage):-
	(setting(search,_)->true; set(search,bf)).
special_consideration(evalfn,S):-
	(setting(search,_)->true; set(search,heuristic)),
	(S = posonly -> noset(noise);
		recorded(progol,atoms(neg,NegE),_),
		recorded(progol,atoms_left(neg,[]),DbRef),
		erase(DbRef),
		reinstate(noise),
		recorda(progol,atoms_left(neg,NegE),_)).
special_consideration(refine,user):-
	set(refineop,true), !.
special_consideration(refine,auto):-
	gen_refine, !.
special_consideration(refine,probabilistic):-
	set(caching,true),
	set(refineop,true),
	gen_refine, !.
special_consideration(construct_bottom,false):-
	set(refineop,true), !.
special_consideration(construct_bottom,reduction):-
	(setting(lazy_bottom,true) -> true; set(lazy_bottom,true)), !.
special_consideration(construct_bottom,saturation):-
	noset(lazy_bottom), !.
special_consideration(lazy_bottom,true):-
	(setting(construct_bottom,false) -> true; set(construct_bottom,reduction)), !.
special_consideration(lazy_bottom,false):-
	(setting(construct_bottom,false) -> true; set(construct_bottom,saturation)), !.
special_consideration(opt_debug,N):-
        set(verbosity,N), !.
special_consideration(pretty_print,true):-
	set(print,1), !.
special_consideration(_,_).

rm_special_consideration(caching,true):-
	noset(cache_clauselength), !.
rm_special_consideration(pretty_print,_):-
	set(print,4), !.
rm_special_consideration(opt_debug,_):-
	noset(verbosity), !.
rm_special_consideration(refine,_):-
	set(refineop,false), !.
rm_special_consideration(lazy_bottom,true):-
	(setting(refine,auto) -> set(refine,false); true), !.
rm_special_consideration(_,_).

show(Area,Name/Arity):-
        functor(Pred,Name,Arity),
        show1(Area,Pred).
show(_,_).

get_hyp((Head:-Body)):-
	recorded(pclause,pclause(Head,Body),_), !.
get_hyp(Hypothesis):-
        recorded(progol,hypothesis(_,Hypothesis,_,_),_).

add_hyp(end_of_file):- !.
add_hyp(Clause):-
        nlits(Clause,L),
	label_create(Clause,Label),
        extract_count(pos,Label,PCount),
        extract_count(neg,Label,NCount),
        Label1 = [PCount,NCount,L],
        retractall(progol,hypothesis(_,_,_,_)),
        extract_pos(Label,P),
        extract_neg(Label,N),
        recorda(progol,hypothesis(Label1,Clause,P,N),_).

add_hyp(Label,Clause,P,N):-
        retractall(progol,hypothesis(_,_,_,_)),
        recorda(progol,hypothesis(Label,Clause,P,N),_).

rmhyp:-
	recorded(pclause,pclause(Head,Body),DbRef),
	erase(DbRef),
	recorda(progol_dyn,tmpclause(Head,Body),_), !.
rmhyp:-
        recorded(progol,hypothesis(Label,Clause1,P,N),DbRef),
	erase(DbRef),
	recorda(progol_dyn,tmphypothesis(Label,Clause1,P,N),_), !.
rmhyp.

restorehyp:-
	recorded(progol_dyn,tmpclause(Head,Body),DbRef),
	erase(DbRef),
	recordz(pclause,pclause(Head,Body),_), !.
restorehyp:-
	recorded(progol_dyn,tmphypothesis(Label,Clause1,P,N),DbRef),
	erase(DbRef),
        recorda(progol,hypothesis(Label,Clause1,P,N),_), !.
restorehyp.

show1(Area,Pred):-
        recorded(Area,Pred,_),
        %copy(Pred,Pred1), numbervars(Pred1,0,_),
        %write(Pred1), write('.'), nl,
	portray_clause(Pred),
        fail.
show1(_,_).

clear_hyp:-
        retractall(progol,hypothesis(_,_,_,_)).


time(P,N,AvTime):-
        %Start is cputime,
	sicstus_statistics(runtime,[Start,_]),
        time_loop(N,P),
        %Stop is cputime,
        sicstus_statistics(runtime,[Stop,_]),
	Time is Stop - Start,
	AvTime is Time/N.
        
 
time_loop(0,_):- !.
time_loop(N,P):-
        P,
        N1 is N - 1,
        time_loop(N1,P).

list_profile :-
	% get number of calls for each profiled procedure
	%findall(D-P,profile_data(P,calls,D),LP),
	profile_data(_,calls,predicate,LP),
	flip(LP,[],FLP),
	% sort them
	keysort(FLP,SLP),
	% and output (note the most often called predicates will come last
	write_profile_data(SLP).

list_profile(Preds,S,R) :-
	% get number of calls for each profiled procedure
	%findall(D-P,profile_data(P,calls,D),LP),
	profile_data(Preds,S,R,LP),
	flip(LP,[],FLP),
	% sort them
	keysort(FLP,SLP),
	% and output (note the most often called predicates will come last
	write_profile_data(SLP).


flip([],Done,Done).
flip([A-B|T],In,Done) :-
	flip(T,[B-A|In],Done).

write_profile_data([]).
	write_profile_data([D-P|SLP]) :-
	% just swap the two calls to get most often called predicates first.
	format('~w: ~w~n', [P,D]),
	write_profile_data(SLP).

test(File,Flag,N,T):-
	retractall(progol_dyn,covered(_)),
	retractall(progol_dyn,total(_)),
	recorda(progol_dyn,covered(0),_),
	recorda(progol_dyn,total(0),_),
	open(File,read,Stream),
	repeat,
	read(Stream,Fact),
	(Fact = end_of_file -> close(Stream);
		recorded(progol_dyn,total(T0),DbRef),
		erase(DbRef),
		T1 is T0 + 1,
		recorda(progol_dyn,total(T1),_),
		(once(Fact) ->
			(Flag = show ->
				p1_message(covered),
				progol_portray(Fact),
				nl;
				true);
			(Flag = show ->
				p1_message('not covered'),
				progol_portray(Fact),
				nl;
				true),
			fail),
		recorded(progol_dyn,covered(N0),DbRef1),
		erase(DbRef1),
		N1 is N0 + 1,
		recorda(progol_dyn,covered(N1),_),
		fail),
	!,
	recorded(progol_dyn,covered(N),DbRef2),
	erase(DbRef2),
	recorded(progol_dyn,total(T),DbRef3),
	erase(DbRef3).

:- progol_version(V), set(version,V), reset.



































/*===================================================================
% Clause Grammer

Parsing SUMO/KIF Chars to a Prolog Term: 'Assertionpterm' placing variables in 'ClauseVariables'

Notice first converted the SForm a which is a more expressive format for compilation

Examples:

| ?- conv_to_pterm("(isa a b)",Clause,Vars).
Clause = isa(a,b)
Vars = _h70

| ?- conv_to_pterm("(thereExists (?a ?b) (isa ?a ?b))",Clause,Vars).
Clause = thereExists(_h1381(_h1388),'surface-instance'(_h1381,_h1388))
Vars = [=(a,_h1381),=(b,_h1388)|_h1696]

| ?- conv_to_pterm("(thereExists (list ?a ?b) (isa ?a ?b))",Clause,Vars).
Clause = thereExists([_h1444,_h1451],'surface-instance'(_h1444,_h1451))
Vars = [=(a,_h1444),=(b,_h1451)|_h1779]

| ?- conv_to_pterm("; This is a comment",Clause,Vars).
Clause = comment('; This is a comment')
Vars = _h70


All other not parsable KIF expressions show to be Comments

| ?- conv_to_pterm("cannot be parsed",Clause,Vars).
Clause = comment('cannot be parsed')
Vars = _h70                                      

              
Most SUMO works as well
              
| ?- conv_to_pterm("(genlMt A B)",Clause,Vars).
Clause = genlMt('A','B')
Vars = _h70

More SUMO:
| ?- conv_to_pterm("(genlMt A ?B)",Clause,Vars).
Clause = genlMt('A',_h1002)
Vars = [=('B',_h1002)|_h1105]

====================================================================*/

% ===================================================================
% add_kb_context(HB,DynStat,KB,Ctx,Prolog,NewProlog)  Converts And/Or/Implies terms to Proper Prolog
% ====================================================================
add_kb_context(HB,DynStat,KB,Ctx,true,true):-!.
add_kb_context(h,DynStat,KB,Ctx,In,Out):- !,
          krlog_to_prolog(In,InP),              !,
         add_kb_context_p(HB,DynStat,KB,Ctx,InP,Out).
add_kb_context(b,DynStat,KB,Ctx,In,Out):-!,
         krlog_to_prolog(In,InP),              !,
         add_kb_context_p(HB,DynStat,KB,Ctx,InP,Out).

add_kb_context_p(HB,DynStat,_,_,Var,Var):-var(Var),!.
add_kb_context_p(HB,DynStat,_,_,'$VAR'(N),'$VAR'(N)):-!.

add_kb_context_p(b,DynStat,KB,Ctx,not('equal'(T1,T2)),( T1 \== T2 )):-!. 
add_kb_context_p(h,DynStat,KB,Ctx,not('equal'(T1,T2)),nop):-!. 

add_kb_context_p(HB,DynStat,KB,Ctx, ':-'(A) , ':-'(AA)):-  !,
            add_kb_context_p(HB,DynStat,KB,Ctx,A,AA).


add_kb_context_p(HB,DynStat,KB,Ctx, ','(A,B) , (AA,BB)):-  !,
            add_kb_context_p(HB,DynStat,KB,Ctx,A,AA),
            add_kb_context_p(HB,DynStat,KB,Ctx,B,BB).

add_kb_context_p(HB,DynStat,KB,Ctx, '/'(A,B) , '/'(AA,BB)):-  !,
            add_kb_context_p(HB,DynStat,KB,Ctx,A,AA),
            add_kb_context_p(HB,DynStat,KB,Ctx,B,BB).

add_kb_context_p(HB,DynStat,KB,Ctx,(A ; B),(AA ; BB)):-  !,
            add_kb_context_p(HB,DynStat,KB,Ctx,A,AA),
            add_kb_context_p(HB,DynStat,KB,Ctx,B,BB).

add_kb_context_p(HB,DynStat,KB,Ctx,(A:-B),(AA:-BB)):-  !,
            add_kb_context_p(h,DynStat,KB,Ctx,A,AA),
            add_kb_context_p(b,DynStat,KB,Ctx,B,BB).

add_kb_context_p(HB,DynStat,KB,Ctx,not(not(TERM)),POS):- !,
         add_kb_context_p(HB,DynStat,KB,Ctx,TERM,POS).

add_kb_context_p(HB,DynStat,KB,Ctx, Num , Num):-  number(Num),!.

add_kb_context_p(HB,DynStat,KB,Ctx, holds , KBCtx):-
         atom_concat(KB,'_',KBU),
         atom_concat(KBU,Ctx,KBCtx),!.

add_kb_context_p(HB,DynStat,KB,Ctx, apply_dyn , DKBCtx):-
         atom_concat(KB,'_',KBU),
         atom_concat(KBU,Ctx,KBCtx),
         atom_concat(KBCtx,'_dyn',DKBCtx),!.

add_kb_context_p(HB,DynStat,KB,Ctx, neg_apply , KBCtx):-
         atom_concat('neg_',KB,KBN),
         atom_concat(KBN,'_',KBU),
         atom_concat(KBU,Ctx,KBCtx),!.

add_kb_context_p(HB,DynStat,KB,Ctx, neg_apply_dyn , DKBCtx):-
         atom_concat('neg_',KB,KBN),
         atom_concat(KBN,'_',KBU),
         atom_concat(KBU,Ctx,KBCtx),
         atom_concat(KBCtx,'_dyn',DKBCtx),!.


add_kb_context_p(HB,DynStat,KB,Ctx, Term , Out):-  
            Term=..[F|ARGS], 'explore_predicate'(F,_),!,
            add_kb_context_l(HB,DynStat,KB,Ctx,ARGS,OARGS),
            Out=..[F|OARGS].

add_kb_context_p(HB,DynStat,KB,Ctx,P,NP):-is_kernel_based(P,NF),!,P=..[F|ARGS],NP=..[NF|ARGS].
%add_kb_context_p(HB,stat,KB,Ctx,not(P),(ground(NP),'t not'(NP))):-is_kernel_based(P,NF),!,P=..[F|ARGS],NP=..[NF|ARGS].
 
add_kb_context_p(h,DynStat,KB,Ctx,not(TERM),neg(AAA)):-
         TERM=..[F|ARGS],
         compile_w_add(F),!,
         atom_concat(KB,'_',KBU),
         atom_concat(KBU,Ctx,KBCtx),
         atom_concat('neg_',F,FN),
         dyn_StatArray(DynStat,DynStatArray),
         atom_concat(FN,DynStatArray,FNS),
         NTERM=..[FNS|ARGS],
         suffix_op(NTERM,AAA,KBCtx).

add_kb_context_p(b,DynStat,KB,Ctx,not(TERM),not(AAA)):-
         TERM=..[F|ARGS],
         compile_w_add(F),!,
         atom_concat(KB,'_',KBU),
         atom_concat(KBU,Ctx,KBCtx),
         atom_concat('neg_',F,FN),
         dyn_StatArray(DynStat,DynStatArray),
         atom_concat(FN,DynStatArray,FNS),
         NTERM=..[FNS|ARGS],
         suffix_op(NTERM,AAA,KBCtx).

add_kb_context_p(b,DynStat,KB,Ctx,neg(TERM),not(AAA)):-
         TERM=..[F|ARGS],
         compile_w_add(F),!,
         atom_concat(KB,'_',KBU),
         atom_concat(KBU,Ctx,KBCtx),
         atom_concat('neg_',F,FN),
         dyn_StatArray(DynStat,DynStatArray),
         atom_concat(FN,DynStatArray,FNS),
         NTERM=..[FNS|ARGS],
         suffix_op(NTERM,AAA,KBCtx).

add_kb_context_p(h,DynStat,KB,Ctx,not(TERM),neg(NEG)):- 
         add_kb_context_p(HB,DynStat,KB,Ctx,TERM,POS),!,
         POS=..[F|ARGS],
         atom_concat('neg_',F,FN),
         NEG=..[FN|ARGS].

add_kb_context_p(b,DynStat,KB,Ctx,not(TERM),not(NEG)):- 
         add_kb_context_p(HB,DynStat,KB,Ctx,TERM,POS),!,
         POS=..[F|ARGS],
         atom_concat('neg_',F,FN),
         NEG=..[FN|ARGS].

add_kb_context_p(b,DynStat,KB,Ctx,neg(TERM),not(NEG)):- 
         add_kb_context_p(HB,DynStat,KB,Ctx,TERM,POS),!,
         POS=..[F|ARGS],
         atom_concat('neg_',F,FN),
         NEG=..[FN|ARGS].


add_kb_context_p(HB,DynStat,KB,Ctx,TERM,AAA):- 
         TERM=..[F|ARGS],
         compile_w_add(F),!,
         atom_concat(KB,'_',KBU),
         atom_concat(KBU,Ctx,KBCtx),
         dyn_StatArray(DynStat,DynStatArray),
         atom_concat(KBCtx,DynStatArray,FNS),
         suffix_op(TERM,AAA,FNS).

add_kb_context_p(HB,DynStat,KB,Ctx,A,AAA):- 
          prolog_to_hilog(A,AA,KB),
          dyn_StatArray(DynStat,DynStatArray),
          atom_concat(Ctx,DynStatArray,FNS),
          suffix_op(AA,AAA,FNS),!.


dyn_StatArray(stat,'').
dyn_StatArray(dyn,'').
%dyn_StatArray(dyn,'_dyn').
dyn_StatArray(sen,'_sen').
                                                           
add_kb_context_l(HB,DynStat,KB,Ctx,[],[]).
add_kb_context_l(HB,DynStat,KB,Ctx,[H|T],[HO|TO]):- !,
            add_kb_context(HB,DynStat,KB,Ctx,H,HO),
            add_kb_context_l(HB,DynStat,KB,Ctx,T,TO).
            

/*
conv_to_pterm(ClauseChars,Assertionpterm,_ClauseVariables):-!,
         once(parse_to_sterm(ClauseChars,AssertionSTerm,_ClauseVariables)),
         once(sterm_to_pterm(AssertionSTerm,Assertionpterm)).

*/
/*===================================================================
% Clause Grammer

Parsing KIF Chars to a S-Expression: 'Clause' placing variables in 'ClauseVariables'

|?-conv_to_pterm("(isa a b)",Clause,Vars).
Clause = [isa,a,b]
Vars = _h70

| ?- conv_to_sterm("(isa a (b))",Clause,Vars).
Clause = [isa,a,[b]]
Vars = _h70

|?-conv_to_sterm("(list a b )",Clause,Vars)
Clause = [list,a,b]
Vars = _h70

| ?- conv_to_sterm("(genlMt A ?B)",Clause,Vars).
Clause = [genlMt,'A',_h998]
Vars = [=('B',_h998)|_h1101]

| ?- conv_to_sterm("(goals Iran  (not   (exists   (?CITIZEN)   (and    (citizens Iran ?CITIZEN)    (relationExistsInstance maleficiary ViolentAction ?CITIZEN
)))))",Clause,Vars).

Clause = [goals,Iran,[not,[exists,[_h2866],[and,[citizens,Iran,_h2866],[relationExistsInstance,maleficiary,ViolentAction,_h2866]]]]]
Vars = [=(CITIZEN,_h2866)|_h3347]

====================================================================*/
     /*
conv_to_sterm(ClauseChars,Clause,ClauseVariables):-isCharCodelist(ClauseChars),!,parse_to_sterm(ClauseChars,Clause,ClauseVariables),!.
conv_to_sterm(PTerm,Clause,ClauseVariables):-!,pterm_to_sterm(PTerm,Clause),!.
       */
parse_to_sterm(Atom,Atom,[_]):-atomic(Atom),!.
parse_to_sterm(Var,Var,[_]):-var(Var),!.
parse_to_sterm(kif(KIFChars),Clause,_ClauseVariables):-!,getSurfaceFromChars(KIFChars,Clause,_ClauseVariables).
parse_to_sterm(cycl(CYCChars),Clause,_ClauseVariables):-!,getSurfaceFromChars(CYCChars,Clause,_ClauseVariables).
parse_to_sterm(prolog(ClauseChars),Clause,_ClauseVariables):-!,chars_to_term(ClauseChars,Clause,_ClauseVariables).
parse_to_sterm(ClauseChars,Clause,_ClauseVariables):-isCharCodelist(ClauseChars),!,atom_codes(ClauseA,ClauseChars),atom_codes(ClauseA,ClauseASCII),guess_char_format(ClauseASCII,Clause,_ClauseVariables),!.
parse_to_sterm([Clause],AssertionSexpress,_ClauseVariables):-compound(Clause),not(is_list(Clause)),!,pterm_to_sterm(Clause,AssertionSexpress).
parse_to_sterm([SomeClause],Clause,_ClauseVariables):-!,parse_to_sterm(SomeClause,Clause,_ClauseVariables),!.
parse_to_sterm(Clause,Clause,_ClauseVariables):-is_list(Clause),!.
parse_to_sterm(Clause,SClause,_ClauseVariables):-!,pterm_to_sterm(Clause,SClause).




kb_make_status_start(kb('logicEngine',_h233) = current).
kb_info_db('logicEngine','/cygdrive/c/cygwin/tks/sigma/inference_engine/logicEngine.can','/cygdrive/c/cygwin/tks/sigma/inference_engine/logicEngine.xkb','/cygdrive/c/cygwin/tks/sigma/inference_engine/logicEngine.pl').


cmd_proc(['Assertion',Object],_Cxt,_TN,Vars):-!,tell_sterm_rev(Object,_Cxt,_TN,Vars).
cmd_proc(['pnx_nf',Object,_Cxt,_TN],_Cxt,_TN,Vars):-!,tell_sterm_rev(Object,_Cxt,_TN,Vars).

cmd_proc([remove,'Assertion',Object],_Cxt,_TN,Vars):-!,prolog_retract(_Cxt,Object,_TN,Vars).

cmd_proc([new,'Function',FunctName,ARGCLASSES|Optionals],_Cxt,_TN,Vars):-!, 
                     tell_sterm_rev(_Cxt,['surface-instance',FunctName,'Function'],_TN,Vars),
                     sterm_to_pterm([list|ARGCLASSES],PCLASSES),
                     length(PCLASSES,Valence),
                     tell_sterm_rev(_Cxt,[valence,FunctName,Valence],_TN,Vars),
                     ignore((
                              Optionals = [ResultIsa|_] ,
                              tell_sterm_rev(_Cxt,[resultIsa,FunctName,ResultIsa],_TN,Vars)
                              )).

cmd_proc([new,'Relation',RelationName,ARGCLASSES|_Optionals],_Cxt,_TN,Vars):-!,
                     tell_sterm_rev(_Cxt,['surface-instance',RelationName,'Relation'],_TN,Vars),
                     length(ARGCLASSES,Valence),
                     tell_sterm_rev(_Cxt,[valence,RelationName,Valence],_TN,Vars).

tell_sterm_rev(Ctx,Stuff,TN,Vars):-tell_sterm(Stuff,Ctx,TN,Vars).

cmd_proc([new,'Entity',EntityName,Class],_Cxt,_TN,Vars):-!,
                     tell_sterm_rev(_Cxt,['surface-instance',EntityName,'Entity'],_TN,Vars),
                     tell_sterm_rev(_Cxt,[genls,Class,'Entity'],_TN,Vars).
%[New,Consultation,Relation,poo,[Po]]

cmd_proc([new,'Consultation','Relation',RelationName,SARGMODES],'ToplevelContext',_TN,Vars):-!, 
                     tell_sterm_rev('ToplevelContext',['surface-instance',RelationName,'Relation'],_TN,Vars),
                     sterm_to_pterm([list|SARGMODES],PARGMODES),
                     length(PARGMODES,Valence),
                     tell_sterm_rev('ToplevelContext',[valence,RelationName,Valence],_TN,Vars),
                     
                     length(ARGS,Valence),
                     tell_sterm_rev('ToplevelContext',[=>,[consultation,RelationName,PARGMODES,ARGS],[RelationName|ARGS]],_TN,Vars).
                     
cmd_proc(['consultation-predicate',RelationName],CTX,_TN,Vars):-!, 
                     assert('consultation-predicate'(RelationName,CTX)).

cmd_proc([remove,'Consultation','Relation',RelationName,SARGMODES],_Cxt,_TN,Vars):-!,
                     sterm_to_pterm([list|SARGMODES],PARGMODES),
                     length(PARGMODES,Valence),
                     length(ARGS,Valence),
                     retract_Sprolog(_Cxt,[=>,[consultation,RelationName,PARGMODES,ARGS],[RelationName|ARGS]],_TN,Vars).

       % Enter the The 'Reduced' is in PNF/KR S-Prolog Form but as Language Remappings
/* Example

Inputs: STerm= [not,[isa,_h133,animal]]
	Ctx=ToplevelContext
	TN=345
	CM=byrd
	Vars=[=('TheAnimal',_h133)|_h233]


STerm: is any format in KIF/SUMO/Prolog
Ctx: Prolog atom defining the context the assertion or command is ran in
Tracking: Prolog Term provided by external source for tracking purposes
CM: compile mode

tell_sterm(STerm,Ctx,TN,KB,CM,Vars):- abolish_all_tables,
            get_default_assertion_context(DCtx), !,  %supplies default context
            getDefaultKB(DKB), !,  %supplies default KB
            ignore((Ctx=DCtx)),!, % binds with CDTX if necessary
            ignore((KB=DKB)),!, % binds with DKB if necessary
            sterm_to_surface_sform(STerm,Ctx,TN,Vars,SSURFACE),!, % Converts [not,[isa,_h133,animal]] -> [not,['instance',_h133,animal]]
            sterm_to_pterm(SSURFACE,PSURFACE), % Converts [not,['instance',_h133,animal]] -> not('instance'(_h133,animal))
            atom_concat(CM,'_compile',CM_compile),!, % build the caller predicate byrd_compile
		CALL_CM_compile=..[CM_compile,surface(DynStat,PSURFACE,KB,Ctx,TN,Vars),CSURFACE],!,
	%  CALL_CM_compile= byrd_compile(surface(not('instance'(_h133,animal)),'nil','ToplevelContext',345,[=('TheAnimal',_h133)|_h233]),CSURFACE)
            
            ignore(CALL_CM_compile), 
            ignore(CSURFACE=end_of_file),
            do_to_conjuncts(CSURFACE,write_oclause_memory),!,
            do_to_conjuncts(CSURFACE,write_oclause_file_note),!,

            surface_to_assertions(SSURFACE,ASSERTIONS),!,
            assertions_to_xkb(ASSERTIONS,Ctx,TN,KB,Vars,XKB),!, 
            tell_sterm_cj(XKB,CFORM,CM),!,
            do_to_conjuncts(CFORM,write_oclause_memory),!,
            do_to_conjuncts(CFORM,write_oclause_file_note),!.
*/




cmd_proc([use_resource_file,FileName]):-   
            assert('resource_context',[resource_module,FileName,prolog_file]),
            sendNote(user,['loading resource file',FileName]),
            retractall(m_loading(_)),
            assert(m_loading(FileName)),
            [FileName].

%% resource_create(_Resource_atom,Source_path):-cmd_proc([resource_create,_Resource_atom, Source_path]).
         
cmd_proc([resource_create,_Resource_atom, Source_path]):-
         assert('resource_context',['surface-instance',_Resource_atom, resource_atom]),
         assert('resource_context',[resource_exists_in_module,_Resource_atom, Source_path]),
         assert('resource_context',[resource_state,_Resource_atom, resource_state_unloaded]).
                     
cmd_proc([remove,scenario]):-
            retractall(sigma_K_scenario(_Cxt,_SigmaView)),
            sendNote(user,logicEngine,'Insightfull information',['sigma_K_scenario cleared']).

cmd_proc([show,scenario]):-
            findall(sigma_K_scenario(_Cxt,_Query_Compiled,_SigmaView,_ResultSingle_bindings,_OtherTruth,_How),
                    (sigma_K_scenario(_Cxt,_Query_Compiled,_SigmaView,_ResultSingle_bindings,_OtherTruth,_How),
                     sendNote(user,logicEngine,'Insightfull information',sigma_K_scenario(_Cxt,_Query_Compiled,_SigmaView,_ResultSingle_bindings,_OtherTruth,_How))),
                        _).

cmd_proc([reset],_,_,_):-  !,    
      ua_out(event,start([reset],'Reseting Sigma Logic Engine')),   
         %cmd_proc([context_create,'ToplevelContext']),
  %    cmd_proc(['read-file','databases/builtin/sample_kb.kif','ToplevelContext']),
  %    cmd_proc(['read-file','databases/builtin/inference_kb.kif','inference_context']),
  %    cmd_proc(['read-file','databases/builtin/universal_kb.kif','universal_kb']),
      %cmd_proc(['read-file','databases/builtin/merged_ontology_kb.kif','ToplevelContext']),
      ua_out(event,end([reset],'')).

  % retractall(showa),


cmd_proc([batch,me],_,_,_):-!,   
                        repeat,
                           ((xsbRequestStream(Stream),conv_exec(Stream,_Chars,_Term,_Vars));conv_exec(_Chars,_Term,_Vars)),
                           ua_command(_Chars),
                        e_o_f(_Term),!.
/*
cmd_proc([batch,FileName]):-   
                        sigma_B_see(FileName), 
                        repeat,                      
                           conv_readS(_Chars,_Term,_),  
                           ua_command(_Chars),
                        e_o_f(_Term),!,
                        sigma_B_seen.

 */

% Reset Defaults And list them out
cmd_proc([clear],_,_,_):-
      sendNote(user,logicEngine,'Insightfull information','Clearing All Non-System Cxts'),
      retractall(kb(_,_,_,_)),
      sendNote(user,logicEngine,'Insightfull information','Done Clearing').

cmd_proc([msg,Class,Message],_,_,_):-
               ua_out(Class,Message).

cmd_proc([echo|REST],_,_,_):-
               sendNote(user,logicEngine,'Insightfull information',['echo: '|REST]).

cmd_proc([halt],_,_,_):-
               sendNote(user,logicEngine,'Insightfull information','Halting Sigma Logic Engine'),
               halt.

cmd_proc([recompile,system],_,_,_):-recompile_all.


cmd_proc([disp_debug,X],_,_,_):-retractall(disp_debugger(_)),assert(disp_debugger(X)).
cmd_proc([trace,X],_,_,_):-retractall(tracer(_)),assert(tracer(X)).
cmd_proc([proof,X],_,_,_):-retractall(displayproof(_)),assert(displayproof(X)).

cmd_proc(['read-file',me],_,_,_):-!,   
                        repeat,
                           ((xsbRequestStream(Stream),conv_readS(Stream,_Chars,Assertion,_Vars));conv_readS(_Chars,Assertion,_Vars)),
                           once(conv_stream_line_format(Assertion,_Language,NewCxt,RealAssertion,_Certainty,_TrackingNumber)),
                           cmd_proc([assert,RealAssertion,NewCxt,_TrackingNumber]),
                        e_o_f(_Term),!.


cmd_proc(['read-file',FileName],_,_,_):-   !,
               cmd_proc(['read-file',FileName,'ToplevelContext']).


cmd_proc(['read-file',FileName,Ctx],_,_,_):-!,   
               once((
                     isSigmaOption(opt_language=Opt_language),
                     isSigmaOption(opt_precompiled=Opt_precompiled),
                     isSigmaOption(opt_kb=Opt_kb),
                     isSigmaOption(opt_compiler=Opt_compiler),
                     isSigmaOption(opt_notation=Opt_notation)
               )), 
               once(cmd_proc_act(['read-file',FileName,Ctx,Opt_kb,Opt_precompiled,Opt_language,Opt_notation,Opt_compiler])).

/*
cmd_proc_act(['read-file',FileName,Ctx,_Opt_kb,true,_Opt_language,_Opt_notation,_]):-
                        sigma_B_see(FileName), 
                        repeat,                      
                           conv_readS(_Chars,Assertion,_),  
                           cmd_proc([assert,_Chars,Ctx,_]),
                        e_o_f(Assertion),!,
                        sigma_B_seen.
                              
*/

cmd_proc_act(['read-file',FileName,Ctx,Opt_kb,Opt_precompiled,Opt_language,Opt_notation,Opt_compiler]):- !,
               cmd_proc_act1(['read-file',FileName,Ctx,Opt_kb,Opt_precompiled,Opt_language,Opt_notation,Opt_compiler]).


cmd_proc_act1(['read-file',FileName,Ctx, nil,false,pnx_nf,kif,sigma]):-
            read_file(Ctx,FileName).


% ===================================================================
         
cmd_proc([context,delete,OldCxt, Source_File_optional_path]):-
         retract('resource_context',['surface-instance',OldCxt, context_atom]),
         retract('resource_context',[context_exists_in_file,OldCxt, Source_File_optional_path]),
         retract('resource_context',['surface-instance',Source_File_optional_path, source_file_path]),
         retract('resource_context',[context_state,_Cxt, context_state_unloaded]),
         retract('context_links_kb',[genMt,'all_contexts', OldCxt]).

% ===================================================================
         
cmd_proc([context_link,Ctx, _Subsumed_context]):-
         assert('context_links_kb',[genMt,Ctx, _Subsumed_context]).

% ===================================================================
         
cmd_proc([context_unlink,Ctx, _Subsumed_context]):-
         retract('context_links_kb',[genMt,Ctx, _Subsumed_context]).


% ===================================================================
%  context_ensure_loaded/1              %TODO
% ===================================================================
cmd_proc([context,ensure_loaded,_Cxt]):-
            kb('resource_context',[context_state,_Cxt,State]),
            member(State,[loaded_unchanged,loaded_changed]),!.

cmd_proc([context,ensure_loaded,_Cxt]):-
            retractall('resource_context',[context_state,_Cxt,_]),
            kb('resource_context',[context_exists_in_file,_Cxt, FileName]),
            cmd_proc(['read-file',_Cxt,FileName]), 
            assert('resource_context',[context_state,_Cxt,loaded_unchanged]).

 % TODO
/*
cmd_proc([contexts]):-context_list(_R),ua_out(returned,_R).
cmd_proc([context,load,CONTEXT]):-context_ensure_loaded(CONTEXT,_R),ua_out(returned,_R).
cmd_proc([context,delete,CONTEXT]):-context_delete(CONTEXT,_R),ua_out(returned,_R).
cmd_proc([context,save,CONTEXT]):-context_save(CONTEXT,CONTEXT,_R),ua_out(returned,_R).
cmd_proc([context,merge,CONTEXT1,CONTEXT2]):-context_merge(CONTEXT1,CONTEXT2,_R),ua_out(returned,_R).
cmd_proc([context,test,CONTEXT]):-context_changed(CONTEXT,_R),ua_out(returned,_R).
cmd_proc([context,CONTEXT]):-context_changed(CONTEXT,_R),ua_out(returned,_R).

  */


% ===================================================================
% callable(Term.

% ===================================================================
                               

conv_apply(_Context_atom,Var,Var):-var(Var).
conv_apply(_Context_atom,Const,Const):-atomic(Const).
conv_apply(_Context_atom,HVar,AGAF):-
      HVar=..[H|ARGS],var(H),
      AGAF=..[holds,HVar|ARGS].
conv_apply(_Context_atom,Skolem,Skolem):-Skolem=..['AssignmentFn'|_].
conv_apply(_Context_atom,Apply,Apply):-Apply=..[holds|_].
conv_apply(_Context_atom,NApply,NApply):-NApply=..[not_apply|_].
conv_apply(_Context_atom,Conj,ConjARGS):-
                     Conj=..[Conjunctive|ARGS],  nonvar(Conjunctive),
                     'surface-instance'(Conjunctive,'Connective',_Context_atom),
                     conv_apply_l(_Context_atom,ARGS,AARGS),
                     ConjARGS=..[Conjunctive|AARGS].
conv_apply(_Context_atom,GAF,AGAF):-
      conv_apply_proc(_Context_atom,GAF,AGAF).


conv_apply_l(_Context_atom,Var,Var):-var(Var),!.
conv_apply_l(_Context_atom,[],[]):-!.
conv_apply_l(_Context_atom,[H|T],[HH|TT]):-
      conv_apply(_Context_atom,H,HH),
      conv_apply_l(_Context_atom,T,TT).


conv_apply_proc(_Context_atom,GAF,AGAF):-
      GAF=..ARGS,
      AGAF=..[holds|ARGS].




% ===================================================================
%  inferSurface_actual Predicates detect and handle potential cyclic logic by only producing GAFS 
%  No axiomizations of forward chaining are ever interpreted
/*
% in our implimentation there are specialized predicates that assert us what kind of GAFs we may create

% Subsumtion Based

genMt(Context_atom1,Context_atom2)
subsumed_relation(Relation1,Relation2)
subsumed_class(Class1,Class2)

% Indivigual Based

['surface-instance',Entity,Class)
skolemize(Entity)

Mathmatical

clsid(Entity)


disjoint(Class1,Class2) %Classes have no shared entities ever
conjoint(Class1,Class2) %Classes have at least on shared entitiy
intersect_by(Entity,Class1,Class2) %Any Shared Entitiy



intersect_by(Class1,Class2,Class3) %Any Shared Entitiy






/*

['surface-instance',Context_atom,Entity,Class):-

subset_of()
nth_domain_set()
*/
% ===================================================================



%:- table inferSurface_actual/2.
%:- table inferSurface_actual/3.
%:- table inferSurface/2.

%:-auto_table.

%:-suppl_table.

inferSurface_false(_Context_atom,[false],[false]).
%inferSurface_false(_Context_atom,GAF,Proof):-not_inferSurface(_Context_atom,GAF,Proof).
inferSurface_false(_Context_atom,[not,GAF],[not,GAF]):-inferSurface(_Context_atom,GAF).

inferSurface(_Context_atom,[PredicateI|Args]):-inferSurface(_Context_atom,[PredicateI|Args],_).

inferSurface(_Context_atom,[true],[true]).
inferSurface(_Context_atom,[not,[not,true]],[true]).
/*
inferSurface(_Context_atom,[and,Gaf1,Gaf2],[and,Proof1,Proof2]):-
                  inferSurface(_Context_atom,Gaf1,Proof1),
                  inferSurface(_Context_atom,Gaf2,Proof2).
inferSurface(_Context_atom,[or,Gaf1,_Gaf2],[or,Proof1,nottried]):-
                  inferSurface(_Context_atom,Gaf1,Proof1).
inferSurface(_Context_atom,[or,_Gaf1,Gaf2],[or,failed,Proof2]):-
                  inferSurface(_Context_atom,Gaf2,Proof2).
   
 */  

inferSurface(_Context_atom,[PredicateI|Args],[resource,[PredicateI|Args]]):-
               resource(_Context_atom,[PredicateI|Args]).

inferSurface(_Context_atom,[PredicateI|Args],[Proof,[PredicateI|Args]]):-
                 ignore((
                        isSigmaOption(cb_consultation = true),
                        consultation(_Context_atom,[PredicateI|Args]),
                        Proof=consultation)),

                 inferSurface_actual(_Context_atom,[PredicateI|Args],Proof).



inferSurface_actual(_Context_atom,[P|ARGS],assertion(TrackingTerm)) :- kb(_Context_atom,TrackingTerm,[P|ARGS],_).
inferSurface_actual(_Context_atom,[P|ARGS],deduced):- nonvar(P),inferSurface_specialized(_Context_atom,[P|ARGS]).
inferSurface_actual(_Context_atom,[P|ARGS],deduced):- ground([P|ARGS]),ground_inferSurface(_Context_atom,[P|ARGS]).
                                 


/*inferSurface_specialized(_Context_atom,[AboutRels,P1,P2]) :- member(AboutRels,[genlInverse]),
                                       inferSurface_transitively(_Context_atom,[subsumed_relation,P1,UnderPred1]),
                                       inferSurface_transitively(_Context_atom,[subsumed_relation,P2,UnderPred2]),
                                       kb(_Context_atom,[genlInverse,UnderPred1,UnderPred2]).

  */

% Instance_of
inferSurface_specialized(_Context_atom,['surface-instance',Entity,Class]):- inferSurface_transitively(_Context_atom,['surface-instance',Entity,Class]).
inferSurface_transitively(_Context_atom,['surface-instance',Entity,Class]) :- 
                              kb(_Context_atom,['surface-instance',Entity,EClass]),
                              inferSurface_specialized(_Context_atom,[genls,EClass,Class]).


% 'surface-domain' Subsumption
inferSurface_specialized(_Context_atom,['surface-domain',Pred,ArgN,EClass]):-inferSurface_transitively(_Context_atom,['surface-domain',Pred,ArgN,EClass]).
inferSurface_transitively(_Context_atom,['surface-domain',Pred,ArgN,EClass]) :- 
                              inferSurface_specialized(_Context_atom,[genls,EClass,Class]),
                              kb(_Context_atom,['surface-domain',Pred,ArgN,Class]).

% Context_atom Subsumption
inferSurface_specialized(_Context_atom,[genMt,C1,C2]) :- inferSurface_transitively(_Context_atom,[genMt,C1,C2]).
inferSurface_transitively(_Context_atom,[genMt,P1,P1]).
inferSurface_transitively(_Context_atom,[genMt,P1,P2]):-kb(_Context_atom,[genMt,P1,P2]).
inferSurface_transitively(_Context_atom,[genMt,P1,P3]):-kb(_Context_atom,[genMt,P1,P2]),kb(_Context_atom,[genMt,P2,P3]).
inferSurface_transitively(_Context_atom,[genMt,P1,P4]):-kb(_Context_atom,[genMt,P1,P2]),kb(_Context_atom,[genMt,P2,P3]),kb(_Context_atom,[genMt,P3,P4]).

% Relation Subsumption
inferSurface_specialized(_Context_atom,[subsumed_relation,C1,C2]) :- inferSurface_transitively(_Context_atom,[subsumed_relation,C1,C2]).
inferSurface_transitively(_Context_atom,[subsumed_relation,P1,P1]).
inferSurface_transitively(_Context_atom,[subsumed_relation,P1,P2]):-kb(_Context_atom,[subsumed_relation,P1,P2]).
inferSurface_transitively(_Context_atom,[subsumed_relation,P1,P3]):-kb(_Context_atom,[subsumed_relation,P1,P2]),kb(_Context_atom,[subsumed_relation,P2,P3]).
inferSurface_transitively(_Context_atom,[subsumed_relation,P1,P4]):-kb(_Context_atom,[subsumed_relation,P1,P2]),kb(_Context_atom,[subsumed_relation,P2,P3]),kb(_Context_atom,[subsumed_relation,P3,P4]).

% Class Subsumption
inferSurface_specialized(_Context_atom,[genls,C1,C2]) :- inferSurface_transitively(_Context_atom,[genls,C1,C2]).

inferSurface_transitively(_Context_atom,[genls,Class,Class]).
inferSurface_transitively(_Context_atom,[genls,ChildClass,ParentClass]) :- kb(_Context_atom,[genls,ChildClass,ParentClass]).
inferSurface_transitively(_Context_atom,[genls,ChildClass,GrandParentClass]) :- kb(_Context_atom,[genls,ParentClass,GrandParentClass]),kb(_Context_atom,[genls,ChildClass,ParentClass]).
inferSurface_transitively(_Context_atom,[genls,ChildClass,GreatGrandParentClass]) :- kb(_Context_atom,[genls,GrandParentClass,GreatGrandParentClass]),kb(_Context_atom,[genls,ParentClass,GrandParentClass]),kb(_Context_atom,[genls,ChildClass,ParentClass]).
inferSurface_transitively(_Context_atom,[genls,ChildClass,GreatGreatGrandParentClass]) :- kb(_Context_atom,[genls,GreatGrandParentClass,GreatGreatGrandParentClass]),kb(_Context_atom,[genls,GrandParentClass,GreatGrandParentClass]),kb(_Context_atom,[genls,ParentClass,GrandParentClass]),kb(_Context_atom,[genls,ChildClass,ParentClass]).


inferSurface_actual(_Context_atom,[Pred,SubClass|Rest],deduced) :- 
                        inferSurface_specialized(_Context_atom,['surface-domain',Pred,1,'Class']),
                        inferSurface_specialized(_Context_atom,[genls,SubClass,SuperClass]),
                        kb(_Context_atom,[Pred,SuperClass|Rest]).

inferSurface_actual(_Context_atom,[Pred,Arg1,Arg2|Rest],deduced) :- 
                        inferSurface_specialized(_Context_atom,['surface-domain',Pred,2,'Class']),
                        inferSurface_specialized(_Context_atom,[genls,Arg2,SuperClass]),
                        kb(_Context_atom,[Pred,Arg1,SuperClass|Rest]).

inferSurface_actual(_Context_atom,[Pred,Arg1,Arg2,Arg3|Rest],deduced) :- 
                        inferSurface_specialized(_Context_atom,['surface-domain',Pred,3,'Class']),
                        inferSurface_specialized(_Context_atom,[genls,Arg3,SuperClass]),
                        kb(_Context_atom,[Pred,Arg1,Arg2,SuperClass|Rest]).

inferSurface_actual(_Context_atom,[Pred,Arg1|Rest],deduced) :- 
                        inferSurface_specialized(_Context_atom,['surface-domain',Pred,1,'Relation']),
                        inferSurface_specialized(_Context_atom,[subsumed_relation,Arg1,SuperRelation]),
                        kb(_Context_atom,[Pred,SuperRelation|Rest]).

inferSurface_actual(_Context_atom,[Pred,Arg1,Arg2|Rest],deduced) :- 
                        inferSurface_specialized(_Context_atom,['surface-domain',Pred,2,'Relation']),
                        inferSurface_specialized(_Context_atom,[subsumed_relation,Arg2,SuperRelation]),
                        kb(_Context_atom,[Pred,Arg1,SuperRelation|Rest]).

inferSurface_actual(_Context_atom,[Pred,Arg1,Arg2,Arg3|Rest],deduced) :- 
                        inferSurface_specialized(_Context_atom,['surface-domain',Pred,3,'Relation']),
                        inferSurface_specialized(_Context_atom,[subsumed_relation,Arg3,SuperRelation]),
                        kb(_Context_atom,[Pred,Arg1,Arg2,SuperRelation|Rest]).


ground_inferSurface(_Context_atom,[P1,Arg1,Arg2]) :- kb(_Context_atom,[negationPreds,P1,P2]),not_inferSurface(_Context_atom,[P2,Arg1,Arg2],_).
ground_inferSurface(_Context_atom,[P1,Arg1,Arg2]) :- inferSurface_actual(_Context_atom,[genlInverse,P1,P2],_),inferSurface_actual(_Context_atom,[P2,Arg2,Arg1,_]).

% ===================================================================
%  possible_chain(_Context_atom,GAF)
% ===================================================================

not_inferSurface(_Context_atom,GAF,[not,GAF]):-possible_chain(_Context_atom,GAF),nonvar(_Context_atom),provenot(inferSurface(_Context_atom,GAF,GAF)).

%% :-table possible_chain/2.

possible_chain(_Context_atom,[true]).
possible_chain(_Context_atom,[fail]).

possible_chain(_Context_atom,[Pred|ArgS]):-
            possible_chain_actual(_Context_atom,[Pred|ArgS]).

possible_chain(_Context_atom,[Pred|ArgS]):-
            inferSurface(_Context_atom,[subsumed_relation,Alias,Pred]),
            possible_chain_actual(_Context_atom,[Alias|ArgS]).

possible_chain_actual(_Context_atom,[Pred,Arg1]):-
            inferSurface(_Context_atom,[functsymbol,Pred,1]),
            sigma_D_nth_domain_check(_Context_atom,Pred,1,Arg1).

possible_chain_actual(_Context_atom,[Pred,Arg1,Arg2]):-
            inferSurface(_Context_atom,[functsymbol,Pred,2]),
            sigma_D_nth_domain_check(_Context_atom,Pred,1,Arg1),
            sigma_D_nth_domain_check(_Context_atom,Pred,2,Arg2).

possible_chain_actual(_Context_atom,[Pred,Arg1,Arg2,Arg3]):-
            inferSurface(_Context_atom,[functsymbol,Pred,3]),
            sigma_D_nth_domain_check(_Context_atom,Pred,1,Arg1),
            sigma_D_nth_domain_check(_Context_atom,Pred,2,Arg2),
            sigma_D_nth_domain_check(_Context_atom,Pred,3,Arg3).

possible_chain_actual(_Context_atom,[Pred,Arg1,Arg2,Arg3,Arg4]):-
            inferSurface(_Context_atom,[functsymbol,Pred,4]),
            sigma_D_nth_domain_check(_Context_atom,Pred,1,Arg1),
            sigma_D_nth_domain_check(_Context_atom,Pred,2,Arg2),
            sigma_D_nth_domain_check(_Context_atom,Pred,3,Arg3),
            sigma_D_nth_domain_check(_Context_atom,Pred,4,Arg4).


possible_chain_actual(_Context_atom,[Pred,Arg1,Arg2,Arg3,Arg4,Arg5]):-
            inferSurface(_Context_atom,[functsymbol,Pred,5]),
            sigma_D_nth_domain_check(_Context_atom,Pred,1,Arg1),
            sigma_D_nth_domain_check(_Context_atom,Pred,2,Arg2),
            sigma_D_nth_domain_check(_Context_atom,Pred,3,Arg3),
            sigma_D_nth_domain_check(_Context_atom,Pred,4,Arg4),
            sigma_D_nth_domain_check(_Context_atom,Pred,5,Arg5).

possible_chain_actual(_Context_atom,[Pred,Arg1,Arg2,Arg3,Arg4,Arg5,Arg6]):-
            inferSurface(_Context_atom,[functsymbol,Pred,6]),
            sigma_D_nth_domain_check(_Context_atom,Pred,1,Arg1),
            sigma_D_nth_domain_check(_Context_atom,Pred,2,Arg2),
            sigma_D_nth_domain_check(_Context_atom,Pred,3,Arg3),
            sigma_D_nth_domain_check(_Context_atom,Pred,4,Arg4),
            sigma_D_nth_domain_check(_Context_atom,Pred,5,Arg5),
            sigma_D_nth_domain_check(_Context_atom,Pred,6,Arg6).

 
%%prove_not(_Context_atom,[P|ARGS]):-  prove(_Context_atom,[P|ARGS],3,not_proved,_,_).  %TODO set chain limit.


% ===================================================================
%  backwards_chain(_Context_atom,Consequent,Antecedant)
% ===================================================================

%% :-table backwards_chain/3.
%% :-table backwards_chain_nonvar/3.

backwards_chain(_Context_atom,FormulaA,FormulaB) :- (nonvar(FormulaA);nonvar(FormulaB)),backwards_chain_nonvar(_Context_atom,FormulaA,FormulaB).

backwards_chain_nonvar(_Context_atom,Consequent,Antecedant):- kb(_Context_atom,[=>,Antecedant,Consequent]).
backwards_chain_nonvar(_Context_atom,Consequent,Antecedant):- kb(_Context_atom,[=>,Antecedant,Consequent]).
backwards_chain_nonvar(_Context_atom,Consequent,Antecedant):- kb(_Context_atom,[=,>,Antecedant,Consequent]).
backwards_chain_nonvar(_Context_atom,Consequent,Antecedant):- kb(_Context_atom,[ <=> ,Consequent,Antecedant]).
backwards_chain_nonvar(_Context_atom,Consequent,Antecedant):- kb(_Context_atom,[ <=> ,Antecedant,Consequent]).
backwards_chain_nonvar(_Context_atom,Consequent,Antecedant):- kb(_Context_atom,[ and,Antecedant,Consequent]).
backwards_chain_nonvar(_Context_atom,Consequent,Antecedant):- kb(_Context_atom,[ and,Consequent,Antecedant]).

%%backwards_chain_nonvar(_Context_atom,[P1|ARGS],Antecedant) :- kb(_,[subsumed_relation,P1,P2]),backwards_chain(_Context_atom,[P2|ARGS],Antecedant)  .
%%%backwards_chain_nonvar(_Context_atom,[not,FormulaA],FormulaB):-not_backwards_chain(_Context_atom,FormulaA,FormulaB).
%%backwards_chain_nonvar(_Context_atom,[not,FormulaA],FormulaB):-!,not_backwards_chain(_Context_atom,FormulaA,FormulaB).
/*
backwards_chain_nonvar(_Context_atom,[PredicateName|Args],Answer) :-  nonvar(PredicateName),
               need_more_answers,
               kb('resource_context',[resource_located,PredicateName,_,Resource,Mode,_,_Often]),
               check_legal_modes(Args,Mode,_Context_atom),!,
               ask_resource(Resource,_Context_atom,[PredicateName|Args],Mode,[PredicateName|Args],Answer).

*/               


% ===================================================================
%  not_backwards_chain(_Context_atom,Consequent,Antecedant)
% ===================================================================

not_backwards_chain(_Context_atom,Consequent,[and,FormulaA,FormulaB]) :- backwards_chain(_Context_atom,Consequent,[and,FormulaA,FormulaB]),
                  (not_inferSurface(_Context_atom,FormulaA);not_inferSurface(_Context_atom,FormulaB)).

not_backwards_chain(_Context_atom,Consequent,[or,FormulaA,FormulaB]) :- backwards_chain(_Context_atom,Consequent,[or,FormulaA,FormulaB]),
                  (not_inferSurface(_Context_atom,FormulaA),not_inferSurface(_Context_atom,FormulaB)).

not_backwards_chain(_Context_atom,Consequent,Antecedant) :- backwards_chain(_Context_atom,Consequent,Antecedant),
                   not_inferSurface(_Context_atom,Antecedant).


% ===================================================================
% LNPROLOG'S LOGICAL NEGATION & QUATIFICATION
% ===================================================================

%sigma_invoke_obj(_Context_atom,FormulaA):-nonvar(FormulaA), is_list(FormulaA),!,prove(_Context_atom,FormulaA,10,tabled_true,_,_).

%sigma_invoke_obj(_Context_atom,FormulaA):-nonvar(FormulaA), (inferSurface_actual(_Context_atom,FormulaA) ; (functor(FormulaA,F,AA),system(F/AA),call(FormulaA)) ).

eval(_Context_atom,FormulaA) :- once(sigma_invoke_obj(_Context_atom,FormulaA)).

name_value_pairs(Goal,Vars,VarList) :- !,'='((Goal,Vars),(Goal,VarList)).

ground_vars(Vars):-findall(X,((member(X,Vars),(nonvar(X);!),X==(N,V),ground(N),N=V)),_).






%:-auto_table.
%====================================================================
% sigma_I_infer(Context_atom(I),Fact(I),Depth(I),Truth(O),Proof(O)(I))
%====================================================================

%% :-table sigma_I_infer/5.

%%sigma_I_infer(_Context_atom,_FormulaA,_Depth,_lost,toofar) :- query_done,!,fail.   %Have the limits been reached?
sigma_I_infer(_Context_atom,VAR,_Depth,_Result,var(VAR)) :- var(VAR).
sigma_I_infer(_Context_atom,[!],_Depth,tabled_true,fact).
sigma_I_infer(_Context_atom,_FormulaA,_Depth,lost,toofar) :- _Depth<1.

sigma_I_infer(_Context_atom,Antecedants,D1,Truth,Proof):- 
              sigma_I_infer_no_backchain(_Context_atom,Antecedants,D1,Truth,Proof).

sigma_I_infer(_Context_atom,[G|AF],_Depth,_Result, (FC '=>' Proof '=>' Antecedants '=>' [G|AF])) :-nonvar(G), 
              inferSurface(_Context_atom,[=>,Antecedants,[G|AF]],FC),
               D1 is _Depth-1,  
              (contained_in_formula([G|AF],Antecedants) -> 
              sigma_I_infer_no_backchain(_Context_atom,Antecedants,D1,_Result,Proof)
               ;
             sigma_I_infer(_Context_atom,Antecedants,D1,_Result,Proof)
              ),assert(_Context_atom,[G|AF]).

/*
% Supports Cut Version TODO
sigma_I_infer(_Context_atom,FormulaA,_Depth,_Result,(Proof '=>' FormulaA)) :-
     backwards_chain(_Context_atom,FormulaA,FormulaB),_Depth1 is D-1,
     detec!(FormulaB,B1,B2,Cut),
     (Cut = tabled_true,sigma_I_infer(_Context_atom,B1,_Depth1,R1,Proof1),
        (R1 \= tabled_true,_Result = R1,return_proof(_Context_atom,Proof1,B2,Proof);
         R1 = tabled_true,!,sigma_I_infer(_Context_atom,B2,_Depth1,_Result,Proof2),
                    conjunct(Proof1,Proof2,Proof));
      Cut = not_proved,sigma_I_infer(_Context_atom,FormulaB,_Depth1,_Result,Proof)).


detec!(FormulaB,B1,B2,tabled_true) :- cutin(FormulaB,B1,B2),!.
detec!(_,_,_,not_proved).


cutin(!,!,!) :- !.
cutin((!,FormulaB),!,FormulaB) :- !.
cutin((FormulaA,!),(FormulaA,!),!) :- !.
cutin((FormulaA,FormulaB),(FormulaA,As),Bs) :- cutin(FormulaB,As,Bs).
    
           
*/

                                      contained_in_formula(_This,_Formula):-!,fail.
%% :-table contained_in_formula/2.
contained_in_formula(This,Formula):-!,
         copy_term(This,ThisCopy),
         copy_term(Formula,FormulaCopy),
         contained_in_formula2(ThisCopy,FormulaCopy).

contained_in_formula2([G|_],[G|_]):-!.
contained_in_formula2(GAF,Antecedants):-!,member(Clause,Antecedants),contained_in_formula(GAF,Clause).


%% :-table sigma_I_infer_no_backchain/5.


sigma_I_infer_no_backchain(_Context_atom,[P|Args],_Depth,Truth,Proof) :-
         sigma_I_infer_no_backchain_tf(_Context_atom,[P|Args],_Depth,Truth,Proof).

sigma_I_infer_no_backchain(_Context_atom,[P|Args],_Depth,_Result,Proof) :- nonvar(P),
         sigma_I_infer_nonvar(_Context_atom,[P|Args],_Depth,_Result,Proof).
                 

sigma_I_infer_no_backchain_tf(_Context_atom,[P|Args],_D,tabled_false,Proof):-nonvar(P),inferSurface_false(_Context_atom,[P|Args],Proof).

sigma_I_infer_no_backchain_tf(_Context_atom,[P|Args],_D,tabled_true,Proof):-inferSurface(_Context_atom,[P|Args],Proof),not(inferSurface_false(_Context_atom,[P|Args],_Proof)).



%sigma_I_infer(_Context_atom,C,_D,not_proved,unprovable(C) ).

%====================================================================
% sigma_I_infer_nonvar(Context_atom(I),Fact(I),Depth(I),Truth(O),Proof(O)(I))
%====================================================================
 
%% :-table sigma_I_infer_nonvar/6.

%and

sigma_I_infer_nonvar(_Context_atom,[and,FormulaA,FormulaB],_Depth,_Result,[and,ProofA,ProofB]) :- 
   D1 is _Depth-1, D1>0,
     sigma_I_infer(_Context_atom,FormulaA,D1,_Result,ProofA),
     sigma_I_infer(_Context_atom,FormulaB,D1,_Result,ProofB).

% or

sigma_I_infer_nonvar(_Context_atom,[or,FormulaA,FormulaB],_Depth,_Result,([or,ProofA,FormulaB])) :- 
   D1 is _Depth-1, D1>3, sigma_I_infer(_Context_atom,FormulaA,D1,_Result,ProofA).
     
sigma_I_infer_nonvar(_Context_atom,[or,FormulaA,FormulaB],_Depth,_Result,(ProofB '=>' [or,FormulaA,ProofB])) :- 
   D1 is _Depth-1, D1>3, sigma_I_infer(_Context_atom,FormulaB,D1,_Result,ProofB).

/*
% AND/2   !
sigma_I_infer_nonvar(_Context_atom,[and,FormulaA,FormulaB],_Depth,_Result,[and,ProofA,ProofB]) :- 
   D1 is _Depth-1, D1>3,
     sigma_I_infer_nonvar(_Context_atom,FormulaA,_Depth,_ResultA,ProofA),
     prove_conj(_Context_atom,_ResultA,FormulaB,_Depth,_Result,ProofB).

prove_conj(_Context_atom,tabled_true,FormulaB,_Depth,_Result,Proof) :- !,sigma_I_infer_nonvar(_Context_atom,FormulaB,_Depth,_Result,Proof).
         %      prove_conj(_Context_atom,RA, FormulaB,_Depth,RA,FormulaB:nottried).
*/

% XOR/2    !
sigma_I_infer_nonvar(_Context_atom,[xor,FormulaA,FormulaB],_Depth,_Result,[xor,Proof1,[not,Proof2]]) :- !,
     (sigma_I_infer_nonvar(_Context_atom,FormulaA,_Depth,tabled_true,Proof1),provenot(sigma_I_infer_nonvar(_Context_atom,FormulaB,_Depth,tabled_true,Proof2)))
   ;
     (sigma_I_infer_nonvar(_Context_atom,FormulaB,_Depth,tabled_true,Proof1),provenot(sigma_I_infer_nonvar(_Context_atom,FormulaA,_Depth,tabled_true,Proof2))).

        
%Not

sigma_I_infer_nonvar(_Context_atom,[not,FormulaA],_Depth,tabled_true,[not,Proof]) :-   
              (sigma_I_infer(_Context_atom,FormulaA,_Depth,Result,Proof)),(Result \= tabled_true).

sigma_I_infer_nonvar(_Context_atom,[not,FormulaA],_Depth,tabled_false,[not,Proof]) :-   
              (sigma_I_infer(_Context_atom,FormulaA,_Depth,Result,Proof)),(Result \= tabled_false).

sigma_I_infer_nonvar(_Context_atom,[not,FormulaA],_Depth,not_tn,[not,Proof]) :-nonvar(FormulaA),
              (sigma_I_infer(_Context_atom,FormulaA,_Depth,Result,Proof)),member(Result,[tabled_false,tabled_true]).

%Non
sigma_I_infer_nonvar(_Context_atom,[not,GAF],_Depth,tabled_true,Proof) :-
               not_inferSurface(_Context_atom,GAF,Proof).


% No/None/1     !
%sigma_I_agreggate(_Context_atom,[no,X,[Pred|ArgS]],_Depth,_Result,([not,[Pred|ArgS]] '=>' _Result)) :- 
%     (not_inferSurface(_Context_atom,[Pred|ArgS]),_Result = tabled_true); _Result = not_proved.



% subquery-ist/2
sigma_I_infer_nonvar(_Context_atom,[dbi,OtherContext_atom,Prop],_Depth,_Result,[subquery,OtherContext_atom,Proof]) :-
               sigma_I_infer(OtherContext_atom,Prop,_Depth,_Result,(Proof)).

% =/2     !
sigma_I_infer_nonvar(_Context_atom,[Same,FormulaA,FormulaB],_Depth,tabled_true,(FormulaA=FormulaB)):-
                  member(Same,[=,equal,is,equal,equivalent]),!,
                  FormulaA=FormulaB.

% ==/2     !
sigma_I_infer_nonvar(_Context_atom,[==,FormulaA,FormulaB],_Depth,tabled_true,(FormulaA==FormulaB)) :-!, FormulaA==FormulaB.

% exists/1 
%sigma_I_infer_nonvar(_Context_atom,[exists,Entity],_Depth,tabled_true,['surface-instance',Entity,Class]) :- kb(_Context_atom,['surface-instance',Entity,Class]).

% class/1 
%sigma_I_infer_nonvar(_Context_atom,[class,Class],_Depth,tabled_true,['surface-instance',Entity,Class]) :- kb(_Context_atom,['surface-instance',Entity,Class]).

%member
sigma_I_infer_nonvar(_Context_atom,[member,E,Set],_Depth,tabled_true,( [member_of,E,Set] )) :-
                  member(E,Set).

sigma_I_infer_nonvar(_Context_atom,[member,E,Set],_Depth,tabled_true,( Proof )) :-
                  inferSurface(_Context_atom,['surface-instance',E,Set],Proof).

% var/1
sigma_I_infer_nonvar(_Context_atom,[var,X],_Depth,tabled_true,var(X)):-var(X),!.

% skolemize/1
sigma_I_infer_nonvar(_Context_atom,[Entity,SKOLEM],_Depth,tabled_true,['equal',SKOLEM]) :-kb(_Context_atom,['surface-instance',Entity,_Class]).
sigma_I_infer_nonvar(_Context_atom,[skolemize,SKOLEM],_Depth,tabled_true,['equal',SKOLEM]) :-ignore(skolem_gen(SKOLEM)).

% is
sigma_I_infer_nonvar(_Context_atom,[is,Result,SMath],_D,tabled_true,[is,Result,SMath]) :-
            ground(SMath), math_sterm_to_pterm(SMath,Math),
              call(Result is Math).      math_sterm_to_pterm(SMath,Math):-sterm_to_pterm(SMath,Math).                                

% intersect/3
sigma_I_infer_nonvar(_Context_atom,[intersector,Class1,Class2,Instance],_Depth,tabled_true,[=>,[and,Proof1,Proof2],[intersect,Class1,Class2,Instance]]) :-
               (nonvar(Class1);nonvar(Class2)),
               sigma_I_infer_nonvar(_Context_atom,[member,Instance,Class1],_Depth,_Result,Proof1),
               sigma_I_infer_nonvar(_Context_atom,[member,Instance,Class2],_Depth,_Result,Proof2).

% stack_inspection/1
sigma_I_infer_nonvar(_Context_atom,[current_context,_Context_atom],_,tabled_true,[current_context,_Context_atom]) :- !.
sigma_I_infer_nonvar(_Context_atom,[current_settings],_,tabled_true,[current_settings]) :- !.
sigma_I_infer_nonvar(_Context_atom,[current_depth,Current_Depth],Current_Depth,tabled_true,[current_depth,Current_Depth]) :- !.

% Disjoint
sigma_I_infer_nonvar(_Context_atom,[disjoint,C1,C2],_D,tabled_true,(no_shared_'instance's(C1,C2))):-ground([disjoint,C1,C2]),
                              bagof(Entity,
                                    ((
                                      inferSurface_specialized(_Context_atom,['surface-instance',Entity,C1]),
                                      inferSurface_specialized(_Context_atom,['surface-instance',Entity,C2])
                                      )),SE),SE=[].


% Conjoint/3
sigma_I_infer_nonvar(_Context_atom,[conjoint,C1,C2,Num],_D,tabled_true,(shared_'instance's(C1,C2))):-
                              bagof(Entity,
                                    ((inferSurface_specialized(_Context_atom,['surface-instance',Entity,C1]),
                                                inferSurface_specialized(_Context_atom,['surface-instance',Entity,C2]))),Num).

% Conjoint/2
sigma_I_infer_nonvar(_Context_atom,[conjoint,C1,C2],_D,tabled_true,(shared_'instance's(C1,C2))):-
                                    ((inferSurface_specialized(_Context_atom,['surface-instance',Entity,C1]),
                                                inferSurface_specialized(_Context_atom,['surface-instance',Entity,C2]))).

%or(X,Y):-sigma_invoke_obj(_C,X);sigma_invoke_obj(_C,Y).

% command
sigma_I_infer_nonvar(_Context_atom,[exec,COMMAND],_Depth,tabled_true,command) :- cmd(COMMAND).

% prolog
sigma_I_infer_nonvar(_Context_atom,[F|ARGS],_Depth,_Result,[macro_script,[F|ARGS]],_Result) :-length(ARGS,N),
            (system(F/N)),(sterm_to_pterm([F|ARGS],PTERM),call(PTERM),_Result = tabled_true; _Result = not_proved).


sigma_I_infer_nonvar(_Context_atom,[thereExists,Entity,Facts],D,tabled_true, (Proof)) :- 
                  (handle_skolem_proof(_Context_atom,[thereExists,Entity,Facts],D,tabled_true, (Proof))).

% Conclusion
%sigma_I_infer_nonvar(_Context_atom,[Pred,P|Args],_Depth,_Result, Proof) :- nonvar(P), 
%                sigma_I_conclusion(_Context_atom,[Pred,P|Args],_Depth,_Result, Proof).

% Agreggation
sigma_I_infer_nonvar(_Context_atom,[Pred,X|[P|Args]],_Depth,_Result, Proof) :- nonvar(P), 
                 sigma_I_agreggate(_Context_atom,[Pred,X|[P|Args]],_Depth,_Result, Proof).



%SKOLEMIZATION and thereExists
handle_skolem_proof(_Context_atom,Consequent1,D,Truth, Proof):- 
    %     sendNote(debug,logicEngine,'Debug Info',handle_skolem_proof(_Context_atom,Consequent1,D,Truth, Proof)),
         handle_skolem_proof1(_Context_atom,Consequent1,D,Truth, Proof).
     %    sendNote(debug,logicEngine,'Debug Info',handle_skolem_proof_returned(_Context_atom,Consequent1,D,Truth, Proof)).

handle_skolem_proof1(_Context_atom,[thereExists,_SkolemedEntity,Formula],D,tabled_true, (Proof '=>' [thereExists,_SkolemedEntity,Formula])) :-      
      sigma_I_infer(_Context_atom,Formula,D,tabled_true,Proof),
      sigma_I_infer_nonvar(_Context_atom,[skolemize,_SkolemedEntity],D,tabled_true,SKOLEMPROOF),
      (sigma_A_do_assert_list(_Context_atom,skolemized(SKOLEMPROOF),[[thereExists,_SkolemedEntity,Formula]])),
      (sigma_A_do_assert_list(_Context_atom,skolemized(SKOLEMPROOF),[Formula])),
      (sigma_A_do_assert_list(_Context_atom,skolemized(SKOLEMPROOF),[[skolemize,_SkolemedEntity]])).

handle_skolem_proof1(_Context_atom,Consequent1,D,tabled_true, (Proof '=>' SKOLEMPROOF '=>' asserted(Consequent1) '=>'asserted(Consequent2))) :-      
      nonvar(Consequent1), not(inferSurface(_Context_atom,Consequent1,_)),
      kb(_Context_atom,[=>,Antecedant,[thereExists,_SkolemedEntity,[and,Consequent1,Consequent2]]]),
      sigma_I_infer_nonvar(_Context_atom,[skolemize,_SkolemedEntity],D,tabled_true,SKOLEMPROOF),
      sigma_I_infer_nonvar(_Context_atom,Antecedant,D,_Result,Proof),
      (sigma_A_do_assert_list(_Context_atom,skolemized(SKOLEMPROOF),Consequent1)),
      (sigma_A_do_assert_list(_Context_atom,skolemized(SKOLEMPROOF),Consequent2)).

handle_skolem_proof1(_Context_atom,Consequent2,D,tabled_true, (Proof '=>' SKOLEMPROOF '=>' asserted(Consequent2) '=>'asserted(Consequent1))) :-      
      nonvar(Consequent2), not(inferSurface(_Context_atom,Consequent2,_)),
      kb(_Context_atom,[=>,Antecedant,[thereExists,_SkolemedEntity,[and,Consequent1,Consequent2]]]),
      sigma_I_infer_nonvar(_Context_atom,[skolemize,_SkolemedEntity],D,tabled_true,SKOLEMPROOF),
      sigma_I_infer_nonvar(_Context_atom,Antecedant,D,_Result,Proof),
      (sigma_A_do_assert_list(_Context_atom,skolemized(SKOLEMPROOF),Consequent1)),
      (sigma_A_do_assert_list(_Context_atom,skolemized(SKOLEMPROOF),Consequent2)).

handle_skolem_proof1(_Context_atom,Consequent1,D,tabled_true, (Proof '=>' SKOLEMPROOF '=>' asserted(Consequent1) )) :-      
      nonvar(Consequent1),
      kb(_Context_atom,[=>,Antecedant,[thereExists,_SkolemedEntity,Consequent1]]),
      sigma_I_infer_nonvar(_Context_atom,[skolemize,_SkolemedEntity],D,tabled_true,SKOLEMPROOF),
      sigma_I_infer_nonvar(_Context_atom,Antecedant,D,_Result,Proof),
      (sigma_A_do_assert_list(_Context_atom,skolemized(SKOLEMPROOF),Consequent1)).

skolem_gen(X):-
      idGen(Y),
      number_codes(Y,CY),
      atom_codes(X,[70,110|CY]). % Adds "'AssignmentFn'" to the next generated identity

%===================================
% Agregation
%===================================


% Meta-Recursion
sigma_I_agreggate(_Context_atom,[sigma_I_infer_nonvar,_Context_atom,FormulaA,Current_Depth,_Result,Proof],Current_Depth,_Result,[sigma_I_infer_nonvar,_Context_atom,FormulaA,Current_Depthepth,_Result,Proof]) :- !,
            sigma_I_infer(_Context_atom,FormulaA,Current_Depthepth,_Result,Proof).


% subquery-ist/2
sigma_I_agreggate(_Context_atom,[subquery,OtherContext_atom,Prop],_Depth,_Result,[subquery,OtherContext_atom,Proof]) :-
               sigma_I_infer(OtherContext_atom,Prop,_Depth,_Result,(Proof)).

% forall
sigma_I_agreggate(_Context_atom,[Forall,X,G],_Depth,Result,[and|ListProof]) :- (member(Forall,[forall,forAll,findall])),!,
                  findall([thereExists,X,Proof],sigma_I_infer(_Context_atom,G,_Depth,Result,Proof),ListProof).


% forAll/2   !
sigma_I_agreggate(_Context_atom,[forAll,L,G],_Depth,_Result,(forAll(L,G) '=>' _Result)) :- 
      (forAll(_Context_atom,L,G),_Result = tabled_true; _Result = not_proved).
%forAll(_Context_atom,L,G) :- copy_term((L,G),(L1,G1)), L = L1,sigma_invoke_obj(_Context_atom,G1),not(not(_Context_atom,G)).

% forAll/3    !
sigma_I_agreggate(_Context_atom,[forAll,G],_Depth,_Result,(forAll(G) '=>' _Result)) :- 
     (forAll(_Context_atom,G),_Result = tabled_true; _Result = not_proved).
%forAll(_Context_atom,G)  :-  G \= (_,_), provenot(not(_Context_atom,G)).

%not(_Context_atom,FormulaA) :- nonvar(FormulaA),FormulaA=[not,FormulaB],sigma_invoke_obj(_Context_atom,FormulaB).
%not(_Context_atom,[Pred|ArgS]) :- not_inferSurface(_Context_atom,[Pred|ArgS]).
%not(_Context_atom,FormulaA) :- nonvar(FormulaA),functor(FormulaA,F,AA),system(F/_),not(FormulaA).

%setof

sigma_I_agreggate(_Context_atom,[setof,X,G,L],_Depth,_Result,( Proof '=>' setof(X,G,L) )) :- 
     setof(p(X,_Depth,R,P),sigma_I_infer_nonvar(_Context_atom,G,_Depth,R,P),ListProof),
     filter_proof(_Context_atom,ListProof,_Result,L,Proof).

      filter_proof(_Context_atom,[p(_X,_Depth,not_proved,P)],not_proved,[],seprovenull(P)).

            filter_proof(_Context_atom,ListProof,tabled_true,L,setfull(LProof)) :-
                 filter(_Context_atom,ListProof,[],L,LProof).
            
            filter(_Context_atom,[],_,[],[]).
            filter(_Context_atom,[p(X,_,R,_)|Rest],L1,L,LProof) :-
                 (R = not_proved; member(X,L1)),!,filter(_Context_atom,Rest,L1,L,LProof).
            filter(_Context_atom,[p(X,_,tabled_true,P)|Rest],L1,[X|Xs],[P|Ps]) :-
                 filter(_Context_atom,Rest,[X|L1],Xs,Ps).

               
%===================================
% How Known
%===================================

sigma_I_conclusion(_Context_atom,[backwards_chain,true,Cons,Antec],_Depth,tabled_true,(Antec'=>'Cons)) :- backwards_chain(_Context_atom,Cons,Antec).


sigma_I_conclusion(_Context_atom,[asserted,true,FormulaA],_Depth,tabled_true,[asserted,FormulaA]) :-   
               kb(__Context_atom,FormulaA).

sigma_I_conclusion(_Context_atom,[asserted,false,FormulaA],_Depth,tabled_true,[asserted,FormulaA]) :-   
               kb(__Context_atom,[not|FormulaA]).

sigma_I_conclusion(_Context_atom,[deduced_forward,true,FormulaA],_Depth,tabled_true,[deduced_forward,true,FormulaA]) :-   
               inferSurface(__Context_atom,FormulaA,_Proof).

sigma_I_conclusion(_Context_atom,[deduced_forward,false,FormulaA],_Depth,tabled_true,[deduced_forward,false,FormulaA]) :-   
               inferSurface_false(__Context_atom,FormulaA,_Proof).

sigma_I_conclusion(_Context_atom,[consultion,true,FormulaA],_Depth,tabled_true,[consultion,FormulaA]) :-   
               consultation(__Context_atom,FormulaA).

sigma_I_conclusion(_Context_atom,[consult,true,FormulaA],_Depth,tabled_true,[consult,FormulaA]) :-   
               agentConsultation(__Context_atom,_,FormulaA,_ListOfGafsAsserted).




% ===================================================================
% File 'sigma_nassert.pl' 
% Authors:  Jay Halcomb ; Douglas Miles
% Contact:  jhalcomb@teknowledge.com ; dmiles@teknowledge.com ; apease@teknowledge.com
% Version: 'sigma_nassert.pl' 1.0.0 
%
% History:
% Created - 2000/02/08 dmiles@teknowledge.com
% ===================================================================

% ===================================================================
%     EXPORTS
% ===================================================================



/*
assert([]):-!. 
assert(end_of_file):-!. 
assert(List):-length(List,X),X<8,!. 

assert(Assert_chars):- 
            once((conv_tell(Assert_chars,PrologForm),tell_sterm(PrologForm))).

retract_native(T):-!,retractall(T).


tell_sterm(end_of_file):-!.
tell_sterm(comment(_C)):-!.
tell_sterm(browser_only(_C)):-!.
tell_sterm(query(_C)):-!.
tell_sterm(=>(A,C)):-!,write_clause_to_file(assert((':-'(C,A)))).
%tell_sterm(=>(A,not(C))):-!,write_clause_to_file(assert((not(C):-A))).
tell_sterm(or(A,B)):-!,
      tell_sterm((A)),
      tell_sterm((B)).
tell_sterm(or(A,B)):-!,
      tell_sterm((A)),
      tell_sterm((B)).


:-dynamic(constsymbol/1).

tell_sterm(PrologKR):- 
	surface_to_normal(PrologKR,PrologForm),
	assertions_to_xkb(PrologForm).

surface_to_normal(PrologKR,PrologForm):-	
	getNegationForm(PrologKR,PrologForm).


assertions_to_xkb(PrologForm):-  %%trace,
   		 mine_semanitics(PrologForm), %trace,
          write_clause_to_file(assert(PrologForm)),nl. % Asserts if Unknown



mine_semanitics(Var):-var(Var),!.  %,handle_entities(Var).

mine_semanitics(Pred):-nonvar(Pred),
				Pred=..[F,X,Y],
				member(F,[genlPreds,genlInverse]),!, % List of 2nd Order Logical Predicates
            write_clause_to_file(assert(Pred)), % Asserts if Unknown
				set_for_genls_hilog(X),set_for_genls_hilog(Y).

mine_semanitics(Pred):-
				Pred=..[F,X,Y],
				'surface-instance'(F,'Connective',_),!, % List  Logical Predicates
				mine_semanitics(X),mine_semanitics(Y).

mine_semanitics(thereExists(V,F)):- %forAll
				handle_entities(V),
				mine_semanitics(F).

mine_semanitics(exists(V,F)):- %forAll
				handle_entities(V),
				mine_semanitics(F).

% Non Semantics?

%..

mine_semanitics(Atom):-atomic(Atom),!,
			handle_entities(Atom).

mine_semanitics(PrologForm):-
			PrologForm =.. [_|ARGS],
			mine_semanitics_l(ARGS),
			set_for_hilog_table(PrologForm).


mine_semanitics(_Atom):-!.


handle_entities(Atom):-ignore((nonvar(Atom),write_clause_to_file(assert(constsymbol(Atom))))).

mine_semanitics_l([]):-!.

mine_semanitics_l([PrologForm|PrologForms]):-
		mine_semanitics(PrologForm),
		mine_semanitics_l(PrologForms).



extract_syms([]).
extract_syms([Var]):-var(Var),!.
extract_syms([Atomic]):-atomic(Atomic),!,write_clause_to_file(assert(constsymbol(Atomic))).
extract_syms([Compound]):-compound(Compound),
	((
		((
		(Compound=..['holds'|ARGS],Compound2=..ARGS)
		;
		(Compound2=Compound)
		)),	
		Compound2=..[_|ARGSyms]
	)),extract_syms(ARGSyms).

extract_syms([_]):-!.
extract_syms([H|T]):-!,
		extract_syms([H]),
		extract_syms(T).
      
      */

%%clausifyP( =>(P,Q), C1, C2 ) :- !, clausifyP( P, C1, C3 ), clausifyP( Q, C3, C2 ).
clausifyP( and(P,Q), C1, C2 ) :- !, clausifyP( P, C1, C3 ), clausifyP( Q, C3, C2 ).
clausifyP( P, [cl(A,B)|Cs], Cs ) :- inclauseP( P, A, [], B, [] ), !.
clausifyP( _, C, C ).

clausifyS( [And,S,Q], C1, C2 ) :- nonvar(And),'surface-instance'(And,'ConjunctiveConnective',_), !, clausifyS( S, C1, C3 ), clausifyS( Q, C3, C2 ).
clausifyS( S, [cl(A,B)|Cs], Cs ) :- inclauseS( S, A, [], B, [] ), !.
clausifyS( _, C, C ).                                                      %DisjunctiveConnective

inclauseS( [Or,S,Q], A, A1, B, B1 ) :- nonvar(Or),'surface-instance'(Or,'DisjunctiveConnective',_), !,  inclauseS( S, A2, A1, B2, B1 ),
                                        inclauseS( Q, A,  A2, B,  B2 ).

inclauseP( or(P,Q), A, A1, B, B1 ) :- !, inclauseP( P, A2, A1, B2, B1 ),
                                        inclauseP( Q, A,  A2, B,  B2 ).

inclauseP( not(P), A,  A, B1, B ) :- !, notinP( P, A ), putinP( P, B, B1 ).
inclauseP( P,  A1, A, B,  B ) :- !, notinP( P, B ), putinP( P, A, A1 ).

inclauseS( [Non,S], A,  A, B1, B ) :- nonvar(Non),'surface-instance'(Non,'NegConnective',_), !, notinS( S, A ), putinS( S, B, B1 ).
inclauseS( S,  A1, A, B,  B ) :- !, notinS( S, B ), putinS( S, A, A1 ).

notinP(X,[Y|_]) :- X==Y, !, fail.
notinP(X,[_|Y]) :- !,notinP(X,Y).
notinP(_,[]).

notinS(X,[Y|_]) :- X==Y, !, fail.
notinS(X,[_|Y]) :- !,notinS(X,Y).
notinS(_,[]).

putinP(X,[],   [X]   ) :- !.
putinP(X,[Y|L],[Y|L] ) :- X == Y,!.
putinP(X,[Y|L],[Y|L1]) :- putinP(X,L,L1).

putinS(X,[],   [X]   ) :- !.
putinS(X,[Y|L],[Y|L] ) :- X == Y,!.
putinS(X,[Y|L],[Y|L1]) :- putinS(X,L,L1).


tqd:- cd('C:\cygwin\tks\sigma\design\test_questions').
% ======================================================================
% Negation normal form: nnf2(A,A1), where A is any legal formula, and A1 returns 
% the negation normal form of A, which is the result of moving forAll negations inward
% until they bind only atoms. 
% ======================================================================

%Leave Vars alone
nnf2(VAR,[VAR],VAR,1) :-var(VAR),!.   

 % Universal Quantification
nnf2([All,X,F],FreeV,[All,X,NNF],Paths) :-  'surface-instance'(All,'UniversalQuantifier',_Cxt),!,   
	nnf2(F,[X|FreeV],NNF,Paths).

% Existentual Quantification
nnf2([Exists,X,Fml],FreeV,NNF,Paths) :-  'surface-instance'(Exists,'ExistentualQuantifier',_Cxt),!,  
   %Create the 'equal' term
   skolem2(Fml,X,FreeV,FmlSk),    
   %Re-test to make sure nothing was forgotten
	nnf2(FmlSk,FreeV,NNF,Paths).    

/*

getNegationForm(exists(X,Fml),FreeV,exists(X,NNF),Paths) :- !,
	getNegationForm(F,[X|FreeV],NNF,Paths),
	'equal'(Fml,X,FreeV,FmlSk),
	getNegationForm(FmlSk,FreeV,NNF,Paths).

*/

nnf2([And,A,B],FreeV,NNF,Paths) :- 'surface-instance'(And,'ConjunctiveConnective',_Cxt),!, %Split to mine out        
	%Handle both clauses 
   nnf2(A,FreeV,NNF1,Paths1),    
	nnf2(B,FreeV,NNF2,Paths2),
   %Conjunctives Create combinational output
	Paths is Paths1 * Paths2,
   %put the greatest likely to fail clause in front
	(Paths1 > Paths2 -> NNF = [And,NNF2,NNF1];  
		            NNF = [And,NNF1,NNF2]).

nnf2([Or,A,B],FreeV,NNF,Paths) :- 'surface-instance'(Or,'DisjunctiveConnective',_Cxt),!, %Split to mine 
	%Handle both clauses
   nnf2(A,FreeV,NNF1,Paths1),    
	nnf2(B,FreeV,NNF2,Paths2),
	%Disjunctives create less output per clause but 'more terms'
   Paths is Paths1 + Paths2,     
   %put the smaller easiest to prove clause in front
   (Paths1 > Paths2 -> NNF = [Or,NNF2,NNF1]; 
		            NNF = [Or,NNF1,NNF2]).
  
nnf2(Fml,FreeV,NNF,Paths) :- 
   'surface-instance'(Non,'NegConnective',_Cxt),    %Enumerate forAll low level symbols 
   'surface-instance'(Or,'DisjunctiveConnective',_Cxt),              % "
   'surface-instance'(And,'ConjunctiveConnective',_Cxt),
   'surface-instance'(Implies,'ImplicationConnective',_Cxt),
   'surface-instance'(Equiv,'EquivalancyConnective',_Cxt),          % "
   'surface-instance'(All,'UniversalQuantifier',_Cxt),
   'surface-instance'(Exists,'ExistentualQuantifier',_Cxt),!,        % Search below for a template match and use it
  ((Fml = [Non,[Non,A]] ->  nnf2(A,FreeV,NNF,Paths));
	 Fml =[Non,[All,X,F]]  -> nnf2([Exists,X,[Non,F]],FreeV,NNF,Paths);
	 Fml =[Non,[Exists,X,F]]   ->nnf2([All,X,[Non,F]],FreeV,NNF,Paths);
	 Fml =[Non,[Or,A,B]]   -> nnf2([And,[Non,A],[Non,B]],FreeV,NNF,Paths);
	 Fml =[Non,[And,A,B]]   -> nnf2([Or,[Non,A],[Non,B]],FreeV,NNF,Paths);
	 Fml =[Implies,A,B]   ->nnf2([Or,[Non,A],B],FreeV,NNF,Paths);
	 Fml =[Non,[Implies,A,B]]  -> nnf2([And,A,[Non,B]],FreeV,NNF,Paths);
	 Fml =[Equiv,A,B]  -> nnf2([Or,[And,A,B],[And,[Non,A],[Non,B]]],FreeV,NNF,Paths);
	 Fml =[Non,[Equiv,A,B]] -> nnf2([Or,[And,A,[Non,B]],[And,[Non,A],B]],FreeV,NNF,Paths);
    NNF=Fml,Paths=1).     %No match was found

/*

:-table('Merge_BaseKB'/1).
:-table('Merge_BaseKB'/2).
:-table('Merge_BaseKB'/3).
:-table('Merge_BaseKB'/4).
:-table('Merge_BaseKB'/5).
:-table('Merge_BaseKB'/6).
:-table('Merge_BaseKB'/7).
:-table('Merge_BaseKB'/8).
:-table('Merge_BaseKB'/9).

:-table('neg_Merge_BaseKB'/1).
:-table('neg_Merge_BaseKB'/2).
:-table('neg_Merge_BaseKB'/3).
:-table('neg_Merge_BaseKB'/4).
:-table('neg_Merge_BaseKB'/5).
:-table('neg_Merge_BaseKB'/6).
:-table('neg_Merge_BaseKB'/7).
:-table('neg_Merge_BaseKB'/8).
:-table('neg_Merge_BaseKB'/9).


skolem2( F, X, FreeV, FmlSk) :-  
	entity_gen(Entity),   % Create a new 'symbolic name'
   close_freeVars(FreeV,CloseFreeV),   %XSB leaves the tail of the varaibles.. open.. we must close it
	Sk = ['AssignmentFn',Entity,CloseFreeV],  %Construct the SK-Term
   subst( F, X, Sk, FmlSk ).   % Replace the Skolems Var with the Term


conv_pred(_C,[Pred|ARGS],[browser_only,[Pred|ARGS]]):-member(Pred,[
                                    exampleAssertions,
                                    exampleNATs,
                                    arg1Format,
                                    arg2Format,
                                    arg3Format,
                                    arg4Format,
                                    arg5Format,
                                    arg6Format,
                                    arg7Format,
                                    arg8Format,
                                    arg9Format,
                                    argFormat,
                                    'SUMO-BasedProject',
                                    'SUMOConstant',
                                    'SUMOExpression',
                                    'SUMOFormula',
                                    'SUMOIndexedTerm',
                                    'SUMOAssertionDirection',
                                    'SUMOlist',
                                    cyckifrovenotes,
                                    'SUMOlistsMt',
                                    'SUMOSystemAtom',
                                    'SUMOSystemInteger',
                                    'prettyName',
                                    'SUMOSystemKeyword',
                                    'SUMOSystemList',
                                    'SUMOSystemRealNumber',
                                    'SUMOSystemString',
                                    'SUMOSystemSymbol',
                                    'SUMOTerm',
                                    'constantID',
                                    'constanprovename',
                                    substring,
                                    'myCreationPurpose',
                                    'myCreationSecond',
                                    'myCreationTime',
                                    'myCreator',
                                    'substring-CaseInsensitive',
                                    'SetTheFormat',
                                    'evaluateAtEL',
                                    sharedNotes,
                                    genFormat,
                                    'Guest',
                                    'HumanSUMOlist',
                                    genKeyword,
                                    'DocumentationConstant',
                                    'ELFormulaTemplate',
                                    'elInverse',
                                    'ELRelation',
                                    'ELTemplate',
                                    'equalStrings-CaseInsensitive',
                                    'equalSymbols',
                                    'EverythingPSC',
                                    'genMassNoun',
                                    'oldConstanprovename',
                                    'PATR-Specification',
                                    'genPreferredKeyword',
                                    'comment',
                                    'Format',
                                    'indexicalReferent',
                                    'InferencePSC',
                                    'LinguisticAVM',
                                    'stringSubword',
                                    'ListTheFormat',

                                    '1']).


r_m_p(_C,[argsIsa,Pred,Class],['domain',Pred,_,Class]).
r_m_p(_C,[arg1Isa,Pred,Class],['domain',Pred,1,Class]).
r_m_p(_C,[arg2Isa,Pred,Class],['domain',Pred,2,Class]).
r_m_p(_C,[arg3Isa,Pred,Class],['domain',Pred,3,Class]).
r_m_p(_C,[arg4Isa,Pred,Class],['domain',Pred,4,Class]).
r_m_p(_C,[arg5Isa,Pred,Class],['domain',Pred,5,Class]).
r_m_p(_C,[arg6Isa,Pred,Class],['domain',Pred,6,Class]).
r_m_p(_C,[arg7Isa,Pred,Class],['domain',Pred,7,Class]).
r_m_p(_C,[arg8Isa,Pred,Class],['domain',Pred,8,Class]).
r_m_p(_C,[arg9Isa,Pred,Class],['domain',Pred,9,Class]).

r_m_p(_C,[argsisa,Pred,Class],['domainSubclass',Pred,_,Class]).
r_m_p(_C,[arg1isa,Pred,Class],['domain',Pred,1,Class]).
r_m_p(_C,[arg2isa,Pred,Class],['domain',Pred,2,Class]).
r_m_p(_C,[arg3isa,Pred,Class],['domain',Pred,3,Class]).
r_m_p(_C,[arg4isa,Pred,Class],['domain',Pred,4,Class]).
r_m_p(_C,[arg5isa,Pred,Class],['domain',Pred,5,Class]).
r_m_p(_C,[arg6isa,Pred,Class],['domain',Pred,6,Class]).
r_m_p(_C,[arg7isa,Pred,Class],['domain',Pred,7,Class]).
r_m_p(_C,[arg8isa,Pred,Class],['domain',Pred,8,Class]).
r_m_p(_C,[arg9isa,Pred,Class],['domain',Pred,9,Class]).

r_m_p(_C,[argsGenls,Pred,Class],['domainSubclass',Pred,_,Class]).
r_m_p(_C,[arg1Genl,Pred,Class],['domainSubclass',Pred,1,Class]).
r_m_p(_C,[arg2genls,Pred,Class],['domainSubclass',Pred,2,Class]).
r_m_p(_C,[arg2Genl,Pred,Class],['domainSubclass',Pred,2,Class]).
r_m_p(_C,[arg3Genl,Pred,Class],['domainSubclass',Pred,3,Class]).
r_m_p(_C,[arg4Genl,Pred,Class],['domainSubclass',Pred,4,Class]).
r_m_p(_C,[arg5Genl,Pred,Class],['domainSubclass',Pred,5,Class]).
r_m_p(_C,[arg6Genl,Pred,Class],['domainSubclass',Pred,6,Class]).
r_m_p(_C,[arg7Genl,Pred,Class],['domainSubclass',Pred,7,Class]).
r_m_p(_C,[arg8Genl,Pred,Class],['domainSubclass',Pred,8,Class]).
r_m_p(_C,[arg9Genl,Pred,Class],['domainSubclass',Pred,9,Class]).

r_m_p(_C,[interArgResultIsa,Pred,ArgN,Class1,Class2],[and,[isa,Pred,'Function'],[=>,[functsymbol,Pred,Arity],[nth_domain_related,Pred,ArgN,Arity,Class1,Class2]]]).
%r_m_p(_C,[interArgResultGenl,Pred,ArgN,Class1,Class2],[and,[isa,Pred,'Function'],[=>,[functsymbol,Pred,Arity],[nth_domain_related,Pred,ArgN,Arity,Class1,Class2]]]).


r_m_p(_C,['interArgIsa',Pred,N1,Class1,N2,Class2],[nth_domain_related,Pred,N1,N2,Class1,Class2]).
r_m_p(_C,['interArgIsa1-2',Pred,Class1,Class2],[nth_domain_related,Pred,1,2,Class1,Class2]).
r_m_p(_C,['interArgIsa1-3',Pred,Class1,Class2],[nth_domain_related,Pred,1,3,Class1,Class2]).
r_m_p(_C,['interArgIsa1-4',Pred,Class1,Class2],[nth_domain_related,Pred,1,4,Class1,Class2]).
r_m_p(_C,['interArgIsa1-5',Pred,Class1,Class2],[nth_domain_related,Pred,1,5,Class1,Class2]).
r_m_p(_C,['interArgIsa1-6',Pred,Class1,Class2],[nth_domain_related,Pred,1,6,Class1,Class2]).
r_m_p(_C,['interArgIsa2-2',Pred,Class1,Class2],[nth_domain_related,Pred,2,2,Class1,Class2]).
r_m_p(_C,['interArgIsa2-3',Pred,Class1,Class2],[nth_domain_related,Pred,2,3,Class1,Class2]).
r_m_p(_C,['interArgIsa2-4',Pred,Class1,Class2],[nth_domain_related,Pred,2,4,Class1,Class2]).
r_m_p(_C,['interArgIsa2-5',Pred,Class1,Class2],[nth_domain_related,Pred,2,5,Class1,Class2]).
r_m_p(_C,['interArgIsa2-6',Pred,Class1,Class2],[nth_domain_related,Pred,2,6,Class1,Class2]).
r_m_p(_C,['interArgIsa3-2',Pred,Class1,Class2],[nth_domain_related,Pred,3,2,Class1,Class2]).
r_m_p(_C,['interArgIsa3-3',Pred,Class1,Class2],[nth_domain_related,Pred,3,3,Class1,Class2]).
r_m_p(_C,['interArgIsa3-4',Pred,Class1,Class2],[nth_domain_related,Pred,3,4,Class1,Class2]).
r_m_p(_C,['interArgIsa3-5',Pred,Class1,Class2],[nth_domain_related,Pred,3,5,Class1,Class2]).
r_m_p(_C,['interArgIsa3-6',Pred,Class1,Class2],[nth_domain_related,Pred,3,6,Class1,Class2]).
r_m_p(_C,['interArgIsa4-2',Pred,Class1,Class2],[nth_domain_related,Pred,4,2,Class1,Class2]).
r_m_p(_C,['interArgIsa4-3',Pred,Class1,Class2],[nth_domain_related,Pred,4,3,Class1,Class2]).
r_m_p(_C,['interArgIsa4-4',Pred,Class1,Class2],[nth_domain_related,Pred,4,4,Class1,Class2]).
r_m_p(_C,['interArgIsa4-5',Pred,Class1,Class2],[nth_domain_related,Pred,4,5,Class1,Class2]).
r_m_p(_C,['interArgIsa4-6',Pred,Class1,Class2],[nth_domain_related,Pred,4,6,Class1,Class2]).
r_m_p(_C,['interArgIsa5-2',Pred,Class1,Class2],[nth_domain_related,Pred,5,2,Class1,Class2]).
r_m_p(_C,['interArgIsa5-3',Pred,Class1,Class2],[nth_domain_related,Pred,5,3,Class1,Class2]).
r_m_p(_C,['interArgIsa5-4',Pred,Class1,Class2],[nth_domain_related,Pred,5,4,Class1,Class2]).
r_m_p(_C,['interArgIsa5-5',Pred,Class1,Class2],[nth_domain_related,Pred,5,5,Class1,Class2]).
r_m_p(_C,['interArgIsa5-6',Pred,Class1,Class2],[nth_domain_related,Pred,5,6,Class1,Class2]).

r_m_p(_C,['interArgReln1-2',Pred,Relation],[nth_relation_exists,Pred,1,2,Relation]).
r_m_p(_C,['interArgReln1-3',Pred,Relation],[nth_relation_exists,Pred,1,3,Relation]).
r_m_p(_C,['interArgReln1-4',Pred,Relation],[nth_relation_exists,Pred,1,4,Relation]).
r_m_p(_C,['interArgReln1-5',Pred,Relation],[nth_relation_exists,Pred,1,5,Relation]).
r_m_p(_C,['interArgReln1-6',Pred,Relation],[nth_relation_exists,Pred,1,6,Relation]).
r_m_p(_C,['interArgReln2-2',Pred,Relation],[nth_relation_exists,Pred,2,2,Relation]).
r_m_p(_C,['interArgReln2-3',Pred,Relation],[nth_relation_exists,Pred,2,3,Relation]).
r_m_p(_C,['interArgReln2-4',Pred,Relation],[nth_relation_exists,Pred,2,4,Relation]).
r_m_p(_C,['interArgReln2-5',Pred,Relation],[nth_relation_exists,Pred,2,5,Relation]).
r_m_p(_C,['interArgReln2-6',Pred,Relation],[nth_relation_exists,Pred,2,6,Relation]).
r_m_p(_C,['interArgReln3-2',Pred,Relation],[nth_relation_exists,Pred,3,2,Relation]).
r_m_p(_C,['interArgReln3-3',Pred,Relation],[nth_relation_exists,Pred,3,3,Relation]).
r_m_p(_C,['interArgReln3-4',Pred,Relation],[nth_relation_exists,Pred,3,4,Relation]).
r_m_p(_C,['interArgReln3-5',Pred,Relation],[nth_relation_exists,Pred,3,5,Relation]).
r_m_p(_C,['interArgReln3-6',Pred,Relation],[nth_relation_exists,Pred,3,6,Relation]).
r_m_p(_C,['interArgReln4-2',Pred,Relation],[nth_relation_exists,Pred,4,2,Relation]).
r_m_p(_C,['interArgReln4-3',Pred,Relation],[nth_relation_exists,Pred,4,3,Relation]).
r_m_p(_C,['interArgReln4-4',Pred,Relation],[nth_relation_exists,Pred,4,4,Relation]).
r_m_p(_C,['interArgReln4-5',Pred,Relation],[nth_relation_exists,Pred,4,5,Relation]).
r_m_p(_C,['interArgReln4-6',Pred,Relation],[nth_relation_exists,Pred,4,6,Relation]).
r_m_p(_C,['interArgReln5-2',Pred,Relation],[nth_relation_exists,Pred,5,2,Relation]).
r_m_p(_C,['interArgReln5-3',Pred,Relation],[nth_relation_exists,Pred,5,3,Relation]).
r_m_p(_C,['interArgReln5-4',Pred,Relation],[nth_relation_exists,Pred,5,4,Relation]).
r_m_p(_C,['interArgReln5-5',Pred,Relation],[nth_relation_exists,Pred,5,5,Relation]).
r_m_p(_C,['interArgReln5-6',Pred,Relation],[nth_relation_exists,Pred,5,6,Relation]).

r_m_p(_C,[Isa,Pred,'UnaryFunction'],[and,[functsymbol,Pred,1],[Isa,Pred,'Function']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'UnaryPredicate'],[and,[functsymbol,Pred,1],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'UnaryRelation'],[and,[functsymbol,Pred,1],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'BinaryFunction'],[and,[functsymbol,Pred,2],[Isa,Pred,'Function']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'BinaryPredicate'],[and,[functsymbol,Pred,2],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'BinaryRelation'],[and,[functsymbol,Pred,2],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'TernaryFunction'],[and,[functsymbol,Pred,3],[Isa,Pred,'Function']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'TernaryPredicate'],[and,[functsymbol,Pred,3],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'TernaryRelation'],[and,[functsymbol,Pred,3],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'QuaternaryFunction'],[and,[functsymbol,Pred,4],[Isa,Pred,'Function']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'QuaternaryPredicate'],[and,[functsymbol,Pred,4],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'QuaternaryRelation'],[and,[functsymbol,Pred,4],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'QuintaryFunction'],[and,[functsymbol,Pred,5],[Isa,Pred,'Function']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'QuintaryPredicate'],[and,[functsymbol,Pred,5],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.
r_m_p(_C,[Isa,Pred,'QuintaryRelation'],[and,[functsymbol,Pred,5],[Isa,Pred,'Predicate']]):-'surface-instance'(Isa,'InvolvedInDomainConstraintPredicate',_),!.


/******************************************************************************
Purpose: 1. Compute normal forms for first-order formula.
         2. Skolemize first-order formula.

Version: 0.1

Language: SWI Prolog

Filename: nfs

Date: 1999.7.15

Author: Anthony Aaby

Usage: see nfs_help

Copyright (C) 1999 Anthony A. Aaby

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

To reach the author send email to aabyan@wwc.edu
or Anthony Aaby
   Computer Science Department
   Walla Walla College
   College Place, WA 99324

FORMULA SYNTAX
        not A = - A
        A/\B  = A & B
        A\/B  = A v B
        A=>B  = A => B
        A<=>B = A <=> B
     forAll(X,A) = forAll(X,A)
  exists(X,A) = ex(X,A)
          []F = always F
          <>F = likely F
          0 F = then F
   until(A,B) = until(A,B)
******************************************************************************/

%:- module(nfs,[getNegationForm/2,cnj_nf/2,getDisjForm/2,pnx_nf/2,clause_nf/2,nfs_help/0,e/0]).

:- op(400,fy,always).  % Necessity, Always
:- op(400,fy,likely).  % Possibly, Eventually
:- op(400,fy,then).  % Next time
:- op(400,fy,not).    % negation
:- op(500,xfy,and).   % conjunction
:- op(600,xfy,or).   % disjunction
:- op(650,xfy,=>).  % implication
:- op(700,xfy,<=>). % equivalence

%%% Negation Normal Form

% Usage: getNegationForm(+Fml, ?NNF)

getNegationForm(Fml,NNF) :- getNegationForm(Fml,[],NNF,_).

% not-------------------------------not
%  getNegationForm(+Fml,+FreeV,nonNNF,nonPaths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

getNegationForm(Var,FreeV,Var,Paths) :- var(Var),!.

getNegationForm(always F,FreeV,BOX,Paths) :- !,
	getNegationForm(F,FreeV,NNF,Paths), cnj_nf(NNF,CNF), alwaysRule(always CNF, BOX).

getNegationForm(likely F,FreeV,DIA,Paths) :- !,
	getNegationForm(F,FreeV,NNF,Paths), getDisjForm(NNF,DNF), likelyRule(likely DNF, DIA).

getNegationForm(then F,FreeV,CIR,Paths) :- !,
	getNegationForm(F,FreeV,NNF,Paths), thenRule(then NNF, CIR).

getNegationForm(until(A,B),FreeV,NNF,Paths) :- !,
	getNegationForm(A,FreeV,NNF1,Paths1),
	getNegationForm(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	NNF = until(NNF1, NNF2).

getNegationForm(forAll(X,F),FreeV,forAll(X,NNF),Paths) :- !,
	getNegationForm(F,[X|FreeV],NNF,Paths).

getNegationForm(exists(X,Fml),FreeV,NNF,Paths) :- !,
	'equal'(Fml,X,FreeV,FmlSk),
	getNegationForm(FmlSk,FreeV,NNF,Paths).


getNegationForm(A and B,FreeV,NNF,Paths) :- !,
	getNegationForm(A,FreeV,NNF1,Paths1),
	getNegationForm(B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> NNF = (NNF2 and NNF1);
		            NNF = (NNF1 and NNF2)).

getNegationForm(A or B,FreeV,NNF,Paths) :- !,
	getNegationForm(A,FreeV,NNF1,Paths1),
	getNegationForm(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2 -> NNF = (NNF2 or NNF1);
		            NNF = (NNF1 or NNF2)).

getNegationForm(Fml,FreeV,NNF,Paths) :- 
	(Fml = not(not A)      -> Fml1 = A;
	 Fml = not(always F)   -> Fml1 = likely (not F);
	 Fml = not(likely F)   -> Fml1 = always (not F);
	 Fml = not(then F)   -> Fml1 = then (not F);
	 Fml = not until(A,B)-> (getNegationForm(not A,FreeV,NNA,_), getNegationForm(not B,FreeV,NNB,_),
                             Fml1 = forAll(NNB) or until(NNB,NNA and NNB));
	 Fml = not forAll(X,F)  -> Fml1 = exists(X,not F);
	 Fml = not exists(X,F)   -> Fml1 = forAll(X,not F);
	 Fml = not(A or B)   -> Fml1 = not A and not B;
	 Fml = not(A and B)   -> Fml1 = not A or not B;
	 Fml = (A => B)   -> Fml1 = not A or B;
	 Fml = not(A => B)  -> Fml1 = A and not B;
	 Fml = (A <=> B)  -> Fml1 = (A and B) or (not A and not B);
	 Fml = not(A <=> B) -> Fml1 = (A and not B) or (not A and B)),!,
	getNegationForm(Fml1,FreeV,NNF,Paths).

getNegationForm(Lit,_,Lit,1).

alwaysRule(always (A and B), (BA) and (BB)) :- !, alwaysRule(always A,BA), alwaysRule(always B,BB).
alwaysRule(BOX, BOX).

likelyRule(likely (A or B), (DA) or (DB)) :- !, likelyRule(likely A,DA), likelyRule(likely B,DB).
likelyRule(DIA, DIA).

thenRule(then (A or B), (DA) or (DB)) :- !, thenRule(then A,DA), thenRule(then B,DB).
thenRule(then (A and B), (DA) and (DB)) :- !, thenRule(then A,DA), thenRule(then B,DB).
thenRule(CIR, CIR).

%%%  Conjunctive Normal Form (CNF) not- assumes Fml in NNF

% Usage: cnj_nf( +NNF, ?CNF )
cnj_nf(Var,Var) :- var(Var),!.

cnj_nf(P and Q, P1 and Q1):- !, cnj_nf(P, P1), cnj_nf(Q, Q1).
cnj_nf(P or Q,     CNF):- !, cnj_nf(P, P1), cnj_nf(Q, Q1), cnj_nf1(P1 or Q1, CNF).
cnj_nf(CNF,       CNF).

cnj_nf1((P and Q) or R, P1 and Q1):- !, cnj_nf1(P or R, P1), cnj_nf1(Q or R, Q1).
cnj_nf1(P or (Q and R), P1 and Q1):- !, cnj_nf1(P or Q, P1), cnj_nf1(P or R, Q1).
cnj_nf1(CNF,             CNF).


%%%  Disjunctive Normal Form (DNF) not- assumes Fml in NNF

% Usage: getDisjForm( +NNF, ?DNF )

getDisjForm(Var,Var) :- var(Var),!.
getDisjForm(P or Q, P1 or Q1):- !, getDisjForm(P, P1), getDisjForm(Q, Q1).
getDisjForm(P and Q,     DNF):- !, getDisjForm(P, P1), getDisjForm(Q, Q1), getDisjForm1(P1 and Q1, DNF).
getDisjForm(DNF,       DNF).

getDisjForm1(P and (Q or R), P1 or Q1):- !, getDisjForm1(P and Q, P1), getDisjForm1(P and R, Q1).
getDisjForm1((P or Q) and R, P1 or Q1):- !, getDisjForm1(P and R, P1), getDisjForm1(Q and R, Q1).
getDisjForm1(DNF,             DNF).

%%%  Prenex Normal Form (PNF)

% Usage: pnx_nf( +Fml, ?PNF ) not- assumes Fml in NNF

pnx_nf(F,PNF) :- pnx_nf(F,[],PNF).

% pnx_nf(+Fml, +Vars, ?PNF)
pnx_nf(Var,FreeV,Var) :- var(Var),!.

pnx_nf(     forAll(X,F),Vs, forAll(X,PNF)) :- !, pnx_nf(F,[X|Vs], PNF).
pnx_nf(      exists(X,F),Vs,  exists(X,PNF)) :- !, pnx_nf(F,[X|Vs], PNF).

pnx_nf(  exists(X,A) and B,Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnx_nf(Ay and B,[Y|Vs], PNF).
pnx_nf(  exists(X,A) or B,Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnx_nf(Ay or B,[Y|Vs], PNF).
pnx_nf( forAll(X,A) and B,Vs, forAll(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnx_nf(Ay and B,[Y|Vs], PNF).
pnx_nf( forAll(X,A) or B,Vs, forAll(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnx_nf(Ay or B,[Y|Vs], PNF).

pnx_nf( A and  exists(X,B),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnx_nf(A and By,[Y|Vs], PNF).
pnx_nf( A or  exists(X,B),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnx_nf(A or By,[Y|Vs], PNF).
pnx_nf( A and forAll(X,B),Vs, forAll(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnx_nf(A and By,[Y|Vs], PNF).
pnx_nf( A or forAll(X,B),Vs, forAll(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnx_nf(A or By,[Y|Vs], PNF).

pnx_nf(        A and B,Vs,       PNF ) :- pnx_nf(A,Vs,Ap), pnx_nf(B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnx_nf(Ap and Bp,Vs,PNF).
pnx_nf(        A or B,Vs,       PNF ) :- pnx_nf(A,Vs,Ap), pnx_nf(B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnx_nf(Ap or Bp,Vs,PNF).

pnx_nf(          PNF, _,       PNF ).

%%%  Clausal Form (CF) not- assumes Fml in PNF and
%                                 each quantified variable is unique

% clause_nf(+Fml, ?Cs)
% Cs is a list of the form: [cl(Head,Body), ...]
% Head and Body are lists.

% ======================================================================
%  Clausify (see also, Skolemize)
%  Clausal Form (CF) -- assumes Fml in NNF and that each quantified variable is unique
% Usage: clause_nf(+Fml, ?Cs)
% Cs is a list of the form: [cl(Head,Body), ...]
% Head and Body are lists.
% ======================================================================

%clause_nf(PNF, Cla):- removeQuantifiers(PNF,[], UnQ), cnj_nf(UnQ,CNF), clausify(CNF,Cla,[]).

clause_nf(PNF, Cla):- 
     getPrologVars(PNF,FreeVars,_,_),  
     removeQuantifiers(PNF,FreeVars, UnQ), cnj_nf(UnQ,CNF), clausify(CNF,Cla,[]).
clause_nf(PNF, Cla):- removeQuantifiers(PNF,[], UnQ), cnj_nf(UnQ,CNF), clausify(CNF,Cla,[]).

% removes quantifiers
removeQuantifiers(Var,FreeV,Var) :- var(Var),!.
removeQuantifiers( forAll(X,F),Vars, RQ) :- removeQuantifiers(F,[X|Vars], RQ).
removeQuantifiers(  exists(X,F),Vars, RQ) :- skolemize(F,X,Vars,Fsk),
                               removeQuantifiers(Fsk,Vars, RQ).
removeQuantifiers( F,_,F ).

clausify(Var,Var,[]) :- var(Var),!.
clausify(Var,Var,Var) :- var(Var),!.
clausify( (P and Q), C1, C2 ) :- !, clausify( P, C1, C3 ), clausify( Q, C3, C2 ).
clausify( P, [cl(A,B)|Cs], Cs ) :- inclause( P, A, [], B, [] ), !.
clausify( _, C, C ).

inclause( (P or Q), A, A1, B, B1 ) :- !, inclause( P, A2, A1, B2, B1 ),
                                        inclause( Q, A,  A2, B,  B2 ).

inclause( not P, A,  A, B1, B ) :- !, nonin( P, A ), putin( P, B, B1 ).
inclause( P,  A1, A, B,  B ) :- !, nonin( P, B ), putin( P, A, A1 ).

nonin(X,[Y|_]) :- X==Y, !, fail.
nonin(X,[_|Y]) :- !,nonin(X,Y).
nonin(_,[]).

putin(X,[],   [X]   ) :- !.
putin(X,[Y|L],[Y|L] ) :- X == Y,!.
putin(X,[Y|L],[Y|L1]) :- putin(X,L,L1).

% ======================================================================
%  Skolemizing -- Two methods
% Method used:
% Usage: 'equal'( +Fml, +X, +FreeV, ?FmlSk )
% Replaces '$existential'ly quantified variable with a unique function
% fN(Vars) N=1,...
% VARIABLES MAY BE EITHER PROLOG VARIABLES OR TERMS
% ======================================================================

'equal'( F, X, FreeV, FmlSk) :- 
	entity_gen(Entity),
   close_freeVars(FreeV,CloseFreeV),
	Sk =..['AssignmentFn',Entity,CloseFreeV],
   subst( F, X, Sk, FmlSk ).

close_freeVars(XX,X):-
                 append(X,[_],XX).
          
%Todo Closing List if there are no free getPrologVars

entity_gen(Ent):-
      idGen(X),number_codes(X,Number),
      atom_codes(Ent,[101,110,116,105,116,121|Number]).  %"entity#"


% Usage: skolemize(+Fml,+X,+FreeV,?FmlSk)
% Replaces '$existential'ly quantified variable with the formula
% VARIABLES MUST BE PROLOG VARIABLES
% exists(X,p(X)) not-> p(p(exists))

skolemize(Fml,X,FreeV,FmlSk):-
	copy_term((X,Fml,FreeV),(Fml,Fml1,FreeV)),
	copy_term((X,Fml1,FreeV),(exists,FmlSk,FreeV)).


%%% Substitution

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

subst( forAll(Y,P), X,Sk, forAll(Y,P1) ) :- !, subst( P,X,Sk,P1 ).
subst(  exists(Y,P), X,Sk,  exists(Y,P1) ) :- !, subst( P,X,Sk,P1 ).
subst(    P and Q, X,Sk,   P1 and Q1 ) :- !, subst( P,X,Sk,P1 ),
                                         subst( Q,X,Sk,Q1 ).
subst(    P or Q, X,Sk,   P1 or Q1 ) :- !, subst( P,X,Sk,P1 ),
                                         subst( Q,X,Sk,Q1 ).
subst(        P, X,Sk,        P1 ) :- functor(P,_,N),
                                      subst1( X, Sk, P, N, P1 ).

subst1( _,  _, P, 0, P  ).
subst1( X, Sk, P, N, P1 ) :- N > 0, P =..[F|Args], subst2( X, Sk, Args, ArgS ),
                             P1 =..[F|ArgS].

subst2( _,  _, [], [] ).
subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, subst2( X, Sk, As, AS).
subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, subst2( X, Sk, As, AS).
subst2( X, Sk, [A|As], [Ap|AS] ) :- subst( A,X,Sk,Ap ),
                                    subst2( X, Sk, As, AS).

/********************************  The End ***********************************/

         /********************************  The End ***********************************/
/* File:      consult.pl
** $Id: sigma_unused.pl,v 1.19 2002/03/04 20:51:53 dmiles Exp $
*/
/*
:- compiler_options([xpp_on]).
#include "standard.h"
#include "char_defs.h"
#include "flag_defs_xsb.h"
  */
compile(Path) :- compile(Path, []).	% compile with no options

compile(X, _) :-
	var(X), !,
	abort('Uninstantiated argument 1 of compile/[1,2], cannot compile!').
compile([], _) :- !.
compile([H|T], Options) :- !,
	\+ (\+ (compile0(H,Options))),
	compile(T, Options).
compile(Path, Options) :- \+ (\+ (compile0(Path,Options))).

compile0(Path, Options) :-
	atom(Path),
	search_module(Path, Dir, Mod, SExt, Base, _Obj),
	SExt \== 'O',
	!,
	compile_f(SExt, Base, Options, _, Mod, Dir).
compile0(Path, _) :-
	file_write0(STDERR, 'Cannot find the file/module '),
	file_write0(STDERR, Path), file_nl(STDERR), fail.

compile_f('P', Base, Opts, ExList, Mod, _) :- 
	compile(Mod, Base, Opts, ExList).
compile_f('c', Base, Opts, ExList, Mod, Dir) :- 
	compile_cH(Mod, Base, Opts, ExList, Dir).
compile_f('', Base, Opts, ExList, Mod, _) :-
	atom_concat(Base, '.pl', FileName),
	sys_link(Base, FileName, _Result),
	compile(Mod, Base, Opts, ExList),
	rm(FileName).

reconsult0(Path, Options, Ensure_loaded) :-
	(Path == user
	 ->	compile_load_user(Options)
	 ;	(search_module(Path, Dir, Mod, SExt, Base, Obj)
		 ->	expand_filename(Obj,ExpObj),
			((SExt \== 'O', i_want_compile(SExt, Base, Obj))
			 ->	compile_f(SExt,Base,Options,_ExList,Mod,Dir),
				load_exec(Obj, Mod),
				(consult_file_loaded(ExpObj)
				 ->	true
				 ;	assert(consult_file_loaded(ExpObj))
				)
			 ;      ((Ensure_loaded==true,
				 consult_file_loaded(ExpObj))
				 ->     true
			         ;	load_exec(Obj, Mod),
				        (consult_file_loaded(ExpObj)
					 ->	true
					 ;	assert(consult_file_loaded(ExpObj))
					)
				)
			)
		)
	 ;	file_write0(STDERR, 'Cannot find the file/module '),
		file_write0(STDERR, Path), file_nl(STDERR), fail
	).

i_want_compile(SExt, Base, Obj) :-
	( not(exists_file(Obj)) -> true ; needs_recompile(SExt, Base, Obj) ).

needs_recompile('P', Base, Obj) :-
	( atom_concat(Base, '.pl', PFileName),
	  file_time(Obj, time(OTime1,OTime2)), 
	  file_time(PFileName, time(PTime1,PTime2)),
	  time(OTime1,OTime2) @< time(PTime1,PTime2)
	)
	;
	( atom_concat(Base, '.H', HFileName),
	  file_time(Obj, time(OTime1,Otime2)),
	  %% If no .H file exists, then HTime = 0
	  file_time(HFileName, time(HTime1,HTime2)), 
	  time(OTime1,Otime2) @< time(HTime1,HTime2)
	).	  
needs_recompile('', Base, Obj) :-
	file_time(Obj, time(OTime1,OTime2)), 
	file_time(Base, time(PTime1,PTime2)),
	time(OTime1,OTime2) @< time(PTime1,PTime2).
needs_recompile('c', Base, Obj) :-
	xsb_configuration(os_type, OS_type),
	( ( str_sub('solaris', OS_type) -> atom_concat(Base, '.so', C_Obj)
          ; (str_sub('linux', OS_type) -> atom_concat(Base, '.so', C_Obj))
	  ; (str_sub('windows', OS_type) -> atom_concat(Base, '.dll', C_Obj)
            ; atom_concat(Base, '.o', C_Obj)
	    )
	  ),
	  ( exists_file(C_Obj) -> 
		file_time(C_Obj, time(COTime1,COTime2)),
		atom_concat(Base, '.c', CFileName),
		file_time(CFileName, time(CTime1,CTime2)),
		time(COTime1,COTime2) @< time(CTime1,CTime2)
	  ; true
	  )
	)
	; 
	( atom_concat(Base, '.H', HFileName),
	  file_time(Obj, time(OTime1,OTime2)), 
	  file_time(HFileName, time(HTime1,HTime2)), 
	  time(OTime1,OTime2) @< time(HTime1,HTime2)
	).


/*======================================================================*/
/*  search_module(+FileName, -Dir, -Mod, -SExt, -Base, -Obj)	        */
/*                                                                      */
/*	Given a FileName (must be an atom), search_module/6 finds the	*/
/*	location of the corresponding module and returns:		*/
/*		1) the directory (Dir),					*/
/*		2) the module name (Mod),				*/
/*		3) the backchain (SExt) of the source file		*/
/*		   ('P'/'c'/''[other name]/'O'[object file only]),	*/
/*		4) the base name of the file (Base = Dir+Mod), and	*/
/*		5) the object file name (Obj)				*/
/* 	The call will fail if the specified file cannot be found.	*/
/*     	       	       	       	       	       	       	       	        */
/*======================================================================*/

search_module(FileName, Dir, Mod, Ext, Base, Obj) :-
	almost_search_module(FileName, D, Mod, E, B),
	( D == '', FileName == Mod -> % only a module name was given
	    real_search_module(Mod, Dir, Base, Ext)
	; Dir = D, Ext = E, Base = B
	),
	atom_concat(Base, '.O', Obj).

/*======================================================================*/
/* real_search_module(+ModName, -Dir, -Base, -Ext)			*/
/*======================================================================*/

real_search_module(ModName, Dir, Base, Ext) :-
        libpath(Dir),
        atom_concat(Dir, ModName, Base),
	existing_file_extension(Base, Ext).

/*======================================================================*/
/* load_exec(+File, +ModName)						*/
/*	Changed to use the C loader (code_load/3) that automatically	*/
/*	imports forAll exported predicates of the module into the current	*/
/*	working module when its third argument is 1.  - Kostis (4/3/93)	*/
/*======================================================================*/

load_exec(File, ModName) :-	
	code_load(File, Init, 1),	% use the changed C loader.
	% jf: for Windows a valid file pointer might be negative
	( Init =\= 0			% load succeeded
	; Init =:= 0, file_write0(STDERR, 'Error in loading file '), 
		file_write0(STDERR, File),
		file_nl(STDERR), fail
	),
	!,
	stat_flag(BANNER_CTL,BannerCtl),
	(   KeepQuiet is BannerCtl mod QUIETLOAD, KeepQuiet =:= 0
	->  true
	;   file_write0(STDMSG, '['), file_write0(STDMSG, ModName),
	    file_write0(STDMSG, ' loaded]'), file_nl(STDMSG)
	),
	(Init > 4			% Prolog byte code loaded
	 ->	co_code_call(Init, true, 2),	% call '_$main'
		unload_seg(Init)		% free space for '_$main'
	 ;	true			% Foreign obj code loaded
	).

co_code_call(A,B,C) :- code_call(A,B,C).

compile_load_user(Options) :-
	compile(user, user, Options, _), !,
	load_exec('user.O', user),
	rm('user.O').

/*-------------define load_dyn for reading in dynamic predicates -----*/

read_canonical(Term) :- 
	current_input_port(File), 
	file_read_canonical(File,Term,_).

cvt_canonical(InFile,OutFile) :-
	atom(InFile), atom(OutFile),
	seeing(OldInFile),
	expand_filename(InFile,InFilename),
	see(InFilename),
	telling(OldOutFile),
	expand_filename(OutFile,OutFilename),
	tell(OutFilename),
	repeat,
	read(Term),
	(Term == end_of_file
	 -> 	told, tell(OldOutFile), seen, see(OldInFile),!
	 ;	expand_term(Term,Term1),
		write_canonical_list(Term1),
		fail
	).

write_canonical_list([]) :- !.
write_canonical_list([Term|Terms]) :- !,
	write_canonical_list(Term),write_canonical_list(Terms).
write_canonical_list(Term) :-
	(Term = (:-(op(A,B,C)))
	 ->	op(A,B,C)
	 ;	true
	),
	write_canonical(Term),write_ln('.').


load_dyn(File) :-
	load_dyn(File,1).
load_dyn(File,Dir) :-
	(atom(File)
	 ->	true
	;	file_write0(STDERR, 'Wrong type in argument 1 of load_dyn/1'),
		fail
	),
	get_fname(File,SExt,Base,Mod),
	load_dyn0(SExt,Base,Mod,Dir).

get_fname(Filename,SExt,Base,Mod) :-
	( search_module(Filename,_Dir,Mod,SExt,Base,_Obj) -> true
	; warning('Cannot find file'(Filename)), fail
	).

load_dyn0(SExt,Base,Mod,Dir) :-
	get_HRname(SExt,Base,Hfname,Rfname),
	expand_filename(Rfname,ARfname),
	init_for_load(ARfname, Mod),
	(Hfname == []
         ->     true
         ;      load_dyn1(Hfname,Dir)
        ),
	load_dyn1(ARfname,Dir),
	cleanup_for_load.

get_HRname(SExt,Base,Hfname,Rfname) :-
	(SExt == ''	% no suffix
	 ->     Rfname = Base,
		Hfname = []
	 ;      dotted_ext(SExt, DExt),
	        atom_concat(Base, DExt, Rfname),
		(SExt == 'P'
		 ->	atom_concat(Base, '.H', Hfname0),
			(exists_file(Hfname0)
			 ->	Hfname = Hfname0
			 ;	Hfname = []
			)
		 ;	Hfname = []
		)
	).

dotted_ext(Ext,DExt) :- atom_concat('.',Ext,DExt).

load_dyn1(Rfname,Dir) :-
	cputime(T0),
	seeing(OldFile),
	open_or_xpp_file(Rfname, XPP_process),
	current_input_port(XPPport), % port from process, if it was launched
	(   read_and_assert(Dir)
	%% If needs restart, close old file and skip to the second load_dyn1
	->  ( conget(needs_restart,1) -> seen, see(OldFile), fail ; true)
	;   conset(needs_restart, 0), conset(xpp_on,0)
	),
	(   XPP_process = none, ! 
	;   %% Wait, if gpp was spawned so as to not leave zombies
	    process_control(XPP_process, wait),
	    %% Also, release the file descriptor used to read from gpp
	    file_close(XPPport)
	),
	seen,
	see(OldFile),
	time_message(T0,(Rfname,' dynamically loaded')).
load_dyn1(Rfname,Dir) :- 
	%% If we are restarting due to gpp, then reset needs_restart.
	conget(needs_restart,1), conset(needs_restart, 0),
	load_dyn1(Rfname,Dir).


:- dynamic load_dyn_trie_retracted(_).
:- dynamic load_dyn_retracted(_,_,_,_,_).
:- dynamic load_dyn_file_loaded(_,_).
:- dynamic load_dyn_pred_loaded(_,_,_,_).
:- dynamic load_dyn_file(_).
:- dynamic load_dyn_module(_).

init_for_load(Filename,Mod) :-
	conset(needs_restart, 0), conset(xpp_on, 0), % Cleanup before XPP
	file_time(Filename,time(Time1,Time2)),
	retractall(load_dyn_file_loaded(Filename,_)),
	assert(load_dyn_file_loaded(Filename,time(Time1,Time2))),
	retractall(load_dyn_trie_retracted(_)),
	retractall(load_dyn_retracted(_,_,_,_,_)),
	assert(load_dyn_file(Filename)),
	retractall(load_dyn_pred_loaded(Filename,_,_,_)),
	assert(load_dyn_module(Mod)).

cleanup_for_load :-
	conset(needs_restart, 0), conset(xpp_on, 0), % Cleanup after XPP
	retractall(load_dyn_trie_retracted(_)),
	retractall(load_dyn_retracted(_,_,_,_,_)),
	retractall(load_dyn_file(_)),
	load_dyn_module(ModName),
	multifile_query(ModName),
	multifile_apply(ModName),
	retractall(load_dyn_module(_)),
	retractall('_$multifile'(_)),
	retractall('_$multifile2'(_,_,_)).
                                                             

%-------------------------------------------------------------------------
% multifile_apply(ModName, Module) adds a query
%	:- multifile([holds/A, holds(_,_), apply_file1(_,_)]).
% to the source program for each
%	'_$apply_arity'(A)
% where file1 is the ModName.
%-------------------------------------------------------------------------
multifile_apply(ModName) :-
	(retract('_$apply_arity'(A)) 
	->	get_p_mod(holds, ModName, P_Mod),
		functor(TT1, holds, A),
		TT1 =.. [holds|Args],
		TT2 =.. [P_Mod|Args],
		call(multifile([holds/A, TT1, TT2])),
		multifile_apply(ModName)
	;	true
        ).
	
%-------------------------------------------------------------------------
% multifile_query(ModName) adds a query
%       :- multifile([P/A, P(_,_), P_file1(_,_)]).
% to the source program for each
%       '_$multifile1'(P/A)
% where file1 is the ModName.
%-------------------------------------------------------------------------
multifile_query(ModName) :-
	(retract('_$multifile1'(P/A)) 
	->	get_p_mod(P, ModName, P_Mod),
		functor(TT1, P, A),
		TT1 =.. [P|Args],
		TT2 =.. [P_Mod|Args],
		call(multifile([P/A, TT1, TT2])),
		multifile_query(ModName)
	;	true).
 
 
tabled_this_term(end_of_file,ModName):-!.
tabled_this_term(NT,ModName):-member(ModName,['inference_engine/sigma_dynamics','inference_engine/startup_persist']),!.

tabled_this_term((Head:-Tail),ModName):-functor(Head,F,A),
            tabled_this(F/A,ModName),!.
tabled_this_term((Head),ModName):-functor(Head,F,A),
            tabled_this(F/A,ModName),!.
tabled_this_term(_,_):-!.

tabled_this(PredSpecs,ModName):-%writeq((PredSpecs,ModName)),nl,
            add_table(PredSpecs).

%% This version is used for load_dyn1 only!
read_and_assert(Dir) :-
	read(Term1),              
	expand_term(Term1,Term2), 
	load_dyn_module(ModName), 
	      tabled_this_term(Term2,ModName),
         change_multifile_directive(Term2, ModName, Term),
	(Term == end_of_file
	 ->	!, fail
	 ;	do_assert_and_fail(Term,Dir)
	).
%% Don''t loop, if restart is requested
read_and_assert(_) :- conget(needs_restart,1), !.
read_and_assert(Dir) :- read_and_assert(Dir).

load_dync(File) :- load_dync(File,1).  % assertz
load_dync(File,Dir) :-
	(atom(File)
	 ->	true
	 ;	file_write0(STDERR, 'Wrong type in argument 1 of load_dync/1'),
		fail
	),
	get_fname(File,SExt,Base,Mod),
	load_dync0(SExt,Base,Mod,Dir).

load_dync0(SExt,Base,Mod,Dir) :-
	get_HRname(SExt,Base,Hfname,Rfname),
	expand_filename(Rfname,ARfname),
	init_for_load(ARfname, Mod),
	(Hfname == []
         ->     true
         ;      load_dync1(Hfname,Dir)
        ),
	load_dync1(ARfname,Dir),
	cleanup_for_load.


load_dync1(Rfname,Dir) :-
	cputime(T0),
	seeing(OldFile),
	open_or_xpp_file(Rfname, XPP_process),
	current_input_port(XPPport),
	file_read_canonical(-1000,0,0),	% initialize previous psc
	(   read_and_assert(XPPport,Dir)
	%% If needs restart, close old file and skip to the second load_dyn1
	->  (conget(needs_restart,1) -> seen, see(OldFile), fail ; true)
	;   conset(needs_restart, 0), conset(xpp_on,0)
	),
	(   XPP_process = none, ! 
	;   %% Wait, if gpp was spawned so as to not leave zombies
	    process_control(XPP_process, wait),
	    %% Also, release the file descriptor used to read from gpp
	    file_close(XPPport)
	),
	seen,
	see(OldFile),
	time_message(T0,(Rfname,' dynamically loaded')).
load_dync1(Rfname,Dir) :- 
	%% If we are restarting due to gpp, then reset needs_restart.
	conget(needs_restart,1), conset(needs_restart, 0),
	load_dync1(Rfname,Dir).


%% This version is used for load_dync1 only!
read_and_assert(IPort,Dir) :-
	file_read_canonical(IPort,Term0,Opsc),	% Opsc: Old(Previous) psc
%	expand_term(Term0,Term1),	% should we do this?
	load_dyn_module(ModName),
	%change_multifile_directive(':-'(multifile(Term0)), ModName, []),
   change_multifile_directive(Term0, ModName, Term),
	(Opsc =\= 0,\+'_$trie_asserted'(Term)
	 ->	load_dyn_retracted(Opsc,Prref,NArity,Index,Hashsize),
		assert_code_to_buff(Term),
		assert_buff_to_clref(Term,NArity,Prref,Dir,Index,Hashsize),
		fail
	 ;	Prref=_,NArity=_,Index=_,Hashsize=_	 
	),
	(Term == end_of_file
	 ->	!, fail
	 ;	do_assert_and_fail(Term,Dir)
	).
read_and_assert(_IPort,_Dir) :- conget(needs_restart,1), !.
read_and_assert(IPort,Dir) :- read_and_assert(IPort,Dir).


/* In change_multifile_directive(TermIn, ModName, TermOut), TermOut may be 
   bound to [] (when TermIn is a multifile directive like :- multifile p/2, 
   q/3).  So we have to add the following line for do_assert_and_fail/2.
*/

do_assert_and_fail([],_) :- !, fail.

do_assert_and_fail(Term,Dir) :-	% Now Term is the output of
				% change_multifile_directive(TermIn,_,Term)
	(Term = (:-Cmd)
	 ->	proc_directive(Cmd),fail
	 ;	true
	),
	(Term = (Head :- _)
	 ->	true
	 ;	Head = Term
	),
	(predicate_property(Head,built_in)
	 ->	warning('Cannot assert to builtin'(Head)),fail
	 ;	true
	),
	(   '_$multifile2'(Origin_Head, _, Head)
	 ->	conpsc(Origin_Head, OPsc), conpsc(Head, Psc)
	 ;	conpsc(Head, Psc), OPsc = Psc
	),
	('_$trie_asserted'(Head) 
	->  (Term = (_ :- _)
	    ->  warning('Asserting a nonfact into a trie, ignored'(Term)),
		fail
	    ;   true
	    ),
	    (load_dyn_trie_retracted(OPsc) 
	    ->  true
	    ;   psc_arity(Psc,Arity),
		psc_name(Psc,Name),
		functor(Gen,Name,Arity),
		retractall(Gen),
		asserta(load_dyn_trie_retracted(OPsc))
	    ),
	    t_assert(Term, _Flag)
	;
	    (load_dyn_retracted(OPsc,Prref,NArity,Index,Hashsize)
	    ->	true
	    ;	psc_arity(Psc,Arity),
		psc_name(Psc,Name),
		functor(Gen,Name,Arity),
		retractall(Gen),
		set_retracted(Head,Arity,OPsc,Psc,Prref,Index,Hashsize),
		NArity is Arity+1
	    ),
	    (Term = (_ :- Body)
	    ->	Head =.. Hlist,
		append(Hlist,[Cutpoint],Nhlist),
		Nhead =.. Nhlist,
		goal_cut_trans(Body,Nbody,Cutpoint),
		(assert_code_to_buff((Nhead:-Nbody)),fail;true)
	    ;	(assert_code_to_buff(Term),fail;true)
	    ),
	    assert_buff_to_clref(Head,NArity,Prref,Dir,Index,Hashsize)
	),
	fail.


/* In set_retracted/7, Head is the predicate which has been transformed by
   change_multifile_directive/3.  So p(_,_) may have been transformed to 
   p_file1(_,_). 

   OPsc is the Original Psc (e.g. for p(_,_)), while Psc is the actually
   used Psc (e.g. for p_file1(_,_)).  OPsc is only used in 
   load_dyn_retracted(OPsc,Prref,NArity,Index,Hashsize).  So, after
   file_read_canonical(IPort,Term0,OPsc) reads a new Term0 (say p(_,_)),
   and the psc address is the equal as that of the previous read term,
   then this term can be asserted directly.   
*/

set_retracted(Head,Arity,OPsc,Psc,Prref,Index,Hashsize) :-
	('_$index'(Head,Index,Hashsize)
	 ->	true
	 ; Arity =:= 0
	 ->	Index = 0,default_assert_size(Hashsize)
	 ;	Index = 1,default_assert_size(Hashsize)
	),
	NArity is Arity+1,		% to hold the cut addr
	psc_type(Psc, SYMTYPE),
	(SYMTYPE =\= 1
	 ->	dynamic(Head)
	 ;	true
	),
	psc_tabled(Psc, Tabled),
	(Tabled =:= 0
	 ->	psc_ep(Psc, Prref)		/* get the Prref */
	 ;	psc_ep(Psc, Prrefa),
		buff_word(Prrefa, 24, Prref)	% !! into calld
	),
	asserta(load_dyn_retracted(OPsc,Prref,NArity,Index,Hashsize)),
	load_dyn_file(Filename),
	asserta(load_dyn_pred_loaded(Filename,Head,Index,Hashsize)).

proc_directive(export(_X)) :- !,
	warning('export directive ignored.').
proc_directive(local(_X)) :- !,
	warning('local directive ignored.').
proc_directive(import(from(X, Mod))) :- !,
	import(from(X, Mod)).
proc_directive(index(X)) :- !,
	proc_index(X).
proc_directive(index(Ps,Arg,Size)) :- !,
	index(Ps,Arg,Size).
proc_directive(index(Ps,trie)) :- !,
	index(Ps,trie).
proc_directive(index(Ps,Arg)) :- !,
	index(Ps,Arg,0).
proc_directive(mode(_X)) :- !,
	warning('mode directive ignored.').
proc_directive(parallel(_X)) :- !,
	warning('parallel directive ignored.').
proc_directive(table(Pred)) :- !,
	proc_table(Pred).
proc_directive(table_all) :- !, 
	warning('table_all directive ignored. Use table/n explicitly').
proc_directive(op(P,T,S)) :- !, op(P,T,S).
proc_directive(hilog(X)) :- !, add_hilog_symbol(X).
%proc_directive(multifile(P/A)) :- !,
%	functor(Term,P,A),
%	conpsc(Term,Psc),
%	psc_arity(Psc,Arity),
%	set_retracted(Term,Arity,Psc,_,_,_).

proc_directive(compiler_options(L)) :-
	!,
	(memberchk(xpp_on,L), conget(xpp_on, 0) 
	->  conset(needs_restart,1), conset(xpp_on, 1)
	;   true
	).

proc_directive(Cmd) :-
	call(Cmd)
	 ->	true
	 ;	warning('Command failed.')
	.

proc_index((Pred, Preds)) :- !,
	proc_index(Pred),
	proc_index(Preds).
proc_index(Pname/Arity-Arg) :- !,
	index(Pname/Arity, Arg, 0).
proc_index(Pname/Arity) :- 
	index(Pname/Arity, 1, 0).

proc_table((Pred, Preds)) :- !,
	proc_table(Pred),
	proc_table(Preds).
proc_table(Pname/Arity) :- 
	table(Pname/Arity).

ensure_loaded(File,dyn) :- ensure_dyn_loaded(File,1).
ensure_loaded(File,dyna) :- ensure_dyn_loaded(File,0).
ensure_loaded(File,dync) :- ensure_dync_loaded(File,1).
ensure_loaded(File,dynca) :- ensure_dync_loaded(File,0).
ensure_loaded(File,consult) :- ensure_loaded(File).

ensure_loaded([]) :- !.
ensure_loaded([File|Files]) :- !, 
	ensure_loaded(File),
	ensure_loaded(Files).
ensure_loaded(File) :-
	reconsult(File,[],true).

ensure_dyn_loaded(Files) :-
	ensure_dyn_loaded(Files,1).

ensure_dyn_loaded([],_Dir) :- !.
ensure_dyn_loaded([File|Files],Dir) :- !, 
	ensure_dyn_loaded(File,Dir),
	ensure_dyn_loaded(Files,Dir).
ensure_dyn_loaded(File,Dir) :-
	(if_should_not_reload(File)
	 ->	true
	 ;	load_dyn(File,Dir)
	).

ensure_dync_loaded(Files) :-
	ensure_dync_loaded(Files,1).

ensure_dync_loaded([],_Dir) :- !.
ensure_dync_loaded([File|Files],Dir) :- !, 
	ensure_dync_loaded(File,Dir),
	ensure_dync_loaded(Files,Dir).
ensure_dync_loaded(File,Dir) :-
	(if_should_not_reload(File)
	 ->	true
	 ;	load_dync(File,Dir)
	).

if_should_not_reload(File) :-
	get_fname(File,SExt,Base,_Mod),
	get_HRname(SExt,Base,_Hfname,Filename),
	expand_filename(Filename,AFilename),
	file_time(AFilename,time(Ctime1,Ctime2)),
	load_dyn_file_loaded(AFilename,time(Ltime1,Ltime2)),
	time(Ltime1,Ltime2) @>= time(Ctime1,Ctime2),
	\+ need_more_index(AFilename).

need_more_index(Filename) :-
	load_dyn_pred_loaded(Filename,Head,OIndex,OHashsize),
	'_$index'(Head,Index,Hashsize),
	(Hashsize > OHashsize
	 ;
	 \+ indexes_subsumed(Index,OIndex)
	).

indexes_subsumed(X,X) :- !.
indexes_subsumed([],_).
indexes_subsumed([Ispec|Ispecs],OIndex) :-
	memberchk(Ispec,OIndex),
	indexes_subsumed(Ispecs,OIndex).
indexes_subsumed(Ispec,OIndex) :-
	memberchk(Ispec,OIndex).


reify_vars(_AssertClause,[]):-!.
reify_vars(Ground,_):-ground(Ground),!.
reify_vars(AssertClause,[VV|More]):-VV=..[_,NameVAR,NameVAR],!,reify_vars(AssertClause,More).
reify_vars(AssertClause,[_VV|More]):-!,reify_vars(AssertClause,More).

  

% ================================================================================
% add_context(C,A,B) adds context C to Single clause A and outputs B
% ================================================================================
%add_context(_,A,A):-not(),!.
add_context(_Context,Var,Var):-is_list(Var),!.
add_context(_Context,Var,Var):-var(Var),!.
add_context(_Context,Const,Const):-atomic(Const),!.
%add_context(_Context,believe(PROVEN,CN,Const),believe(PROVEN,CN,Const)):-!.
add_context(Context,GAF,OGAF):-
                     add_context_r(Context,GAF,OGAF1),
                     add_context_a(Context,OGAF1,OGAF).


add_context_a(Context,GAF,OGAF):- is_explorable(GAF),
                     GAF=..[H|ARGS],
                     add_context_l(Context,ARGS,AARGS),
                     OGAF=..[H|AARGS].
add_context_a(Context,OGAF,OGAF):-!.

add_context_l(_Context,[],[]):-!.
add_context_l(_Context,[H|T],[HH|TT]):-!,
      add_context(_Context,H,HH),
      add_context_l(_Context,T,TT).

add_context_r(Context,GAF,GAF):-  no_context(GAF),!.
add_context_r(Context,GAF,OGAF):-not( no_context(GAF) ),!,
      GAF=..LGAF,
      append(LGAF,[Context],OLGAF),
      OGAF=..OLGAF.
add_context_r(Context,GAF,GAF).

no_context(Var):-var(Var),!.
no_context(not(_)):-!.
no_context((_;_)):-!.
no_context((_,_)):-!.
no_context(':-'(_)):-!.
no_context(not(_)):-!.
no_context(neg(_)):-!.
no_context('<=>'(_)):-!.
no_context('=>'(_)):-!.
no_context('AssignmentFn'(_,_)):-!.
no_context(T):-functor(T,F,A),'surface-instance'(F,'Connective',_).
no_context(G):-predicate_property(G,(built_in)),!,fail.
no_context(G):-predicate_property(G,(loaded)),!,fail.
no_context(_):-!,fail.

is_explorable(not(_)).
is_explorable(neg(_)).
is_explorable(T):-functor(T,F,A),'surface-instance'(F,'Connective',_).
is_explorable((_;_)).
is_explorable((_:-_)).
is_explorable((_,_)).
is_explorable(G):-is_exported(G).

'known-constant'(member(_,_)).
'known-constant'((_=_)).
'known-constant'('instance'_of(_,_)).


% ================================================================================
% is_exported_f(X) checks to see if X is a known functor
% ================================================================================
is_exported_f(X):-GAF=..[X,_],is_exported(GAF),!.
is_exported_f(X):-is_exported(X),!.
is_exported_f(X):-GAF=..[X,_,_],is_exported(GAF),!.
is_exported_f(X):-GAF=..[X,_,_,_],is_exported(GAF),!.




% ================================================================================
% is_exported(G) checks to see if G is a known Term
% ================================================================================
is_exported(GAF):-
            GAF=..[H|ARGS],
            once((
            var(H);
            'surface-instance'(H,'Connective',Context)
            ;
            predicate_property(GAF,loaded)
            )).



chars_call_plex(Vars):-var(Vars),!,
      write_response_begin,
      ignore(ua_out(disp_error,'syntax error'('format for chars_call_plex/1 is chars_call_plex("append(A,B,[a,b,c,1,2,3])").  (You passed in a variable)'),_)),
      write_response_end.

chars_call_plex(Chars):-is_list(Chars),
         chars_to_term(Chars,Term,Vars),!,
         write_response_begin,
         sigma_inference_proc(Term,Vars),
         write_response_end.

chars_call_plex(Term):-ground(Term),!,
         write_response_begin,
         sigma_inference_proc(Term,Vars),
         write_response_end.

chars_call_plex(Prolog):-!,
         write_response_begin,
         ignore(sendNote(debug,logicEngine,'Debug Info','prolog cannot get varaibles'('you need to put quotes arround you call for chars_call_plex/1 is chars_call_plex("append(A,B,[a,b,c,1,2,3])").  '),_)),
         sigma_inference_proc(Term,Vars),
         write_response_end.

chars_call(Vars):-var(Vars),!,
      write_response_begin,
      ignore(ua_out(disp_error,'syntax error'('format for chars_call/1 is chars_call("append(A,B,[a,b,c,1,2,3])").  (You passed in a variable)'),_)),
      write_response_end.

chars_call(Chars):-is_list(Chars),
         chars_to_term(Chars,Term,Vars),!,
         write_response_begin,
         call_in_xml(Term,Vars),
         write_response_end.

chars_call(Term):-ground(Term),!,
         write_response_begin,
         call_in_xml(Term,Vars),
         write_response_end.

chars_call(Prolog):-!,
         write_response_begin,
         ignore(sendNote(debug,logicEngine,'Debug Info','prolog cannot get varaibles'('you need to put quotes arround you call for chars_call/1 is chars_call("append(A,B,[a,b,c,1,2,3])").  '),_)),
         call_in_xml(Term,Vars),
         write_response_end.

tck:-trace,chars_call_kif("( = X ?Y ) ").

chars_call_kif(Vars):-var(Vars),!,
      write_response_begin,
      ignore(ua_out(disp_error,'syntax error'('format for chars_call_kif/1 is chars_call_kif("( member ?X (SetFn (1 2 3) ) )").  (You passed in a variable)'),_)),
      write_response_end.

chars_call_kif(Chars):-is_list(Chars),!,
         guess_char_format(Chars,STerm,Vars),
         sterm_to_pterm_native(STerm,Term),
         write_response_begin,
         call_in_kif(Term,Vars),
         write_response_end.

chars_call_kif(Term):-ground(Term),!,
         write_response_begin,
         call_in_kif(Term,Vars),
         write_response_end.

chars_call_kif(Prolog):-!,
         write_response_begin,
         ignore(sendNote(debug,logicEngine,'Debug Info','prolog cannot get varaibles'('you need to put quotes arround you call for chars_call_kif/1 is chars_call_kif("append(A,B,[a,b,c,1,2,3])").  '),_)),
         call_in_kif(Term,Vars),
         write_response_end.

call_in_xml(Term,Vars):-
         fmt_write("<Answer>",_),
         Term,
         write_xml_vars_call(Vars),
         fail.
   
call_in_xml(Term,Vars):-!,
         fmt_write("</Answer>",_).

write_xml_vars_call(V):-var(V),
         fmt_write("<Result/>",_).

write_xml_vars_call(V):-
         fmt_write("<Result>",_),
         numbervars(V),!,
         write_xml_vars_call1(V),
         fmt_write("</Result>",_).

write_xml_vars_call1([]):-!.
write_xml_vars_call1([VV|R]):-!,
         VV=..[_,Name,Val],
         fmt_write("<Var name=""%S"">%S</Var>",args(Name,Val)),
         write_xml_vars_call1(R).
write_xml_vars_call1(_):-!.

call_in_kif(Term,Vars):-
         fmt_write("<Answer>",_),
         Term,
         write_kif_vars_call(Vars),
         fail.
   
call_in_kif(Term,Vars):-!,
         fmt_write("</Answer>",_).

write_kif_vars_call(V):-var(V),
         fmt_write("<Result/>",_).

write_kif_vars_call(V):-
         fmt_write("<Result>",_),
         numbervars(V),!,
         write_kif_vars_call1(V),
         fmt_write("</Result>",_).

write_kif_vars_call1([]):-!.
write_kif_vars_call1([VV|R]):-!,
         VV=..[_,Name,Val],
         toMarkUp(kif,Val,_,ValStr),
         fmt_write("<Var name=""%S"">%s</Var>",args(Name,ValStr)),
         write_kif_vars_call1(R).
write_kif_vars_call1(_):-!.




ua_read_from_socket(OptionList):- 
      once((ua_process_command(['read-file',me],OptionList))).

% Reading an Assertion Resource Location
   
% 
/*
ua_read(LocationObject):- ua_read(LocationObject,[]).

ua_read(InFile,OptionList):-
         ignore(member(xkb_output=XKBFile,OptionList)),
         write_response_begin,
         ua_out(user,['beginning to compile ', InFile,' to ',XKBFile]),
         ignore(can_to_xkb_file(_,InFile,XKBFile)),
         ua_out(user,['done compiling ', InFile,' to ',XKBFile]),
         ua_out(user,['beginning to load ', XKBFile]),
         ignore(load_compiled(Ctx,XKBFile)),
         ua_out(user,['finised loading ', XKBFile]),
         write_response_end.
  */




%=================================================================
% Add A Resource
%=================================================================

ua_resource_add(_AtomObject,FileName,OptionList):-
         ua_command([use_resource_file,FileName],OptionList).

                                                                                              /******************************************************************************
Purpose: 1. Compute normal forms for first-order formula.
         2. Skolemize first-order formula.

Version: 0.1

Language: SWI Prolog

Filename: nfs

Date: 1999.7.15

Author: Anthony Aaby

Usage: see nfs_help

Copyright (C) 1999 Anthony A. Aaby

This program is free software; you can redistribute it  and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place  not   Suite 330, Boston, MA  02111-1307, USA.

To reach the author send email to aabyan@wwc.edu
or Anthony Aaby
   Computer Science Department
   Walla Walla College
   College Place, WA 99324

FORMULA SYNTAX
        not A = not   A
        A/\B  = A  and  B
        A\/B  = A or B
        A=>B  = A => B
        A<=>B = A <=> B
     forAll(X,A) = forAll(X,A)
  exists(X,A) = exists(X,A)
          []F = known F
          <>F = consistent F
          0 F = next F
   until(A,B) = until(A,B)
******************************************************************************/

%:- module(nfs,[getNegationForm/2,cnj_nf/2,getDisjForm/2,pnx_nf/2,clause_nf/2,nfs_help/0,e/0]).

:- op(400,fy,known),  % Necessity, Always
   op(400,fy,consistent),  % Possibly, Eventually
   op(400,fy,next),  % Next time
   op(400,fy, not  ),    % negation
   op(500,xfy, and ),   % conjunction
   op(600,xfy,or),   % disjunction
   op(650,xfy,=>),  % implication
   op(700,xfy,<=>). % equivalence

e :- edit(nfs).

nfs_help :- nl,nl,nl,
   write('nfs version 0.1, Copyright (C) 1999 Anthony A. Aaby'),nl,
   write('nfs comes with ABSOLUTELY NO WARRANTY; This is free software,'),nl,
   write(' and you are welcome to redistribute it under GNU General'),nl,
   write('Public License.'),nl,
   write('Entry points are:'),nl,
   write('   getNegationForm(+Fml, ?NNF),'),nl,
   write('   cnj_nf(+Fml, ?CNF),'),nl,
   write('   getDisjForm(+Fml, ?DNF),'),nl,
   write('   pnx_nf(+Fml, ?PNF) not   Fml is in NNF, CNF, or DNF,'),nl,
   write('   clauses(+Fml, ?CN) not   Fml is in PNF,  and'), nl,
   write('   nfs_help.'), nl.

%%% Negation Normal Form

% Usage: getNegationForm(+Fml, ?NNF)

getNegationForm(Fml,NNF) :- getNegationForm(Fml,[],NNF,_).

% not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not   not  
%  getNegationForm(+Fml,+FreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

getNegationForm(known F,FreeV,BOX,Paths) :- !,
	getNegationForm(F,FreeV,NNF,Paths), cnj_nf(NNF,CNF), boxRule(known CNF, BOX).

getNegationForm(consistent F,FreeV,DIA,Paths) :- !,
	getNegationForm(F,FreeV,NNF,Paths), getDisjForm(NNF,DNF), diaRule(consistent DNF, DIA).

getNegationForm(next F,FreeV,CIR,Paths) :- !,
	getNegationForm(F,FreeV,NNF,Paths), cirRule(next NNF, CIR).

getNegationForm(until(A,B),FreeV,NNF,Paths) :- !,
	getNegationForm(A,FreeV,NNF1,Paths1),
	getNegationForm(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	NNF = until(NNF1, NNF2).

getNegationForm(forAll(X,F),FreeV,forAll(X,NNF),Paths) :- !,
	getNegationForm(F,[X|FreeV],NNF,Paths).

getNegationForm(exists(X,Fml),FreeV,NNF,Paths) :- !,
	skolemize(Fml,X,FreeV,FmlSk),
	getNegationForm(FmlSk,FreeV,NNF,Paths).

getNegationForm(A  and  B,FreeV,NNF,Paths) :- !,
	getNegationForm(A,FreeV,NNF1,Paths1),
	getNegationForm(B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2  -> NNF = (NNF2  and  NNF1);
		            NNF = (NNF1  and  NNF2)).

getNegationForm(A or B,FreeV,NNF,Paths) :- !,
	getNegationForm(A,FreeV,NNF1,Paths1),
	getNegationForm(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2  -> NNF = (NNF2 or NNF1);
		            NNF = (NNF1 or NNF2)).

getNegationForm(Fml,FreeV,NNF,Paths) :- 
	(Fml = not  ( not  A)      -> Fml1 = A;
	 Fml = not  (known F)   -> Fml1 = consistent ( not  F);
	 Fml = not  (consistent F)   -> Fml1 = known ( not  F);
	 Fml = not  (next F)   -> Fml1 = next ( not  F);
	 Fml = not until(A,B) -> (getNegationForm( not  A,FreeV,NNA,_), getNegationForm( not  B,FreeV,NNB,_),
                             Fml1 = forAll(NNB)or until(NNB,NNA  and  NNB));
	 Fml = not forAll(X,F)  -> Fml1 = exists(X, not(F));
	 Fml = not exists(X,F)   -> Fml1 = forAll(X,not(F));
	 Fml = not  (A or B)   -> Fml1 = not A  and  not B;
	 Fml = not  (A  and  B)   -> Fml1 = not A or not B;
	 Fml = (A => B)   -> Fml1 = not A or B;
	 Fml = not  (A => B)  -> Fml1 = A  and  not B;
	 Fml = (A <=> B)  -> Fml1 = (A  and  B) or ( not  A  and  not B);
	 Fml = not  (A <=> B) -> Fml1 = (A  and  not B) or ( not  A  and  B)),!,
	getNegationForm(Fml1,FreeV,NNF,Paths).

getNegationForm(Lit,_,Lit,1).

boxRule(known (A and B), (BA)  and  (BB)) :- !, boxRule(known A,BA), boxRule(known B,BB).
boxRule(BOX, BOX).
diaRule(consistent (A or B), (DA) or (DB)) :- !, diaRule(consistent A,DA), diaRule(consistent B,DB).
diaRule(DIA, DIA).
cirRule(next (A or B), (DA) or (DB)) :- !, cirRule(next A,DA), cirRule(next B,DB).
cirRule(next (A  and  B), (DA)  and  (DB)) :- !, cirRule(next A,DA), cirRule(next B,DB).
cirRule(CIR, CIR).

%%%  Conjunctive Normal Form (CNF) not   not   assumes Fml in NNF

% Usage: cnj_nf( +NNF, ?CNF )

cnj_nf(P  and  Q, P1  and  Q1):- !, cnj_nf(P, P1), cnj_nf(Q, Q1).
cnj_nf(P or Q,     CNF):- !, cnj_nf(P, P1), cnj_nf(Q, Q1), cnj_nf1(P1 or Q1, CNF).
cnj_nf(CNF,       CNF).

cnj_nf1((P  and  Q) or R, P1  and  Q1):- !, cnj_nf1(P or R, P1), cnj_nf1(Q or R, Q1).
cnj_nf1(P or (Q  and  R), P1  and  Q1):- !, cnj_nf1(P or Q, P1), cnj_nf1(P or R, Q1).
cnj_nf1(CNF,             CNF).

%%%  Disjunctive Normal Form (DNF) not   not   assumes Fml in NNF

% Usage: getDisjForm( +NNF, ?DNF )

getDisjForm(P or Q, P1 or Q1):- !, getDisjForm(P, P1), getDisjForm(Q, Q1).
getDisjForm(P  and  Q,     DNF):- !, getDisjForm(P, P1), getDisjForm(Q, Q1), getDisjForm1(P1  and  Q1, DNF).
getDisjForm(DNF,       DNF).

getDisjForm1(P  and  (Q or R), P1 or Q1):- !, getDisjForm1(P  and  Q, P1), getDisjForm1(P  and  R, Q1).
getDisjForm1((P or Q)  and  R, P1 or Q1):- !, getDisjForm1(P  and  R, P1), getDisjForm1(Q  and  R, Q1).
getDisjForm1(DNF,             DNF).

%%%  Prenex Normal Form (PNF)

% Usage: pnx_nf( +Fml, ?PNF ) not   not   assumes Fml in NNF

pnx_nf(F,PNF) :- pnx_nf(F,[],PNF).

% pnx_nf(+Fml, +Vars, ?PNF)

pnx_nf(     forAll(X,F),Vs, forAll(X,PNF)) :- !, pnx_nf(F,[X|Vs], PNF).
pnx_nf(      exists(X,F),Vs,  exists(X,PNF)) :- !, pnx_nf(F,[X|Vs], PNF).

pnx_nf(  exists(X,A)  and  B,Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnx_nf(Ay  and  B,[Y|Vs], PNF).
pnx_nf(  exists(X,A) or B,Vs,  exists(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnx_nf(Ay or B,[Y|Vs], PNF).
pnx_nf( forAll(X,A)  and  B,Vs, forAll(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnx_nf(Ay  and  B,[Y|Vs], PNF).
pnx_nf( forAll(X,A) or B,Vs, forAll(Y,PNF)) :- !, copy_term((X,A,Vs),(Y,Ay,Vs)),
                                        pnx_nf(Ay or B,[Y|Vs], PNF).

pnx_nf( A  and   exists(X,B),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnx_nf(A  and  By,[Y|Vs], PNF).
pnx_nf( A or  exists(X,B),Vs,  exists(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnx_nf(A or By,[Y|Vs], PNF).
pnx_nf( A  and  forAll(X,B),Vs, forAll(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnx_nf(A  and  By,[Y|Vs], PNF).
pnx_nf( A or forAll(X,B),Vs, forAll(Y,PNF)) :- !, copy_term((X,B,Vs),(Y,By,Vs)),
                                        pnx_nf(A or By,[Y|Vs], PNF).

pnx_nf(        A  and  B,Vs,       PNF ) :- pnx_nf(A,Vs,Ap), pnx_nf(B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnx_nf(Ap  and  Bp,Vs,PNF).
pnx_nf(        A or B,Vs,       PNF ) :- pnx_nf(A,Vs,Ap), pnx_nf(B,Vs,Bp), 
                                     (A\=Ap; B\=Bp), pnx_nf(Ap or Bp,Vs,PNF).

pnx_nf(          PNF, _,       PNF ).

%%%  Clausal Form (CF) not   not   assumes Fml in PNF  and
%                                 each quantified variable is unique

% clause_nf(+Fml, ?Cs)
% Cs is a list of the form: [cl(Head,Body), ...]
% Head  and Body are lists.

clause_nf(PNF, Cla):- removeQuantifiers(PNF,[], UnQ), cnj_nf(UnQ,CNF), clausify(CNF,Cla,[]).

% removes quantifiers
removeQuantifiers( forAll(X,F),Vars, RQ) :- removeQuantifiers(F,[X|Vars], RQ).
removeQuantifiers(  exists(X,F),Vars, RQ) :- skolemize(F,X,Vars,Fsk),
                               removeQuantifiers(Fsk,Vars, RQ).
removeQuantifiers( F,_,F ).

clausify( (P  and  Q), C1, C2 ) :- !, clausify( P, C1, C3 ), clausify( Q, C3, C2 ).
clausify( P, [cl(A,B)|Cs], Cs ) :- inclause( P, A, [], B, [] ), !.
clausify( _, C, C ).

inclause( (P or Q), A, A1, B, B1 ) :- !, inclause( P, A2, A1, B2, B1 ),
                                        inclause( Q, A,  A2, B,  B2 ).

inclause( not P, A,  A, B1, B ) :- !, notin( P, A ), putin( P, B, B1 ).
inclause( P,  A1, A, B,  B ) :- !, notin( P, B ), putin( P, A, A1 ).

notin(X,[Y|_]) :- X==Y, !, fail.
notin(X,[_|Y]) :- !,notin(X,Y).
notin(_,[]).

putin(X,[],   [X]   ) :- !.
putin(X,[Y|L],[Y|L] ) :- X == Y,!.
putin(X,[Y|L],[Y|L1]) :- putin(X,L,L1).

%%%  Skolemizing  not   not   Two methods

% Usage: skolemize(+Fml,+X,+FreeV,?FmlSk)
% Replaces '$existential'ly quantified variable with the formula
% VARIABLES MUST BE PROLOG VARIABLES
% exists(X,p(X)) not   -> p(p(exists))

skolemize(Fml,X,FreeV,FmlSk):-
	copy_term((X,Fml,FreeV),(Fml,Fml1,FreeV)),
	copy_term((X,Fml1,FreeV),(exists,FmlSk,FreeV)).

% Usage: skolem( +Fml, +X, +FreeV, ?FmlSk )
% Replaces '$existential'ly quantified variable with a unique function
% fN(Vars) N=1,...
% VARIABLES MAYBE EITHER PROLOG VARIABLES OR TERMS

skolem( F, X, FreeV, FmlSk) :- gensym( f, Fun ), Sk =..[Fun|FreeV],
                                subst( F, X, Sk, FmlSk ).

%%% Substitution

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

subst( forAll(Y,P), X,Sk, forAll(Y,P1) ) :- !, subst( P,X,Sk,P1 ).
subst(  exists(Y,P), X,Sk,  exists(Y,P1) ) :- !, subst( P,X,Sk,P1 ).
subst(    P  and  Q, X,Sk,   P1  and  Q1 ) :- !, subst( P,X,Sk,P1 ),
                                         subst( Q,X,Sk,Q1 ).
subst(    P or Q, X,Sk,   P1 or Q1 ) :- !, subst( P,X,Sk,P1 ),
                                         subst( Q,X,Sk,Q1 ).
subst(        P, X,Sk,        P1 ) :- functor(P,_,N),
                                      subst1( X, Sk, P, N, P1 ).

subst1( _,  _, P, 0, P  ).
subst1( X, Sk, P, N, P1 ) :- N > 0, P =..[F|Args], subst2( X, Sk, Args, ArgS ),
                             P1 =..[F|ArgS].

subst2( _,  _, [], [] ).
subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, subst2( X, Sk, As, AS).
subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, subst2( X, Sk, As, AS).
subst2( X, Sk, [A|As], [Ap|AS] ) :- subst( A,X,Sk,Ap ),
                                    subst2( X, Sk, As, AS).

/********************************  The End ***********************************/




list_to_atom_nc([],X):-idGen(Y),number_codes(Y,CY),atom_codes(X,CY).
list_to_atom_nc([H|T],A):-list_to_atom_nc(T,R),atom_concat(H,R,A).


delete_log([],[]):-!.
delete_log([A|B],C):-
	member(A,[and,or,=>,exists,',','$VAR',holds,<=>,not,forAll,(':-'),',',';','instance',equal,equal,'subclass']),!,
	delete_log(B,C).  
delete_log([A|B],C):-number(A),
	delete_log(B,C).         

delete_log([A|B],[A|C]):-
	delete_log(B,C).         

p2s(V,V):-isSlot(V),!.
p2s(P,P):-atomic(P),!.
p2s(P,S):-is_list(P),!,p2s_l(P,S).
p2s(P,S):-compound(P),!,
             P=..SS,
             p2s_l(SS,S).




p2s_l([],[]).
p2s_l([H|T],[HH|TT]):-
         p2s(H,HH),
         p2s_l(T,TT).

neg_ants_s([],[]):-!.
neg_ants_s([C|PURE],[CN|PURENEG]):-!,
   neg_ants(C,CN),
   neg_ants_s(PURE,PURENEG).

neg_ants([C],[C]):-!.
neg_ants([C|PURE],[C|PURENEG]):-!,
      negate_s(PURE,PURENEG),!.

conv_listcl([],[]).
conv_listcl([H|T],[OH|OT]):-
	conv_listcl_1(H,OH),
	conv_listcl(T,OT).

conv_listcl_1([H|B],cl([H],B)).
conv_listcl_1([H|B],cl([H],B)).

make_var_pairs([],[],[]).  %Close Tail of list
make_var_pairs([A|RGS],[B|RGN],[[nd,A,B]|NDS]):-
         make_var_pairs(RGS,RGN,NDS).
            

normalize_names_ss([],[]).  %The List version of the Above
normalize_names_ss([H|T],[HN|TN]):-
                  normalize_names_s(H,HN),      %Each Head
                  normalize_names_ss(T,TN).


clauses_to_pclauses(IN,OUT):-%trace, 
         clauses_to_pclauses0(IN,MID0),!, 
         unjoin_pclauses(MID0,MID1), %trace,
         clauses_to_pclauses2(MID1,MID2),          %trace,
         clauses_to_pclauses3(MID2,MID3),
         clauses_to_pclauses2(MID3,MID4),
         clauses_to_pclauses4(MID4,MID5),
         delete_duplicates(MID5,OUT).

clauses_to_pclauses0([],[]):-!.
clauses_to_pclauses0([cl([H|ARGS],[])|T],TT):-
         found_in0(H,ARGS),!,
         clauses_to_pclauses0(T,TT).
clauses_to_pclauses0([H|T],[H|TT]):-
         clauses_to_pclauses0(T,TT).

found_in0(not(H),ARGS):-imember(H,ARGS),!.
found_in0((H),ARGS):-imember(H,ARGS),!.

imember(H,[HH|_]):-H==HH,!.
imember(H,[_|TT]):-!,imember(H,TT).

clauses_to_pclauses4([],[]):-!.
clauses_to_pclauses4([CLAUSE|T],TT):-
         singletonic(CLAUSE),!,
         clauses_to_pclauses4(T,TT).
clauses_to_pclauses4([H|T],[H|TT]):-
         clauses_to_pclauses4(T,TT).

singletonic(CLAUSE):-nonvar(CLAUSE),getPrologVars(CLAUSE,Vars,Singles,_),length(Singles,N),!,N>3.
      


delete_duplicates([],[]):-!.
delete_duplicates([H|T],[H|Rest]):-!,
      delete_ident(T,H,HeadGone),
      delete_duplicates(HeadGone,Rest).

unjoin_pclauses(Var,Var):-isSlot(Var),!.
unjoin_pclauses([],[]):-!.
unjoin_pclauses([H|T],Out):-%trace,
      clause_to_pclauses(H,HOut),
      unjoin_pclauses(T,TOut),
      append(HOut,TOut,Out).

clauses_to_pclauses2(Var,Var):-isSlot(Var),!.
clauses_to_pclauses2([],[]):-!.
clauses_to_pclauses2([H|T],Out):-
      pclause_to_npclauses(H,HOut),
      clauses_to_pclauses2(T,TOut),
      append(HOut,TOut,Out).


pclause_to_npclauses(Var,Var):-isSlot(Var),!.
pclause_to_npclauses(cl(A,B),[]):-A==B,!. %Tautology
pclause_to_npclauses(cl(C,_),[]):-member(not('equal'(_,_)),C),!.
pclause_to_npclauses(cl(_,A),[]):-member(not('equal'(_,_)),A),!.
pclause_to_npclauses(cl([not(A)],ANT),[cl([neg(A)],SANT)]):-!,sort_ant(ANT,SANT).
pclause_to_npclauses(cl([(A)],ANT),[cl([(A)],SANT)]):-!,sort_ant(ANT,SANT).


clauses_to_pclauses3(Var,Var):-isSlot(Var),!.
clauses_to_pclauses3([],[]):-!.
clauses_to_pclauses3([H|T],Out):-
      no_redundant(H,HOut),
      clauses_to_pclauses3(T,TOut),
      append(HOut,TOut,Out).

no_redundant(Var,Var):-isSlot(Var),!.
%no_redundant(Var,Var):-!.
no_redundant(cl([neg(HEAD)],BODY),[cl([neg(HEAD)],NEWBODY)]):-!,delete_ident(BODY,not(HEAD),NEWBODY).
no_redundant(cl([(HEAD)],BODY),[cl([(HEAD)],NEWBODY)]):-!,delete_ident(BODY,(HEAD),NEWBODY).


sort_ant(A,C):-
   sort_ant1(A,B),
   sort_ant2(B,C).
   
sort_ant1(Var,Var):-isSlot(Var),!.
sort_ant1([],[]):-!.
sort_ant1([not(A),B],[B,not(A)]):-!.
sort_ant1([not(A),B,C],[B,C,not(A)]):-!.
sort_ant1([not(A),B,C,D],[B,C,D,not(A)]):-!.
sort_ant1(A,A):-!.
sort_ant2(Var,Var):-isSlot(Var),!.
sort_ant2([],[]):-!.
%sort_ant2(['equal'(X,Y),B],[B,'equal'(X,Y)]):-!.
%sort_ant2(['equal'(X,Y),B,C],[B,C,'equal'(X,Y)]):-!.
%sort_ant2(['equal'(X,Y),B,C,D],[B,C,D,'equal'(X,Y)]):-!.
sort_ant2(A,A):-!.

clause_to_pclauses(Var,Var):-isSlot(Var),!.
%clause_to_pclauses(cl(['equal'(_,_)],_),[]):-!. % Pog Exists
%clause_to_pclauses(cl(C,_),[]):-unifiable_member(not('equal'(_,_)),C),!.
%clause_to_pclauses(cl(_,A),[]):-unifiable_member(not('equal'(_,_)),A),!.
%clause_to_pclauses(cl([browser_only(_)],[]),[]):-!.
%clause_to_pclauses(cl(['browser-only'(_)],[]),[]):-!.
%clause_to_pclauses(cl([CON],[]),[cl([CON],[])]):-!.
clause_to_pclauses(cl(CON,A),List):- cv(CON,NEWCON),!,
               make_cl_s(NEWCON,A,List),!.
                  
/*
cv([A],[[A]]).
cv([A,B],[[A,B],[B,A]]).
cv([A,B,C],[[A,B,A],[B,A,C],[C,A,B]]).
cv([A,B,C,D],[[A,B,C,D],[B,A,C,D],[C,A,B,D],[D,A,B,C]]).
cv([A,B,C,D,E],[[A,B,C,D,E],[B,A,C,D,E],[C,A,B,D,E],[D,A,B,C,E],[E,A,B,C,D]]).
cv([A,B,C,D,E,Fml],[[A,B,C,D,E,Fml],[B,A,C,D,E,Fml],[C,A,B,D,E,Fml],[D,A,B,C,E,Fml],[E,A,B,C,D,Fml],[Fml,A,B,C,D,E]]).
  */

cv(Var,Var):-isSlot(Var),!.
cv(List,Out):-!,findall([E|New],(member(E,List),delete_ident(List,E,New)),Out),!.

make_cl_s(Var,_,Var):-isSlot(Var),!.
make_cl_s([],_,[]):-!.
make_cl_s([H|T],A,[HOut|TOut]):-!,
      make_cl(H,A,HOut),!,
      make_cl_s(T,A,TOut),!.

make_cl(Var,_,Var):-isSlot(Var),!.
make_cl([H|T],A,cl([CH],NewAnt)):-clean_head(H,CH),negate_s(T,NegT),!,append(NegT,A,NewAnt),!.

negate_s(Var,Var):-isSlot(Var),!.
negate_s([],[]):-!.
negate_s([H|T],[HOut|TOut]):-
      negate_i(H,HOut),!,
      negate_s(T,TOut),!.

negate_i(Var,Var):-isSlot(Var),!.
%negate_i(not(consistent(A)),(A)):-!.
negate_i(not(A),A):-!.
negate_i(A,not(A)):-!.

clean_head(Var,Var):-isSlot(Var),!.
clean_head(not(consistent(A)),not(A)):-!.
clean_head(A,A):-!.

pclauses_to_assertions([],nop):-!.
pclauses_to_assertions([cl([],[])],nop):-!.
pclauses_to_assertions([cl([A],[])],(A:-true)):-!.
pclauses_to_assertions([cl([A],PLIST)],(A:-Prolog)):-!,conv(PLIST,Prolog),!.
pclauses_to_assertions([CL|LIST],(C,PL)):-!,
            pclauses_to_assertions([CL],C),
            pclauses_to_assertions(LIST,PL).



   declausify(CF,QF):- %  Entry point into Two-Pass declausification
   declausify1(CF,AF), % Pass one: convert list back to prolog
   declausify2(AF,QF).  %Pass two: Simplify Clause


declausify1([],true):- !.
declausify1([cl([=>(BodyP,HeadP)],[])],(HeadP:-BodyP)):-!.
declausify1([cl(Head,Body)],(HeadP:-BodyP)):-conv_det(Head,HeadP),conv_det(Body,BodyP). %Simple Head/Body Build
declausify1([cl(Head,Body)|More],or((HeadP:-BodyP),MoreQuerys)):-!,  %Break clause list
       conv_det(Head,HeadP),conv_det(Body,BodyP), %Recurse for OR
          declausify1(More,MoreQuerys).


declausify2((true :- QF),not(QF)).  %Invert the inverted (TODO monitor)
declausify2((QF :- true),QF).       %Simplify Clause
declausify2((QF :- Given),or(Given,QF)). 



constrain_wff_argtypes(Fml,Fml):-var(Fml),!.
constrain_wff_argtypes((FmlA,FmlB),(FmlOutA,FmlOutB)):-!,
         constrain_wff_argtypes(FmlInA,FmlOutA),
         constrain_wff_argtypes(FmlInB,FmlOutB).

constrain_wff_argtypes(Fml,Fml):-ground(Fml),!.
constrain_wff_argtypes(FmlIn,FmlOut):-constrain_wff(FmlIn,FmlOut).

            
% Short circuit temporaily
%constrain_wff(A,A):-!.

% break appart into two tasks if disjunctive 
constrain_wff(or(A,B),or(AA,BB)):-   !,
         constrain_wff(A,AA),constrain_wff(B,BB).

constrain_wff(or(A,B),or(AA,BB)):-   !,
         constrain_wff(A,AA),constrain_wff(B,BB).
constrain_wff(';'(A,B),';'(AA,BB)):-   !,
         constrain_wff(A,AA),constrain_wff(B,BB).

% Add the constraints 
constrain_wff(A,C):- %trace,
         once(constrain_wff_0(A,B)),  %trace,
         once(constrain_wff_move(B,C)),!.

% Ignore Varaibles and constants
constrain_wff_0(Var,Var):-var(Var).
constrain_wff_0(Const,Const):-atomic(Const).

% explore into connectives
constrain_wff_0(Conj,ConjARGS):-
                     Conj=..[Conjunctive|ARGS],  nonvar(Conjunctive),
                     'logical-connective'(Conjunctive),
                     constrain_wff_ls(ARGS,AARGS),
                     ConjARGS=..[Conjunctive|AARGS].

'logical-connective'('or').
'logical-connective'('and').
'logical-connective'(';').
'logical-connective'(',').

% No connectives are found
constrain_wff_0(GAF,AGAF):-
      constrain_wff_1(GAF,AGAF).


% This is called without connectives being present
constrain_wff_1(GAF,CONSTRAINTS):-  %trace,
               GAF=..[P|ARGS],
               constrain_wff_arg(GAF,P,ARGS,1,ARGCONSTRAINTS),
               constrain_wff_funct(ARGCONSTRAINTS,GAF,CONSTRAINTS).

% List batch version
constrain_wff_ls(Var,Var):-var(Var),!.
constrain_wff_ls([],[]):-!.
constrain_wff_ls([H|T],[HH|TT]):-
      constrain_wff_0(H,HH),
      constrain_wff_ls(T,TT).
               

% Add functsymbol (Hilog) to terms with variables in the Clause
constrain_wff_funct(CONSTRAINED,GAF,CONSTRAINED):-
                  GAF=..[H|_],
                  nonvar(H),!.
constrain_wff_funct(ARGCONSTRAINTS,GAF,((functsymbol(H,N),GAF),ARGCONSTRAINTS)):-
                  GAF=..[H|ARGS],var(H),!,length(ARGS,N).


% Treats each sub argument
constrain_wff_arg(GAF,H,_,1,GAF):-nonvar(H),'instance'(H,'InvolvedInDomainConstraintPredicate'),!.
constrain_wff_arg(GAF,_P,[],_N,GAF):-!.
constrain_wff_arg(GAF,P,[A|RGS],N,('domain'(P,N,A),REST)):-var(A),
                 NN is N+1, 
                 constrain_wff_arg(GAF,P,RGS,NN,REST).
constrain_wff_arg(GAF,P,[A|RGS],N,(REST)):-nonvar(A),
                 NN is N+1, 
                 constrain_wff_arg(GAF,P,RGS,NN,REST).

% Example:  
% ?- constrain_wff_move((walks(_h99,home)  ','  'instance'(_h99,agent) :-   'instance'(_h99,human)),S).
% S = (walks(_h95,home) :- 'instance'(_h95,agent)  ','  'instance'(_h95,human))

constrain_wff_move(((A,Cons):-B),MvD):-
               functor(Cons,H,_),'instance'(H,'InvolvedInDomainConstraintPredicate'),!,
               constrain_wff_move((A :- (Cons,B)),MvD).

constrain_wff_move(((Cons,A):-B),MvD):-
               functor(Cons,H,_),'instance'(H,'InvolvedInDomainConstraintPredicate'),!,
               constrain_wff_move((A :- (Cons,B)),MvD).

constrain_wff_move(MvD,MvD):-!.

  
% ===================================================================
%  nth_domain_check(Pred,ArgNum,Arg)
% ===================================================================

               

% ===================================================================
%  sigma_D_legal_modes_check(Pred,ArgNum,Arg)
% ===================================================================


sigma_D_legal_modes_check(Pred,Args,C):-
         (nth_mode_list(Pred,MODES,C)),!,
         check_legal_modes(Args,MODES,C).
sigma_D_legal_modes_check(_Pred,_Args,_C):-!.


/*
assert_predicates_nth_domain(_,_PredicateName,[]):-!.
assert_predicates_nth_domain(Number,PredicateName,[ArgRef|RestArgs]):-
            nth_domain_list(PredicateName,OLDCLASSES),
            nthmember(OldArgClass,OLDCLASSES,Number),
            newstance_of(ArgRef,ArgClass),
            (
                        (var(ArgRef))    % ArgRef=VAR, ArgClass=VAR, OldArgClass=_,  %Do nothing
                   ;
                        (
                        % ArgRef=ground
         
                              % ArgClass=VAR, OldArgClass=VAR % Do nothing ,   
                              (var(ArgClass),var(OldArgClass),!);
                              % ArgClass=VAR, OldArgClass=ground, %make 'instanceof   
                              (var(ArgClass),nonvar(OldArgClass),!,newstance_of(ArgRef,OldArgClass)  );
                              % ArgClass=ground, OldArgClass=VAR,  %update
                              (nonvar(ArgClass),var(OldArgClass),!,       update_nth_domain(PredicateName,Number,ArgClass) );
                              % ArgClass=ground, OldArgClass=ground,  OldArgClass=ArgClass    % Do nothing
                              (nonvar(ArgClass),nonvar(OldArgClass),ArgClass=OldArgClass );
                              % ArgClass=ground, OldArgClass=ground,  OldArgClass \= ArgClass  % Add
                              (nonvar(ArgClass),nonvar(OldArgClass),ArgClass \= OldArgClass, !,  add_nth_domain(PredicateName,Number,ArgClass) )
                         )
            ),              
         Nexprovenumber is Number  +1,
         assert_predicates_nth_domain(Nexprovenumber,PredicateName,RestArgs).
            

new_nth_domain(PredicateName,Number,ArgClass):-add_nth_domain(PredicateName,Number,ArgClass).

add_nth_domain(PredicateName,Number,ArgClass):-
                  nth_domain_list(PredicateName,CLASSES),!,
                  replace_nth(ArgClass,Number,CLASSES,NEWCLASSES),
                  retractAllProlog(nth_domain_list(PredicateName,NEWCLASSES)),
                  sigma_X_assert(nth_domain_list(PredicateName,NEWCLASSES)),
                  ua_out(domain,nth_domain_list(PredicateName,NEWCLASSES)).

update_nth_domain(PredicateName,Number,ArgClass):-
                  retract(nth_domain_list(PredicateName,CLASSES)),!,
                  ua_out(domainsuficient,nth_domain_list(PredicateName,CLASSES)),!,
                  once(replace_nth(ArgClass,Number,CLASSES,NEWCLASSES)),
                  retractAllProlog(nth_domain_list(PredicateName,NEWCLASSES)),
                  sigma_X_assert(nth_domain_list(PredicateName,NEWCLASSES)),
                   ua_out(domain_new,nth_domain_list(PredicateName,NEWCLASSES)).

  */


% ======================================================
% Constraint Logic (NNF)
% ======================================================
get_predicates_with(X,Fml,[]):-isSlot(Fml),!.
get_predicates_with(X,true,[]):-!.
get_predicates_with(X,Fml,[]):-ground(Fml),!.
get_predicates_with(X,'instance'(_,_),[]):-!.       % Type Logic meta-predicates are not re-typed
get_predicates_with(X,'domain'(_,_,_),[]):-!.
get_predicates_with(X,'domain-check'(_,_,_),[]):-!.


get_predicates_with(X,((Fml1,Fml2)),Preds):- !,
          get_predicates_with(X,Fml1,Preds1),
          get_predicates_with(X,Fml2,Preds2),
          append(Preds1,Preds2,Preds).

get_predicates_with(X,(and(Fml1,Fml2)),Preds):- !,
          get_predicates_with(X,Fml1,Preds1),
          get_predicates_with(X,Fml2,Preds2),
          append(Preds1,Preds2,Preds).

get_predicates_with(X,(or(Fml1,Fml2)),Preds):- !,
          get_predicates_with(X,Fml1,Preds1),
          get_predicates_with(X,Fml2,Preds2),
          append(Preds1,Preds2,Preds).

get_predicates_with(X,(=>(Fml1,Fml2)),Preds):-!,
          get_predicates_with(X,Fml1,Preds1),
          get_predicates_with(X,Fml2,Preds2),
          append(Preds1,Preds2,Preds).

get_predicates_with(X,(<=>(Fml1,Fml2)),Preds):-!,
          get_predicates_with(X,Fml1,Preds1),
          get_predicates_with(X,Fml2,Preds2),
          append(Preds1,Preds2,Preds).

get_predicates_with(X,(forall(_,Fml)),Preds):-!,
          get_predicates_with(X,Fml,Preds).
          
get_predicates_with(X,(exists(_,Fml)),Preds):-!,
          get_predicates_with(X,Fml,Preds).

get_predicates_with(X,(not(Fml)),Preds):-!,
          get_predicates_with(X,Fml,Preds).

get_predicates_with(X,(known(Fml)),Preds):-!,
          get_predicates_with(X,Fml,Preds).

get_predicates_with(X,(consistent(Fml)),Preds):-!,
          get_predicates_with(X,Fml,Preds).

get_predicates_with(X,Term,[]):-X==Term,!.
get_predicates_with(X,Term,[]):-atomic(Term),!.

get_predicates_with(X,Term,['domain-check'(P,ArgNum,X)]):-Term=..[holds,P|Args],nth_identical_member(X,Args,ArgNum),!.
get_predicates_with(X,not(Term),['domain-check'(P,ArgNum,X)]):-Term=..[holds,P|Args],nth_identical_member(X,Args,ArgNum),!.

get_predicates_with(X,Term,['domain-check'(P,ArgNum,X)]):-Term=..[P|Args],nth_identical_member(X,Args,ArgNum),!.
get_predicates_with(X,not(Term),['domain-check'(P,ArgNum,X)]):-Term=..[P|Args],nth_identical_member(X,Args,ArgNum),!.

%get_predicates_with(X,Term,[(and('domain'(P,ArgNum,Class),'instance'(X,Class)))]):-Term=..[P|Args],nth_identical_member(X,Args,ArgNum),!.

%get_predicates_with(X,Term,Preds):-
 %        Term=..[_|Args],nonvar(Args),not(ground(Args)),
  %       plist_to_term(Args,NewTerm),!,
   %      get_predicates_with(X,NewTerm,Preds),!.

get_predicates_with(X,Term,[]):-!.


% ======================================================================
% constrain_wff_argtypes(FmlIn,FmlOut)
% This takes a conjunctive list of clause forms and adds Constraint logic 
% constrain_wff_argtypes/2 is imported from module(sigma_arg_domains)
% ======================================================================


constrain_wff_argtypes(true,true).
constrain_wff_argtypes((A,B),(AA,BB)):-!,
		constrain_wff_argtypes(A,AA),
		constrain_wff_argtypes(B,BB).

constrain_wff_argtypes((resolve_skolem(A,B):-true),(resolve_skolem(A,B):-true)).
constrain_wff_argtypes(end_of_file,true).
constrain_wff_argtypes(entails(Ante,Cons),Out):-
	constrain_wff_argtypes((Cons:-Ante),Out).

constrain_wff_argtypes((not(Cons):-Ante),(not(NCons):-FAnte)):-%trace,
	constrain_wff_argtypes((Cons:-Ante),(NCons:-FAnte)).

constrain_wff_argtypes((Cons:-Ante),(NCons:-FAnte)):-%trace,
	Cons=..[RType,Cast,P|Args],!,	 
	logOnFailure(add_f2(P,1,Args,Ante,Nargs,FAnte)),
	NCons=..[RType,Cast,P|Nargs].

constrain_wff_argtypes((Cons),Out):-!,
	constrain_wff_argtypes((Cons:-true),Out).


holds_format_check(P):-
	held_wrapped(P),!.
holds_format_check(P):-
	held_wrapped_n(P),!.


request_wrap(H):-held_wrapped(H),!.
request_wrap(H):-asserta(sigmaCache(held_wrapped(H))).
request_wrap(H):-atom_concat('~',H,NH),asserta(sigmaCache(held_wrapped_n(NH))).

held_wrapped(NH):-sigmaCache(held_wrapped(NH)).
held_wrapped_n(NH):-sigmaCache(held_wrapped_n(NH)).


testf1:-constrain_wff_argtypes(('~holds'('Entity-Physical-Physical', synonymousExternalConcept, A, B, tt):-holds('Entity-Physical-Physical', subsumedExternalConcept, A, B, tt)),O),writeq(O),nl.
testf2:-constrain_wff_argtypes(('~holds'('Entity-Physical-Physical', synonymousExternalConcept, A, B, C):-holds('Entity-Physical-Physical', subsumedExternalConcept, A, B, C)),O),writeq(O),nl.
testf3:-constrain_wff_argtypes(('~holds'('Entity-Physical-Physical', synonymousExternalConcept, A, B, C)),O),writeq(O),nl.
testf4:-constrain_wff_argtypes(('~holds'('Entity-Physical-Physical', synonymousExternalConcept, A, B, concept)),O),writeq(O),nl.


add_f2(P,_,[],Ante,[],Ante).
	
add_f2(P,N,[A|RGS],Ante,[NA|NCons],FAnte):-
	logOnFailure(is_nth_domain_of(P,N,Domain)),!,
	make_rule(NA,A,Rule,Ante,Domain),!,
	conjoin(Rule,Ante,NAnte),
	NN is N+1,
	logOnFailure(add_f2(P,NN,RGS,NAnte,NCons,FAnte)).

make_rule(NewVar,A,true,Ante,_):-isVar_or_skolem(A),in_ant(A,Ante),NewVar=A,numbervars(NewVar),!.
make_rule(NewVar,A,t_instance(A,Domain),_,Domain):-isVar_or_skolem(A),NewVar=A,numbervars(NewVar),!.
make_rule(NewVar,A,true,_,Domain):-atom(A),NewVar=A,!.
make_rule(NewVar,A,true,_,Domain):- Domain=='Relation',NewVar=A,!.
make_rule(NewVar,A,u(NewVar,A,Domain),_,Domain):-numbervars(NewVar).


in_ant(A,true):-!,fail.
in_ant(A,Ante):-subst(Ante,A,'zzskFn'(_),Res),!,not(Ante=Res).	


% ================================================================
%   XML Output
% ================================================================


:-dynamic xsbRequestStream/1.


%write_direct(Term):-once((curent_output_port(Write_directIO),atom_codes(Term,Chars),length(Chars,Size),file_putbuf(1,Size,Chars,0,Written),(Written=Size;write_direct(2,'eror writing')))).

write_direct_l(Var):-var(Var),!,write_direct(Var).
write_direct_l([]):-!.
write_direct_l([H|T]):-!,write_direct(H),write_direct_l(T).


write_direct(Var):-var(Var),!,write_now(Var).
write_direct(nl):-!,nl.
write_direct(br):-!,write_now('&lt;BR>'),nl.
write_direct(ln):-!,nl.
write_direct(Atom):-atomic(Atom),!,write_now(Atom).
write_direct(UNK):-!,write_now(UNK).


write_now(Atom):-!,write(Atom),flush_output.

sock_write(Atom):-atomic(Atom),
    xsbResponseStream(ClientRespStream),
   atom_codes(Atom,Char),put_socket_chars(ClientRespStream,Char).

put_socket_chars(_ClientRespStream,[]).
put_socket_chars(ClientRespStream,[Char|S]):-
      socket_put(ClientRespStream,Char,ErorCode),
      !,((ErorCode=0),put_socket_chars(ClientRespStream,S) ; dgbln(['Eror Writing to Socket (ClientRespStream)',ClientRespStream])).


%:-include('sigma_header.pl').


                            
/*

Base 3 Hash




Relation Nth-Domain HASH (Object/Abstract) * ((Collection/Region/Individual)/(Predicate/Function/NamedClass)/(Set/Quantity/Attribute)/(Proposition))

	Object  (Collection/Region/Individual)
	
	  ('subclass' Collection Object)  * Collection
	  ('subclass' Agent Object)  *  Individual
	  ('subclass' Region Object)  *  Region
	  ('subclass' Organic Object)	*  Individual
	  ('subclass' Inorganic Object)  *  Individual
	  ('subclass' Substance Object)  * Individual
	  ('subclass' CorpuscularObject Object) * Collection
	  
	
	Abstract  ( Predicate/Function/Set/NamedClass/Quantity/Attribute/Proposition )
	
	  ('subclass' Class Abstract)
		  ('subclass' Relation Class) * Predicate/Function
		  ('subclass' Set Class) * Set
		  ('subclass' PairwiseDisjointClass Class) * NamedClass
		  ('subclass' MutuallyDisjointClass Class) * NamedClass
	  ('subclass' Quantity Abstract) * Quantity
		('subclass' Number Quantity) * Number
		('subclass' PhysicalQuantity Quantity) * PhysicalQuantity
	   ('subclass' Attribute Abstract) * Attribute
	   ('subclass' Proposition Abstract) * Proposition


Predicate Type/Use HASH 

	('subclass' ProbabilityRelation Relation)
	('subclass' SpatialRelation Relation)
	('subclass' TemporalRelation Relation)
	('subclass' RelationExtendedToQuantities Relation)
	('subclass' Predicate Relation)
		('subclass' SententialOperator Predicate) *SententialOperator
		('subclass' BinaryRelation Predicate)  2 + 
			('subclass' ReflexiveRelation BinaryRelation) *  + Reflexivity (+/-/?) 
			('subclass' IrreflexiveRelation BinaryRelation) * -  Reflexivity (+/-/?) 
			('subclass' SymmetricRelation BinaryRelation) +  SymmetricRelation/AntisymmetricRelation
			('subclass' AntisymmetricRelation BinaryRelation) + SymmetricRelation/AntisymmetricRelation
			('subclass' TrichotomizingRelation BinaryRelation) + TrichotomizingRelation	   (7)
			('subclass' TransitiveRelation BinaryRelation) + TransitiveRelation/IntransitiveRelation
			('subclass' IntransitiveRelation BinaryRelation) + TransitiveRelation/IntransitiveRelation
		('subclass' TernaryRelation Predicate) 3 +
		('subclass' QuaternaryRelation Predicate)  4 +
		('subclass' QuintaryRelation Predicate) 5 +
		('subclass' VariableArityRelation Predicate) N +  
	('subclass' Function Relation)
  
  


*/

get_formula_hash(Predicate,0,Args,holds):-!.   
get_formula_hash(Predicate,N,Args,Hash):-isSlot(Predicate),!,	
	logOnFailure(get_arg_based_hash(Predicate,Arity,Args,Hash)),!.

get_formula_hash(exists,2,Args,'Entity-Representation'):-!.
get_formula_hash(forall,2,Args,'Entity-Representation'):-!.
get_formula_hash(Predicate,Arity,Args,Hash):-!,
	logOnFailure(get_predicate_hash(Predicate,Arity,Args,Hash)),!.
	

get_arg_based_hash(Predicate,Arity,Args,Hash):-entity_make_hash(Hash,Arity),!,logOnFailure(request_wrap(Hash)).

entity_make_hash('Entity',1).
entity_make_hash('Entity-Entity',2).
entity_make_hash('Entity-Entity-Entity',3).
entity_make_hash(Atom,N):-NN is N-1,entity_make_hash(FORM,NN),!,fmtString(SFORM,'Entity-~w',[FORM]),string_to_atom(SFORM,Atom),!.

get_arg_based_hash(Predicate,Arity,Args,Hash):-
	logOnFailure(get_list_based_hash(Args,HashList)),
	concat_atom(HashList,'-',Hash).
	
get_list_based_hash([],[]):-!.
get_list_based_hash([H|T],[CH|CT]):-
	logOnFailure(get_term_hash(H,CH)),
	get_list_based_hash(T,CT).
		
get_term_hash(H,Hash):-
	is_instance_of(H,Class),!,
	get_class_hash(Class,Hash),!.


get_predicate_hash(exists,2,Args,'Set-Representation'):-!.
get_predicate_hash(forall,2,Args,'Set-Representation'):-!.
get_predicate_hash(X,Y,A,Q):-sigmaCache(PredR,predicate_hash(X,Y,Q)),!.
get_predicate_hash(X,Y,Z,Q):-
	logOnFailure(make_get_predicate_hash(X,Y,Z,Q)),!,
	asserta(sigmaCache(PredR,predicate_hash(X,Y,Q))).
get_predicate_hash(_,_,Args,'NoPredicateHash'):-!.

make_get_predicate_hash(Predicate,Arity,Args,Hash):-
	logOnFailure(get_predicate_nth_domains(Predicate,Arity,ClassList)),!,
	logOnFailure(class_list_to_hash(ClassList,Hash,HashList)),!.
	
get_predicate_nth_domains(Predicate,Arity,ClassList):-sigmaCache(PredR,domain_set(Predicate,ClassList)),!.
get_predicate_nth_domains(Predicate,Arity,ClassList):-
	logOnFailure(make_get_predicate_nth_domains(Predicate,Arity,ClassList)),
	asserta(sigmaCache(PredR,domain_set(Predicate,ClassList))),!.

make_get_predicate_nth_domains(Predicate,Arity,ClassList):-
	logOnFailure(get_predicate_nth_domains_rev(Predicate,Arity,ClassListRev)),!,
	reverse(ClassListRev,ClassList),!.

get_predicate_nth_domains_rev(Predicate,0,[]):-!.
get_predicate_nth_domains_rev(Predicate,Arity,[Class|List]):-
	 is_nth_domain_of(Predicate,Arity,Class),!,
	ArityN is Arity -1,
	get_predicate_nth_domains_rev(Predicate,ArityN,List),!.
	
	
class_list_to_hash(ClassList,Hash,HashList):-
	logOnFailure(class_list_to_hash_list(ClassList,HashList)),
	logOnFailure(concat_atom(HashList,'-',Hash)),
	logOnFailure(request_wrap(Hash)),!.
	
class_list_to_hash_list([],[]):-!.
class_list_to_hash_list([H|L],[HH|LL]):-
	get_class_hash(H,HH),!,
	class_list_to_hash_list(L,LL),!.
	



the_hash_set([
	'Collection',
	'Representation',
	'Formula',
	'Region',
	'Process',
	'Object',
	'Attribute',
	'FunctionQuantity',
	'Function','Relation','Quantity','Proposition',
	'Set',
	'Class',
	'Physical',
	'Abstract',
	'Entity'
	]).
	
get_class_hash('Entity','Entity'):-!.
get_class_hash('Sentence','Representation').
get_class_hash('Class','Class').
get_class_hash(C,H):-sigmaCache(PredR,class_hash(C,H)),!.
get_class_hash(C,H):-class_hash0(C,H),!,
	asserta(sigmaCache(PredR,class_hash(C,H))),!.
get_class_hash(_,'Entity').

% is_subclass_of/2 is imported from sigma_surface_inference

class_hash0(Class,'Relation'):-is_subclass_of(Class,'Relation'),!.
class_hash0(Class,'Collection'):-is_subclass_of(Class,'Collection'),!.
class_hash0(Class,'Proposition'):-is_subclass_of(Class,'Proposition'),!.
class_hash0(Class,'Formula'):-is_subclass_of(Class,'Formula'),!.
class_hash0(Class,'Representation'):-is_subclass_of(Class,'Representation'),!.
class_hash0(Class,'Region'):-is_subclass_of(Class,'Region'),!.
class_hash0(Class,'Process'):-is_subclass_of(Class,'Process'),!.
class_hash0(Class,'Attribute'):-is_subclass_of(Class,'Attribute'),!.
class_hash0(Class,'FunctionQuantity'):-is_subclass_of(Class,'FunctionQuantity'),!.
class_hash0(Class,'Function'):-is_subclass_of(Class,'Function'),!.
class_hash0(Class,'Object'):-is_subclass_of(Class,'Object'),!.
class_hash0(Class,'Quantity'):-is_subclass_of(Class,'Quantity'),!.
class_hash0(Class,'Set'):-is_subclass_of(Class,'Set'),!.
class_hash0(Class,'Class'):-is_subclass_of(Class,'Class'),!.
class_hash0(Class,Class):-disjointed_classes(Class),!.
class_hash0(Some,Class):-disjointed_classes(Class),!,is_subclass_of(Some,Class),!.
class_hash0(Class,'Physical'):-is_subclass_of(Class,'Physical'),!.
class_hash0(Class,'Abstract'):-is_subclass_of(Class,'Abstract'),!.

class_hash0(Class,'Entity'):-!.





