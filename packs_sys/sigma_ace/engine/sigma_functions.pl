% ======================================================================
% Purpose:
% This takes logical sentence FmlIn and turns treated_as_function/1s into predicates except inside not_treated_as_function_while_inside/1.
% ======================================================================
:-include('sigma_header.pl').

:-dynamic(not_treated_as_function_while_inside/1).
:-dynamic(treated_as_function/1).
:-dynamic(is_a_function/1).
       

% Functional Normal Form - Removes Functions and replaces with NewVars then puts them in Flags with information
getFunctionalForm(Axiom,NewFlags,NewVars,OTerm):-
       % trace,
	logOnFailure(getFunctionFlags(toplevel,1,GFlags,Axiom)),!,
	% trace,
	logOnFailure(setFunctionFlags(GFlags,Axiom,NewFlags,NewVars,OTerm)),!. % apply flags

setFunctionFlags([],Axiom,[],[],Axiom):-!.

setFunctionFlags([dom(V,D)|MoreFlags],Axiom,[dom(V,D)|NewFlags],NewVars,OTerm):-  %Skip Over flags
		setFunctionFlags(MoreFlags,Axiom,NewFlags,NewVars,OTerm),!.

setFunctionFlags([fun(Caller,ArgN,F,Term)|MoreFlagsIn],Axiom,NewFlags,NewVars,OTerm):-
	subst(Axiom,Term,NewVar,MAxiom),
	Axiom==MAxiom, !,% Function Gone Already (Do nothing)
	%writeq(notfound(fun(Caller,ArgN,F,Term),Axiom)),nl,
	setFunctionFlags(MoreFlagsIn,MAxiom,NewFlags,NewVars,OTerm),!.

setFunctionFlags([fun(Caller,ArgN,F,Term)|MoreFlagsIn],Axiom,[dom(NewVar,[Caller:ArgN,F:range])|NewFlags],[NewVar|NewVars],OTerm):-
	pvar_gen(NewVar),
	subst(Axiom,Term,NewVar,MAxiom),
	%writeq(found(fun(Caller,ArgN,F,Term),Axiom)),nl,
	Term =.. TermL,
	append(TermL,[NewVar],FunctionL),!,
	Holds=..[holds|FunctionL],
	%subst(MoreFlagsIn,Term,NewVar,MoreFlags),
	setFunctionFlags(MoreFlagsIn,(Holds) => MAxiom,NewFlags,NewVars,OTerm),!.

setFunctionFlags([NF|MoreFlags],Axiom,[NF|NewFlags],NewVars,OTerm):-  %Skip Over flags
		setFunctionFlags(MoreFlags,Axiom,NewFlags,NewVars,OTerm),!.
		
getFunctionFlags(Caller,ArgN,[dom(Term,[Caller:ArgN])],Term):- isSlot(Term),!.

getFunctionFlags(Caller,ArgN,[],Term):- not(compound(Term)),!. % catches Term=[].

getFunctionFlags(Caller,ArgN,Flags,Term):-
	Term=..['holds',F|ArgS],!,
	logOnFailure(getFunctionFlags_l(F,1,Flags,ArgS)).

getFunctionFlags(Caller,ArgN,Flags,Term):-
	Term=..['AssignmentFn',F|ArgS],!,
	logOnFailure(getFunctionFlags_l(F,1,Flags,ArgS)).

getFunctionFlags(Caller,ArgN,Flags,Term):-
	Term=..[Q,V|MTerm],memberchk(Q,[forall,exists]),!,
	logOnFailure(getFunctionFlags_l(toplevel,1,Flags,MTerm)).

getFunctionFlags(Caller,ArgN,Flags,[A|ArgS]):-!, %trace,
	logOnFailure(getFunctionFlags_l(Caller,ArgN,Flags,[A|ArgS])).

getFunctionFlags(Caller,ArgN,Flags,Term):-
	Term=..[F|ArgS],hlPredicateAttribute(F,'Function'),!,
	logOnFailure(getFunctionFlags_l(F,1,FlagsM,ArgS)),
	append(FlagsM,[fun(Caller,ArgN,F,Term)],Flags).
		
		
% Special case for Instance
getFunctionFlags(Caller,ArgN,[dom(A,['$instanceof':Class])],instance(A,Class)):-isSlot(A),atom(Class),!.

getFunctionFlags(Caller,ArgN,Flags,Term):-!, %trace,
	Term=..[F|ArgS],
	logOnFailure(getFunctionFlags_l(F,1,Flags,ArgS)).
	
getFunctionFlags_l(Caller,N,[],[]).
getFunctionFlags_l(Caller,N,Flags,[Arg|S]):-
	getFunctionFlags(Caller,N,Flags1,Arg),
	NN is N+1,
	getFunctionFlags_l(Caller,NN,Flags2,S),
	append(Flags1,Flags2,Flags),!.

   
test_extend_functions(Term):-
     %  numbervars(Term,'$VAR',15,_),
	extend_rewrites(Term,Best),!,
	format('<pre><B><font color=green>~w</font></B>\n',['Original']),
	write_conj_test_function(KRVars,Term),
	format('<pre><B><font color=green>~w</font></B>\n',['Functions']),
	write_conj_test_function(KRVars,Best),
	format('<B><font size color=red>Compiled</font></B>\n'),
	getAssertionClauses(KB,Ctx,Term,Clauses,KRVars,How),!,
	write_conj_test_function(KRVars,Clauses),
	format('\n\n</pre>').
				
write_conj_test_function(KRVars,A):-notrace(write_conj_test_function1(KRVars,A)).

write_conj_test_function1(KRVars,A):-isSlot(A),format('~q\n',A),!.
write_conj_test_function1(KRVars,and(A,B)):-
	write_conj_test_function1(KRVars,A),!,
	write_conj_test_function1(KRVars,B).
write_conj_test_function1(KRVars,Term):-
	toMarkUp(kif,Term,KRVars,O),!,
	format('<pre>\n\n~w\n\n</pre>',[O]).
	

remove_true(O,O):-isSlot(O),!.
remove_true(I,O):-remove_true1(I,M),!,
	((M==I,O=I);remove_true(M,O)),!.

remove_true1(O,O):-isSlot(O),!.
remove_true1(O,O):-not(compound(O)),!.

% true => Fact -> Fact
remove_true1('=>'(True,Original),Out):-
	True==true,!,
	remove_true1(Original,Out).

/*
% Fact => Fact -> true
remove_true1('=>'(True,Original),true):-
	True==Original,!.
*/
% true <=> Fact
remove_true1('<=>'(True,Original),Out):-
	True==true,!,
	remove_true1(Original,Out).

% Original <=> true -> Original
remove_true1('<=>'(Original,True),Out):-
	True==true,!,
	remove_true1(Original,Out).

/*
% Original <=> Original -> Original
remove_true1('<=>'(True,Original),Out):-
	True==Original,!,
	remove_true(Original,Out).
*/
remove_true1([A|AL],[B|BL]):-
	remove_true1(A,B),
	remove_true1(AL,BL).

remove_true1(Term,Out):-
	Term=..[Save|List],
	remove_true1(List,OutL),!,
	Out=..[Save|OutL].
	
remove_true1(O,O).


% ======================================================================
% Code for Holds Expansion
% ======================================================================
/*
  (=>
          (instance ?PROCESS Process) 
          (exists
             (?CAUSE) 
             (effector ?PROCESS ?CAUSE) ) )


  (entails 
          (instance ?PROCESS Process) 
          (exists
             (?CAUSE) 
             (effector ?PROCESS (skolemFn (EffectorProcessSKFn ?PROCESS))
	     


(entails 
        (instance ?PROCESS Process) 
	(if-else (effector ?PROCESS ?CAUSE)) (equals ?CAUSE (

	(effector ?PROCESS (skolemFn (EffectorProcessSKFn ?PROCESS))
	     


	     


*/

extend_rewrites(A,A):-!.


extend_rewrites(A,C):-
	get_do_job(A,B),!,
	((A==B,!,C=B);extend_rewrites(B,C)),!.	 


get_do_job(Original,Original):-isSlot(Original),!.
		
get_do_job(Original,Done):-
	get_job_sent(Original,Original,[],Tasks),!,
	do_job_until_done(Tasks,Original,Done),!.
	
get_do_job(Original,Original):-!.


% Skip getPrologVars and some terms
get_job_sent(Original,Term,In,Tasks):-
	notrace((isSlot(Term);
	atom(Term);
	list(Term);
	number(Term))),!,fail.

% ( ....... ....... .......)
get_job_sent(Original,Holds,In,Tasks):-
	Holds=..[Start,Pred|Args],!,
	get_job_sent_t(Original,Holds,Start,Pred,Args,In,Tasks).

	
	
	% (holds .......)
	get_job_sent_t(Original,Holds,holds,Pred,Args,In,Tasks):-
		get_job_sent_holds(Original,Holds,Pred,Args,In,Tasks).
	
				% (holds ?Relation ............... )  Catches has_sub_jobs
				get_job_sent_holds(Original,Holds,Pred,Args,In,Tasks):-
					is_a_var(Pred),!,
					get_job_sent_holds_args(Original,Holds,Pred,Args,In,Tasks).
				
				% (holds equals ?? ??) Rewrite ( equals ?? ??)
				get_job_sent_holds(Original,Holds,equals,[A,B],[subst(Holds,equals(A,B)),restart]):-!.      		
					
				% (holds someFn A ) 
				get_job_sent_holds(Original,Term,FnA,Args,In,Tasks):-
					hlPredicateAttribute(FnA,'Function'),
					get_job_sent_holds_args_fn(Original,Term,FnA,!,Args,In,Tasks).
					
				
			get_job_sent_holds_args_fn(Original,Term,Funct,N,[],[]):-!.
			get_job_sent_holds_args_fn(Original,Term,Funct,N,[Node|Rgs],[Todo|Tasks]):-
					get_job_sent_term_fn(Original,Term,Funct,N,Node,Todo),NN is N+1,
					get_job_sent_holds_args(Original,Term,Funct,NN,Rgs,In,Tasks),!.

			%get_job_sent_term_fn(Original,Term,Funct,N,Node,In,[addconstraint()]):-isSlot()
			get_job_sent_term_fn(Original,Term,Funct,N,Node,In,In):-not(compound(Node)),!.
			get_job_sent_term_fn(Original,Term,Funct,N,Node,In,In):-is_list(Node),!.
			get_job_sent_term_fn(Original,Term,Funct,N,Node,[subst(Node,NewVar),addprecond(NTerm),domain(NTerm)]):-
					fn_to_holds(Node,[T|ErmL],NewVar,NTerm),
					hlPredicateAttribute(T,'Function'),!.
			get_job_sent_term_fn(Original,Term,Funct,Node,In,In):-!.	
					
				
				% (holds pred ????) Catches has_sub_jobs
				get_job_sent_holds(Original,Holds,Pred,Args,In,Tasks):-
					get_job_sent_holds_args(Original,Holds,Pred,1,Args,In,Tasks).


					get_job_sent_holds_args_pred(Original,Holds,Pred,N,[],[]):-!.
					get_job_sent_holds_args_pred(Original,Holds,Pred,N,[Node|Rgs],[Todo|Tasks]):-
							get_job_sent_term(Original,Holds,Pred,N,Node,Todo),NN is N+1,
                                                        get_job_sent_holds_args(Original,Holds,Pred,NN,Rgs,In,Tasks),!.


      			
		  

      		

	% (AssignmentFn .......)
	get_job_sent_t(Original,Holds,'AssignmentFn',Funct,Args,In,Tasks):-
		Holds=..['AssignmentFn',Funct|Args],!,
		get_job_sent_AssignmentFn(Original,Holds,Funct,Args,In,Tasks).

				% (AssignmentFn ?? Fn)
				get_job_sent_AssignmentFn(Original,Holds,Funct,Args,In,Tasks):-
					has_sub_job(Original,Args),
					get_job_sent(Original,Args,In,Tasks),!.
						
				% (AssignmentFn ?? ?Args)
				get_job_sent_AssignmentFn(Original,Holds,Funct,Args,[subst(Holds,FNTerm),addprocond(Holds),restart]):-
					is_a_var(Funct),
					append([holds,Funct|Args],[NewVar],HoldsL),
					Predicate =.. PredicateL,!.
				
				% (AssignmentFn Fn ?Args)
				get_job_sent_AssignmentFn(Original,Holds,Funct,Args,[subst(Holds,NewVar),addcons(Holds),restart]):-
					hlPredicateAttribute(Funct,'Function'),
					append([holds,Funct|Args],[NewVar],HoldsL),
					Predicate =.. PredicateL,!.

	% (FnA ............)  % Catches has_sub_jobs
	get_job_sent_t(Original,Holds,FnA,Funct,Args,In,Tasks):-
		hlPredicateAttribute(FnA,'Function'),!,
		get_job_sent_function(Original,Holds,FnA,[Funct|Args],In,Tasks),!.
		
			% (FnA ????)  % Catches has_sub_jobs
			get_job_sent_function(Original,Holds,FnA,Args,In,Tasks):-
				get_job_sent(Original,Args,In,Tasks),!.
			
			% (FnA ?Args)
			get_job_sent_function(Original,Holds,FnA,Args,[subst(Holds,NewVar),addprecond(FHolds),restart]):-
				fn_to_holds(Holds,NewVar,FHolds),!.


	% (equals ............ )
	get_job_sent_t(Original,Holds,equals,A,[B],In,Tasks):-
		get_equals_job(Original,Holds,A,B,In,Tasks).				
				

				% (equals ?X ?X)   (equals (FnA ?X) (FnA ?X)) 
				get_equals_job(Original,Holds,A,B,[delete(Holds)]):-A==B,!.
				
				% (equals ?A ?B) 
				get_equals_job(Original,Holds,A,B,[delete(Holds),subst(A,B)]):-
					isSlot(A),isSlot(B),!.
				
				% (equals (FnA (FnA ?X)) ??)
				get_equals_job(Original,Holds,A,B,subfuncts):-isEntityFunction(A,FnA,ArgsA),has_sub_job(Original,A),!,fail.
				
				% (equals ?? (FnB (FnA ?X)) )
				get_equals_job(Original,Holds,A,B,subfuncts):-isEntityFunction(B,FnB,ArgsB),has_sub_job(Original,B),!,fail.
				
				get_equals_job(Original,Holds,A,B,In,Tasks):-
					isEntityFunction(A,FnA,ArgsA),
					isEntityFunction(B,FnB,ArgsB),
					get_job_sent_equal_fns(Original,Holds,A,FnA,ArgsA,B,FnB,ArgsB,In,Tasks),!.
				
				% (equals (FnA ?X) (FnB ?Y))
				get_job_sent_equal_fns(Original,Holds,A,FnA,ArgsA,B,FnB,ArgsB,[delete(Holds),subst(A,NV),subst(B,NV),addcons(AH <=> BH)]):-
					fn_to_holds(B,NV,AH),
					fn_to_holds(A,NV,BH),!.
					
				% (equals (FnA ??) ?B)
				get_equals_job(Original,Holds,A,B,[delete(Holds),subst(A,B),addprecond(AH)]):-
					isEntityFunction(A,FnA,ArgsA),isSlot(B),
					fn_to_holds(A,B,AH),!.
					
				% (equals ?A (FnB ?X))
				get_equals_job(Original,Holds,A,B,[delete(Holds),subst(B,A),addprecond(BH)]):-
					isEntityFunction(B,FnB,ArgsB),isSlot(A),
					fn_to_holds(B,A,BH),!.


% Recurse downward
get_job_sent(Original,Holds,In,Tasks):-
	Holds=..[_|Args],!,
	get_job_sent(Original,Args,In,Tasks).

has_sub_job(Original,Args):- notrace(not(not(get_job_sent(Original,Args,_)))),!.
no_sub_job(Original,Arg):-notrace((Arg=..[_|Args],!,no_sub_job(Original,Args))).
no_sub_job(Original,Args):-notrace((not(get_job_sent(Original,Args,_)))),!.


% ===========================================================
% do_job(+Tasks,+Original,?Done)
% do_job_w_status(+Tasks,+Original,?Done,?HasTasksLeft)
% ===========================================================
not_a_function(F):-not(hlPredicateAttribute(F,'Function')).

not_part_of(Term,Original):-subst(Original,Term,foo,Changed),!,Original==Changed.


do_job_until_done(Tasks,Original,Done):-
	do_job_w_status(Tasks,Original,Mid,HasTasksLeft),!,
	 (HasTasksLeft -> do_job_until_done(Tasks,Mid,Done) ; Done=Mid),!.		

do_job_w_status(Tasks,Original,Done,HasTasksLeft):-
	do_job(Tasks,Original,Original,Done),!,
		(Original==Done -> HasTasksLeft=fail ;HasTasksLeft=true),!.


do_job(Tasks,Original,Done):-
	do_job(Tasks,Original,Original,Done),!.

do_job([],Original,Done,Done):-!.
do_job(_,Original,Done,Done):-isSlot(Done),!.
do_job([Task|List],Original,Start,Done):-
	do_job(Task,Original,Start,Lunch),
	do_job(List,Original,Lunch,Done),!.
	
do_job(subst(This,That),Original,Start,Done):-subst(Start,This,That,Done),!.
do_job(replace(This,That),Original,Start,Done):-subst(Start,This,That,Done),!.
do_job(delete(That),Original,Start,Done):-!,do_job(subst(That,true),Original,Start,Done),!.
do_job(delete(That),Original,Start,Done):-!,do_job(subst(That,true),Original,Start,Done),!.
do_job(addprecond(That),Original,Start,Done):-
	do_job(delete(That,true),Original,Start,Mid),
	do_job(simplify,Original,entails(That,Mid),Done).

do_job(simplify,Original,Start,Done):-getLogicalReduction(Start,Done).

do_job(Tasks,Original,Done,Done):-!.


% funct
do_job(funct,Original,Found,Out):-
	once((fn_to_holds(Found,NewVar,Holds),
	replc(Original,equal(Found,NewVar),Holds,Out))),
	Out\==Original,!.
do_job(funct,Original,Found,Out):-
	once((fn_to_holds(Found,NewVar,Holds),
	replc(Original,equal(NewVar,Found),Holds,Out))),
	Out\==Original,!.
do_job(funct,Original,Found,(Holds=>Out)):-
	fn_to_holds(Found,NewVar,Holds),
	subst(Original,Found,NewVar,Out),!.
do_job(funct,Original,Found,Original).	

fn_to_holds(Term,[T|ErmL],NewVar,Holds):-
      Term=..[T|ErmL],
	append([holds|TermL],[NewVar],HoldsL),
	Holds=..HoldsL,!.

fn_to_holds(Term,NewVar,Holds):-
      Term=..TermL,
	append([holds|TermL],[NewVar],HoldsL),
	Holds=..HoldsL,!.
	
is_a_equals(V):-isSlot(V),!,fail.
is_a_equals(equal).


% ======================================================================
% Testing
% ======================================================================
				 
ef0:-test_extend_functions(equal('AFn'(A),'1Fn'(A))).
ef1:-catch(test_extend_functions('AFn'(A)),E,write(E)).
ef2:-test_extend_functions(equal('AFn'(A),'1Fn'(B))).
ef3:-test_extend_functions(=>(equal('SquareRootFn'(A), B), equal('MultiplicationFn'(B, B), A))).
ef4:-test_extend_functions(=>(instance(A, 'NegativeRealNumber'), equal('SignumFn'(A), -1))).
ef5:-test_extend_functions(=>(instance(A, 'NegativeRealNumber'), equal('SignumFn'(A), 'SignumFn'(-1)))).
ef6:-test_extend_functions(instance(fooFn, 'FunctionRealNumber')).
ef7:-test_extend_functions(equal(fooFn, 66)).
ef8:-test_extend_functions(=>(equal(V, 66),isa(V,'Number'))).
ef8i:-test_extend_functions(=>(isa(V,'Number'),equal(V,66))).
ef9:-test_extend_functions(=>(equal('EndFn'(A), B), forall(C, =>(and(temporalPart(C, A), not(equal(C, B))), before(C, B))))).
ef10:-test_extend_functions(=>(origin(A, B), located('WhereFn'(A, 'BeginFn'('WhenFn'(A))), 'WhereFn'(B, 'BeginFn'('WhenFn'(B)))))).
ef11:-test_extend_functions(=>(instance(A, human), exists(B, part(B, A)))).

