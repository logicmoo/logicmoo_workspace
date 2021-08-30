% ===================================================================
% File: sigma_surf_to_can.pl
% Author: Douglas Miles (dmiles@teknowledge.com) 
%
% Two Principal predicates defined: 
%
%    getAssertionClauses0/3 Converts a KIF Surface Form to Rule Form
%    getQueryClauses/3 Converts a KIF Surface Query to Query Macro in Heuristic Language
%
%  With 
%
%     Connectives <=> (bi-implication),impiles,and,or (logical version - i.g. as pure disjuction)
%     Quantifiers: forall, exists, exists (logical version - i.g. as per sentence)
%     Identity: equal (As in the equal Object), equal (As in the deisNotInClauseg an equivent within a certain domain)
%     Higher Order Predicates: 
%               Temporal:  after (preconditional) , before (causality)
%               FOL: not (Refuted)
%               Model: not (Unknown), known (In the KB), possible (Not refuted) 
%     Type Predicates:
%               domainV, domain-check
%     
% http://www.enm.bris.ac.uk/research/aigroup/enjl/logic/sld002.htm
% http://www.enm.bris.ac.uk/ai/enjl/logic1.pldf
% http://www.dyade.fr/fr/actions/vip/jgl/SPP/dowek.pls 
% http://logic.stanford.edu/~cs157/lectures/lecture04/sld004.htm
% ===================================================================
:-include('sigma_header.pl').

/*----------------------------------------------------------------------

    This module has three entry points:
	clausal_form(Formula, Clauses)
	clausal_form_of_negation(Formula, Clauses)
	units_separated(Clauses, PosUnits, NegUnits, NonUnits)

    The Formula is an <expr>, where
	<expr> ::= all(<variable>, <expr>)
		|  exists(<variable>, <expr>)
		|  <expr> => <expr>
		|  <expr> <=> <expr>
		|  if(<expr>,<expr>,<expr>)
		|  <expr> and <expr>
		|  <expr> or <expr>
		|  ~ <expr>
		|  <atom>

	<atom> ::= <predicate>(<term>,...,<term>)

	<term> ::= <variable>
		|  <constant>
		|  <functor>(<term>,...,<term>)

    The Clauses are a sentence, where
	<sentence> ::= []			(true)
		|  <clause> . <sentence>	(and)

	<clause> ::= clause(<atoms>, <atoms>)
	<atoms> ::= [] | <atom> . <atoms>

    Note that this representation of a clause is not quite the
    usual one.  clause([a,b,c], [d,e,f]) represents
	a v b v c <- d & e & f
    or, if you don'writeTranslation like "Kowalski form",
	a v b v c v ~d v ~e v ~f

    The reason for the two entry points is that the formula may
    contain free variables, these are to be understood as being
    universally quantified, and the negation of the universal
    closure of a formula is not at all the same thing as the
    universal closure of the negation!

    units_separated takes a list of clauses such as the other two predicates
    might produce, and separates them into a list of positive unit clauses
    (represented just by <atom>s), a list of negative unit clauses (also
    represented by their single <atom>s), and a list of non-unit clauses.
    Some theorem provers might find this separation advantageous, but it is
    not buillt into clausal_form becauses exists provers would not benefit.

----------------------------------------------------------------------*/
:- op(700, xfx, [contains,literally_contains,does_not_literally_contain]).

% ======================================================================
% EXPORTS
%
% getAssertionClauses0/3
% getQueryClauses/3
% ======================================================================

ttsurf:-tsurf(
 (=>(instance(A, 'Transaction'), 
	exists(B, exists(C, exists(D, exists(E, exists(F, exists(G, 
		and(instance(E, 'Giving'), and(instance(D, 'Giving'), and(subProcess(E, A), and(subProcess(D, A), and(agent(E, G), and(agent(D, F), and(patient(E, C), 
			and(patient(D, B), and(destination(E, F), and(destination(D, G), 
				and(not(equal(G, F)), not(equal(C, B))))))))))))))))))))),
		  ['TRANS'=A, 'AGENT1'=G, 'AGENT2'=F, 'GIVE1'=E, 'GIVE2'=D, 'OBJ1'=C, 'OBJ2'=B]).

% ======================================================================
% IMPORTS
%
% sigma_utility.pl (only common predicates like getPrologVars/4)
% ======================================================================
% ======================================================================
% Converts a KIF Surface Assertion to Entailment Tests
% getAssertionClauses(-KB,-Ctx,-Prop,+NConjAssertsClauses,+KRVars,-AllFlagsO)
% ======================================================================

/*
getAssertionClauses(KB,Ctx,Surface,surface,[/*Vars*/],[/*Flags*/]):-
	once(getConstants(atomic,Surface,Cs,_,_)),
	intersection(Cs,[<=>,exists,and,or,'',equal,entails,forall,=>,not,possible,known],[]),!.
*/
getAssertionClauses(KB,Ctx,Prop,NConjAssertsClauses,KRVars,AllFlags):- 
	logOnFailure(canonicalizeProposition(KB,Ctx,Prop,CNF,DNF,ConjAssertsClauses,KRVars,AllFlags)),
	debugOnFailure(putFeaturesInFormula(assertion,KRVars,ConjAssertsClauses,AllFlags,NConjAssertsClauses)),!.
	
% ======================================================================
% Converts a KIF Surface Query to Entailment Tests
% getQueryClauses(-KB,-Ctx,-Prop,+NConjAssertsClauses,+KRVars,-AllFlagsO)
% ======================================================================

getQueryClauses(KB,Ctx,Surface,surface,[/*Vars*/],[/*Flags*/]):-
	once(getConstants(atomic,Surface,Cs,_,_)),
	intersection(Cs,[<=>,exists,and,or,equal,entails,forall,=>,<=,not,possible,known],[]),!.

getQueryClauses(KB,Ctx,Prop,NConjAssertsClauses,KRVars,AllFlags):- 
	canonicalizeProposition(KB,Ctx,Prop,CNF,DNF,ConjAssertsClauses,KRVars,AllFlags),
	logOnFailure(putFeaturesInFormula(query,KRVars,ConjAssertsClauses,AllFlags,NConjAssertsClauses)),!.
	

% ======================================================================
% canonicalizeProposition(+KB,+Ctx,+Prop,-ConjAssertsClauses,+KRVars,-AllFlags)
% ======================================================================
canonicalizeProposition(KB,Ctx,Prop,CNF,DNF,ConjAssertsClauses,KRVars,AllFlags):- 
	close_list(KRVars),!,                  
	%writeObject(Prop,KRVars),
	sigma_numbervars(Prop:KRVars:KB:Ctx,666,_),
	logOnFailure(getModeledPredicates(Prop,FmlInOpen)),!,
	%writeObject('<hr>getModeledPredicates: \n',KRVars),
	%writeObject(FmlInOpen,KRVars),
        getClosedVersionWFF(forall,FmlInOpen,Axiom),!,
	%logOnFailure(getFunctionalForm(Axiom,FunctionFlags,NewVarsForFunctions,NoFunctions)),!,
	%writeObject('<hr>FunctionFlags: \n',KRVars),!,
	%getPrologVars(NoFunctions,FFV,_,_),
	%logOnFailure(getFlagsValidToVars(FFV,FunctionFlags,FFC)),!,
	%ignore(writeObject(ff(FFC),KRVars)),!,
	%writeObject('<hr>NoFunctions: \n',KRVars),
	%writeObject(NoFunctions,KRVars),
	logOnFailure(
	getNegationForm(toplevel,0,KB,Ctx,KRVars,Flags,Axiom,_,NNF,_)),!,
	%logOnFailure(getDisjForm(NNF,DNF)),!,
	logOnFailure(getConjForm(NNF,CNF)),!,
	%write_cnf_lit(CNF,KRVars),!,
	%writeq_conj(DNF),nl,nl,
	%writeObject('<hr>Conjuntive Form: \n',KRVars),!,
	%writeObject(CNF,KRVars),!,
	logOnFailure(toClauseFormOptimal(CNF,ConjAssertsClauses)),!,
	getPrologVars(ConjAssertsClauses:NewVarsForFunctions,FVars,_,_),
	logOnFailure(getFlagsValidToVars(FVars,Flags,AllFlags)),!.

       
putFeaturesInFormula(AssertQuery,KRVars,entails(true,true),AllFlags,true):-!.

putFeaturesInFormula(AssertQuery,KRVars,and(A,B),AllFlags,and(AA,BB)):-!,
	putFeaturesInFormula(AssertQuery,KRVars,A,AllFlags,AA),
	putFeaturesInFormula(AssertQuery,KRVars,B,AllFlags,BB).

putFeaturesInFormula(AssertQuery,KRVars,entails(Ante,Cons),AllFlags,AProof):-
	getPrologVars(Cons:Ante,Together,_,_),
	getPrologVars(Cons,ConVars,_,_),
	add_ante(AllFlags,Together,Ante,NewAnte),
	add_skolems_to_body(AllFlags,NewAnte,Cons,ConVars,NewestAnte),
	cleanProof(Cons,NewestAnte,AProof),!.

%skolemIfRequired(AllFlags,NewAnte,KRVars,Cons,AllFlags,NewCons),
	
	
cleanProof(NewCons,true,NewCons).
cleanProof(NewCons,NewAnte,entails(NewAnte,NewCons)).

putFeaturesInFormula(AssertQuery,KRVars,Cons,AllFlags,NewCons):-!,
	putFeaturesInFormula(AssertQuery,KRVars,entails(true,Cons),AllFlags,NewCons).
	
% =============================================================



% =============================================================
	
ifThenElse(I,T,_):- I,!,T,!.
ifThenElse(_,_,E):- E,!.
  
% =============================================================

ground_unused_vars([],KRVars,SKG,SKG):-!.
ground_unused_vars([V1|Ante],KRVars,SK,SKG):-!,
	toMarkUp(kif,V1,KRVars,VN),
	ok_subst(SK,V1,VN,SKGM),
	ground_unused_vars(Ante,KRVars,SKGM,SKG).
	
add_ante([],Together,NewAnte,NewAnte):-!.
add_ante([F|Flags],Together,Ante,NewAnte):-!,
	do_flag(F,Together,Ante,AnteMid),
	add_ante(Flags,Together,AnteMid,NewAnte).
	
do_flag(post(Var,Call),Together,Ante,AnteMid):-
	getPrologVars(Call,CV,_,_),
	member(Var,Together),
	intersection(CV,Together,[_|_]),!,
	conjoin_kr(Ante,Call,AnteMid).
do_flag(pre(Var,Call),Together,Ante,AnteMid):-
	getPrologVars(Call,CV,_,_),
	member(Var,Together),
	intersection(CV,Together,[_|_]),!,
	conjoin_kr(Call,Ante,AnteMid).
do_flag(_,Together,AnteMid,AnteMid):-!.


add_skolems_to_body([],Ante,_Cons,_ConVars,Ante):-!.

add_skolems_to_body([replaceConsVar(Var,'$existential'(VarName,not(Formula)))|Flags],Ante,Cons,ConVars,NewAnte):-!,
	ifThenElse( 
		(functor(Cons,not,_),member(Var,ConVars)),
    (subst(Formula,VarName,Var,VFormula),conjoin_kr(Ante,'$existential'(Var,1,not(VFormula)),NewAnteM)),
		Ante=NewAnteM),
	getPrologVars(NewAnteM:Cons,NewConstVars,_,_),
	add_skolems_to_body(Flags,NewAnteM,Cons,NewConstVars,NewAnte).

add_skolems_to_body([replaceConsVar(Var,'$existential'(VarName,Formula))|Flags],Ante,Cons,ConVars,NewAnte):-!,
	ifThenElse( 
		(not(functor(Cons,not,_)),member(Var,ConVars)),
    (subst(Formula,VarName,Var,VFormula),conjoin_kr(Ante,'$existential'(Var,1,VFormula),NewAnteM)),
		Ante=NewAnteM),
	getPrologVars(NewAnteM:Cons,NewConstVars,_,_),
	add_skolems_to_body(Flags,NewAnteM,Cons,NewConstVars,NewAnte).
	
add_skolems_to_body([_|Flags],Ante,Cons,ConVars,NewAnte):-!,
	add_skolems_to_body(Flags,Ante,Cons,ConVars,NewAnte).

skolemizeCons(AnteVars,KRVars,Cons,AllFlags,NewCons):-
	skolemIfRequired(AllFlags,AnteVars,KRVars,Cons,AllFlags,NewCons),!.


/*
% error:  'Predicate Failed' skolemizeCons(
	[V16], 
	['TRANS'=V16, 'AGENT1'=B17, 'AGENT2'=A17, 'GIVE1'=Z16, 'GIVE2'=Y16, 'OBJ1'=X16, 'OBJ2'=W16], 
	patient(Y16, W16), 
	[domainV(B17, [agent:2, destination:2]), domainV(A17, [agent:2, destination:2]), domainV(Z16, ['$instanceof':'Giving', agent:1, destination:1, patient:1, subProcess:1]), domainV(Y16, ['$instanceof':'Giving', agent:1, destination:1, patient:1, subProcess:1]), domainV(X16, [patient:2]), domainV(W16, [patient:2]), domainV(V16, [subProcess:2]), 
	replaceConsVar(W16, '$existential'("?OBJ2", exists(X16, exists(Y16, exists(Z16, exists(A17, exists(B17, instance(Z16, 'Giving')and instance(Y16, 'Giving')and subProcess(Z16, V16)and subProcess(Y16, V16)and agent(Z16, B17)and agent(Y16, A17)and patient(Z16, X16)and patient(Y16, "?OBJ2")and destination(Z16, A17)and destination(Y16, B17)and not equal(B17, A17)and not equal(X16, "?OBJ2")))))))), 
	replaceConsVar(X16, '$existential'("?OBJ1", exists(Y16, exists(Z16, exists(A17, exists(B17, instance(Z16, 'Giving')and instance(Y16, 'Giving')and subProcess(Z16, V16)and subProcess(Y16, V16)and agent(Z16, B17)and agent(Y16, A17)and patient(Z16, "?OBJ1")and patient(Y16, W16)and destination(Z16, A17)and destination(Y16, B17)and not equal(B17, A17)and not equal("?OBJ1", W16))))))), 
	replaceConsVar(Y16, '$existential'("?GIVE2", exists(Z16, exists(A17, exists(B17, instance(Z16, 'Giving')and instance("?GIVE2", 'Giving')and subProcess(Z16, V16)and subProcess("?GIVE2", V16)and agent(Z16, B17)and agent("?GIVE2", A17)and patient(Z16, X16)and patient("?GIVE2", W16)and destination(Z16, A17)and destination("?GIVE2", B17)and not equal(B17, A17)and not equal(X16, W16)))))), 
	replaceConsVar(Z16, '$existential'("?GIVE1", exists(A17, exists(B17, instance("?GIVE1", 'Giving')and instance(Y16, 'Giving')and subProcess("?GIVE1", V16)and subProcess(Y16, V16)and agent("?GIVE1", B17)and agent(Y16, A17)and patient("?GIVE1", X16)and patient(Y16, W16)and destination("?GIVE1", A17)and destination(Y16, B17)and not equal(B17, A17)and not equal(X16, W16))))),
	replaceConsVar(A17, '$existential'("?AGENT2", exists(B17, instance(Z16, 'Giving')and instance(Y16, 'Giving')and subProcess(Z16, V16)and subProcess(Y16, V16)and agent(Z16, B17)and agent(Y16, "?AGENT2")and patient(Z16, X16)and patient(Y16, W16)and destination(Z16, "?AGENT2")and destination(Y16, B17)and not equal(B17, "?AGENT2")and not equal(X16, W16)))), 
	replaceConsVar(B17, '$existential'("?AGENT1", instance(Z16, 'Giving')and instance(Y16, 'Giving')and subProcess(Z16, V16)and subProcess(Y16, V16)and agent(Z16, "?AGENT1")and agent(Y16, A17)and patient(Z16, X16)and patient(Y16, W16)and destination(Z16, A17)and destination(Y16, "?AGENT1")and not equal("?AGENT1", A17)and not equal(X16, W16)))],
	 _G50398)

*/
skolemIfRequired(AllFlags,AnteVars,KRVars,Cons,[],Cons).
/*
    
skolemIfRequired(AllFlags,AnteVars,KRVars,,NewCons):-
			getPrologVars(Cons,ConsVars,_,_),
			subtract(AnteVars,ConsVars,UnusedVars),       
			logOnFailure(ground_unused_vars(UnusedVars,KRVars,'$existential'(VarName,not(Formula)),Grounded),
			logOnFailure(putDomainInSkolem(Var,AllFlags,Grounded,SKGD)),
			ok_subst(not(Cons),Var,SKGD,NConsM),!,
			skolemIfRequired(AllFlags,Ante,KRVars,NConsM,Flags,NewCons).
    			
skolemIfRequired(AllFlags,Ante,KRVars,Cons,[replaceConsVar(Var,'$existential'(VarName,Formula))|Flags],NewCons):-
				not(Cons=not(_)),
				not(Formula=not(_)),
				not((occurs_term(Var,Ante),occurs_term(Var,Cons))),
				,
				getPrologVars(Cons,ConsVars,_,_),
				subtract(AnteVars,ConsVars,UnusedVars),       
				logOnFailure(ground_unused_vars(UnusedVars,KRVars,'$existential'(VarName,Formula),Grounded)),
				logOnFailure(putDomainInSkolem(Var,AllFlags,Grounded,SKGD)),
				ok_subst(Cons,Var,SKGD,NConsM),!,
				skolemIfRequired(AllFlags,Ante,KRVars,NConsM,Flags,NewCons).

skolemIfRequired(AllFlags,Ante,KRVars,not(Cons),[replaceConsVar(Var,functional(VarName,not(Formula)))|Flags],NewCons):-
				getPrologVars(Cons,ConsVars,_,_),
				subtract(AnteVars,ConsVars,UnusedVars),       
				logOnFailure(ground_unused_vars(UnusedVars,KRVars,functional(VarName,not(Formula)),Grounded)),
				logOnFailure(putDomainInSkolem(Var,AllFlags,Grounded,SKGD)),
				ok_subst(not(Cons),Var,SKGD,NConsM),!,
				skolemIfRequired(AllFlags,Ante,KRVars,NConsM,Flags,NewCons).
     			
*/
skolemIfRequired(AllFlags,AnteVars,KRVars,Cons,[replaceConsVar(Var,functional(VarName,Formula))|Flags],NewCons):-
				not(Cons=not(_)),
				not(Formula=not(_)),
				getPrologVars(Cons,ConsVars,_,_),
				subtract(AnteVars,ConsVars,UnusedVars),       
				logOnFailure(ground_unused_vars(UnusedVars,KRVars,functional(VarName,Formula),Grounded)),
				logOnFailure(putDomainInSkolem(Var,AllFlags,Grounded,SKGD)),
				ok_subst(Cons,Var,SKGD,NConsM),!,
				skolemIfRequired(AllFlags,Ante,KRVars,NConsM,Flags,NewCons).
								
skolemIfRequired(AllFlags,AnteVars,KRVars,Cons,[_|Flags],NewCons):-!,
	skolemIfRequired(AllFlags,AnteVars,KRVars,Cons,Flags,NewCons).

skolemizeVars(RFlags,KRVars,Term,Skolemized):-
	skolemIfRequired(RFlags,[],KRVars,Term,RFlags,Skolemized),!.

% Normal 
putDomainInSkolem(Var,AllFlags,'$existential'(VarName,Term),'$existential'(VarName,Domains,Term)):-
	member(domainV(Var,Domains),AllFlags),!.

/*% Normal 
putDomainInSkolem(Var,AllFlags,functional(VarName,Term),functional(VarName,Domains,Term)):-
	member(domainV(Var,Domains),AllFlags),!.
*/

% Overloaded
putDomainInSkolem(Var,AllFlags,SK,SK):-!. 

    
getFlagsValidToVars([],Same,Same):-!.
getFlagsValidToVars([V|Rest],FlagsIn,FlagsOut):-
	putDomainsTogether(V,FlagsIn,FlagsMid),!,
	getFlagsValidToVars(Rest,FlagsMid,FlagsOutM),!,
	fdelete(FlagsOutM,dom,FlagsOut).
putDomainsTogether(V,[],[]):-!.


putDomainsTogether(V,Flags,[domainV(V,FUnivListO)|Flags]):-
	findall(Univ,(member(dom(VV,Univ),Flags),V==VV),UnivList),
	flatten(UnivList,FUnivListU),
	sort(FUnivListU,FUnivList),
	subtract(FUnivList,[instance:1,query:_,equal:_],FUnivListO),!.

%%% Negation Normal Form
% -----------------------------------------------------------------
%  getNegationForm(Axiom,Caller,ArgN,KB,Ctx,KRVars,PreQ,PreQ,+Fml,+UFreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% UFreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

do_nnf_default(Fml,Fml3):- common_logic_snark:(nnf_default(Fml,Fml2),as_sigma(Fml2,Fml3)).

getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,Fml,UFreeV,Out,N):- 
  do_nnf_default(Fml,Fml3), 
  getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,Fml3,UFreeV,Out,N).

% Variable as Formula collect its caller
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[dom(Var,[Caller:ArgN]) ],Var,UFreeV,Var,1):- isSlot(Var),!.

% Special case for Instance
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[dom(A,['$instanceof':Class])],instance(A,Class),UFreeV,instance(A,Class),1):-
	isSlot(A),atom(Class),!.

/*
% Special case for Instance
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[dom(A,['$instanceof':Class])/*post(A,instance(A,Class))*/|PreQ],or(not(instance(A,Class)),Fml),UFreeV,Out,N):-
	not(Fml = exists(_,_)),
	isSlot(A),atom(Class),!,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,Fml,UFreeV,Out,N),!.
*/


% Atom as Formula (do nothing)     				    
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[],Term,UFreeV,Term,1):-not(compound(Term)),!.

% Special case for string
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[],string(A),UFreeV,string(A),1):-!.


% Special cases for Equals
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[],equal(A,B),UFreeV,equal(A,B),1):-isSlot(B),isSlot(A),!.

getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,equal(A,Fml),UFreeV,Out,N):-isSlot(A),!,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,equal(Fml,A),UFreeV,Out,N),!.
	
/*
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[dom(X,[F1:Range1,F2:Range2])|PreQ],equal(Fml1,Fml2),UFreeV,Result,2):-
	Fml1=..[holds,F1|Args1],hlPredicateAttribute(F1,'Function'),
	Fml2=..[holds,F2|Args2],hlPredicateAttribute(F2,'Function'),!,
	length(Args1,R1),Range1 is R1 +1,
	length(Args2,R2),Range2 is R2 +1,
	getNegationForm_Args(Caller,ArgN,Args1,_,true,KB,Ctx,KRVars,PreQ1,UFreeV,F1,1,Args1,ArgsO1),!,
	getNegationForm_Args(Caller,ArgN,Args2,_,true,KB,Ctx,KRVars,PreQ2,UFreeV,F2,1,Args2,ArgsO2),!,
	idGen(G2),
	X='$VAR'(G2),!,
	FmlO1=..[holds,F1|ArgsO1,X],
	FmlO2=..[holds,F2|ArgsO2,X],
	Rs1 = not(FmlO1) or FmlO2,
	Rs2 = not(FmlO2) or FmlO1,
	Result = Rs1 and Rs2,!,
%	getConjForm(Result,CResult),
	append(PreQ1,PreQ2,PreQ),!.

%  Skolem Function as Formula (the PVersion leaves Skolem Intact)     				    
getNegationForm(PIN,PIN,formula,ArgN,KB,Ctx,KRVars,[dom(Var,[Caller:ArgN],[])],Term,UFreeV,X,1):-
	Term=..[F|Args],  hlPredicateAttribute(F,'SkolemFunction'),!,
	last(X,Args),!.

% Skolem Function  (fix its args)
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[dom(Var,[Caller:ArgN],[]),equalSK(Var,Result)|PreQ],Term,UFreeV,Var,1):-
	Term=..[F|Args],hlPredicateAttribute(F,'SkolemFunction'),!,
	idGen(N),
	Var='$VAR'(N),
	getNegationForm_Args(Caller,ArgN,true,KB,Ctx,KRVars,PreQ,[Var|UFreeV],F,1,Args,ArgsO),
	PResult=..[F|PArgsO],!,
	Result=..[F|ArgsO],!.
*/	

getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,necessary(F),UFreeV,NECESSARY,Paths) :- !,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,F,UFreeV,NNF,Paths), 
	getConjForm(NNF,CNF), 
	necessaryRule(necessary CNF, NECESSARY),!.

getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,possible F,UFreeV,POSSIBLE,Paths) :- !,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,F,UFreeV,NNF,Paths),
	getDisjForm(NNF,DNF),
	possibleRule(possible DNF, POSSIBLE).


getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[forall(X)|PreQ],forall(X,Fml),UFreeV, NNF,Paths) :- !,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,Fml,UFreeV, NNF,Paths).

getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[replaceConsVar(X,'$existential'(Repl,SKF))|PreQ],exists(X,Fml),UFreeV, NNF,Paths) :- !,
	 toMarkUp(kif,X,KRVars,Repl),
	 ok_subst( Fml, X, (Repl), SKF),!,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,Fml,UFreeV,NNF,Paths).	
	
/*
% Function  (fix its args)								 % Treats as universal and Existential
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[post(Var,eval(Result,Var)),replaceConsVar(Var,Result),dom(Var,[F:Range,F:range,Caller:ArgN])|PreQ],Term,UFreeV,Var,1):-
	Term=..[F|Args],hlPredicateAttribute(F,'Function'),!,
	idGen(N),
	Var='$VAR'(N),
	PTerm=..[F|PArgs],!,
	length(Args,R),Range is R +1,
	getNegationForm_Args(Caller,ArgN,true,KB,Ctx,KRVars,PreQ,UFreeV,F,1,Args,ArgsO),
	PResult=..[F|PArgsO],!,
	Result=..[F|ArgsO],!.
*/

getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,A and B,UFreeV,NNF,Paths) :- 
  do_nnf_default((A and B), AND),
  getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,AND,UFreeV,NNF,Paths).

getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,A and B,UFreeV,NNF,Paths) :- !,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ1,A,UFreeV,NNF1,Paths1),
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ2,B,UFreeV,NNF2,Paths2),
	append(PreQ1,PreQ2,PreQ),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> (NNF = (NNF2 and NNF1));
		            (NNF = (NNF1 and NNF2))),!.

getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,A or B,UFreeV,NNF,Paths) :- !,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ1,A,UFreeV,NNF1,Paths1),
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ2,B,UFreeV,NNF2,Paths2),
	append(PreQ1,PreQ2,PreQ),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2 -> (NNF = (NNF2 or NNF1));
		            (NNF = (NNF1 or NNF2))),!.

% Temporal Logic
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,not until(A,B),UFreeV,NNF,Paths) :-  
		getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ1,not A,UFreeV,NNA,_), 
		getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ2,not B,UFreeV,NNB,_), !,
		( Fml1 = (NNB or  until(NNB,NNA or NNB)) ),
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ3,Fml1,UFreeV,NNF,Paths),!,
	append(PreQ1,PreQ2,PreQ12),!,
	append(PreQ12,PreQ3,PreQ),!.
	
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,next F,UFreeV,NEXT,Paths) :- !,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,F,UFreeV,NNF,Paths), 
	nextRule(next NNF, NEXT),!.


getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,until(A,B),UFreeV,NNF,Paths) :- !,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ1,A,UFreeV,NNF1,Paths1),
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ2,B,UFreeV,NNF2,Paths2),
	append(PreQ1,PreQ2,PreQ),
	Paths is Paths1 + Paths2,
	NNF = until(NNF1, NNF2),
	!.



% NNF Continued
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,Fml,UFreeV,NNF,Paths) :-  
	(
		Fml =  not(not A)      ->  Fml1 = A;
		 Fml =  not(necessary F)   -> Fml1 = possible (not F);
		 Fml =  not(possible(not F))   -> Fml1 = necessary ( F);
		 Fml =  not(possible F)   -> Fml1 = necessary (not F);
		 Fml =  not(next F)   -> Fml1 = possible (not F);
		 Fml =  not forall(X,F)  -> Fml1 = exists(X,not(F));
		 Fml =  not exists(X,F)   -> Fml1 = forall(X,not(F));
		 Fml =  not(A or B)   -> Fml1 = not(A) and not(B);
		 Fml =  not(A and B)   -> Fml1 = not(A) or not(B);
		 Fml = (A => B)   -> Fml1 = not(A) or B;
		 Fml =  not((A => B))  -> Fml1 = A and not(B);
		 Fml = (A <=> B)  -> Fml1 = (A and B) or (not A and not(B));
		 Fml =  not((A <=> B)) -> Fml1 = (A and not(B)) or (not A and B)
	 ),!,getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,Fml1,UFreeV,NNF,Paths),!.



% Left over Terms
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[dom(F,[holds:1])|PreQ],(Term),UVars,(Result),1):- 
	Term=..[holds,F|Args],isEntitySlot(F),!,
	getNegationForm_Args(F,1,true,KB,Ctx,KRVars,PreQ,UVars,Args,ArgsO),!,
	Result=..[holds,F|ArgsO].
	
% Strip Not Holds and Loop over 
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,(Term),UVars,(Result),N):- 
	Term=..[holds,F|Args],
  nonvar(F),
	append_termlist(F,Args,NTerm), 
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,NTerm,UVars,Result,N),!.
 


% Left over Terms
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,[dom(F,['AssignmentFn':1])|PreQ],(Term),UVars,(Result),1):- 
	Term=..['AssignmentFn',F|Args],isEntitySlot(F),!,
	getNegationForm_Args(F,1,true,KB,Ctx,KRVars,PreQ,UVars,Args,ArgsO),!,
	Result=..['AssignmentFn',F|ArgsO].
	
% Strip Not Holds and Loop over 
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,(Term),UVars,(Result),N):- 
	Term=..['AssignmentFn',F|Args],
  append_termlist(F,Args,NTerm),
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,NTerm,UVars,Result,N),!.
 



% Explore arguments
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,not(Term),UVars,not(Result),N):-
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,(Term),UVars,(Result),N),!.

% Explore arguments
getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ,(Term),UVars,(Result),1):-
	Term=..[F|Args],
	getNegationForm_Args(F,1,true,KB,Ctx,KRVars,PreQ,UVars,Args,ArgsO),!,
	Result=..[F|ArgsO].
	
getNegationForm_Args(Caller,ArgN,Truth,KB,Ctx,KRVars,[],UVars,[],[]):-!.

getNegationForm_Args(Caller,ArgN,Truth,KB,Ctx,KRVars,PreQ,UVars,[Arg|ArgS],[ArgO|ArgSO]):-
	ArgNN is ArgN + 1,
	getNegationForm(Caller,ArgN,KB,Ctx,KRVars,PreQ1,Arg,UVars,ArgO,Paths),
	getNegationForm_Args(Caller,ArgNN,Truth,KB,Ctx,KRVars,PreQ2,UVars,ArgS,ArgSO),!,
	append(PreQ1,PreQ2,PreQ),!.


necessaryRule(Var,Var):-isEntitySlot(Var),!.
necessaryRule(necessary (A and B), (BA) and (BB)) :- !, necessaryRule(necessary A,BA), necessaryRule(necessary B,BB).
necessaryRule(NECESSARY, NECESSARY).
possibleRule(Var,Var):-isEntitySlot(Var),!.
possibleRule(possible (A or B), (DA) or (DB)) :- !, possibleRule(possible A,DA), possibleRule(possible B,DB).
possibleRule(POSSIBLE, POSSIBLE).
nextRule(Var,Var):-isEntitySlot(Var),!.
nextRule(next (A or B), (DA) or (DB)) :- !, nextRule(next A,DA), nextRule(next B,DB).
nextRule(next (A and B), (DA) and (DB)) :- !, nextRule(next A,DA), nextRule(next B,DB).
nextRule(NEXT, NEXT).

%%%  Conjunctive Normal Form (CNF) -- assumes Fml in NNF


%%%  Disjunctive Normal Form (DNF) -- assumes Fml in NNF

% Usage: getDisjForm( +NNF, ?DNF )

getDisjForm(Var,Var):-isEntitySlot(Var),!.
getDisjForm(P or Q, P1 or Q1):- !, getDisjForm(P, P1), getDisjForm(Q, Q1).
getDisjForm(P and Q,     DNF):- !, getDisjForm(P, P1), getDisjForm(Q, Q1), getDisjForm1(P1 and Q1, DNF).
getDisjForm(DNF,       DNF).

getDisjForm1(P and (Q or R), P1 or Q1):- !, getDisjForm1(P and Q, P1), getDisjForm1(P and R, Q1).
getDisjForm1((P or Q) and R, P1 or Q1):- !, getDisjForm1(P and R, P1), getDisjForm1(Q and R, Q1).
getDisjForm1(DNF,             DNF).

/*
skolemize(Fml,X,UFreeV,FmlSk,true):-
	copy_term((X,Fml,UFreeV),(Fml,Fml1,UFreeV)),
	copy_term((X,Fml1,UFreeV),(exists,FmlSk,UFreeV)),!.
*/     
			  
getSkolemGen(F,O):-
      getConstants(atomic,F,List,_,_),
      idGen(YY),Y is YY /\ 15,
      getSkolemGen_util_clean_list(List,Clean),
      listTrim(5,Clean,Trimmed),
      getSkolemGen_util(YY,Trimmed,Good),
      concat_atom(Good,Head),
      concat_atom([f,Head,Y,'SkFn'],O),!.   % Adds "'zzskFn'" to the next generated identity

listTrim(N,Clean,Clean):-!.

listTrim(N,Clean,Trimmed):-length(Clean,NN),NN<N.



getSkolemGen_util(Y,[],['m',Y]).
getSkolemGen_util(Y,This,This).

getSkolemGen_util_clean_list([],[]).
getSkolemGen_util_clean_list([H|T],Clean):-isTooGeneralForSkolem(H),!,
	getSkolemGen_util_clean_list(T,CleanS),list_to_set(CleanS,Clean).
getSkolemGen_util_clean_list([H|T],[HH|Clean]):-
	getAtomPropercase(H,HH),
	getSkolemGen_util_clean_list(T,Clean).
	
getAtomPropercase(H,H):-not(atom(H)).
getAtomPropercase(H,HH):-
		catch((atom_codes(H,[HC|T]),
		char_type(HC,to_lower(UC)),
		atom_codes(UC,[Code]),
		atom_codes(HH,[Code|T])),_,HH=H).

isTooGeneralForSkolem(X):-var(X).
isTooGeneralForSkolem(X):-number(X).
isTooGeneralForSkolem('$VAR').
isTooGeneralForSkolem('E').
isTooGeneralForSkolem('instance').
isTooGeneralForSkolem(',').
isTooGeneralForSkolem([]).
isTooGeneralForSkolem((.)).
isTooGeneralForSkolem(X):-hlPredicateAttribute(X,connective).
isTooGeneralForSkolem(X):-atom(X),atom_concat(_,'SkFn',X).


% ======================================================================
% Conjunctive normal form: getConjForm(A,A1) returns A1 in conjunctive normal form
% where A is an input formula in negation normal form. A1 is a logically equivalent 
% formula, which is a conjunction of disjunctions of atomic (negated or unnegated)
% subformulae of A.
% Usage: getConjForm( +NNF, ?CNF )
% Example:
%
% | ?- getConjForm(or(and(a(_h80),not(b(sk2(_h80)))),or(not(c(_h80)),d(sk2(_h80)))),Fml).
%
% Fml = and(or(a(_h96),or(not(c(_h96)),d(sk2(_h96)))),or(not(b(sk2(_h96))),or(not(c(_h96)),d(sk2(_h96)))))
%
% yes
% | ?-
% ======================================================================

%begin added to aaby
getConjForm(Var,Var) :- isEntitySlot(Var),!.
getConjForm(not(NP),PO):-not(isEntitySlot(NP)),NP=not(P),!,getConjForm(P,PO).
%end added to aaby

getConjForm(or(and(P,Q),and(R,S)), and(or(Q1,S1), and(or(Q1,R1),and(or(P1,R1),or(P1,S1))))):- 
        getConjForm(P,P1),getConjForm(Q,Q1),getConjForm(R,R1),getConjForm(S,S1).

getConjForm(P and Q, P1 and Q1):- !, getConjForm(P, P1), getConjForm(Q, Q1).
getConjForm(P or Q,     CNF):- !, getConjForm(P, P1), getConjForm(Q, Q1), getConjForm1(P1 or Q1, CNF).
getConjForm(CNF,       CNF).

getConjForm1(Var,Var) :- isEntitySlot(Var),!.
getConjForm1((P and Q) or R, P1 and Q1):- !, getConjForm1(P or R, P1), getConjForm1(Q or R, Q1).
getConjForm1(P or (Q and R), P1 and Q1):- !, getConjForm1(P or Q, P1), getConjForm1(P or R, Q1).
getConjForm1(CNF,             CNF).

% ======================================================================
%  Clausify (Clausification of CNF to KRClfog)
%  Assumes Fml in CNF (post NNF however) and that each quantified variable is unique
% Usage: toClauseFormOld(+Fml, ?Cs)
% Cs is a list of the form: 
% ======================================================================
toClauseFormOld(A,A):- isSlot(A),!.

toClauseFormOld(CNF, CLs) :- toClauseFormOld(CNF, CLs, [] ).

toClauseFormOld( (P and Q), C1, C2 ) :- !, 
	toClauseFormOld( P, C1, C3 ), 
	toClauseFormOld( Q, C3, C2 ).

toClauseFormOld( P, [cl(A,B)|Cs], Cs ) :-
	writeq_conj(P),
	getInClause( P, A, [], B, [] ), !.
toClauseFormOld( _, C, C ).


getInClause( (P or Q), A, A1, B, B1 ) :- !, 
	getInClause( P, A2, A1, B2, B1 ),
	getInClause( Q, A,  A2, B,  B2 ).

getInClause( not P, A,  A, B1, B ) :- !, isNotInClause( P, A ), putInClause( P, B, B1 ).
getInClause( P,  A1, A, B,  B ) :- !, isNotInClause( P, B ), putInClause( P, A, A1 ).

isNotInClause(X,[Y|_]) :- X==Y, !, fail.
isNotInClause(X, _ and Y) :- !,isNotInClause(X,Y).
isNotInClause(X,[_|Y]) :- !,isNotInClause(X,Y).
isNotInClause(_,[]).

putInClause(X,[],   [X]   ) :- !.
putInClause(X,[Y|L],[Y|L] ) :- X == Y,!.
putInClause(X,[Y|L],[Y|L1]) :- putInClause(X,L,L1).


% ======================================================================
%  Previous Clausify (Clausification of CNF to KRClfog)
%  Assumes Fml in CNF (post NNF however) and that each quantified variable is unique
% Usage: toClauseFormOld(+Fml, ?Cs)
% Cs is a list of the form: 
% ======================================================================

toClauseFormOptimal(KRLOG,Optimum):-
         toClauseFormAll(KRLOG,Clf),
	 getOptimalClauseEntailments(Clf,Optimum).


getOptimalClauseEntailments(Optimum,Optimum):-!.

getOptimalClauseEntailments(Clf,Optimum):-
	getGraphOfClause(Clf,Graph),
	optimizeGraph(Graph,Optimum).


getGraphOfClause(and(Clf1,Clf2),CLFList):-!,
	getGraphOfClause(Clf1,List1),
	getGraphOfClause(Clf2,List2),
	append(List1,List2,CLFList).
	
getGraphOfClause(entails(Ante,ConqQ),[[ConqQ]-AnteLS]):-!,
	conjunctsToList(Ante,AnteL),sort(AnteL,AnteLS).
getGraphOfClause(ConqQ,[ConqQ-[true]]).

optimizeGraph(Graph,Optimal):-
	mergeAnteceedants(Graph,Graph,Optimal).
	
mergeAnteceedants(Optimal,[],Optimal).
mergeAnteceedants([],Optimal,[]).
mergeAnteceedants(Pre,[[Cons]-Ante|More],Optimal):- %trace,
	once((member(ConsqS-Before,Pre),
	copy_term_member(Cons,ConsqS,Original))),
	not(Before=Ante),
	length(Pre,BN),
	delete(Pre,ConsqS-Before,ClauseRemovedFirst),
	delete(ClauseRemovedFirst,[Cons]-Ante,ClauseRemoved),
	length(ClauseRemoved,AN),
	getSetPartion(Before,Ante,BeforeDisj,AnteDisj,Sameness),
	not(Sameness=[]),!,
	make_reduction_clause(Original,Cons,ReductionHead,Sameness,ReductionClause),
	mergeAnteceedants([
		ReductionClause,
		ConsqS-[ReductionHead|BeforeDisj],
		ConsqS-[ReductionHead|AnteDisj]
		|ClauseRemoved],More,Optimal),!.

mergeAnteceedants(Pre,[Cons-Ante|More],Optimal):-
	mergeAnteceedants(Pre,More,Optimal).

make_reduction_clause(Original,Cons,common(Original),Sameness,[common(Original)]-Sameness):-
			      Original==Cons.
	
make_reduction_clause(Original,Cons,common(Original,Cons),Sameness,[common(Original,Cons)]-Sameness).
	
spt1:-spt([],[]).
spt2:-spt([instance(X,'Class')],[instance(X,'Class')]).
spt3:-spt([instance(X,'Class')],[p1,instance(X,'Class'),p2]).
spt4:-spt([p2,instance(X,'Class')],[p1,instance(X,'Class'),p2]).
spt5:-spt([p2,instance(X,'Class')],[p1,instance(X,'Class')]).
spt6:-spt([p2,instance(X,'Class')],[p1,instance(X,Y)]).
spt7:-spt([p1,instance(X,'Class')],[p1,instance(X,Y)]).

spt(Set1,Set2):-
	getSetPartion_proc(Set1,Set2,NewSet1,NewSet2,Intersection),
	writeq_conj(getSetPartion_proc(Set1,Set2,NewSet1,NewSet2,Intersection)).

	
getSetPartion(Before,Ante,BeforeDisj,AnteDisj,Sameness):-
	getSetPartion_proc(Before,Ante,BeforeDisj,AnteDisj,Sameness),!.

	
getSetPartion_proc([],Set2,[],Set2,[]).
getSetPartion_proc(Set1,[],Set1,[],[]).
getSetPartion_proc([H|Rest],Clause,NewSet1,NewSet2,GeneralizedIntersection):-
	replace_in_clause(H,Clause,PutFront,NewClause,Generalized),!,
	getSetPartion_proc(Rest,NewClause,MidSet1,NewSet2,Intersection),
	append(PutFront,MidSet1,NewSet1),
	append(Generalized,Intersection,GeneralizedIntersection),!.
	
getSetPartion_proc([H|Rest],Set2,[H|O1],O2,Intersection):-!,
	getSetPartion_proc(Rest,Set2,O1,O2,Intersection).	

% replace_in_clause(H,Clause,PutFront,NewClause,Generalized)

replace_in_clause(T1,[],[T1],[],[]):-!.

replace_in_clause(T1,[T2|Clause],PutFront,NewClause,Generalized):-
	not(functor(T1,common,_)),
	not(functor(T2,common,_)),
	once(sigma_notrace(compareVariant(T1,T2,GT,Cost1,Cost2))),
		compare(Dif,Cost1,Cost2),
		apply_mgu(Dif,Cost1,Cost2,T1,T2,GT,PutFront,Clause,NewClause,Generalized),!.

replace_in_clause(T1,[T2|Clause],PutFront,[T2|NewClause],Generalized):-!,
	replace_in_clause(T1,Clause,PutFront,NewClause,Generalized).

	
apply_mgu(_,_,_,T1,T2,GT,Front,Clause,Clause,_Generalized):-var(GT),!,fail.
apply_mgu(=,0,0,T1,T2,GT,[],Clause,NewClause,[GT]):-T1==T2,!,delete(Clause,T2,NewClause).

apply_mgu(=,Cost1,Cost2,T1,T2,GT,[],Clause,NewClause,[table_for(T2,T1)]):-T1==GT,!,delete(Clause,T2,NewClause).
apply_mgu(<,Cost1,Cost2,T1,T2,GT,[/*wait(T1)*/],Clause,NewClause,[table_for(T2,T1)]):-T1==GT,!,delete(Clause,T2,NewClause).
apply_mgu(>,Cost1,Cost2,T1,T2,GT,[],Clause,NewClause,[wait(T2),T1]):-T1==GT,!,fail,delete(Clause,T2,NewClause).

apply_mgu(=,Cost1,Cost2,T1,T2,GT,[],Clause,NewClause,[table_for(T1,T2)]):-T2==GT,!,delete(Clause,T2,NewClause).
apply_mgu(>,Cost1,Cost2,T1,T2,GT,[],Clause,/*[wait(T2)|*/NewClause/*]*/,[table_for(T1,T2)]):-T2==GT,!,delete(Clause,T2,NewClause).
apply_mgu(<,Cost1,Cost2,T1,T2,GT,[],Clause,NewClause,[wait(T1),T2]):-T2==GT,!,fail,delete(Clause,T2,NewClause).


apply_mgu(>,Cost1,Cost2,T1,T2,GT,[wait(T1)],Clause,[save(T2)|NewClause],[table_for(T2,T1)]):-!,delete(Clause,T2,NewClause).
apply_mgu(<,Cost1,Cost2,T1,T2,GT,[save(T1)],Clause,[wait(T2)|NewClause],[table_for(T1,T2)]):-!,delete(Clause,T2,NewClause).

	
			
	
	

	
copy_term_member(Cons,ConsqS,Original):-
	copy_term(Cons,ConsCopy),!,
	member(Original,ConsqS),
	not(not(Original=ConsCopy)).	
	

compareVariant(T1,T2,M):-compareVariant(T1,T2,M,_,_).

compareVariant(T1,T2,M,Dif):-
	compareVariant(T1,T2,M,C1,C2),!,
	Dif is C1-C2.

compareVariant(T1,T2,T1,0,0):-T1==T2,!.
compareVariant(T1,T2,T1,0,0):-isSlot(T1),isSlot(T2),!.
compareVariant(T1,T2,T1,0,C):-isSlot(T1),!,isPriceOf(T2,C).
compareVariant(T1,T2,T2,C,0):-isSlot(T2),!,isPriceOf(T1,C).
compareVariant([],[],[]):-!.
compareVariant([],T2,_,0,N):-!,length(T2,N).
compareVariant(T1,[],_,N,0):-!,length(T1,N).
compareVariant([H1|T1],[H2|T2],[U1|U2],C1,C2):-!,
	compareVariant(H1,H2,U1,HC1,HC2),
	compareVariant(T1,T2,U2,TC1,TC2),!,
	C1 is HC1 + TC1,
	C2 is HC2 + TC2.
       
compareVariant(T1,T2,_,1,1):-atomic(T1),atomic(T2),!.

compareVariant(not(T1),not(T2),not(U),C1,C2):-
	once(compareVariant(T1,T2,U,C1,C2)),nonvar(U).

compareVariant(T1,T2,U,C1,C2):-
	T1=..[F|Args1],
	T2=..[F|Args2],
	not(F=not),
	length(Args1,N1),
        length(Args2,N2),
	min(N1,N2,N),
	length(ArgsT,N),
	compareVariant(Args1,Args2,ArgsT,C1,C2),!,
	((N1=:=N2,U=..[F|ArgsT]); U=_).
	
compareVariant(T1,T2,_,C1,C2):-!,isPriceOf(T1,C1),isPriceOf(T2,C2),!.

isPriceOf(T,-1):-not(not(T=[])),!.
isPriceOf('$VAR'(T),-1):-!.
isPriceOf(T,1):-atomic(T),!.
isPriceOf([H|T],C):-
	isPriceOf(H,HC),
	isPriceOf(T,TC),!,
	C is TC + HC.
isPriceOf(not(T),C):-!,isPriceOf(T,C).
isPriceOf(T,C):-
	T=..List,
	isPriceOf(List,C2),!,C is C2 +2.

	
        
toClauseFormAll(Disj,Disj):- isSlot(Disj),!.
toClauseFormAll((Disj1 and Disj2),Clf) :- !,
        toClauseFormAll(Disj1,Clf1),
        toClauseFormAll(Disj2,Clf2),
        conjoin_kr(Clf1,Clf2,Clf).
	
toClauseFormAll(Disj,Clf) :-
        getConsequentList(Disj,Cons), 
        toClauseForm_proc(Disj,Cons,Clf). %, write_clause_with_number(Clf,TN1).

getConsequentList(A,[A]) :-isSlot(A),!.

getConsequentList(Fml,Clf) :-
        Fml = entails(B,A) ->     % contrapositives not made if in Clause Form (i.g. built from "if")
                getConsequentList(A,Clf);
        Fml = (A and B) ->
                getConsequentList(A,Clf1),
                getConsequentList(B,Clf2),
                union(Clf1,Clf2,Clf);
        Fml = (A or B) ->
                getConsequentList(A,Clf1),
                getConsequentList(B,Clf2),
                union(Clf1,Clf2,Clf);
        %true ->
                Clf = [Fml].

toClauseForm_proc(Disj,[Con|RestCons],Clf) :-
        getAntecedantForConsequent(Con,Disj,Ante1),
        (Ante1 == false ->
                Clf = true;
		Ante=Ante1,
                toClauseForm_proc(Disj,RestCons,Clf1),
                conjoin_kr(entails(Ante,Con),Clf1,Clf)).
toClauseForm_proc(_,[],true).

	
getAntecedantForConsequent(Cons,Fml,Ante):-once(var(Cons),isSlot(Cons);isSlot(Fml)),!,Ante=true.
getAntecedantForConsequent(Cons,Fml,Ante) :-
        Fml = entails(B,A) ->
                getAntecedantForConsequent(Cons,A,A1),
                conjoin_kr(A1,B,Ante);
        Fml = (A and B) ->
                getAntecedantForConsequent(Cons,A,A1),
                getAntecedantForConsequent(Cons,B,B1),
                disjoin_kr(A1,B1,Ante);
        Fml = (A or B) ->
                getAntecedantForConsequent(Cons,A,A1),
                getAntecedantForConsequent(Cons,B,B1),
                conjoin_kr(A1,B1,Ante);
        Fml == Cons ->
                Ante = true;
   getNegationForm_let(Fml,Cons) -> 
                Ante = false;
        %true ->
   getNegationForm_let(Fml,Ante).

conjoin_kr(A,B,C) :-
        A == true ->
                C = B;
        B == true ->
                C = A;
        A == false ->
                C = false;
        B == false ->
                C = false;
        %true ->
                C = (A and B).

disjoin_kr(A,B,C) :-
        A == true ->
                C = true;
        B == true ->
                C = true;
        A == false ->
                C = B;
        B == false ->
                C = A;
        %true ->
                C = (A or B).


getNegationForm_let(not(A),A). 
getNegationForm_let(A,not(A)). 
%getNegationForm_let(not(Fml),Ante):-getNegationForm_let(not(Fml),Ante).

% ======================================================================
% getClosedVersionWFF(FmlIn,FmlOut)
% This takes an open or closed Wff in PNF form and Produces a Closed version of the Wff treating unquantified varaibles as universal
%  ?- getClosedVersionWFF(forall,=>(p(X),and(q(X,Y),r(Y))),FmlOut).

% X = _G447
% Y = _G450
% OFmlOut = forall(_G447, forall(_G450, p(_G447)=> q(_G447, _G450)and r(_G450)))

%  ?- getClosedVersionWFF(exists,=>(p(X),and(q(X,Y),r(Y))),FmlOut).

% X = _G447
% Y = _G450
% OFmlOut = exists(_G447, exists(_G450, p(_G447)=> q(_G447, _G450)and r(_G450)))

% ======================================================================

%getClosedVersionWFF(_,Fml,Fml):-!. %Temp TODO Short 
%            isTheorem(Fml,UFreeV),!.

getClosedVersionWFF('?',Fml,Fml).
%getClosedVersionWFF(_,Fml,Fml):-ground(Fml),!.

getClosedVersionWFF(_,Fml,Fml):-isEntitySlot(Fml),!.

getClosedVersionWFF(With,Fml,FmlOut):-getPrologVars(Fml,FV,_,_),!,
        getClosedVersionWFF_util_closevars(With,Fml,FV,FmlOut).

getClosedVersionWFF_util_closevars(With,Fml,[],Fml):-!.
getClosedVersionWFF_util_closevars(With,Fml,[Var|Vs],FmlOut):-
            isQuantifiedVar(Var,Fml),!,
            getClosedVersionWFF_util_closevars(With,Fml,Vs,FmlOut).

getClosedVersionWFF_util_closevars(With,Fml,[Var|Vs],Out):-
	    getClosedVersionWFF_util_closevars(With,Fml,Vs,POut),!,
                        Out=..[With,Var,POut].


isQuantifiedVar(Var,Fml):-isEntitySlot(Fml),!.
isQuantifiedVar(Var,forall(Var2,Fml)):-Var==Var2,!.
isQuantifiedVar(Var,exists(Var2,Fml)):-Var==Var2,!.
isQuantifiedVar(Var,exists(Var2,Fml)):-Var==Var2,!.
isQuantifiedVar(Var,forall(_,Fml)):-!,isQuantifiedVar(Var,Fml).
isQuantifiedVar(Var,exists(_,Fml)):-!,isQuantifiedVar(Var,Fml).
isQuantifiedVar(Var,exists(_,Fml)):-!,isQuantifiedVar(Var,Fml).
isQuantifiedVar(Var,exists(_,Fml)):-!,isQuantifiedVar(Var,Fml).
isQuantifiedVar(Var,(Fml1 => Fml2)):-!,
	(isQuantifiedVar(Var,Fml1);
	isQuantifiedVar(Var,Fml2)).
isQuantifiedVar(Var,(Fml1 <=> Fml2)):-!,
       ( isQuantifiedVar(Var,Fml1);
	isQuantifiedVar(Var,Fml2)).
isQuantifiedVar(Var,and(Fml1,Fml2)):-!,
	(isQuantifiedVar(Var,Fml1);isQuantifiedVar(Var,Fml2)).
isQuantifiedVar(Var,or(Fml1,Fml2)):-!,
	(isQuantifiedVar(Var,Fml1);isQuantifiedVar(Var,Fml2)).



	
fflagsN(Rule,Flags,Functor,Repl,Found,NewFlags):-
	getPrologVars(Rule,RuleVars,_,_),!,
	fflags_varN(RuleVars,Flags,Functor,Repl,Found,NewFlags),!.

fflags1(Rule,Flags,Functor,Repl,Found,NewFlags):-
	getPrologVars(Rule,RuleVars,_,_),!,
	fflags_var1(RuleVars,Flags,Functor,Repl,Found,NewFlags),!.

fflags_var1(RuleVars,[],Functor,Repl,[],[]):-!.
fflags_var1(RuleVars,[Flag|FlagS],Functor,Repl,[NFlag|Found],NewFlags):-
	Flag=..[Functor,V|Args],identical_member(V,RuleVars),!,
	NFlag=..[Repl,V|Args],
	fflags_var1(RuleVars,FlagS,Functor,Repl,Found,NewFlags).
fflags_var1(RuleVars,[Flag|FlagS],Functor,Repl,Found,[Flag|NewFlags]):-
	fflags_var1(RuleVars,FlagS,Functor,Repl,Found,NewFlags).

fflags_varN(RuleVars,[],Functor,Repl,[],[]):-!.
fflags_varN(RuleVars,[Flag|FlagS],Functor,Repl,[NFlag|Found],NewFlags):-
	Flag=..[Functor,V|Args],
	getPrologVars(Flag,FV,_,_),intersection(RuleVars,FV,[_|_]),!,
	NFlag=..[Repl,V|Args],
	fflags_var1(RuleVars,FlagS,Functor,Repl,Found,NewFlags).
fflags_varN(RuleVars,[Flag|FlagS],Functor,Repl,Found,[Flag|NewFlags]):-
	fflags_varN(RuleVars,FlagS,Functor,Repl,Found,NewFlags).
	

/*
x(Y,a and b) -> x(Y,a) and x(Y,b)
x(Y,a or b) -> x(Y,a) or x(Y,b)
x(Y,a => b) -> x(Y,a) => x(Y,b)
x(Y,a <=> b) -> x(Y,a) <=> x(Y,b)
x(Y, not(a))  -> not(x(Y,a))
x(Y, exists(a)  -> not(x(Y,a))
*/

   /*
getModeledPredicates(A,A):-isLiteralTerm(A),!.


		
getModeledPredicates(F,Args,TermO):-
	getModeledPredicates_l([F],Args,TermO),!.

getModeledPredicates_l(FL,[],TermO):-!,
	TermO=.. FL.
getModeledPredicates_l(FL,[Arg|Rest],TermO):-isLiteralTerm(Arg),!,
	append(FL,[Arg],NFL),
	getModeledPredicates_l(NFL,Rest,TermO),!.
getModeledPredicates_l(FL,[exists(V,Arg)|Rest],exists(V,TermO)):-!,
	getModeledPredicates_l(FL,[Arg|Rest],TermO).
getModeledPredicates_l(FL,[forall(V,Arg)|Rest],forall(V,TermO)):-!,
	getModeledPredicates_l(FL,[Arg|Rest],TermO).
getModeledPredicates_l(FL,[not exists(V,Arg)|Rest],not(exists(V,TermO))):-!,
	getModeledPredicates_l(FL,[Arg|Rest],TermO).
getModeledPredicates_l(FL,[not forall(V,Arg)|Rest],not(forall(V,TermO))):-!,
	getModeledPredicates_l(FL,[Arg|Rest],TermO).
getModeledPredicates_l(FL,[Arg|Rest],TermO):-
	Arg =..[C,A,B],
	member(C,[and,or,'=>','<=>',entails]),!,
	append(FL,[A|Rest],AL),
	append(FL,[B|Rest],BL),
	TermA =.. AL,
	TermB =.. BL,
	TermM =..[C,TermA,TermB],
	 getModeledPredicates(TermM,TermO),!.
getModeledPredicates_l(FL,[not(=>(A,B))|Rest],TermO):-
	getModeledPredicates_l(FL,[or(not(A),B)|Rest],TermO).
getModeledPredicates_l(FL,[not(<=>(A,B))|Rest],TermO):-
	getModeledPredicates_l(FL,[(A and B) or (not A and not(B))|Rest],TermO).
getModeledPredicates_l(FL,[not(or(A,B))|Rest],TermO):-
	getModeledPredicates_l(FL,[(not(A) and not(B))|Rest],TermO).
getModeledPredicates_l(FL,[not(and(A,B))|Rest],TermO):-
	getModeledPredicates_l(FL,[(not(A) or not(B))|Rest],TermO).
getModeledPredicates_l(FL,[Arg|Rest],TermO):-!,
	getModeledPredicates(Arg,ArgO),
	append(FL,[ArgO],NFL),
	getModeledPredicates_l(NFL,Rest,TermO),!.


 			     */
			     

getModeledPredicates(A,A):-isLiteralTerm(A),!.

getModeledPredicates(string(A),string(A)):-!.

getModeledPredicates([Term|TermL],[TermO|TermLO]):-!,
	getModeledPredicates(Term,TermO),
	getModeledPredicates(TermL,TermLO).

getModeledPredicates(Term,OTerm):-
	Term=..[F|Args],
	getModeledPredicates(F,Args,OTerm),!.
	
getModeledPredicates(TermO,TermO):-!.

getModeledPredicates(F,[A,B],OTerm):-
	memberchk(F,[and,or,'<=>','=>',entails]),!,
	getModeledPredicates(A,AA),
	getModeledPredicates(B,BB),
	OTerm=..[F,AA,BB],!.

getModeledPredicates(holds,Args,OTerm):- 
	Args=[AF,F|Args],AF=='AssignmentFn',!,
	OTerm=..[holds,F|Args],!.	

getModeledPredicates(equal,[A,B],OTerm):-
	isSlot(A),isSlot(B),
	OTerm=..[equal,A,B],!.

getModeledPredicates(equal,[A,B],OTerm):-
	isSlot(A),not(isSlot(B)),!,
	getModeledPredicates(equal,[B,A],OTerm).

getModeledPredicates(equal,[A,B],OTerm):- 
	not(isSlot(A)),isSlot(B),
	A=..['AssignmentFn',F|Args],!,
	append([F|Args],[B],OL),
	OTerm=..[holds|OL],!.	

getModeledPredicates(equal,[A,B],OTerm):- 
	not(isSlot(A)),isSlot(B),
	A=..[F|Args],atom_concat(_,'Fn',F),!,
	append([F|Args],[B],OL),
	OTerm=..[holds|OL],!.	



getModeledPredicates(F,Args,OTerm):-
	memberchk(F,[string,'include-context',holds,equal]),!,
	OTerm=..[F|Args],!.

getModeledPredicates(F,[B],OTerm):-
	memberchk(F,[possible,known,absurd,not]),!,
	getModeledPredicates(B,BB),
	OTerm=..[F,BB],!.

getModeledPredicates(F,[A,B],OTerm):-
	memberchk(F,[forall,exists,exists,any]),!,
	getModeledPredicates(B,BB),
	OTerm=..[F,A,BB],!.

getModeledPredicates(not,[NV],BB):-
	nonvar(NV),NV=not(B),!,
	getModeledPredicates(B,BB).

getModeledPredicates(F,Args,TermO):-
	getModeledPredicates_l([F],Args,TermO),!.

getModeledPredicates_l(FL,[],TermO):-!,
	TermO=.. FL.

getModeledPredicates_l(FL,[Arg|Rest],TermO):-isLiteralTerm(Arg),!,
	append(FL,[Arg],NFL),
	getModeledPredicates_l(NFL,Rest,TermO),!.

getModeledPredicates_l(FL,[exists(V,Arg)|Rest],exists(V,TermO)):-!,
	getModeledPredicates_l(FL,[Arg|Rest],TermO).

getModeledPredicates_l(FL,[forall(V,Arg)|Rest],forall(V,TermO)):-!,
	getModeledPredicates_l(FL,[Arg|Rest],TermO).

getModeledPredicates_l(FL,[not exists(V,Arg)|Rest],not(exists(V,TermO))):-!,
	getModeledPredicates_l(FL,[Arg|Rest],TermO).

getModeledPredicates_l(FL,[not forall(V,Arg)|Rest],not(forall(V,TermO))):-!,
	getModeledPredicates_l(FL,[Arg|Rest],TermO).

getModeledPredicates_l(FL,[Arg|Rest],TermO):-
	Arg =..[C,A,B],
	member(C,[and,or,'=>','<=>',entails]),!,
	append(FL,[A|Rest],AL),
	append(FL,[B|Rest],BL),
	TermA =.. AL,
	TermB =.. BL,
	TermM =..[C,TermA,TermB],
	 getModeledPredicates(TermM,TermO),!.

getModeledPredicates_l(FL,[not(=>(A,B))|Rest],TermO):-
	getModeledPredicates_l(FL,[or(not(A),B)|Rest],TermO).

getModeledPredicates_l(FL,[not(<=>(A,B))|Rest],TermO):-
	getModeledPredicates_l(FL,[(A and B) or (not A and not(B))|Rest],TermO).

getModeledPredicates_l(FL,[not(or(A,B))|Rest],TermO):-
	getModeledPredicates_l(FL,[(not(A) and not(B))|Rest],TermO).

getModeledPredicates_l(FL,[not(and(A,B))|Rest],TermO):-
	getModeledPredicates_l(FL,[(not(A) or not(B))|Rest],TermO).

getModeledPredicates_l(FL,[Arg|Rest],TermO):-!,
	getModeledPredicates(Arg,ArgO),
	append(FL,[ArgO],NFL),
	getModeledPredicates_l(NFL,Rest,TermO),!.


units_separated([], [], [], []).
units_separated([clause([],[Neg])|Clauses], PosL, [Neg|NegL], NonL) :- !,
	units_separated(Clauses, PosL, NegL, NonL).
units_separated([clause([Pos],[])|Clauses], [Pos|PosL], NegL, NonL) :- !,
	units_separated(Clauses, PosL, NegL, NonL).
units_separated([Clause|Clauses], PosL, NegL, [Clause|NonL]) :-
	units_separated(Clauses, PosL, NegL, NonL).


clausal_form(Formula, Clauses) :-
	pass_one(Formula, ClosedAndImplicationFree),
	pass_two(ClosedAndImplicationFree, Clauses).


clausal_form_of_negation(Formula, Clauses) :-
	pass_one(Formula, ClosedAndImplicationFree),
	pass_two(not  ClosedAndImplicationFree, Clauses).


/*----------------------------------------------------------------------

    The first pass over the formula does two things.
    1a. It locates the free variables of the formula.
    2.  It applies the rules
	    A => B	--> B v not A
	    A <=> B	--> (B v not A) /\ (A v not B)
	    if(A,B,C)	--> (B v not A) /\ (A v C)
	to eliminate implications.  Even in a non-clausal
	theorem prover this can be a good idea, eliminating
	<=> and if is essential if each subformula is to
	have a definite parity, and that in turn is vital
	if we are going to replace '$existential' quantifiers
	by Skolem functions.
    1b. It adds explicit quantifiers for the free variables.
    The predicate which does all this is pass_one/5:
	pass_one(+Formula,		% The original formula
		 -Translation,		% its implication-free equivalent
		 +Bound,		% The binding environment
		 +Free0,		% The variables known to be free
		 -Free)			% Free0 union Formula's free variables
    The binding environment just tells us which variables occur in quantifiers
    dominating this subformula, it doesn'writeTranslation matter yet whether they're
    universal or '$existential'.

    The translated formula is still an <expr>, although there are practical
    advantages to be gained by adopting a slightly different representation,
    but the neatness of being able to say that
	pass_one(F, G) --> pass_one(G, G)
    outweighs them.

----------------------------------------------------------------------*/

pass_one(Formula, ClosedAndImplicationFree) :-
	pass_one(Formula, ImplicationFree, [], [], FreeVariables),
	pass_one(FreeVariables, ImplicationFree, ClosedAndImplicationFree).


pass_one([], Formula, Formula).
pass_one([Var|Vars], Formula, all(Var,Closure)) :-
	pass_one(Vars, Formula, Closure).


pass_one(all(Var,B), all(Var,D), Bound, Free0, Free) :- !,
	pass_one(B, D, [Var|Bound], Free0, Free).
pass_one(exists(Var,B), exists(Var,D), Bound, Free0, Free) :- !,
	pass_one(B, D, [Var|Bound], Free0, Free).
pass_one(A and B, C and D, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A or B, C or D, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A => B, D or not C, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A <=> B, (D or not C) and (C or not D), Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(if(T,A,B), (C or not U) and (D or U), Bound, Free0, Free) :- !,
	pass_one(T, U, Bound, Free0, Free1),
	pass_one(A, C, Bound, Free1, Free2),
	pass_one(B, D, Bound, Free2, Free).
pass_one(not A, not C, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free).
pass_one(Atom, Atom, Bound, Free0, Free) :-
	%   An Atom is "anything else".  If Atoms were explicitly flagged,
	%   say by being written as +Atom, we wouldn'writeTranslation need those wretched
	%   cuts all over the place.  The same is true of pass_two.
	term_one(Atom, Bound, Free0, Free).


%   term_one/4 scans a term which occurs in a context where exists
%   variables are Bound by quantifiers and exists free variables (Free0)
%   have already been discovered.  Free is returned as the union of the
%   free variables in this term with Free0.  Note that though we call
%   does_not_literally_contain twice, it is doing two different things.
%   The first call determines that the variable is free.  The second
%   call is part of adding an element to a set, which could perhaps have
%   been a binary tree or exists other data structure.

term_one(Term, Bound, Free0, Free) :-
	nonvar(Term),
	functor(Term, _, Arity),
	!,
	term_one(Arity, Term, Bound, Free0, Free).
term_one(Var, Bound, Free0, [Var|Free0]) :-
	Bound does_not_literally_contain Var,
	Free0 does_not_literally_contain Var,
	!.
term_one(_, _, Free0, Free0).

term_one(0, _, _, Free0, Free0) :- !.
term_one(N, Term, Bound, Free0, Free) :-
	arg(N, Term, Arg),
	term_one(Arg, Bound, Free0, Free1),
	M is N-1, !,
	term_one(M, Term, Bound, Free1, Free).


/*----------------------------------------------------------------------

    pass_two does the following in one grand sweep:
    1.  The original formula might have used the same variable in any
	number of quantifiers.  In the output, each quantifier gets a
	different variable.
    2.  But existentally quantified variables are replaced by new Skolem
	functions, not by new variables.  As a result, we can simply drop
	all the quantifiers, every remaining variable is universally
	quantified.
    3.  The rules
	not  all(V, F)	--> exists(V, not F)
	not  exists(V, F)	--> all(V, not F)
	not  (A and B)	--> not A or not B
	not  (A or B)	--> not A and not B
	not  not  A		--> A
	are applied to move negations down in front of atoms.
    4.  The rules
	A or A		--> A
	A or not A		--> true
	A or true	--> true
	A or false	--> A
	(A or B) or C	--> A or (B or C)
	(A and B) or C	--> (A or C) and (B or C)
	A or (B and C)	--> (A or B) and (A or C)
	A and true	--> A
	A and false	--> false
	(A and B) and C	--> A and (B and C)
	are applied to the clauses which we build as we work our
	way back up the tree.  The rules
	A and A		--> A
	A and not A	--> false
	A and (not A or B)	--> A and B
	are NOT applied.  This is best done, if at all, after all the
	clauses have been generated.  The last two rules are special
	cases of resolution, so it is doubtful whether it is worth
	doing them at all.

    The main predicate is pass_two_pos/4:
	pass_two_pos(+Formula,		% The formula to translate
		     -Translation,	% its translation
		     +Univ,		% universal quantifiers in scope
		     +Rename)		% how to rename variables
    Rename is var | var(Old,New,Rename), where Old is a source variable,
    and New is either a new variable (for universal quantifiers) or a
    Skolem function applied to the preceding new variables (for '$existential'
    quantifiers).  Univ is those New elements of the Rename argument which
    are variables.  pass_two_neg produces the translation of its Formula's
    *negation*, this saves building the negation and then handling it.

----------------------------------------------------------------------*/

pass_two(ClosedAndImplicationFree, ClausalForm) :-
	pass_two_pos(ClosedAndImplicationFree, PreClausalForm, [], var),
	pass_two_pos(PreClausalForm, ClausalForm).


%   pass_two_pos/2 does two things.  First, if there was only one clause,
%   pass_two_pos/4 wouldn'writeTranslation have wrapped it up in a list.  This we do here.
%   Second, if one of the clauses is "false", we return that as the only
%   clause.  This would be the place to apply A & A --> A.

pass_two_pos(clause(P,N), [clause(P,N)]) :- !.
pass_two_pos(Sentence, [clause([],[])]) :-
	Sentence contains clause([],[]),
	!.
pass_two_pos(Sentence, Sentence).


pass_two_pos(all(Var,B), Translation, Univ, Rename) :- !,
	pass_two_pos(B, Translation, [New|Univ], var(Var,New,Rename)).
pass_two_pos(exists(Var,B), Translation, Univ, Rename) :- !,
	gensym('f-', SkolemFunction),
	SkolemTerm =.. [SkolemFunction|Univ],
	pass_two_pos(B, Translation, Univ, var(Var,SkolemTerm,Rename)).
pass_two_pos(A and B, Translation, Univ, Rename) :- !,
	pass_two_pos(A, C, Univ, Rename),
	pass_two_pos(B, D, Univ, Rename),
	sent_and(C, D, Translation).
pass_two_pos(A or B, Translation, Univ, Rename) :- !,
	pass_two_pos(A, C, Univ, Rename),
	pass_two_pos(B, D, Univ, Rename),
	sent_or(C, D, Translation).
pass_two_pos(not A, Translation, Univ, Rename) :- !,
	pass_two_neg(A, Translation, Univ, Rename).
pass_two_pos(true, [], _, _) :- !.
pass_two_pos(false, clause([],[]), _, _) :- !.
pass_two_pos(Atom, clause([Renamed],[]), _, Rename) :-
	%   An Atom is "anything else", hence the cuts above.
	term_two(Atom, Renamed, Rename).


pass_two_neg(all(Var,B), Translation, Univ, Rename) :- !,
	gensym('g-', SkolemFunction),
	SkolemTerm =.. [SkolemFunction|Univ],
	pass_two_neg(B, Translation, Univ, var(Var,SkolemTerm,Rename)).
pass_two_neg(exists(Var,B), Translation, Univ, Rename) :- !,
	pass_two_neg(B, Translation, [New|Univ], var(Var,New,Rename)).
pass_two_neg(A and B, Translation, Univ, Rename) :- !,
	pass_two_neg(A, C, Univ, Rename),
	pass_two_neg(B, D, Univ, Rename),
	sent_or(C, D, Translation).
pass_two_neg(A or B, Translation, Univ, Rename) :- !,
	pass_two_neg(A, C, Univ, Rename),
	pass_two_neg(B, D, Univ, Rename),
	sent_and(C, D, Translation).
pass_two_neg(not A, Translation, Univ, Rename) :- !,
	pass_two_pos(A, Translation, Univ, Rename).
pass_two_neg(true, clause([],[]), _, _) :- !.
pass_two_neg(false, [], _, _) :- !.
pass_two_neg(Atom, clause([],[Renamed]), _, Rename) :-
	%   An Atom is "anything else", hence the cuts above.
	term_two(Atom, Renamed, Rename).



term_two(OldTerm, NewTerm, Rename) :-
	nonvar(OldTerm),
	functor(OldTerm, FunctionSymbol, Arity),
	functor(NewTerm, FunctionSymbol, Arity),
	!,
	term_two(Arity, OldTerm, NewTerm, Rename).
term_two(OldVar, NewTerm, Rename) :-
	term_var(Rename, OldVar, NewTerm).


term_var(var(Old,New,_), Var, New) :-
	Old == Var,
	!.
term_var(var(_,_,Rest), Var, New) :-
	term_var(Rest, Var, New).


term_two(0, _, _, _) :- !.
term_two(N, OldTerm, NewTerm, Rename) :-
	arg(N, OldTerm, OldArg),
	term_two(OldArg, NewArg, Rename),
	arg(N, NewTerm, NewArg),
	M is N-1, !,
	term_two(M, OldTerm, NewTerm, Rename).


/*----------------------------------------------------------------------

	sent_and(S1, S2, "S1 and S2")
	sent_or(S1, S2, "S1 or S2")
    perform the indicated logical operations on clauses or sets of
    clauses (sentences), using a fair bit of propositional reasoning
    (hence our use of "literally" to avoid binding variables) to try
    to keep the results simple.  There are several rules concerning
    conjunction which are *not* applied, but even checking for
	A and A --> A
    would require us to recognise alphabetic variants of A rather
    than literal identity.  So far the naivety abount conjunction
    has not proved to be a practical problem.

----------------------------------------------------------------------*/

sent_or(clause(P1,_), clause(_,N2), []) :-
	P1 contains Atom,
	N2 literally_contains Atom,
	!.
sent_or(clause(_,N1), clause(P2,_), []) :-
	N1 contains Atom,
	P2 literally_contains Atom,
	!.
sent_or(clause(P1,N1), clause(P2,N2), clause(P3,N3)) :- !,
	ord_union(P1, P2, P3),
	ord_union(N1, N2, N3).
sent_or([], _, []) :- !.
sent_or(_, [], []) :- !.
sent_or([Clause|Clauses], Sentence, Answer) :- !,
	sent_or(Sentence, Clause, X),
	sent_or(Clauses, Sentence, Y),
	sent_and(X, Y, Answer).
sent_or(Sentence, [Clause|Clauses], Answer) :- !,
	sent_or(Sentence, Clause, X),
	sent_or(Clauses, Sentence, Y),
	sent_and(X, Y, Answer).


sent_and([], Sentence, Sentence) :- !.
sent_and(Sentence, [], Sentence) :- !.
sent_and([H1|T1], [H2|T2], [H1,H2|T3]) :- !,
	sent_and(T1, T2, T3).
sent_and([H1|T1], Clause, [Clause,H1|T1]) :- !.
sent_and(Clause, [H2|T2], [Clause,H2|T2]) :- !.
sent_and(Clause1, Clause2, [Clause1,Clause2]).


[Head|_] contains Head.
[_|Tail] contains Something :-
	Tail contains Something.


[Head|_] literally_contains Something :-
	Head == Something,
	!.
[_|Tail] literally_contains Something :-
	Tail literally_contains Something.


[] does_not_literally_contain Anything.
[Head|Tail] does_not_literally_contain Something :-
	Head \== Something,
	Tail does_not_literally_contain Something.


