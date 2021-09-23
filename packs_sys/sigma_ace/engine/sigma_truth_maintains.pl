
:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

:-include('sigma_header.pl').

% ===================================================================
% SIGMA TM (Surface)
% ===================================================================
	 
% Assertion Time Errors

% Contradiction: The assertion contradicts other assertion(s) in the knowledge base. ; RAP note: this should be followed by a proof as per the XML element definition for "proof" 
% (done/implied) Syntax error: Illegal character in assertion 
% (done) Syntax error: Unmatched parentheses in assertion 
% (done)  Syntax error: Missing parentheses in assertion 
% (done/implied) Syntax error: Unspecified 
% Argument type violation ; RAP note: this should be followed by a proof of the type violation as per the XML element definition for "proof" 
% Out of memory error 
% (in Java) Broken socket: The connection between the web-based GUI and the inference engine is broken 
% (done to test)  Redundant assertion: ; RAP note: this should be followed by a proof of the type violation as per the XML element definition for "proof" 
% (done to test)  Undefined constant: Do you wish to add the constants to the KB? ; RAP note: this should be followed by a list of constants and a prompt to the user 

/* 
% KB/Ctx Must Be loaded
getTruthCheckResults(Action,Driver,surface,Formula,Ctx,TN,SKB,Vars,Author,notice(' You need to load the KB. Would you like to do so now?',not(isKnowledgeBaseLoaded(SKB,Ctx)))):-
		not(isKnowledgeBaseLoaded(SKB,Ctx)),!.
*/

% ===================================================================
% SIGMA Consitancy checking
% ===================================================================

% This next line disables truth checks on everything 
getTruthCheckResults(AskTell,Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,on):-!.

% Normalize the Forms and make appropiate checks
getTruthCheckResults(tell,Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,Result):-
	unnumbervars((Surface,CAN,Flags,Ctx,TN,KB,Vars,Author),(USurface,UCAN,UFlags,UCtx,UTN,UKB,UVars,UAuthor)),
	sigma_numbervars((USurface,UCAN,UFlags,UCtx,UTN,UKB,UVars,UAuthor),0,_),
	getTruthCheckResults_tell(Driver,USurface,UCAN,UFlags,UCtx,UTN,UKB,UVars,UAuthor,Result),!.

getTruthCheckResults(Ask,Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,Result):- !,
	unnumbervars((Surface,CAN,Flags,Ctx,TN,KB,Vars,Author),(USurface,UCAN,UFlags,UCtx,UTN,UKB,UVars,UAuthor)),
	sigma_numbervars((USurface,UCAN,UFlags,UCtx,UTN,UKB,UVars,UAuthor),0,_),
	getTruthCheckResults_ask(Driver,USurface,UCAN,UFlags,UCtx,UTN,UKB,UVars,UAuthor,Result),!.

% ===================================================================
% SIGMA Consitancy For Assert/Tell
% ===================================================================

% This next line disables truth checks on Tell
getTruthCheckResults_tell(Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,on):-!.

% Allow Trusted Driver
getTruthCheckResults_tell(Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,on):-memberchk(trusted,Driver),!.

% Surface is Redundant and On?
getTruthCheckResults_tell(Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,notice(' Redundant assertion <pre>~s</pre> original author was ~w.\n',[AuthorForm,PAuthor])):-
	sigmaCache(_,surface,Surface,_,KB,Ctx,PTN,PAuthor,_),!,flag(indent,_,0),flag(proof_linenumber,_,0),
	isSigmaOption(client=E),
	toMarkUp(E,surf(SKB,TN),OldVars,AuthorForm),!.
	
% Surface Contants must forall be declared
getTruthCheckResults_tell(Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,Result):-
	isSigmaOption(opt_infer_domains=off),       
		once(getConstants(atomic,Surface,UsedConstants,_,_)),	
		getTruthCheckResults_constants(Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,Result),!.
		
getTruthCheckResults_constants(Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,
	notice('Undefined constant: ~w\nTry asserting the as "(instance -Word- -Something-)"',[UnDefinedList])):-
		logOnFailure(check_all_constants_have_type(Formula,Constants,UnDefinedList)),
		UnDefinedList=[_|_],!. %TODO

% Surface Contants must forall be declared
getTruthCheckResults_tell(Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,Result):-
	isSigmaOption(opt_infer_domains=off),       
		once(getConstants(atomic,Surface,UsedConstants,_,_)),	
		getTruthCheckResults_constants(Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,Result),!.


% Each predicate has adequate nth-domains
/* TDODO
getTruthCheckResults(Action,Driver,surface,Formula,Ctx,TN,SKB,Vars,Author,notice(' Relation missing Domian constaints',R)):-
		Formula=..[Relation|ARGS],
		relation_missing_nth_domains_l([Relation|ARGS],R),!.
*/		
relation_missing_nth_domains(Formula,domain(R,Missing,_)):-
		Formula=..[R|ARGS],
		length(ARGS,L),
		is_nth_domain_missing(Relation,L,Missing),!.
relation_missing_nth_domains(Formula,nv(MissingList)):-
		Formula=..[_|ARGS],
		relation_missing_nth_domains_l(ARGS,MissingList).

relation_missing_nth_domains_l([],[]):-!.
relation_missing_nth_domains_l([A|RGS],[Missing|List]):-
		relation_missing_nth_domains(A,Missing),
		relation_missing_nth_domains_l(RGS,List).

% Nth Domains

getTruthCheckResults(Action,Driver,surface,Formula,Ctx,TN,SKB,Vars,Author,notice('Nth-domain violations ~w.\n',[BadList])):-	 
       Formula =.. [V|Ector],
       logOnFailure(once(nth_domain_check_surface_expression(V,1,Ector,BadList))),memberchk(and(_,_),BadList).

% Relation on Head
getTruthCheckResults(Action,Driver,surface,Formula,Ctx,TN,SKB,Vars,Author,notice('Clause heads must be relations. "~w"\n',['instance'(V,AC)])):-
       Formula =.. [V|Ector],not(arg_meets_class_contraint(V,VS,'Relation')),!.
       	

getTruthCheckResults(Action,Driver,surface,Formula,Ctx,TN,SKB,Vars,Author,accept('Passed Checks')):-!.


nth_domain_check_surface_expression(V,N,[],[]):-!.
nth_domain_check_surface_expression(V,N,[E|Ctor],[B|AdList]):-
		nth_domain_each_arg(V,N,E,B),
		NN is N + 1,
		nth_domain_check_surface_expression(V,NN,Ctor,AdList).
				
nth_domain_each_arg(V,N,E,null):-isSlot(E),!.

nth_domain_each_arg(P,N,E,(and('instance'(E,EC),'domain'(P,N,PC)))):-
	is_nth_domain_of(P,N,PC),
	not(arg_meets_class_contraint(E,EC,PC)).

nth_domain_each_arg(P,N,E,(and('instance'(E,EC),'domainSubclass'(P,N,PC)))):-
	is_nth_domain_of(P,N,PC),
	not(arg_meets_class_contraint(E,EC,PC)).

nth_domain_each_arg(V,N,E,null):-!.

	
% (not A:-u(A, holds('Class-Class', 'subclass', B, C), 'Formula'), not holds('Entity-Class', 'instance', D, C), holds('Entity-Class', 'instance', D, B))

        
check_all_constants_have_type(Formula,[],[]):-!.

check_all_constants_have_type('instance'(C,H),[TC|List],UnDefinedList):-C==TC,!, 
		check_all_constants_have_type('instance'(C,H),List,UnDefinedList).

check_all_constants_have_type('subclass'(C,H),[TC|List],UnDefinedList):-C==TC,!,
		check_all_constants_have_type('subclass'(C,H),List,UnDefinedList).

check_all_constants_have_type('subAttribute'(C,H),[TC|List],UnDefinedList):-C==TC,!,
		check_all_constants_have_type('subAttribute'(C,H),List,UnDefinedList).

check_all_constants_have_type('subcontext'(C,H),[TC|List],UnDefinedList):-C==TC,!,
		check_all_constants_have_type('subcontext'(C,H),List,UnDefinedList).

check_all_constants_have_type('subrelation'(C,H),[TC|List],UnDefinedList):-C==TC,!,
		check_all_constants_have_type('subrelation'(C,H),List,UnDefinedList).
		
check_all_constants_have_type(Formula,[C|List],UnDefinedList):-
		member(C,['$VAR',exists,forall,'instance','subclass',and,=>,or,<=>,not,not,'Entity','Context','KnowledgeBase']),!,
		check_all_constants_have_type(Formula,List,UnDefinedList).

check_all_constants_have_type(Formula,[C|List],UnDefinedList):-number(C),!, % Numbers
		check_all_constants_have_type(Formula,List,UnDefinedList).

check_all_constants_have_type(Formula,[C|List],UnDefinedList):-catch(atom_codes(C,[34|Codes]),_,fail),!, %Strings
		check_all_constants_have_type(Formula,List,UnDefinedList).

check_all_constants_have_type(Formula,[C|List],UnDefinedList):-
		in_cache('instance'(C,_),SKB,SCtx,O),!,
		check_all_constants_have_type(Formula,List,UnDefinedList).

check_all_constants_have_type(Formula,[C|List],UnDefinedList):-
		in_cache('subclass'(C,_),SKB,SCtx,O),!,
		check_all_constants_have_type(Formula,List,UnDefinedList).

check_all_constants_have_type(Formula,[C|List],UnDefinedList):-
		in_cache('subrelation'(C,_),SKB,SCtx,O),!,
		check_all_constants_have_type(Formula,List,UnDefinedList).

check_all_constants_have_type(Formula,[C|List],UnDefinedList):-
		in_cache('subAttribute'(C,_),SKB,SCtx,O),!,
		check_all_constants_have_type(Formula,List,UnDefinedList).

check_all_constants_have_type(Formula,[C|List],[C|UnDefinedList]):-!,
		check_all_constants_have_type(Formula,List,UnDefinedList).
		



% TODO - NTH-DOMAIN CHECK


% TODO - CONTRADICTION CHECK

% Surface Is accepted becasue none of the above cauth a rejection
getTruthCheckResults(Action,Driver,Form,Formula,Ctx,TN,SKB,Vars,Author,accept('Accepted')):-!.	
	
	% ask_pclause_proc(inconsistent(CL),SCtx,SKB,QM,Vars,Result,Proof),!,
        %  ( (Result > 0) -> ( sendNote('<A Color="red">(Retraction for Hypothetcal)</A>'),assert(tq_skipped),nl,nl,sendNote(user,truthConsistency,'Retract: '(not(CL)),['truthConsistency Says Veto this Assertion ',Proof]),nl,nl); true),
	 
% ===================================================================
% SIGMA TM (Canonical Form)
% ===================================================================
	 
% TN Must be Bound
getTruthCheckResults_can(Driver,wfs,CAN,Ctx,TN,KB,Vars,Author,notice(' Prolog Code Error: You Sent a Tracking Number?',TN)):-isSlot(TN),!.

% Surface Contants must forall be declared
getTruthCheckResults_can(Driver,wfs,CAN,Ctx,TN,KB,Vars,Author,notice('Currently Unused Constant in Inference',Const)):-	 
		getConstants(atomic,CAN,UsedConstants,_,_),
		'not-implemented'(Const),member(Const,UsedConstants),!.


% Normalize the Wfs to check Redudant
getTruthCheckResults_can(Driver,wfs,CAN,Ctx,TN,KB,Vars,Author,R):-
	catch(unnumbervars(CAN,RFormula),_,fail),sigma_numbervars(RFormula,0,_),
	getTruthCheckResults_can_redundant(CAN,Action,Driver,wfs,RFormula,Ctx,TN,KB,Vars,Author,R),!.

% Wfs is Redundant and On?
getTruthCheckResults_can_redundant(CAN,Action,Driver,wfs,RFormula,Ctx,TN,KB,Vars,Author,notice(' Redundant assertion',author(PAuthor,STN))):-
	sigmaCache(PredR,Form,RFormula,Rvars,KB,Ctx,STN,PAuthor,on),!.
	
% Wfs is Redundant and Disabled?
getTruthCheckResults_can_redundant(CAN,Action,Driver,wfs,RFormula,Ctx,TN,KB,Vars,Author,notice(' Redundant assertion (and Disabled)',author(PAuthor,OFF,STN))):-
	sigmaCache(PredR,Form,RFormula,Rvars,KB,Ctx,STN,PAuthor,rejected),!.
	 
% =============================================================================
% HEAD BODY CHECK FOR PROLOG
% =============================================================================

% Clause is ground (Fine)
getTruthCheckResults_can(Driver,Form,CAN,Ctx,TN,KB,Vars,Author,accept('Ground Fact')):-
		getPrologVars(CAN,[],_,_),!.

% No Singles (Fine)
getTruthCheckResults_can(Driver,Form,CAN,Ctx,TN,KB,Vars,Author,accept('Complete')):-
		getPrologVars(CAN,_,[],_),!.

% Head is ground (Fine)
getTruthCheckResults_can(Driver,Form,entails(ANT,CAN) ,Ctx,TN,KB,Vars,Author,warn(Warning)):-
		once(getPrologVars(CAN,[],_,_)),
		once(getPrologVars(CAN,ANT,_,_)),
		O=' ', %toMarkUp(kif,ANT,Vars,O),
		fmtString(S,'Creates ground fact from ante variables ~w (<font color=green>Warning</font>)',[O]),
		string_to_atom(S,Warning),!.
		

% Detect 3 Singletons in Head or Gaf (<font color=red>rejected</font>)
getTruthCheckResults_can(Driver,Form,entails(true,CAN),Ctx,TN,KB,Vars,Author,notice(Warning,'rejected')):-
		once(getPrologVars(CAN,_,[A,B,C|D],_)),
		O=' ', %toMarkUp(kif,[A,B,C|D],Vars,O),
		fmtString(S,'3 or more Universal Variables In Head on Canonicalization ~w (<font color=red>rejected</font>)',[O]),
		string_to_atom(S,Warning),!.

% Detect 2 Singletons in Head or Gaf (<font color=green>warning</font>)
getTruthCheckResults_can(Driver,Form,entails(true,CAN),Ctx,TN,KB,Vars,Author,warn(Warning)):-not(memberchk(test_question,Driver)),
		once(getPrologVars(CAN,_,[A,B|D],_)),
		O=' ', %toMarkUp(kif,[A,B|D],Vars,O),
		fmtString(S,'2 Universal Variables In Head on Canonicalization ~w (<font color=red>rejected</font>)',[O]),
		string_to_atom(S,Warning),!.

% Detect 3 Singletons in Clause (<font color=red>rejected</font>)
getTruthCheckResults_can(Driver,Form,CAN,Ctx,TN,KB,Vars,Author,notice(Warning,'rejected')):-
		once(getPrologVars(CAN,_,[A,B,C|D],_)),
		O=' ', %toMarkUp(kif,[A,B,C|D],Vars,O),
		fmtString(S,'3 or more Universal Variables In Clause on Canonicalization ~w (<font color=red>rejected</font>)',[O]),
		string_to_atom(S,Warning),!.

% Detect No Overlap  (<font color=red>rejected</font>)
getTruthCheckResults_can(Driver,Form,entails(B,CAN),Ctx,TN,KB,Vars,Author,notice('No connection between Head and Body Variables (<font color=red>rejected</font>)','rejected')):-
		once(getPrologVars(CAN,Avars,_,_)),
		once(getPrologVars(B,[BV|BVars],_,_)),
		intersection(Avars,[BV|BVars],[]),!.

% Detect 1 Singleton in Head or Gaf (<font color=green>warning</font>)
getTruthCheckResults_can(Driver,Form,entails(true,CAN),Ctx,TN,KB,Vars,Author,warn(Warning)):-not(memberchk(test_question,Driver)),
		once(getPrologVars(CAN,_,[A|B],_)),
		O=' ', %toMarkUp(kif,[A|B],Vars,O),
		fmtString(S,'Universal Variables In Head on Canonicalization ~w (<font color=red>warning</font>)',[O]),
		string_to_atom(S,Warning),!.
		
% Detect 1 Singleton in Head or Gaf (<font color=green>warning</font>)
getTruthCheckResults_can(Driver,Form,CAN,Ctx,TN,KB,Vars,Author,warn(Warning)):- not(memberchk(test_question,Driver)),
		once(getPrologVars(CAN,_,[A|B],_)),
		O=' ', %toMarkUp(kif,[A|B],Vars,O),
		fmtString(S,'Universal Variables In Canonicalization ~w (<font color=red>warning</font>)',[O]),
		string_to_atom(S,Warning),!.

getTruthCheckResults_can(Driver,Form,CAN,Ctx,TN,KB,Vars,Author,accept('Accepted')):-!.


% TODO Turn on/off

% =========================================
% Limits for Compiler
% =========================================
%clean_clauses((CAN,B),BB):-found_in(CAN,B),!,clean_clauses(B,BB).
clean_clauses((CAN,B),BB):-non_used_clause(CAN),!,clean_clauses(B,BB).
clean_clauses((CAN,B),(CAN,BB)):-!,clean_clauses(CAN,CAN),clean_clauses(B,BB).
clean_clauses((equal(X,Y) :- CAN),(same(X,Y) :- CAN)):-!.
clean_clauses((CAN , B),BB):-non_used_clause(CAN),!,clean_clauses(B,BB).
clean_clauses((CAN , B),(CAN , BB)):-!,clean_clauses(CAN,CAN),clean_clauses(B,BB).
clean_clauses(CAN,true):-non_used_clause(CAN).
clean_clauses(CAN,CAN).

non_used_clause(end_of_file).
non_used_clause((not(equal(_,_)) :- _ )).
non_used_clause((not(same(_,_)) :- _ )).
non_used_clause((_ :- not(equal(_,_))  )).
non_used_clause(true).
non_used_clause(nop_ok).
non_used_clause(surf).
%non_used_clause((equal(_,_):-_)).
%non_used_clause((not(equal(_,_)):-_)).
non_used_clause((not('domain-check'(_,_,_)):-_)).
found_in(CAN,B):-CAN==B,!.
found_in(CAN,(B , BB)):- !,
      found_in(CAN,B),
      found_in(CAN,BB).



/*
sigma_assert_can_rule_phase2(Consq,ProtoConsq,AnteListS,AnteProto,Vars,KB,Ctx,Proof):-
		contridictory([+Consq|AnteListS]),!,
		sigma_assert_can_rule_phase3(contridictory,Consq,ProtoConsq,AnteProto,AnteListS,Vars,KB,Ctx,Proof).

sigma_assert_can_rule_phase2(Consq,ProtoConsq,AnteProto,AnteListS,Vars,KB,Ctx,Proof):-
		contridictory([-Consq|AnteListS]),!,
		sigma_assert_can_rule_phase3(contridictory,Consq,ProtoConsq,AnteProto,AnteListS,Vars,KB,Ctx,Proof).

contridictory(List):-
		findall(Mem,member(+Mem,List),Pos),
		findall(Mem,member(-Mem,List),Negs),!,
		intersection(Pos,Negs,[_|_]).

sigma_assert_can_rule_phase2(Consq,ProtoConsq,AnteListS,AnteProto,Vars,KB,Ctx,Proof):-
		sigmaCache(Consq,PBefore,AnteListS,PAfter,PCost,KB,PCtx,PProof),
		intersection(PAnteListS,AnteListS,[_|_]),
		is_semantic_duplication(Consq,ProtoConsq,AnteProto,AnteListS,Vars,KB,Ctx,Proof,PAnteListS,PBefore,PAfter,PCost,KB,PCtx,PProof),!.
		
% Logically entails the same things
is_semantic_duplication(Consq,ProtoConsq,AnteProto,AnteListS,Vars,KB,Ctx,Proof,PAnteListS,PBefore,PAfter,PCost,KB,PCtx,PProof):-
		subset(PAnteListS,AnteListS),!,
		sigma_assert_can_rule_phase3(slower(Consq,PProof),ProtoConsq,AnteProto,AnteListS,Vars,KB,Ctx,Proof),!.

is_semantic_duplication(Consq,ProtoConsq,AnteProto,AnteListS,Vars,KB,Ctx,Proof,PAnteListS,PBefore,PAfter,PCost,KB,PCtx,PProof):-
		subset(AnteListS,PAnteListS),!,
		sigma_assert_can_rule_phase3(faster(Consq,PProof),ProtoConsq,AnteProto,AnteListS,Vars,KB,Ctx,Proof),!.

*/








