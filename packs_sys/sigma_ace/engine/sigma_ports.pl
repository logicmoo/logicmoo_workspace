% fguard(Template,RH,NVars,Proof,Functor)
fguard(RH,NVars,surf(KB,TN,CID,[]),Functor):-!.
	
      %  not(recorded(Functor,Template,FRef)),
	%unify_with_occurs_check(Template,RH).


%guard(instance,instance(v(_G238, _G238, _G240), v('Abstract', _G238, ['Class'|_G247])):instance(v(_G238, _G238, _G240), v('Abstract', _G238, ['Class'|_G247])), [_G238, _G238, _G240, _G238, _G247]:[_G238, _G238, _G240, _G238, _G247], (subclass(v('Abstract', _G238, ['Class'|_G327]), v('Abstract', _G238, ['Class'|_G247])), instance(v(_G238, _G238, _G240), v('Abstract', _G238, ['Class'|_G327]))): (subclass(v('Abstract', _G238, ['Class'|_G361]), v('Abstract', _G238, ['Class'|_G247])), instance(v(_G238, _G238, _G240), v('Abstract', _G238, ['C
/*
guard(instance, 
instance(v(_G235, _G236, _G237), v('Abstract', _G240, ['Class'|_G244])):instance(v(_G266, _G267, _G268), v('Abstract', _G271, ['Class'|_G275])),
 [_G235, _G236, _G237, _G240, _G244]:[_G266, _G267, _G268, _G271, _G275], 
 (subclass(v('Abstract', _G317, ['Class'|_G321]), v('Abstract', _G271, ['Class'|_G275])),instance(v(_G266, _G267, _G268), v('Abstract', _G317, ['Class'|_G321]))), 
  188, 3, ['SUBCLASS'=_G317, 'CLASS'=_G271, 'INST'=_G267], [_G267, _G271, _G317], [], [], [_G317], [_G267, _G271])
*/
			   
					 /*
guard(Functor,RealHead:Head,RFVH:FVH,Body,TN,CID,KRVars,RuleVars,UnivHead,BodyUniv,BodySelfConnected,RealShared):- 
	unify_with_occurs_check(RFVH,FVH), %unifies list of 'real' prolog variables
	recorded(TN,KRVars,Ref),!,
	writeDebugS(bumping(KRVars)),!,
	catch(exit(Ref,cut),_,fail).
	%(catch(fail(Ref),_,(writeDebugS(missing(Copy)),fail)),fail),write(bollk),nl.
%	catch(exit(Ref,previousCallMoreSpecific(KRVars,Copy)),_,fail),fail.
			     		   */
					   
'$existential'(v(_,V,_),A,F):-ignore(V=F).


guard(Functor,RealHead:Head,RFVH:FVH,Body,TN,CID,KRVars,RuleVars,UnivHead,BodyUniv,BodySelfConnected,RealShared):-!,


%        not(Functor=not(_)),
        not(recorded(TN,KRVars,Ref)),
 	%ground(UnivHead), % Makes sure this is valid
	unify_with_occurs_check(RFVH,FVH), %unifies list of 'real' prolog variables
	copy_term(KRVars,Session),
	sigma_numbervars(Session,0,_),
	recorda(TN,Session,Ref),
     %   recorda(Functor,RealHead,FRef),
	%stepQualifier(Ref,Functor,RealHead:Head,RFVH:FVH,Body,TN,CID,KRVars,RuleVars,UnivHead,BodyUniv,BodySelfConnected,RealShared,Qualifier),
%findall(Body,call_with_depth_limit(block(Ref,callBody(Body,true),Result),47,_),Sols),!,	
	findall(Body,call_with_depth_limit(Body,47,_),Sols),!,
	%findall(Body,Body,Sols),!,
	sort(Sols,SolsS),
	member(Body,SolsS).
	
	
	%(Result=cut -> !,),
	%catch(erase(Ref),_,writeDebugS(missingRef(Vars))).
%	processResult(Result,TN,CID,F,KRVars,RB,Session,Ref),!,
%	ground(RealShared).
	
%processResult(Result,TN,CID,F,Vars,RB,Session,Ref):-var(Result),!, % normal completion
	
%processResult(previousCallMoreSpecific(Vars,Copy),TN,CID,F,KRVars,RB,Session,Ref):- % decendant aborted to here
	%catch(erase(Ref),_,writeDebugS(missingRefInpreviousCallMoreSpecific(KRVars))),
 %       writeDebugS(previousCallMoreSpecific(KRVars,Vars,Copy)). %,!,fail.
	


stepQualifier(Ref,Functor,RealHead:Head,RFVH:FVH,Body,TN,CID,
	KRVars,RuleVars,UnivHead,[BodY|Univ],BodySelfConnected,RealShared,(ground(BodY),exit(Ref,bodyUnivBound))).
	

stepQualifier(Ref,Functor,RealHead:Head,RFVH:FVH,Body,TN,CID,
	KRVars,RuleVars,UnivHead,BodYUniv,BodySelfConnected,RealShared,true).

%callBody(Body,true):-!,Body.
callBody((A,B),Qualifier):-
	callBody(A,Qualifier),Qualifier,
	callBody(B,Qualifier).
callBody(Body,Qualifier):-not(functor(Body,function,_)),Body.
	
unguard(TN,F,Vars,Session):-
	recorded(TN,Session,Ref),!,erase(Ref),!.
	
unguard(TN,F,Vars,Session):-
	writeDebugS(somethingKilled(Session)).


sigmaCall(X):-
	sigmaCache(X, Cost,KB, Ctx,surf(KB,TN,CID,[])).

sigmaCall(X):-
	sigmaCache(X, Cost,KB, Ctx,surf(KB,TN,CID,[])).
		

sigmaCall(Flags,KB):-
	sigmaCache(Cons, Ante,Vars,KB, Ctx,TN).

guard(Functor,RealHead:Head,RFVH:FVH,Body,TN,CID,KRVars,RuleVars,UnivHead,BodyUniv,BodySelfConnected,RealShared):-!,
%        not(Functor=not(_)),
        not(recorded(TN,KRVars,Ref)),
 	%ground(UnivHead), % Makes sure this is valid
	unify_with_occurs_check(RFVH,FVH), %unifies list of 'real' prolog variables
	copy_term(KRVars,Session),
	sigma_numbervars(Session,0,_),
	recorda(TN,Session,Ref),
     %   recorda(Functor,RealHead,FRef),
	%stepQualifier(Ref,Functor,RealHead:Head,RFVH:FVH,Body,TN,CID,KRVars,RuleVars,UnivHead,BodyUniv,BodySelfConnected,RealShared,Qualifier),
%findall(Body,call_with_depth_limit(block(Ref,callBody(Body,true),Result),47,_),Sols),!,	
	findall(Body,call_with_depth_limit(Body,47,_),Sols),!,
	%findall(Body,Body,Sols),!,
	sort(Sols,SolsS),
	member(Body,SolsS).

	
/*
	
	assertz((term_expansion(X,Y) :- catch((!,getTermExpansionLogged(X,Y)),E,fail))),
	told.
*/
    

	

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).


ca:-compile_show('Merge',instance,2,Debug).
	
cf:- compile_to_file(instance,2,'Merge').


va:- compile_show('Merge',valence,2,Debug).

ensure_all_compiled:-!.

ensure_all_compiled:-
	getAllSigmaKB(X),
	compileKB(X),fail.
	

getAllSigmaKB(X):-fail.

compileKB(KB):-!.
compileKB(KB):-
	compileInstanceSubclass(KB).



make_kb(KB):-
	retractall(sigmaCache(KB,_,_)),
	atom_concat(KB,'.plrolog',PrologFile),
	tell(PrologFile),
	format('

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

	'),
	image_to_prolog(KB),!,
	told,
	save_make_kb(PrologFile,KB,FeatureFile),
	atom_concat(KB,'.pll',OutputFile),
	concat_atom([cat,FeatureFile,PrologFile,'>',OutputFile],' ',Cmd),
	format('~n~w~n',[Cmd]). 
	

save_make_kb(PrologFile,KB,FeatureFile):-
	atom_concat(KB,'.feature',FeatureFile),
	tell(FeatureFile),
	format('

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

	'),
	save_features_kb(KB),
	told.
	
save_features_kb(KB):-
	format('~n~n% Predicates~n~n'),
	sigmaCache(KB,type(dynamic),Data),
	format(':-~q.~n',[dynamic(Data)]),fail.

save_features_kb(KB):-
	format('~n~n% Predicates~n~n'),
	sigmaCache(KB,type(dynamic),Data),
	format(':-~q.~n',[tabled(Data)]),fail.
	
/*
save_features_kb(KB):-
	format('~n~n% Tables~n~n'),
	sigmaCache(KB,type(tabled),Data),
	format(':-~q.~n',[tabled(Data)]),fail.
  */
  
/*
save_features_kb(KB):-
	format('~n~n% Not Tabled ~n~n'),
	sigmaCache(KB,type(dynamic),Data),
	not(sigmaCache(KB,type(tabled),Data)),
	format(':-~q.~n',[prolog(Data)]),fail.
*/

save_features_kb(KB):-
	format('~n~n% Rules~n~n'),
	sigmaCache(KB,type(rule),Data),
	format('~q.~n',[rules_for(Data)]),fail.

save_features_kb(KB):-
	format('~n~n% No Rules For~n~n'),
	sigmaCache(KB,type(dynamic),Data),
	not(sigmaCache(KB,type(rule),Data)),
	format('~q.~n',[no_rules_for(Data)]),fail.

save_features_kb(KB):-
	format('~n~n% Facts~n~n'),
	sigmaCache(KB,type(fact),Data),
	format('~q.~n',[facts_for(Data)]),fail.

save_features_kb(KB):-
	format('~n~n% No Facts~n~n'),
	sigmaCache(KB,type(dynamic),Data),
	not(sigmaCache(KB,type(fact),Data)),
	format('~q.~n',[no_facts_for(Data)]),fail.

save_features_kb(KB):-
	format('~n~n% No Rules/Facts~n~n'),
	sigmaCache(KB,type(dynamic),Data),
	not(sigmaCache(KB,type(fact),Data)),
	not(sigmaCache(KB,type(rule),Data)),
	format('~q.~n',[no_assertions_for(Data)]),fail.

save_features_kb(KB):-
	format('~n~n% Lemma Reqs:~n~n'),
	findall((F-C),sigmaCache(KB,type(F/A),(C/N)),Edges),
	keysort(Edges,Sorted),
	format('lemma_edges(~q).~n~n',[Sorted]),!.
	

image_to_prolog:-
	tell('Merge.pll'),
	image_to_prolog(KB),
	told.	


image_to_prolog(KB):-
	hardcoded(HardCoded),
	image_to_prolog([holds,neg(lit)|HardCoded],KB).

image_to_prolog(Flags,KB):-format(
'
:- op(400,fy,~~).
%:- op(500,xfy,:).
:- op(500,xfx,#).
%:- op(500,xfx,@).

:- op(400,fy,not).    % negation
:- op(500,xfy,and).   % conjunction
:- op(600,xfy,or).   % disjunction
%:- op(500,xfy,:).
:- op(0,xfx, equal ).
:- op(900,xfx,''<='').
:- op(900,xfx,if).
:- op(400,fy,known).  % Found in Model
:- op(400,fy,possible).  % Not In Model
:- op(400,fy,next).  % Next time
:- op(400,fy,after).  % Next time
:- op(400,fy,then).  % Next time
:- op(650,xfy,=>).  % implication
:- op(700,xfy,<=>). % equivalence
:- op(400,fy,always).  % Necessity, Always
:- op(400,fy,possible).  % Possibly, Eventually
:- op(400,fy,necessary).  % Necessity

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

:-set_prolog_flag(unknown,fail).

'),fail.


	

	
/*
delayBody(TN,(B1,B2)):-
	delayBody(TN,B1),
	delayBody(TN,B2).

delayBody(TN,B2):-
	functor(B2,F,_),
	recorda(F,delayed).
*/
	
      
mcl1:-mcl(instance(X,Y)).
  
mcc(X):-
	resetTableFlags,X.
	

mcl2:-
	mcl(X,instance(v(_,X,_),v('Abstract', 'BinaryRelation',_))).
	


mcl(X):-
	resetTableFlags,
       findall(X,(X,writeq(X),nl),L),
       writeq(L),nl,
       length(L,N),
       write(num:N),nl.

mcl(X,Y):-
	resetTableFlags,
       findall(X,(Y,writeq(Y),nl),L),
       writeq(L),nl,
       length(L,N),
       write(num:N),nl.
		
	



image_to_prolog(Flags,KB):-
	sigmaCache(Cons, Cost,KB, Ctx,Proof),
		convertPrologWFS(Flags,KB,Activation,Proof,Cons,Prolog),
	writeAsProlog(Prolog),fail.
	

image_to_prolog(Flags,KB):-
	sigmaCache(Cons, Ante,Cost,KB, Ctx,Proof),
	convertPrologWFS(Flags,KB,Activation,Proof,(Cons:-Ante),Prolog),
	writeAsProlog(Prolog),fail.



image_to_prolog(Flags,KB):-!.



tabled_consult(File):-
	tell('tabling.log'),
	assertz((term_expansion(X,Y) :- catch((!,getTermExpansionLogged(X,Y)),E,fail))),
	consult(File),
	abolish(term_expansion/2),
	dynamic(term_expansion/2),
	multifile(term_expansion/2),!,
	told.

/*
tkb(KB):-
	tell('tabling.log'),
	sigmaCache(Cons, Cost,KB, Ctx,Proof),
	assertClauseTable(KB,Cons,Proof),fail.
	
tkb(KB):-
	sigmaCache(Cons, Ante,Cost,KB, Ctx,Proof),
	assertClauseTable(KB,(Cons:-Ante),Proof),fail.

tkb(KB):-told.

*/

assertClauseTable(KB,WFS,Proof):-
	convertPrologWFS(Flags,KB,assertClauseTable,Proof,WFS,Prolog),
	getTermExpansionLogged(Prolog,Tabled),
	assertAll(Tabled),!.	
		


tconsult(File):-
	tell('tabling.log'),
	see(File),
	repeat,
		read(X),
		catch((!,getTermExpansionLogged(X,Y)),E,(writeq(E),nl,fail)),
		once(assertAll(Y)),
	   X==end_of_file,
        seen,
	told.
    
assertClauseTable(KB,WFS,Proof):-
	convertPrologWFS(Flags,KB,assertClauseTable,Proof,WFS,Prolog),
	getTermExpansionLogged(Prolog,Tabled),
	assertAll(Tabled).	
		

getTermExpansionLogged(end_of_file,end_of_file):-!.
getTermExpansionLogged(X,Y):-
	slg_term_expansion(X,Y),
	log_term_expansion(X,Y).
	
log_term_expansion(X,Y):-X==Y,!,
	format('\n%no_expansion\n'),write_te_list(Y).
log_term_expansion(X,Y):-
	format('\n%~q~n',[X]),write_te_list(Y).
	
write_te_list([]):-nl,!.
write_te_list([X|T]):-!,write_te_list(X),write_te_list(Y).
write_te_list(X):-format('~q.\n',X).


convertPrologWFS(Flags,KB,Activation,surf(KB,TN,CID,KRVars),
	(C :- A),
	((
	RealHead :- 
		guard(Functor,(RealHead:Head),(RFVH:FVH),Body,TN,CID,KRVars,
		RuleVars,UnivHead,BodyUniv,BodySelfConnected,Shared)))):-!,
	functor(C,F,_),convertNegations((not),F,Functor,_),
	convertRuleHeadWFS(Flags,KB,Activation,C,Head,RuleHead),
	convertRuleBodyWFS(Flags,KB,Activation,RuleHead,A,Body),!,
	getPrologVars(KRVars,RuleVars,_,_),!,
	getPrologVars(Head,FVH,_,_),!,set_partition(RuleVars,FVH,_,_,HeadVars),
	getPrologVars(Body,FVB,BSingles,_),!,set_partition(RuleVars,FVB,_,_,BodyVars),
	set_partition(HeadVars,BodyVars,PrivHead,PrivBody,Shared),!,
	set_partition(PrivBody,BSingles,BodySelfConnected,_,BodyUniv),!,
	copy_term((Head,FVH,PrivHead),(RealHead,RFVH,UnivHead)).
	
convertPrologWFS(Flags,KB,Activation,Proof,(C),(RH:- fguard(RH,NVars,Proof,Functor))):-!,
	functor(C,F,_),convertNegations((not),F,Functor,_),
	convertFactHeadWFS(Flags,KB,Activation,C,RH),
	functor(RH,F,A),functor(Template,F,A),Template=..[F|NVars].

	
	
writeAsProlog([]):-format('\n\n'),!.	
writeAsProlog([H]):-format('\n\t~q.~n',[C]),!.
writeAsProlog(':-'(Cons, Ante)):-format('\n ~q :-',[Cons]),writeAsProlog(Ante),!.
writeAsProlog('<-'(Cons,Ante)):-format('\n ~q <-',[Cons]),writeAsProlog(Ante),!.
writeAsProlog('<--'(Cons,Ante)):-format('\n ~q <--',[Cons]),writeAsProlog(Ante),!.
writeAsProlog('::-'(Cons,Ante)):-format('\n ~q ::-',[Cons]),writeAsProlog(Ante),!.
writeAsProlog((H,T)):-format('\n\t~q,',[H]),writeAsProlog(T),!.   
writeAsProlog([H|T]):-format('\n\t~q,',[H]),writeAsProlog(T),!.
writeAsProlog(C):-format('\n\t ~q.~n',[C]),!.	

recordIfNew(Activation,KB,Cons,Type):-!.
	
recordIfNew(Activation,KB,Cons,Type):-
	atom(Type),!,
	functor(Cons,Pred,Arity),
	recordIfNewCache(KB,Type,Pred/Arity),!.
recordIfNew(Activation,KB,Cons,Type):-
	functor(Type,T,A),
	functor(Cons,Pred,Arity),
	recordIfNewCache(KB,(T/A),(Pred/Arity)).

recordIfNewCache(KB,Type,Data):-
	sigmaCache(KB,type(Type),Data),!.
recordIfNewCache(KB,Type,Data):-
	assertz(sigmaCache(KB,type(Type),Data)),!.
	
       
     
/*
     
Subclasses of Physical

        Object
                SelfConnectedObject
                Collection
                ContentBearingObject
        Process

Subclasses of Abstract

         Quantity
                Number (how many)
                PhysicalQuantity (how much)
         Attribute
         Proposition
         Class 
                Set
         Relation
                Predicate
                Function
               VariableArityRelation

   Other Disjoint CLasses like the new 'Graph' 



*/





/*
compile_to_file(SUMOPred,Arity,KB):-
	make_relation_profile(KB,SUMOPred,Logic,Arity,N,Module,SourceInfo,Functor),
	open(SourceInfo, write, Stream, [buffer(full),type(text),alias(SUMOPred)]),
	compile_show(SUMOPred,KB,SUMOPred,Arity,Debug),
	close(SUMOPred).
*/
compile_to_file(SUMOPred,Arity,KB):-
	make_relation_profile(KB,SUMOPred,Logic,Arity,N,Module,SourceInfo,Functor),!,
	open(SourceInfo, write, Stream, [buffer(full),type(text),alias(SUMOPred)]),
	compile_show(Stream,KB,SUMOPred,Arity,Debug),!,
	close(Stream),!.

compile_show(KB,SUMOPred,Arity,Debug):-
	current_output(Stream),!,
	compile_show(Stream,KB,SUMOPred,Arity,Debug),!.
	

compile_show(Stream,KB,SUMOPred,Arity,Debug):-
	ignore(Debug=no_debug),!,
	make_relation_profile(KB,SUMOPred,Logic,Arity,N,Module,SourceInfo,Functor),
	mkImported(full,Functor,Arity,Logic,Vect,Ctx,ProofIn,Proof,Imported),
	mkArgsAtom(Arity,ArgsAtom),
	mkHolds(SUMOPred,Arity,Vect,Cons),
	mkIndex(Functor,N,Index),
	Dash = (Functor/N),
	format(Stream,'
/* <PRE face="Arial,Helvetica"><font face="Arial,helvetica"><font color=green>
File: "~w"

Author:      dmiles@teknowledge.com [Douglas R. Miles]
Contacts:   dmiles@teknowledge.com, 
		apease@ks.teknowledge.com,  
		sigma-dev@ks.teknowledge.com (support)

Purpose:  Individual loading of Sigma KB Predicates.

Exported:  
	proof_line/2,  									% ProofIn of Proof Formats
	full_~w(Logic, ~w, Ctx, ProofIn, Proof),                                   % Basic Access
	bk_~w(Logic, ~w, Ctx,ProofIn, Proof),                    % Backward Chain
	fw_~w(Logic, ~w, Ctx,ProofIn,Proof),                % Forward Chain
	gaf_~w(Logic, ~w, Ctx,ProofIn,Proof),                % Forward Chain

Uses: inferTransitiveClosure_PartialOrderingRelation/6, fw_instance/5, fw/6.

Type of SourceInfo: predicate_module (generated runtime)
Module:   ~q
SUMO:   ~q
Cons:   ~q
KB:   ~q
Debug:  ~q
</font>
*/

:-module(~q, 
       [
	full_~w, 
	bk_~w,
	gaf_~w,
	fw_~w
	]).

:-include(\'sigma_multifile.pl\').

% =====================================================
% inference_module(KB,Ctx,SUMOPred,Cons,Imported,ConnectionType,SourceInfo,HowOften).
% 	KB = The KnowedgeBase
% 	Ctx = The Context 
% 	SUMOPred = The Functor\'s SUMO Name
%	Goal = Functors SUMO Prototype	 "goal(Logic,ProofIn,holds(SUMOPred,A,B),Ctx,KB,ProofOut)"
%	Imported = Functor\'s Prolog Prototype
% 	ConnectionType = \'prolog\'  meaning its compiled by consulting this file to memory.
% 	SourceInfo = The connection parameters
% 	HowOften = always,never,once
% =====================================================

:-was_indexed(inference_module(1,0,1,1,1,0,0,0)).

	
inference_module(
	~q,_AllContexts,
	~q,goal(Logic,ProofIn,holds(~q,~w),Ctx,~q,ProofOut),
	full_~w(Logic, ~w, Ctx, ProofIn, ProofOut),
	prolog, ~q,
	always).
			    

% =====================================================
% index/dynamic All exported predicates
% =====================================================

:-was_indexed(full_~w).
:-was_indexed(fw_~w).
:-was_indexed(bk_~w).
:-was_indexed(gaf_~w).

:-dynamic(full_~w).
:-dynamic(fw_~w).
:-dynamic(bk_~w).
:-dynamic(gaf_~w).

',     [SourceInfo, % Comment SourceInfoname
	Functor,ArgsAtom,  % Comment full_Functor 
	Functor,ArgsAtom,  % Comment bk_Functor 
	Functor,ArgsAtom,  % Comment fw_Functor
	Functor,ArgsAtom,  % Comment gaf_Functor
	Module,SUMOPred,Cons,KB,Debug, % Comments 
	Module,Dash,Dash,Dash,Dash,  % module/2
	KB,SUMOPred,SUMOPred,ArgsAtom,KB,Functor,ArgsAtom,SourceInfo, % inference_module/5
	Index,Index,Index,Index,    % index/1   
	Dash,Dash,Dash,Dash	 % dynamic/1
	]),!,
	create_entry_points(Stream,non_singleValued,Functor,KB,SUMOPred,Arity,N,Module,Debug,ArgsAtom),!,
	make_pred_data(Stream,Functor,KB,SUMOPred,Arity,N,Module,Debug,ArgsAtom),!.
	
mkArgsAtom(Arity,ArgsAtom):-
	length(Arglist,Arity),
	sigma_numbervars(Arglist,0,_),	  
	term_to_atom(Arglist,ArgListAtom),
	atom_codes(ArgListAtom,[_|ArgListAtomCodesRight]),
	append(ArgListAtomCodes,[93],ArgListAtomCodesRight),
	atom_codes(ArgsAtom,ArgListAtomCodes).

mkHolds(SUMOPred,Arity,Vect,Cons):-
	Cons=..[holds,SUMOPred|Vect],!.
	
mkImported(full,Module,Arity,Logic,Vect,Ctx,ProofIn,Proof,Imported):-
	length(Vect,Arity),
	append([Module,Logic|Vect],[Ctx,ProofIn,ProofIn * Proof],SUMOPredList),
	Imported =.. SUMOPredList,!.
	
mkIndex(SUMOPred,Arity,Index):-
	interate_copy(Arity,IndexArgs),
	Index=..[SUMOPred|IndexArgs],!.

interate_copy(N,[1,1,1,1|IndexArgs]):-
	NN is N - 4,
	length(IndexArgs,NN),
	put_in_n(NN,0,IndexArgs),!.
	
put_in_n(NN,Value,[]).
put_in_n(NN,Value,[Value|L]):-put_in_n(NN,Value,L).
	

create_head(Tag,KB,SUMOPred,Logic,Args,Head):-
	concat_atom([Tag,KB,'_',SUMOPred],Functor),
	Head=..[Functor|Args],!.

	
% ===============================================================
% make_relation_profile(-KB,-SUMOPred,-Logic,-Arity,+N,+Module,+SourceInfo,+Functor)
% ===============================================================
make_relation_profile(KB,SUMOPred,Logic,Arity,N,Module,SourceInfo,Functor):-!,
	ignore(KB='Merge'),
	ignore(SUMOPred='attribute'),
	ignore(Arity=2),
	is(N,(Arity + 4)),
	concat_atom([KB,'_',SUMOPred],Module),
	concat_atom([Module,Arity],Functor),
	concat_atom(['pred_', Module, '.pll' ],SourceInfo),!.


create_entry_points(Stream,non_singleValued,Functor,KB,SUMOPred,Arity,N,Module,Debug,ArgsAtom):-
format(Stream,'
% ==================================================================================
% <B>Entry Points</B> for <font color=red>non_singleValued</font>   <font size=+1 color=green>~w/~w</font>
% ==================================================================================

% gaf hook
full_~w(Logic,~w, Ctx,ProofIn,(Proof * ProofIn)):-
	gaf_~w(Logic,~w ,Ctx,ProofIn,Proof),not_in(Proof,ProofIn).

% fw hook
full_~w(Logic,~w, Ctx,ProofIn,(Proof * ProofIn)):-
	fw_~w(Logic,~w ,Ctx,ProofIn,Proof),not_in(Proof,ProofIn).

% bk hook
full_~w(Logic,~w, Ctx,ProofIn,Proof):-
	bk_~w(Logic,~w ,Ctx,ProofIn,Proof).

',     [SUMOPred,Arity, %Comment
	Functor,ArgsAtom,Functor,ArgsAtom,
	Functor,ArgsAtom,Functor,ArgsAtom,
	Functor,ArgsAtom,Functor,ArgsAtom
	]),!. % never fails

make_pred_data(Stream,Functor,KB,SUMOPred,Arity,N,Module,Debug,ArgsAtom):-
format(Stream,'
	
% ================================================================================================================
% <B>Rules/Facts</B> for <font size=+1 color=green>~w/~w</font>
% ================================================================================================================<B>

',[SUMOPred,Arity]),once((flag(rule_num,_,1),flag(proof_linenumber,_,1))),fail.

write_rulenum(Stream,Proof):-!,
	flag(rule_num,RN,RN+1),flag(proof_linenumber,_,1),!,
	format(Stream,'~n/*Id: ~q */~n',[Proof]),!.

make_head_t(true,SUMOPred,N,Cons):-
		length(Args,N),
		Cons=..[SUMOPred|Args],!.

make_head_t(false,SUMOPred,N,Cons):-
		length(Args,N),
		atom(SUMOPred),atom_concat('~',SUMOPred,SUMOPredN),
		Cons=..[SUMOPredN|Args],!.

	
getrule(SUMOPred,Arity,Cons,Precond, KB,Ctx, surf(KB,TN,CID),Vars):-
	make_head_t(true,SUMOPred,Arity,Cons),
	sigmaCache(Cons, A1,A2,A3, Cost,KB, Ctx,surf(KB,TN,CID,Vars)),
	once((append(A1,A2,AM),
	append(AM,A3,Precond))).
getrule(SUMOPred,Arity,Cons,Precond, KB,Ctx, surf(KB,TN,CID),Vars):-
	make_head_t(false,SUMOPred,Arity,Cons),
	sigmaCache(Cons, A1,A2,A3, Cost,KB, Ctx,surf(KB,TN,CID,Vars)),
	once((append(A1,A2,AM),
	append(AM,A3,Precond))).

% True GAFS       
make_pred_data(Stream,Functor,KB,SUMOPred,Arity,N,Module,Debug,ArgsAtom):-
	make_head_t(true,SUMOPred,Arity,Cons),
	sigmaCache(Cons, Precon, KB,Ctx, TID),
	write_rulenum(Stream,TID),
	sigma_numbervars((Cons, Precon, KB,Ctx, TID,Vars),15,_),	
	submit_ado_cache(Stream,SUMOPred,Cons, Precon, KB,Ctx, TID,Vars),fail.

% False GAFS       
make_pred_data(Stream,Functor,KB,SUMOPred,Arity,N,Module,Debug,ArgsAtom):-
	make_head_t(false,SUMOPred,Arity,Cons),
	sigmaCache(Cons, Precon, KB,Ctx, TID),
	write_rulenum(Stream,TID),
	sigma_numbervars((Cons, Precon, KB,Ctx, TID,Vars),15,_),	
	submit_ado_cache(Stream,SUMOPred,Cons, Precon, KB,Ctx, TID,Vars),fail.

% True then Fasle Rules
make_pred_data(Stream,Functor,KB,SUMOPred,Arity,N,Module,Debug,ArgsAtom):- %trace,
	getrule(SUMOPred,Arity,Cons,Precond, KB,Ctx, TID,Vars), %trace,
        write_rulenum(Stream,TID),
	close_list(Vars),
	sigma_numbervars((Stream,SUMOPred,Cons,Precond, KB,Ctx, TID,Vars),15,_),
	submit_ado_cache(Stream,SUMOPred,Cons,Precond, KB,Ctx, TID,Vars),fail.

       % toMarkUp(kif,DispProof,Vars,PrettyForm),
	%logOnFailure(format(Stream,'/*~n</B><font color=green>~n~nForms:~n~n~s~nFlags: ~w \n</font><B>*/',[PrettyForm,Flags])),
%	make_disp_proof(Logic,TID,Vars,Cons,Ante,DispProof),
%	format(Stream,'~q. ~n~n',[proof_line(TID,Vars,DispProof)])	 %trace, 

	
% Write footer
make_pred_data(Stream,Functor,KB,SUMOPred,Arity,N,Module,Debug,ArgsAtom):-
	getPrettyDateTime(String),
	format(Stream,'\n/* \n Last Saved: ~s</B></font>\n</PRE>*/\n\n\n',[String]),!.


%submit_ado_cache(Stream,SUMOPred,Cons,_,_,Logic, KB, Ctx, Proof * _):-trace,fail.


% No antecedents flags
submit_ado_cache(Stream,SUMOPred,Cons, [], KB,Ctx, TID,Vars):-
	Cons=..[_|Arguments],
	append(Arguments,[Ctx,TID],Args),  !,
	create_head('gaf_',KB,SUMOPred,true,Args,PrologHead),
	format(Stream,'~n~q.~n',[PrologHead]),!.

% With antecedents flags
submit_ado_cache(Stream,SUMOPred,Cons,Ante, KB,Ctx, TID,Vars):- 
	Cons=..[_|Arguments],
	sigma_numbervars(SubCtx),
	append(Arguments,[Ctx,ProofIn,[proof_line(TID,VarsRef)|ProofOut]],Args),
	create_head('bk_',KB,SUMOPred,true,Args,PrologHead),
	format(Stream,'~n ~q :- \n',[PrologHead]),!,
	%format(Stream,'\t~q,~n',[not_near_member(TID,ProofIn)]), !,
      %  findall(Neg,member(-(Neg),Ante),NegS),
	% findall(Pos,member(+(Pos),Ante),PosS),
	%format(Stream,'\t~q,~n',[minor_interception(PosS,NegS,ProofIn,ProofMid)]),
	%write_std_flags(Stream,KB,Flags,(Cons:Ante)),!,
	format(Stream,'\t~q,~n',[subcontext(Ctx,SubCtx)]),
	write_prolog_body_clause(Stream,Ante,SUMOPred,Cons,Ante,KB, SubCtx, TID,Vars,ProofIn,ProofOut),!,
	format(Stream,'\t~q.~n~n',[(ground(Vars),VarsRef=Vars)]),!.
	
write_std_flags(Stream,KB,Flags,Term):-	
	write_lllist(Stream,KB,Flags),!.
	
write_lllist(Stream,KB,[]):-!.
write_lllist(Stream,KB,[Arg|List]):-
	format(Stream,'\tk~w_~q,~n',[KB,Arg]),
	write_lllist(Stream,KB,List),!.
	
	
/*

submit_ado_cache(Stream,SUMOPred,Cons,Ante,Flags,Logic, KB, Ctx, TID * Vars):-
	write_prolog_rule(Stream,SUMOPred,Cons,Ante,Flags,Logic, KB, Ctx, TID,Vars),!.

write_prolog_rule(Stream,SUMOPred,Cons,Ante,Flags,Logic, KB, Ctx, TID,Vars):-
	format(Stream,' ~q. ~n',[proof_line(TID,Vars,DispProof)]),
	Cons=..[_|Arguments],
	append(Arguments,[Ctx,ProofIn,(ProofOut * TID,Vars)],Args),
	create_head('bachchain_',KB,SUMOPred,Logic,Args,PrologHead),
	format(Stream,' ~q :- ~n',[PrologHead]),
	write_prolog_body_start(Stream,Ante,SUMOPred,Cons,Ante,Flags,Logic, KB, Ctx, TID,Vars,ProofIn,ProofMid),!,
	write_prolog_body_clause(Stream,Ante,SUMOPred,Cons,Ante,Flags,Logic, KB, Ctx, TID,Vars,ProofMid,ProofOut),!.

*/
% 	write_prolog_body_clause(Stream,Ante,SUMOPred,Cons,Ante,Flags,Logic, KB, Ctx, TID,Vars,ProofMid,ProofOut),!.

write_prolog_body_clause(Stream,OrigAnte,SUMOPred,Cons,[], KB, Ctx, TID,Vars,ProofIn,ProofOut):-!,
	format('\t~w,~n',[ProofIn=ProofOut]).

write_prolog_body_clause(Stream,OrigAnte,SUMOPred,Cons,[Ante], KB, Ctx, TID,Vars,ProofIn,ProofOut):-!,%trace,
	write_prolog_term(Stream,OrigAnte,KB,Flags,Caller,FlagsSUMOPred,Cons,Ante,KB, Ctx, TID,Vars,ProofIn,ProofOut),!.
	
write_prolog_body_clause(Stream,OrigAnte,SUMOPred,Cons,[Ante|More], KB, Ctx, TID,Vars,ProofIn,ProofOut):-!,%trace,
	write_prolog_term(Stream,OrigAnte,KB,Flags,Caller,FlagsSUMOPred,Cons,Ante,KB, Ctx, TID,Vars,ProofIn,ProofMid),
	write_prolog_body_clause(Stream,OrigAnte,SUMOPred,Cons,More, KB, Ctx, TID,Vars,ProofMid,ProofOut),!.


	
	
check_end_flags(FlagsList,ProofIn,ProofOut):-ProofIn=ProofOut.
check_begin_flags(FlagsList,ProofIn,ProofOut):-ProofIn=ProofOut.
	
 						  
	
write_prolog_term(Stream,OrigAnte,KB,Flags,Caller,FlagsSUMOPred,Cons,Useless, KB, Ctx, TID,Vars,ProofIn,ProofIn):- useless(Useless),!.
write_prolog_term(Stream,OrigAnte,KB,Flags,Caller,FlagsSUMOPred,Cons,Ante, KB, Ctx, TID,Vars,ProofIn,ProofOut):-
	Ante=..[P|Arguments],
	append(Arguments,[Ctx,ProofIn,ProofOut],Args),
	create_head('fw_',KB,P,true,Args,PrologHead),
	format(Stream,'\t~q,~n',[PrologHead]),!.

useless(domainC(_,[])).	
useless(domainA(_,[])).	
	
	

make_disp_proof(true,surf(KB,TID),Vars,Cons,Conds,via(entails(CondsO,Cons),Vars) * surf(KB,TID)):-fix_conds(Conds,CondsO),!.
make_disp_proof(false,surf(KB,TID),Vars,Cons,Conds,via(entails(CondsO,not(Cons)),Vars) * surf(KB,TID)):-fix_conds(Conds,CondsO),!.
make_disp_proof(true,surf(KB,TID,ID),Vars,Cons,Conds,via(entails(CondsO,Cons),Vars) * surf(KB,TID,ID)):-fix_conds(Conds,CondsO),!.
make_disp_proof(false,surf(KB,TID,ID),Vars,Cons,Conds,via(entails(CondsO,not(Cons)),Vars) * surf(KB,TID,ID)):-fix_conds(Conds,CondsO),!.

fix_conds(Var,Var):-isSlot(Var),!.
fix_conds([],true):-!.
fix_conds([A],AA):-!,
	fix_conds(A,AA).
fix_conds(+Conds,Conds).
fix_conds(-Conds,not(Conds)).
fix_conds([A|B],and(AA,BB)):-!,
	fix_conds(A,AA),
	fix_conds(B,BB).


/*

/***************************************************************************/
/*                                                                         */
/* The SLG System                                                          */
/* Authors: Weidong Chen and David Scott Warren                            */
/* Copyright (C) 1993 Southern Methodist University                        */
/*               1993 SUNY at Stony Brook                                  */
/* See file COPYRIGHT for copying policies and disclaimer.                 */
/*                                                                         */
/***************************************************************************/

/*==========================================================================
  File               : slg.doc
  Last Modification  : November 1, 1993 by Weidong Chen
===========================================================================*/

This file describes the SLG system in this directory, and how to use
it. The SLG system is a meta interpreter written in Prolog that
supports the following features:

    * effective goal-oriented query evaluation of normal logic programs
      under the well-founded semantics [Van Gelder, Ross, Schlipf, 
      JACM, Vol. 38, July 1991];
    * effective goal-oriented query evaluation of general logic 
      programs under the alternating fixpoint logic [Van Gelder,
      JCSS, Vol. 47, 1993], with the restriction that the body of
      a clause is either a conjunction of literals or universal 
      disjunction of literals;
    * query evaluation under stable model semantics [Gelfond, Lifschitz,
      JICSLP, 1988];
    * integration with Prolog execution.

Predicates may be declared as "prolog" predicates or "tabled"
predicates. Prolog predicates are solved by calling Prolog directly.
Calls to tabled predicates are remembered in a table with their
corresponding answers. Future calls to tabled predicates will not be
re-executed, but will be satisfied using answers that were computed as
a result of the initial call. This mechanism prevents certain positive
loops (i.e., loops that involve no negation), and terminates much more
often than Prolog's strategy.

In case of negative loops, ground negative literals that are selected
in relevant clauses are delayed so that computation can proceed to
solve the remaining literals in the bodies of the relevant clauses.
Both positive and negative loops are detected efficiently by
incremental maintenance of dependencies among calls.

The table of calls and their answers is maintained in the meta
interpreter in a term structure (NOT in the Prolog database).
Therefore each invocation of the meta interpreter starts with an empty
table, and the table is lost when the meta interpreter returns. (See
slgall/4 and other predicates, to have the table returned to be reused
in later evaluations.) There is no support for assert/retract.  Also,
whenever the meta interpreter is invoked, ALL answers to the call are
computed before any is returned.

Prolog predicates can call tabled predicates. However, each call will
start with a new empty table. Tabled predicates can call Prolog
predicates. There is no support for modules. All clauses are asserted
in the default module (typically called 'user').

The syntax of Prolog is used for input programs, with additional
directives for predicate declarations and a minor variation for
clauses with universal disjunctions in the body (<-- instead of 
:-).

The file of interest is slg.pll, which consists of three sections: 1)
Prolog code that is more system dependent. Currently specific codes
are included for Quintus Prolog (TM), SICStus Prolog, and XSB System.
2) Prolog code that defines term_expansion/2 for loading files. It
intercepts the SLG declarations (:- tabled, prolog, default) and
converts clauses of tabled predicates into a form that is accepted by
the meta interpreter. ALL TABLED PREDICATES SHOULD BE DEFINED IN THE
DEFAULT MODULE. 3) Prolog code that implements the SLG meta
interpreter.

/***************************************************************************/

HOW TO USE THE SLG SYSTEM

With the SLG system, you can write Prolog programs and declare certain
predicates as tabled predicates. (Certain restrictions apply to
clauses of tabled predicates. See later.) Calls to tabled predicates
are intercepted and saved in a table. Later invocations of the same
call will use the table.

For the example of win/1, we could create a file, say win.pll:

	:- tabled win/1.
	win(X) :- move(X,Y), \+win(Y).

	move(a,a).
	move(a,b).
	move(b,a).
	move(b,c).

Within Prolog, we load the SLG system and then load win:

	| ?- [slg].
	| ?- [win].

We are now ready to ask queries:

	| ?- win(X).

	X = b ;

	no
	| ?- win(X)<-C.

	X = a,
	C = [\+win(a)] ;

	X = b,
	C = [] ;

	no
	| ?- \+ win(c).

	yes

The first query asks for all true answers, and the second query asks
for all true or undefined answers under the well-founded semantics.
For undefined answers, a list of delayed literals is also returned.
The third query checks whether w(c) is not true under the well-founded
semantics.

The next sections describe in detail the capabilities that SLG
supports.

TABLING DIRECTIVES:

SLG supports three tabling directives: tabled, prolog, and default. By
initial default, all predicates are treated as Prolog predicates. This
means that they will be evaluated by call-ing them as Prolog
predicates. Predicates that are declared as tabled will have a table
entry created for each distinct (i.e., nonvariant) call.

Tabled predicates are evaluated within the SLG meta-interpreter. The
first invocation of a tabled predicate will enter the meta
interpreter. Computation will then continue to be controlled by the
meta interpreter. Prolog predicates are evaluated by calling Prolog
directly. It is actually legal for a tabled predicate to call a prolog
predicate which in turn calls a tabled predicate.  However, in this
case computation will leave the meta interpreter and then later enter
a new invocation of it so that an entirely NEW table will be
constructed (and then discarded) for each invocation and Prolog's
infinite loops will NOT be terminated.

There are also certain constraints on the form of clauses that can be
used to define tabled predicates. In particular, the body of a clause
for a tabled predicate should be a conjunction of literals (or a comma
list). Cuts are allowed in the body before any occurrence of a tabled
predicate. Cuts that occur after the first occurrence of a tabled
predicate are NOT allowed. The body of a tabled predicate may also be
a (universal) disjunction of literals (or a semi-colon list), in which
case cut is simply not allowed, and all variables that occur only in
the body are universally quantified. Universal quantification is
indicated by using a different operator (<-- instead of :-).

A tabled or Prolog predicate must be declared as such (if it is not
defaulted) before clauses that define the predicate and before being
used in the definition of a tabled predicate. Each of the tabling
directives can take several arguments; simply separate them with
commas.

Tabled predicates are evaluated with respect to the well-founded
semantics, whose residual program may be processed to produce answers
in a stable model.

Detailed (or summarized) documentation of each directive follows:

DIRECTIVE: :- tabled <pred>/<arity>.

This declares the predicate (or sequence of predicates, if more than
one) to be tabled predicates. A table will be maintained on calls to
the indicated predicate. Clauses defining this predicate will be
stored in a predicate: 'slg$<pred>'/<arity+1>. Prolog built_ins (as
defined by predicate_property/2) cannot be tabled.

DIRECTIVE: :- prolog <pred>/<arity>.

This declares that the indicated predicate is to be evaluated by
call-ing Prolog evaluation directly. This is initially the default.

DIRECTIVE: :- default (tabled).
           :- default (prolog).

This changes the default declaration for how predicates are to be
evaluated. Initially, the default treatment for predicates is to
consider them as prolog predicates. The default can be changed with
one of the two directives. (The parentheses may be omitted for some
Prolog readers.) The default can be changed several times throughout a
compilation. However, care must be taken to insure that the system
correctly and consistently compiles all clauses and all calls. Each
new compile starts with the standard default of prolog.

The definition for term_expansion/2 intercepts tabling directives and
converts clauses of tabled predicates into a certain format. For the
previous example of win, the corresponding clauses are:

	'slg$tabled'(win,1).

	win(X) :- slg(win(X)).
	'slg$win'(X,[\+win(Y)]) :- move(X,Y).

	move(a,a).
	move(a,b).
	move(b,a).
	move(b,c).

Several aspects should be noted. First, there is no support for module
facilities. The meta interpreter does not work for tabled predicates
that are not in the default module 'user'. Second, clauses of Prolog
predicates are kept in their original form. In a clause of a tabled
predicate, all literals before the first occurrence of a tabled
predicate in the body are kept in their original Prolog format. If
clauses for a tabled predicate are loaded from multiple files, the
tabled predicate should be declared 'multifile' as in Prolog.

UNIVERSAL DISJUNCTION:

For tabled predicates, clauses that have a universal disjunction in
the body are allowed. For instance, the following clause:

	founded(X) <-- \+move(X,Y) ; founded(Y).

says that X is founded if for all Y, either \+move(X,Y) or founded(Y).
(You may think of <-- as indicating All and ';' as "or".) All
variables that occur only in the body are universally quantified.
Further restrictions include the standard safety constraints, namely
the head should be ground when the clause is used, and all variables
that occur in a positive literal in a universal disjunction should
also occur in negative literals in the disjunction or in the head. Cut
is definitely not allowed.

QUERY INTERFACE:

SLG/1: slg(+Call)
       Call

If Call is an atom of a Prolog predicate, it is evaluated simply by
call(Call). If Call is an atom of a tabled predicate, a new table is
created and the meta interpreter is invoked. Notice that Call and
slg(Call) are equivalent. It returns true answers of Call under the
well-founded semantics.

(<-)/2: +Call <- -Delay

This predicate returns true or undefined answers for Call under the
well-founded semantics. Undefined answers are returned with a
non-empty list, Delay, of delayed literals. If Call is an atom of a
Prolog predicate, Delay is bound to [], and call(Call) is executed.

Negation of a call to a tabled predicate, \+ Call, succeeds only if
Call is ground and Call does not have a true answer. That is \+Call is
the same as \+slg(Call). To check if Call is false (according to the
well-founded semantics), one should use \+(Call<-_).

SLGALL/4: slgall(+Call,-Anss,+Table0,-Table)

If Call is an atom of a Prolog predicate, findall/3 is used, and Table
and Table0 are unified. If Call is a call to a tabled predicate, the
meta interpreter is invoked with the current table Table0. If Call is
already in Table0, its answers are simply looked up in the table and
returned and Table and Table0 are unified. Otherwise, Call is
evaluated, leading to a new table Table.

Every element in Anss is an instance of Call or of the form H <- D,
where H is an instance of Call, and D is a non-empty list of delayed
literals. For a call to a tabled predicate, slgall/4 ELIMINATES
REDUNDANT ANSWERS.

EMPTYTABLE/1: emptytable(?Table)

It unifies Table with the empty table.

ST/2: st(+Call,-PSM)

Call must be ground. It finds a stable model, PSM, in which Call is
true. PSM is a list of ground literals. The stable model is a
two-valued stable model of the ground clauses that are relevant to
Call. This applies to all predicates that computes answers in a stable
model.

STNOT/2: stnot(+Call,-PSM)

Call must be ground. It finds a stable model, PSM, in which Call is
false. PSM is a list of ground literals.

STALL/3: stall(+Call,-Anss,-PSM)
STALL/5: stall(+Call,-Anss,-PSM,+Tab0,-Tab)

It computes all answers, Anss, of Call in a stable model PSM.

STSELECT/3: stselect(+Call,+PSM0,-Anss,-PSM)
STSELECT/5: stselect(+Call,+PSM0,-Anss,-PSM,+Tab0,-Tab)

It computes all answers of Call in a stable model in which all
literals in PSM0 are true. The final stable model is PSM. PSM0 is a
list of ground literals that are used to select stable models.

TRACING: xtrace, xnotrace.

These predicates turn on (and off) a primitive tracing (really
logging) facility in the SLG meta-interpreter. When tracing is on
(after a call to :- xtrace), information is displayed when calls
become completely evaluated or calls in potential negative loops are
found. It displays the current stack and the table. (The tracing
has not been modified since the first release of the SLG system.)

ASSERT/RETRACT: NOT!

Database modification operations are NOT supported within the SLG meta
interpreter. Those who try to use them in conjunction with this
subsystem are completely on their own, and will probably get what they
deserve.

/*================ end of slg_doc =====================================*/

*/

