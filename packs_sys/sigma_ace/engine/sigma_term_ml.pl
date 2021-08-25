% ===================================================================
% File 'sigma_term_ml.pl'
% Authors: Douglas Miles
% Contact: dmiles@teknowledge.com ;  apease@teknowledge.com
% Version: 'sigma_term_ml.pl' 1.0.0
% Revision:     $Revision: 1.42 $
% Revised At:  $Date: 2002/03/04 16:52:01 $
% History:
% Created - 2001/03/10 dmiles@teknowledge.com
% ===================================================================
% Major functions:  
% This file meets the needs of an external agent working for the needs of eigther an automated or human user 
% Interface with Java with XML to display proof trees and variable bindings
% ===================================================================

:-include('sigma_header.pl').
:-was_indexed(getTermlFormula(1,1,1,1)).
:-was_indexed(toMarkUp_lang(1,1,1,1)).



%Tests

%stest3 :- toMarkUp(html, proof(('Military':996:subclass('IntransitiveRelation', 'BinaryRelation')^B)* ('Military':836:subclass('BinaryRelation', 'Relation')^C)*forall('IntransitiveRelation', forall(D, forall('Relation', holds(subclass, 'IntransitiveRelation', D)and holds(subclass, D, 'Relation')=>holds(subclass, 'IntransitiveRelation', 'Relation'))))*sfind(instance(subclass, 'PartialOrderingRelation'))*sfind(subclass('PartialOrderingRelation', 'TransitiveRelation'))* ('Military':2756:instance(on, 'IntransitiveRelation')^E)), ['X'=on|A],O),write_ln(O).

% ================================================================
%   Transform Signals to Objects
% ================================================================

% ===================================================================
% writeMarkup(-Prolog)
%
% Replaces writeq in some cases
% ===================================================================
writeMarkup(Term):-term_to_leml(Term,Chars),write(Chars).


% ===================================================================
% toMarkUp_lang(-Markup,-Prolog,-PrologVarableList, +Output)
%
% Markup := [html,kif,pml,leml] (Expandable)
% Prolog := any prolog term
% PrologVaraibles list is the equal list as produced by read/3  [=(Name,Val)|...]
% Output is an CharicterAtom (the difference is this atom is not added the the symbol table)
% ===================================================================
% ===================================================================
% term_to_leml(-Prolog, +Output)
%
% arity 2 version (note html) is sufficient for printing values
% ===================================================================
term_to_leml(Term,Chars):-toMarkUp(html,Term,_,Chars),!.


toMarkUp(chat,Var,VS,Chars):-!,catch(toMarkUp(kif,Var,VS,Chars),_,true),!.
toMarkUp(java,Var,VS,Chars):-!,catch(toMarkUp(html,Var,VS,Chars),_,true),!.

toMarkUp(L,T,V,Chars):-!,
	ignore(catch(/*notrace*/((
	copy_term((T,V),(CT,CV)),
	numbervars((CT,CV),'$VAR',0,_),%trace,
	toMarkUp_lang(L,CT,CV,Chars))),_,true)),!.

% VARIABLES
toMarkUp_lang(L,C,Vars,Out):-isSlot(C),!,toMarkUp_slotValue(L,C,Vars,Out).

toMarkUp_lang(html,'$spacer',Vars,'\n<hr>\n').
toMarkUp_lang(_,'$spacer',Vars,'\n;; ------------------------------------------------------------------------------\n\n').

tml(Form):-toMarkUp_lang(html,formula(Form),Vars,Out),write(Out),nl.
	
toMarkUp_lang(L,formula(C),Vars,Out):-!,
	getTermlFormula(L,C,Vars,Out).
	
% ===================================================
% Pretty Print Formula
% ===================================================
%getTermlFormula(L,C,Vars,Out):-	writeq( C=Vars),nl,fail.


getTermlFormula(L,C,Vars,Out):-isSlot(C),!,toMarkUp_lang(L,C,Vars,Out).
getTermlFormula(L,C,Vars,Out):-not(compound(C)),!,toMarkUp_lang(L,C,Vars,Out).

% QUOTED STRING FORMAT
getTermlFormula(L,Atom,_VS,Chars):-((isCharCodelist(Atom);string(Atom))),!,
	catch(sformat(Chars,'"~s"',[Atom]),_,sformat(Chars,'"~w"',[Atom])).

getTermlFormula(L,string(C),Vars,C):-!.

getTermlFormula(L,hidden(F,Args),Vars,''):-!.

getTermlFormula(html,colourize(Color,Thing),Vars,Chars):-!,
	getTermlFormula(html,Thing,Vars,Chars1),!,
	sformat(Chars,'<font color="~w">~w</font>\n',[Color,Chars1]).

getTermlFormula(L,colourize(Color,Thing),Vars,Chars):-!,
	getTermlFormula(L,Thing,Vars,Chars),!.

/*
getTermlFormula(L,','(A,B),Vars,Chars):-!,
	prolog_to_krlog(','(A,B),KR),
	getTermlFormula(L,KR,Vars,Chars),!.
*/
	

getTermlFormula(L,write_dollar('$v',[A|Args]),Vars,Chars):-!,
		Flag=..[getPrologVars,A|Args],!,
		getTermlFormula(L,Flag,Vars,Chars).

getTermlFormula(L,table_(Goal,Lits),Vars,Chars):-!,
		getTermlFormula(L,table_p(Lits,Goal),Vars,Chars).
		

getTermlFormula(L,write_dollar(F,[A|Args]),Vars,Chars):-!,
	getTermlFormula(L,A,Vars,Chars1),
	getTermlFormula(L,hidden(F,Args),Vars,Chars2),!,
	sformat(Chars,'~w~w',[Chars1,Chars2]).

getTermlFormula(L,'$existential'(VarName,Name,Literal),Vars,O):-!,
	getTermlFormula(L,'existential'(VarName),Vars,O).

getTermlFormula(L,'$eval'(Function),Vars,O):-!,
	getTermlFormula(L,' eval'(Function),Vars,O).


getTermlFormula(L,functional(VarName,Domains,Literal),Vars,O):-
	toMarkUp_lang(L,Literal,Vars,O),!.

close_list_var(M,[]):-isSlot(M),!.
close_list_var([[M]|Ms],[M|Ls]):-!,
	close_list_var(Ms,Ls).
close_list_var([M|Ms],[M|Ls]):-!,
	close_list_var(Ms,Ls).
	
getTermlFormula(L,Term,Vars,Chars):-
	Term=..[F,A|Args],
	atom_concat('$',_,F), !,
	getTermlFormula(L,write_dollar(F,[A|Args]),Vars,Chars).
 

getTermlFormula(L,unused(C,P),Vars,O):-!,
	getTermlFormula(L,notused(C,writeq(P)),Vars,O).

getTermlFormula(L,ff([]),Vars,'[]'):-!.

getTermlFormula(L,ff([Flag|Flags]),Vars,Chars):-!,
	getTermlFormula(L,flag(Flag),Vars,Chars1),
	getTermlFormula(L,ff(Flags),Vars,Chars2),
	sformat(Chars,'~w, ~w',[Chars1, Chars2]).

getTermlFormula(L,domargs([]),Vars,''):-!.

getTermlFormula(L,domargs([(P:N)]),Vars,Chars):-!,
	getTermlFormula(L,P,Vars,Chars1),
	sformat(Chars,'~w:~w',[Chars1,N]).

getTermlFormula(L,domargs([(P:N)|Flags]),Vars,Chars):-!,
	getTermlFormula(L,P,Vars,Chars1),
	getTermlFormula(L,domargs(Flags),Vars,Chars2),
	sformat(Chars,'~s:~w,~w',[Chars1,N,Chars2]).

getTermlFormula(L,flag(Flag),Vars,Chars):-
	Flag=..[domainV,Var,DomArgs],!,
	getTermlFormula(L,Var,Vars,VarChars),
	getTermlFormula(L,domargs(DomArgs),Vars,ArgChars),
	sformat(Chars,'~w(~w,[~w])',[domainV,VarChars,ArgChars]).
	
getTermlFormula(L,flag(Flag),Vars,Chars):-
	Flag=..[Name,Var,Args],!,
	getTermlFormula(L,Var,Vars,VarChars),
	sformat(Chars,'~w(~w, ~q)',[Name,VarChars,Args]).
getTermlFormula(L,flag(Flag),Vars,Chars):-!,
	getTermlFormula(L,writeq(Flag),Vars,Chars).
	

	
	

	

getTermlFormula(L,writeq(Atom),_VS,Chars):-!,sformat(Chars,'~q',[Atom]).

getTermlFormula(L,[],Vars,''):-!.
%getTermlFormula(L,[A | B],Vars,Chars):-proper_list([A | B]),append(['('|[A | B],[')'],TRY),toMarkUp_list(L,[Su|Bj],Vars,Chars).
%getTermlFormula(L,[A | B],Vars,Chars):-catch(TRY=..['',A | B],_,fail),getTermlFormula(L,TRY,Varsr,Chars),!.
%getTermlFormula(L,[A | B],Vars,Chars):-catch(TRY=..[A | B],_,fail),getTermlFormula(L,TRY,Vars,Chars),!.
%getTermlFormula(L,[A | B],Vars,Chars):-catch(TRY=..[A | B],_,fail),getTermlFormula(L,TRY,Vars,Chars),!.
getTermlFormula(L,[Su|Bj],Vars,Chars):-
	toMarkUp_list(L,[Su|Bj],Vars,Chars1),
	sformat(Chars,'(~w)',[Chars1]).

/*
getTermlFormula(L,Term,Vars,O):- 
	Term=..[holds,F|Args],isNonVar(F),not_a_function(F),!,
	NTerm=..[F|Args],
	getTermlFormula(L,NTerm,Vars,O).
*/
getTermlFormula(L,'$VAR'(_)* X ,Vars,Out):-!,getTermlFormula(L, X ,Vars,Out).
getTermlFormula(L, X * '$VAR'(_) ,Vars,Out):-!,getTermlFormula(L, X ,Vars,Out).
getTermlFormula(L,(A * []),Vars,Out):-!,getTermlFormula(L,A ,Vars,Out).
getTermlFormula(L,([] * A),Vars,Out):-!,getTermlFormula(L,A ,Vars,Out).
getTermlFormula(L,deduced* X ,Vars,Out):-!,getTermlFormula(L, X ,Vars,Out).
getTermlFormula(L, X * deduced ,Vars,Out):-!,getTermlFormula(L, X ,Vars,Out).


getTermlFormula(L,domainV(Var,ReqsL),Vars,Chars):-
	getTermlFormula(L,' domainV'(Var,writeq(ReqsL)),Vars,Chars).	
getTermlFormula(L,domainC(Var,ReqsL),Vars,Chars):-
	getTermlFormula(L,' domainC'(Var,writeq(ReqsL)),Vars,Chars).	
getTermlFormula(L,domainA(Var,ReqsL),Vars,Chars):-
	getTermlFormula(L,' domainA'(Var,writeq(ReqsL)),Vars,Chars).	
getTermlFormula(L,existsC(Var,ReqsL),Vars,Chars):-
	getTermlFormula(L,' existsC'(Var,writeq(ReqsL)),Vars,Chars).	
getTermlFormula(L,existsA(Var,ReqsL),Vars,Chars):-
	getTermlFormula(L,' existsA'(Var,writeq(ReqsL)),Vars,Chars).	

getTermlFormula(L,(A * B),Vars,Chars):-!,
	getTermlFormula(L,B,Vars,Chars2),
	getTermlFormula(L,A,Vars,Chars1),
	sformat(Chars,'~w\n~w',[Chars2, Chars1]).

getTermlFormula(L,formula(C),Vars,Out):-!,
	getTermlFormula(L,C,Vars,Out).


getTermlFormula(html,undefined_constants(UnDefinedList),_,O):-
	getTermlFormula(kif,nv(UnDefinedList),_,I),
	sformat(O,'\n<font color=red>Warning Undefined constants: <font color=black size=+1>~w</font></font>',[I]).

getTermlFormula(kif,undefined_constants(UnDefinedList),_,O):-
	getTermlFormula(kif,(UnDefinedList),_,I),
	sformat(O,'\nWarning Undefined constants ~w',[I]).
	


getTermlFormula(L,C,Vars,Out):-is_list(C),!,make_args_out(L,C,Vars,Out1),sformat(Out,'(~w)',[Out1]).
%getTermlFormula(L,C,Vars,Out):-not(compound(C)),!,toMarkUp_lang(L,C,Vars,Out).

/*
getTermlFormula(L,and(A,B),VS,Chars):-
	collect_op(and(A,B),O),!,
	getTermlFormula(L,O,VS,Chars).

collect_op(and(A,B),and(A,B)):-not(A=and(_,_)),not(B=and(_,_)).
collect_op(and(A,B
*/

% ==================================================
% Unest And/Or 
% ==================================================

getTermlFormula(L,and(and(and(and(and(F,E),D),C),B),A),VS,Chars):-!, getTermlFormula(L,and(F,E,D,C,B,A),VS,Chars).
getTermlFormula(L,and(and(and(and(E,D),C),B),A),VS,Chars):-!, getTermlFormula(L,and(E,D,C,B,A),VS,Chars).
getTermlFormula(L,and(and(and(D,C),B),A),VS,Chars):-!, getTermlFormula(L,and(D,C,B,A),VS,Chars).
getTermlFormula(L,and(and(B,C),A),VS,Chars):-!, getTermlFormula(L,and(C,B,A),VS,Chars).
getTermlFormula(L,and(A,and(B,and(C,and(D,and(E,F))))),VS,Chars):-!, getTermlFormula(L,'and'(A,B,C,D,E,F),VS,Chars).
getTermlFormula(L,and(A,and(B,and(C,and(D,E)))),VS,Chars):-!, getTermlFormula(L,'and'(A,B,C,D,E),VS,Chars).
getTermlFormula(L,and(A,and(B,and(C,D))),VS,Chars):-!, getTermlFormula(L,'and'(A,B,C,D),VS,Chars).
getTermlFormula(L,and(A,and(B,C)),VS,Chars):-!, getTermlFormula(L,'and'(A,B,C),VS,Chars).
getTermlFormula(L,or(or(or(or(D,E),D),B),A),VS,Chars):-!, getTermlFormula(L,or(E,D,C,B,A),VS,Chars).
getTermlFormula(L,or(or(or(C,D),B),A),VS,Chars):-!, getTermlFormula(L,or(D,C,B,A),VS,Chars).
getTermlFormula(L,or(or(B,C),A),VS,Chars):-!, getTermlFormula(L,or(C,B,A),VS,Chars).
getTermlFormula(L,or(A,or(B,or(C,or(D,E)))),VS,Chars):-!, getTermlFormula(L,'or'(A,B,C,D,E),VS,Chars).
getTermlFormula(L,or(A,or(B,or(C,D))),VS,Chars):-!, getTermlFormula(L,'or'(A,B,C,D),VS,Chars).
getTermlFormula(L,or(A,or(B,C)),VS,Chars):-!, getTermlFormula(L,'or'(A,B,C),VS,Chars).

% ==================================================
% Mark terms as implemented in code
% ==================================================

getTermlFormula(html,incode(X),Vars,HAtom):-!,
	getTermlFormula(L,bullet(X),Vars,Atom),
	sformat(HAtom,'<table border=0><tr><td><pre>~w</pre></td><td><pre>Implemented in code.</pre></td></tr></table>',[Atom]).

getTermlFormula(kif,incode(X),Vars,HAtom):-!,
	getTermlFormula(L,bullet(X),Vars,Atom),
	sformat(HAtom,'~w\nImplemented in code.\n',[Atom]).


getTermlFormula(html,incode(X,M),Vars,HAtom):-!,
	getTermlFormula(L,bullet(X),Vars,Atom),
	sformat(HAtom,'<table border=0><tr><td><pre>~w</pre></td><td><pre>Implemented in code.\n~w</pre></td></tr></table>',[Atom,M]).

getTermlFormula(kif,incode(X,M),Vars,HAtom):-!,
	getTermlFormula(L,bullet(X),Vars,Atom),
	sformat(HAtom,'~w\nImplemented in code.\n (~w)\n',[Atom,M]).

% ==================================================
% Finds the clausification then displays the proof
% ==================================================

getTermlFormula(L,cfind(entails(Pre,Post)),Vars,Out):-
	sigmaCache(PredR,Post,Pre,T,true,KB,Ctx,Proof),
	getTermlFormula(L,Proof,Vars,Out),!.
		
% ==================================================
% Show proof of cross reference optimization
% ==================================================
getTermlFormula(L,g_h(_),Vars,''):-!.
getTermlFormula(L,tid(_),Vars,''):-!.

getTermlFormula(L,crossref(X,Y),Vars,Atom):-!,
	crossref_to_proof(crossref(X,Y),P),
	getTermlFormula(L,P,Vars,Atom).

getTermlFormula(L,crossref(X),Vars,Atom):-!,
	crossref_to_proof(crossref(X),P),
	getTermlFormula(L,P,Vars,Atom).


% ==================================================
% Surface Find
% ==================================================

getTermlFormula(L,sfind(X),Vars,Out):- nonvar(X),
	sigmaCache(PredR, surface, X,V,KB, Ctx, TN, Auth, State),!,
	var_merge(Vars,V,TVars),!,
	getTermlFormula(L,surf(KB,TN),TVars,Out).

% ==================================================
% Find a surface form, Display its proof, show instanced version
% ==================================================

getTermlFormula(L,sfindi(X),Vars,Out):- nonvar(X),
	sigmaCache(PredR, surface, X,V,KB, Ctx, TN, Auth, State),!,
	var_merge(Vars,V,TVars),!,
	getTermlFormula(L,surf(KB,TN) * bullet_a(X),TVars,Out).

getTermlFormula(L,sfindi(X),Vars,Out):- nonvar(X),
	getTermlFormula(L,bullet_a(X),Vars,Out).

       
% ==================================================
% VIA
% ==================================================

getTermlFormula(L,via(Form,V),Vars,Out):-
	(var_merge(Vars,V,TVars)),
	getTermlFormula(L,via(Form),TVars,Out),!.


:-dynamic(show_entails).

getTermlFormula(L,via(entails(Pre,(Post))),Vars,Out):-not(show_entails),!,
	getTermlFormula(L,(  via('=>'(Pre,Post)) ),Vars,Out).


getTermlFormula(L,'-'(Form),Vars,Out):-
	getTermlFormula(L,not(Form),Vars,Out).

getTermlFormula(L,'+'(Form),Vars,Out):-
	getTermlFormula(L,(Form),Vars,Out).

getTermlFormula(L,via(Form),Vars,Out):-
	getTermlFormula(L,bullet_a(Form),Vars,Out).


getTermlFormula(L,(entails(CList,UConsq,false)),Vars,Out):-!,
	getTermlFormula(L,entails(CList,not(UConsq)),Vars,Out).

getTermlFormula(L,(entails(CList,UConsq,true)),Vars,Out):-!,
	getTermlFormula(L,entails(CList,(UConsq)),Vars,Out).

getTermlFormula(L,(entails(true,(Post))),Vars,Out):-!,
	getTermlFormula(L,(Post),Vars,Out).

% ==================================================
% nv(_) Print list as non-vecorted
% ==================================================

getTermlFormula(L,nv(Subj),Vars,Chars):-!,toMarkUp_list(L,Subj,Vars,Chars).

% ==========================
% Authorial writing
% ==========================

getTermlFormula(L,surf(KB,TN),Vars,Atom):-
	sigmaCache(PredR,surface, OForm, OVars,KB,Ctx,TN,_, _),!,
	getTermlFormula(L,OForm,OVars,Orig),
	flag(proof_linenumber,LN,LN+1),
	getTermlFormula(L,bullet(KB,Ctx,TN,LN,Orig),Vars,Atom).
getTermlFormula(L,surf(KB,TN),Vars,Atom):-!,
	getTermlFormula(L,bullet('assertion lookup failure'(KB,TN)),Vars,Atom).

% ==========================
% Bullet writing
% ==========================

getTermlFormula(L,bullet_a(X),Vars,S):-
	flag(indent,_,0),
	getTermlFormula(L,X,Vars,SStatement),
	flag(proof_linenumber,LN,LN),
	LNB is LN-1,
	sformat(S,'~wa. ~w\n',[LNB,SStatement]).

getTermlFormula(L,bullet(X),Vars,Atom):-!,
	flag(proof_linenumber,LN,LN+1),
	getTermlFormula(L,X,Vars,Orig),
	getTermlFormula(L,bullet('Kernel','ToplevelContext',9100000,LN,Orig),Vars,Atom).
	
getTermlFormula(html,bullet(KB,Ctx,TN,LN,Orig),Vars,Atom):-!,%trace,
	flag(indent,_,0),
	(catch((TN < 100000),_,fail) -> 
		sformat(Atom,'~w <A href="skb.jsp?req=SA&skb=~w&id=~w" title="~w ~w ~w" ><img border=0 src="bullet.gif"/></A> ~w',[LN,KB,TN,TN,KB,Ctx,Orig]);
		sformat(Atom,'~w <img border=0 src="bullet.gif" title="Not added to browser ~w (~w)"> ~w',[LN,KB,Ctx,Orig])),!.

getTermlFormula(kif,bullet(KB,Ctx,TN,LN,Orig),Vars,Atom):-!,
	flag(indent,_,0),
%	getTermlFormula(kif,asserted(Ctx,Orig),Vars,F),
	getTermlFormula(kif,Orig,Vars,F),
	sformat(Atom,'~w. ~w',[LN,F]).

% ==========================
% Slolem  rewriting
% ==========================

getTermlFormula(L,(X),Vars,Out):- nonvar(X),X=..['E',Sk|ArgS],!,
	Y=..[Sk|ArgS],!,
	getTermlFormula(L,Y,Vars,Out).

% =====================
% remove_nonvars
% =====================

remove_nonvars(V,V):-isSlot(V),!.
remove_nonvars([],[]):-!.
remove_nonvars([V|L],LL):-isNonVar(V),!,remove_nonvars(L,LL).
remove_nonvars([V|L],[V|LL]):-remove_nonvars(L,LL).



% =====================
% Forall
% =====================

getTermlFormula(L,forall(V,F),Vars,Chars):-not(is_list(V)),!, 
	group_forall(forall(V,F),Next),!,
	cleanQuantifierConversionForWrite_forall(Next,O),
	getTermlFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_forall(forall(VL,F),O):-
	remove_nonvars(VL,NL),
	((NL=[],!,O=F);(!,O=forall(NL,F))).

getTermlFormula(L,forall(V,F),Vars,Chars):- not(is_list(V)),!,
	getTermlFormula(L,forall([V],F),Vars,Chars).

group_forall(forall(V1,forall(V2,forall(V3,forall(V4,forall(V5,F))))),forall([V1,V2,V3,V4,V5],F)):-!.
group_forall(forall(V1,forall(V2,forall(V3,forall(V4,F)))),forall([V1,V2,V3,V4],F)):-!.
group_forall(forall(V1,forall(V2,forall(V3,F))),forall([V1,V2,V3],F)):-!.
group_forall(forall(V1,forall(V2,F)),forall([V1,V2],F)):-!.
group_forall(forall(V1,F),forall([V1],F)):-!.

% =====================
% Exists
% =====================



getTermlFormula(L,exists(V,F),Vars,Chars):-not(is_list(V)),!, 
	group_exists(exists(V,F),Next),!,
	cleanQuantifierConversionForWrite_exists(Next,O),
	getTermlFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_exists(exists(VL,F),O):-
	remove_nonvars(VL,NL),
	((NL=[],!,O=F);(!,O=exists(NL,F))).

getTermlFormula(L,exists(V,F),Vars,Chars):- not(is_list(V)),!,
	getTermlFormula(L,exists([V],F),Vars,Chars).

group_exists(exists(V1,exists(V2,exists(V3,exists(V4,exists(V5,F))))),exists([V1,V2,V3,V4,V5],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,exists(V4,F)))),exists([V1,V2,V3,V4],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,F))),exists([V1,V2,V3],F)):-!.
group_exists(exists(V1,exists(V2,F)),exists([V1,V2],F)):-!.
group_exists(exists(V1,F),exists([V1],F)):-!.
% =====================
% Exists
% =====================

getTermlFormula(L,exists(V,F),Vars,Chars):-not(is_list(V)),!, 
	group_exists(exists(V,F),Next),!,
	cleanQuantifierConversionForWrite_exists(Next,O),
	getTermlFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_exists(exists(VL,F),O):-
	remove_nonvars(VL,NL),
	((NL=[],!,O=F);(!,O=exists(NL,F))).

getTermlFormula(L,exists(V,F),Vars,Chars):- not(is_list(V)),!,
	getTermlFormula(L,exists([V],F),Vars,Chars).

group_exists(exists(V1,exists(V2,exists(V3,exists(V4,exists(V5,F))))),exists([V1,V2,V3,V4,V5],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,exists(V4,F)))),exists([V1,V2,V3,V4],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,F))),exists([V1,V2,V3],F)):-!.
group_exists(exists(V1,exists(V2,F)),exists([V1,V2],F)):-!.
group_exists(exists(V1,F),exists([V1],F)):-!.

% =====================
% Findall
% =====================
	/*
getTermlFormula(L,findall(V,F),Vars,Chars):-not(is_list(V)),!, 
	group_findall(findall(V,F),Next),!,
	cleanQuantifierConversionForWrite_findall(Next,O),
	getTermlFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_findall(findall(VL,F),O):-
	remove_nonvars(VL,NL),
	((NL=[],!,O=F);(!,O=findall(NL,F))).

getTermlFormula(L,findall(V,F),Vars,Chars):- not(is_list(V)),!,
	getTermlFormula(L,findall([V],F),Vars,Chars).

group_findall(findall(V1,findall(V2,findall(V3,findall(V4,findall(V5,F))))),findall([V1,V2,V3,V4,V5],F)):-!.
group_findall(findall(V1,findall(V2,findall(V3,findall(V4,F)))),findall([V1,V2,V3,V4],F)):-!.
group_findall(findall(V1,findall(V2,findall(V3,F))),findall([V1,V2,V3],F)):-!.
group_findall(findall(V1,findall(V2,F)),findall([V1,V2],F)):-!.
group_findall(findall(V1,F),findall([V1],F)):-!.
				 */
% =====================
% Indentation
% =====================
	
getTermlFormula(L,C,Vars,Out):-
		C=..[Pred|ARGS],!,
		flag(indent,X,X+1),
		indent_it_x(X,PreOut),!,
		toMarkUp_lang(L,Pred,Vars,PredOut),!,
		make_args_out(L,ARGS,Vars,ArgsOut),!,
		sformat(Out,'~w(~w ~w)',[PreOut,PredOut,ArgsOut]), !,   
		flag(indent,NX,NX-1).

make_args_out(L,[],Vars,''):-!.
make_args_out(L,[C],Vars,ArgsOut):-
		getTermlFormula(L,C,Vars,ArgsOut).
make_args_out(L,[C|GS],Vars,ArgsOut):-
		getTermlFormula(L,C,Vars,Out1),
		make_args_out(L,GS,Vars,Out2),!,
		sformat(ArgsOut,'~w ~w',[Out1,Out2]).
		
indent_it_x(0,''):-!.
indent_it_x(1,'\n         '):-!.
indent_it_x(X,Out):-XX is X -1,!, indent_it_x(XX,OutP),!,sformat(Out,'~w   ',[OutP]),!.

% =====================
% Prolog Tr./ansformation
% =====================

toMarkUp_lang(L,':-'(C,true),Vars,Out):-prolog_to_krlog(C,KR),!,toMarkUp_lang(L,KR,Vars,Out).
toMarkUp_lang(L,':-'(C,A),Vars,Out):-prolog_to_krlog(C,KRC),prolog_to_krlog(A,KRA),!,toMarkUp_lang(L,'=>'(KRA,KRC),Vars,Out).

toMarkUp_lang(L,(T^V),Vars,Out):-var_merge(Vars,V,TVars),!,toMarkUp_lang(L,T,TVars,Out).

%Terminal Control
toMarkUp_lang(html,lparen,Vars,'('):-!.
toMarkUp_lang(html,rparen,Vars,')'):-!.
toMarkUp_lang(kif,lparen,Vars,'('):-!.
toMarkUp_lang(kif,rparen,Vars,')'):-!.
toMarkUp_lang(html,nl,Vars,'<br>'):-!.
toMarkUp_lang(html,tab,Vars,'<li>'):-!.
toMarkUp_lang(kif,nl,Vars,'\n'):-!.
toMarkUp_lang(kif,tab,Vars,'\t'):-!.

% No parens (nv = no vector)
toMarkUp_lang(L,nv(Subj),Vars,Chars):-is_list(Subj),!,toMarkUp_list(L,Subj,Vars,Chars).
toMarkUp_lang(L,nv(Subj),Vars,Chars):-!,toMarkUp_lang(L,Subj,Vars,Chars).

toMarkUp_lang(_,writeFmt(F,A),Vars,Out):-sformat(Out,F,A),!.
toMarkUp_lang(_,surf,Vars,''):-!.
toMarkUp_lang(_,end_of_file,Vars,''):-!.

toMarkUp_lang(_,',',Vars,'and'):-!.
toMarkUp_lang(_,';',Vars,'or'):-!.
toMarkUp_lang(_,'=',Vars,'equal'):-!.
toMarkUp_lang(_,'deduced',Vars,' ').

% QUOTED STRING FORMAT
toMarkUp_lang(kif,Atom,_VS,Chars):-isCharCodelist(Atom),!,
	catch(sformat(Chars,'"~s"',[Atom]),_,sformat(Chars,'"~w"',[Atom])).
toMarkUp_lang(kif,Atom,_VS,Chars):-string(Atom),!,
	catch(sformat(Chars,'"~s"',[Atom]),_,sformat(Chars,'"~w"',[Atom])).


%LISTS
%toMarkUp_lang(LANG,[COMP],Vars,Atom)

toMarkUp_lang(L,[],Vars,Atom):-toMarkUp_lang(L,'NullSet',Vars,Atom).
%toMarkUp_lang(html,[Su|Bj],Vars,Chars):-toMarkUp_list(html,[Su|Bj],Vars,Chars1),sformat(Chars,'<div>(<ul>~w </ul>)</div>',[Chars1]).
toMarkUp_lang(kif,[Su|Bj],Vars,Chars):-toMarkUp_list(kif,[Su|Bj],Vars,Chars1),sformat(Chars,'(~w)',[Chars1]).


close_varlist([]):-!.
close_varlist('$VAR'(_)):-!.
close_varlist([V|VV]):-close_varlist(VV),!.

% SPECIAL FORMATS

toMarkUp_lang(_,writeq(Term),Vars,Atom):-!,sformat(Atom,'~q',[Term]).
toMarkUp_lang(kif,maillink(Title,Address,Subject),Vars,Address):-!.
toMarkUp_lang(kif,weblink(Title,URL),Vars,Title):-!.
toMarkUp_lang(kif,helplink(Title,URL),Vars,Title):-!.
toMarkUp_lang(L,proof(PB),Vars,Atom):-
	flag(proof_linenumber,_,1),
	getTermlFormula(L,PB,Vars,AtomS),!,
	sformat(Atom,'\nProof:\n~w\n',[AtomS]).

toMarkUp_lang(LANG,krlog(COMP),Vars,Atom):-!,prolog_to_krlog(COMP,KR),toMarkUp_lang(LANG,KR,Vars,Atom).

toMarkUp_lang(LANG,kif(COMP),Vars,Atom):-!,toMarkUp_lang(kif,COMP,Vars,Atom).
toMarkUp_lang(LANG,html(COMP),Vars,Atom):-!,toMarkUp_lang(html,COMP,Vars,Atom).

toMarkUp_lang(html,select(Name,OptionList),Vars,Out):-toMarkUp_lang(html,options(OptionList),Vars,Options),sformat(Out,'<select sort name="~w" id="~w" size="1">~w</select>',[Name,Name,Options]).
toMarkUp_lang(html,checkbox(Name,on),Vars,Out):-
		sformat(Out,'<input type=checkbox name="~w" id="~w" checked>',[Name,Name]),!.
toMarkUp_lang(html,checkbox(Name,_),Vars,Out):-
		sformat(Out,'<input type=checkbox name="~w" id="~w">',[Name,Name]),!.
toMarkUp_lang(html,options([]),Vars,'').
	    
toMarkUp_lang(L,getPrologVars(Form),Vars,Chars):-markUpVARLIST(L,Form,Vars,SChars),sformat(Chars,'~w',[SChars]),!.

toMarkUp_lang(L,getPrologVars(Form),Vars,Chars):-!,sformat(Chars,'; var_post_err (~q). ',[Form]).


toMarkUp_lang(html,qresult(Res),Vars,Chars):-!,sformat(Chars,'Result ',[Res]).

toMarkUp_lang(kif,qresult(Res),Vars,''):-!. %,sformat(Chars,'res="~w"\n',[Res]).

% Back into Standard Terms

format_o(Format,Stuff):-
	toMarkUp_lang(html,Stuff,_,Out),writeFmt(Format,[Out]).
	

toMarkUp_lang(html,options([Option|List]),Vars,Out):-
               toMarkUp_lang(html,option(Option),Vars,Out2),
               toMarkUp_lang(html,options(List),Vars,Out3),
               atom_concat(Out2,Out3,Out).
	       
toMarkUp_lang(html,option(Option),Vars,Out):-sformat(Out,'<option value="~w">~w</option>',[Option,Option]).

% Numbers
toMarkUp_lang(_,Atom,_VS,Chars):-number(Atom),!,sformat(Chars,'~w',[Atom]).

toMarkUp_lang(L,Value,Vars,Chars):-
	sigmaCache(PredR, skolem, Value = x(Name,Expression),SKVARS,KB, Ctx, TN, Auth, State),!, 
	    toMarkUp_lang(kif,Name,Vars,NameQ),  prependQuestionMark(NameQ,NameQM),
	    subst(x(Sk,Expression),Sk,NameQM,x(NSk,NExpression)),!,
            toMarkUp_lang(L,exists([NSk],NExpression),SKVARS,Chars).

% all other Formulas get intercepted here
toMarkUp_lang(L,Term,Vars,Chars):-compound(Term),!,
	getTermlFormula(L,Term,Vars,Chars),!.

% PRETTYNESS
toMarkUp_lang(_,';',Vars,'or ').
toMarkUp_lang(_,',',Vars,'and ').
toMarkUp_lang(_,'neg',Vars,'neg ').
%toMarkUp_lang(_,entails,Vars,'modus-ponens ').
%toMarkUp_lang(_,entails,Vars,'modus-tollens ').


		
% Not compound - TEXT
toMarkUp_lang(html,Atom,Vars,Chars):-
	atom_codes(Atom,[115,107|_]),!,
		atom_lookup_kb_ctx(html,Atom,KB,Ctx,Result,ID,Color,Page),!,
		(Result=ml(This) -> toMarkUp_lang(html,This,Vars,SResult) ; SResult=Result),
		(KB=none -> 
			sformat(Chars,'<font color=~w>~w</font>',[Color,SResult]);
			sformat(Chars,'<A HREF="~w.jsp?logicforms=logicforms&submit=All%20Forms&data=~w&kb=~w">~w</A>',[Page,ID,KB,SResult])
		).

toMarkUp_lang(html,Atom,Vars,Chars):-
		atom_lookup_kb_ctx(html,Atom,KB,Ctx,Result,ID,Color,Page),!,
		(Result=ml(This) -> toMarkUp_lang(html,This,Vars,SResult) ; SResult=Result),
		(KB=none -> 
			sformat(Chars,'<font color=~w>~w</font>',[Color,SResult]);
			sformat(Chars,'<A HREF="~w.jsp?req=SC&term=~w&skb=~w">~w</A>',[Page,ID,KB,SResult])
		).

toMarkUp_lang(kif,Atom,Vars,Chars):-
		atom_lookup_kb_ctx(kif,Atom,KB,Ctx,Result,ID,Color,Page),!,
		(Result=ml(This) -> toMarkUp_lang(html,This,Vars,SResult) ; SResult=Result),
			sformat(Chars,'~w',[SResult]).

% Lookup Proc
atom_lookup_kb_ctx(kif,Atom,none,none,Atom,Atom,black,skb):-!.

atom_lookup_kb_ctx(_,Atom,KB,'ToplevelContext',Atom,Atom,purple,skolems):-
	hlPredicateAttribute(Atom,'SkolemFunction'),!,isSigmaOption(opt_kb=KB),!. 

atom_lookup_kb_ctx(Lang,Atom,KB,Ctx,Atom,B,C,skb):-
	atom_lookup_kb_ctx(Lang,Atom,KB,Ctx,Atom,B,C).

atom_lookup_kb_ctx(kif,Atom,none,none,Atom,Atom,black):-!.
atom_lookup_kb_ctx(L,Atom,none,none,Atom,Atom,black):-once(atom_codes(Atom,Codes)),
	once((memberchk(34,Codes);memberchk(63,Codes);memberchk(32,Codes);memberchk(37,Codes))),!. % String
atom_lookup_kb_ctx(_,Atom,KB,'ToplevelContext',Atom,Atom,blue):-!,isSigmaOption(opt_kb=KB),!. % Leftover must be Merge (TODO)
atom_lookup_kb_ctx(_,Atom,'Merge','ToplevelContext',Atom,Atom,blue):-!.

codes_to_links(Codes,PrettyAtom):-
	getUnquotedCodes(Codes,UCodes),
	getKIFTokens(UCodes,WordList),
	concat_atom(WordList,'-',PrettyAtom),!.

getUnquotedCodes([34|Codes],UCodes):-
	(reverse(Codes,RCodes)),
	(ltrim(RCodes,[34|RUCodes])),
	reverse(RUCodes,UCodes).

getUnquotedCodes(UCodes,UCodes):-!.


%TODO Number?

% ================================================
%      toMarkUp_list
% ================================================ 

toMarkUp_list(L,Var,VS,Chars):-isSlot(Var),!,toMarkUp_slotValue(L,Var,VS,Chars).
toMarkUp_list(_,[],VS,''):-!.
toMarkUp_list(LANG,[H],VS,Chars):-!,
        toMarkUp_lang(LANG,H,VS,Chars).
toMarkUp_list(LANG,[H|T],VS,Chars):-!,
        toMarkUp_lang(LANG,H,VS,Chars1),
        toMarkUp_list(LANG,T,VS,Chars2),
        sformat(Chars,'~w ~w',[Chars1,Chars2]).
         
markUpVARLIST(L,[],Vars,''):-!.
markUpVARLIST(L,'$VAR'(_),Vars,''):-!.

markUpVARLIST(L,[VV|Varnames],Vars,Chars):-
                  VV=..[_,Name,Value],!, 
                  toMarkupVarEquals(L,Name,Value,Vars,Chars1),
                  markUpVARLIST(L,Varnames,Vars,Chars2),
                  sformat(Chars,'~w\n~w',[Chars1,Chars2]).

toMarkupVarEquals(_,Name,Value,Vars,Chars):-
            toMarkUp_lang(kif,Name,Vars,NameQ),
            toMarkUp_slotValue(L,Value,Vars,ValChars),
            sformat(Chars,'~w = ~w',[NameQ,ValChars]).


% Real Prolog Var
toMarkUp_slotValue(L,Slot,VarList,Chars):- isVarProlog(Slot),!,
	toMarkUp_makeNamePrologVar(L,VarList,Slot,Name),
	atom_concat('?',Name,Chars),!.
% Slot 'Typed' 
toMarkUp_slotValue(L,Slot,VarList,Chars):-isQualifiedAs(Slot,BaseType,Value,Subtype), !,
	toMarkUp_makeName(L,VarList,Slot,Subtype,Value,Name),
	close_freeVars(VarList,NVarList),
	append(NVarList,[Name=Value],NV),
	toMarkUp_lang(L,Value,NV,VChars),
	sformat(Chars,'<div title="~w">~w</div>',[Subtype,VChars]).
	
toMarkUp_makeNamePrologVar(L,VarList,Value,Name):-member(Name=Var,VarList),Var==Value,!.
toMarkUp_makeNamePrologVar(L,VarList,Value,Name):-getVarAtom(Value,NUame),atom_concat('?',NUame,Name).
	
getVarAtom(Value,Name):-var(Value),!,term_to_atom(Value,Vname),atom_codes(AVAR,[95,_|CODES]),atom_codes(Name,CODES),!.
getVarAtom('$VAR'(VNUM),Name):-concat_atom([VNUM],Name),!.



toMarkUp_makeName(L,VarList,Slot,BaseType,Value,Name):-
	member(Name=Var,VarList),Var==Slot,!.
toMarkUp_makeName(L,VarList,Slot,BaseType,Value,Name):-
	member(Name=Var,VarList),Var==Value,!.
toMarkUp_makeName(L,VarList,Slot,BaseType,Value,Name):-atom_concat('?',BaseType,Name).
	
	
	
close_freeVars(V,V):-proper_list(V),!.
close_freeVars(V,[]):-isSlot(V),!. %Closing List if there are no free getPrologVars
close_freeVars([X|XX],[X|More]):- close_freeVars(XX,More). 
         
	
	      
  	

toMarkup_varProlog(kif,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],number(Name),Var==VarRef,!,sformat(NameQ,'?~d',[Name]).
toMarkup_varProlog(kif,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],Var==VarRef,!,sformat(NameQ,'?~w',[Name]).

toMarkup_varProlog(html,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],number(Name),Var==VarRef,!,sformat(NameQ,'?~d',[Name]).
toMarkup_varProlog(html,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],Var==VarRef,!,sformat(NameQ,'?~w',[Name]).

toMarkup_varProlog(T,Var,[_|Rest],Name):-nonvar(Rest),toMarkup_varProlog(T,Var,Rest,Name).
toMarkup_varProlog(kif,VAR,_,VarName):-term_to_atom(VAR,AVAR),atom_codes(AVAR,[95|CODES]),!,catch(sformat(VarName,'?HYP-~s',[CODES]),_,VarName='?HYP-AVAR').
toMarkup_varProlog(kif,VAR,_,VarName):-term_to_atom(VAR,AVAR),atom_codes(AVAR,CODES),!,catch(sformat(VarName,'?HYP-~s',[CODES]),_,VarName='?HYP-AVAR').
toMarkup_varProlog(html,VAR,VS,VarName):-toMarkup_varProlog(kif,VAR,VS,VarName).

prependQuestionMark(Name,NameQ):-atom_concat('?',Name,NameQ).


