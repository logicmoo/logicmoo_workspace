/* 
   Douglas Miles Feb 1, 2002
   This version handles query answering with integrity constraints.
*/

:-include('sigma_header.pl').

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

:- export(isPrologCurrently/1).
:- export(member_anss/2).
:- export(prologCall/1).
:- export(wfs_all/2).
:- export((<---)/2).
:- dynamic(isPrologCurrently/1).
:- dynamic(member_anss/2).
:- dynamic(prologCall/1).
:- dynamic(wfs_all/2).
:- dynamic((<---)/2).
:- multifile(isPrologCurrently/1).
:- multifile(member_anss/2).
:- multifile(prologCall/1).
:- multifile(wfs_all/2).
:- multifile((<---)/2).


:- op(1200,fx,  ::-).         /* operator for integrity constraints */

:- op(1200,xfx,<----).
%:- op(1150,fx,[(tabled),(prolog),(default)]).
:- op(900,xfx,<---).
:- op(900,xfx,<---).

:- dynamic slg_expanding/0.
:- dynamic wfs_trace/0.

:-dynamic('slg$default'/1).
:-dynamic('slg$prolog'/1).
:-dynamic('slg$tabled'/2).
:-dynamic('slg$tab'/2).
:-dynamic('inconsistent'/0).
:-dynamic('slg$inconsistent'/1).
:-dynamic((~)/1).
:-dynamic(('slg$~')/2).

:-multifile('slg$default'/1).
:-multifile('slg$prolog'/1).
:-multifile('slg$tabled'/2).
:-multifile('slg$tab'/2).
:-multifile('inconsistent'/0).
:-multifile('slg$inconsistent'/1).
:-multifile((~)/1).
:-multifile(('slg$~')/2).

:- dynamic 'sLGx$prolog'/1, 'sLGx$tab'/2.
:- dynamic sLGx_xexpanxdxing/0.
:- dynamic wfsx_trace/0.
:- op(1200,xfx,'<x--').
%:- op(1150,fx,[(xtabled),(prolog),(xdefault)]).
:- op(900,xfx,'<x-').

addDeclTabled(P,A):-(isDeclTabled(P,A);assert('slg$tab'(P,A))),!. 
remDeclTable(P,A):-retractall('slg$tab'(P,A)).

isDeclTabled(G):-functor(G,P,A),!,isDeclTabled(P,A).

isDeclTabled(P,A):-'slg$tab'(P,A);sigmaCache(_,type(tabled),(P/A)).
isDeclTabled(member,2):-!.
isDeclTabled(/*neg*/ \+,1):-!,fail.
isDeclTabled(P,A):-isAlwaysProlog(P/A),!,fail.
isDeclTabled(surf,4):-!,fail.
isDeclTabled(A,_):-nonvar(A),!.


addHasTable(P,A):-('slg$tabled'(P,A);assert('slg$tabled'(P,A))),!.
remHasTable(P,A):-retractall('slg$tabled'(P,A)).
isHasTable(P,A):-'slg$tabled'(P,A).  % Not used
failedHasTable(P,A):-not('slg$tabled'(P,A)).

:- addHasTable(inconsistent,0).

remDefaultDecl:-retractallLogged('slg$default'(_)).
setDefaultDecl(D):-assertLogged('slg$default'(D)).

getDefaultDecl(Default):-'slg$default'(Default),!.
getDefaultDecl(prolog).

isDeclProlog(Pred/A):-'slg$prolog'(Pred/A).      
isDeclProlog(surf/4):-!.
isDeclProlog(member/2):-!,fail.


addDeclProlog(Pred):-(isDeclProlog(Pred) ; assert('slg$prolog'(Pred))),!.

add_slg_functor(P,NewP):-atom_concat('slg$',P,NewP).

/*
% Must allow failures?
new_slg_head(Head,Body,NewHead) :-var(Head),!,
	NewHead=..[NF|NArgs],
	append(Args,[Body],NArgs),
	add_slg_functor(F,NF),
	Head=..[F|Args],
	ensure_slghead(Head,NewHead).

new_slg_head(Head,Body,NewHead) :-
	Head=..[F|Args],
	add_slg_functor(F,NF),
	append(Args,[Body],NArgs),
	NewHead=..[NF|NArgs],
	ensure_slghead(Head,NewHead).
	
ensure_slghead(Head,SLGHead):-
	predicate_property(SLGHead,number_of_clauses(H)),
	((H < 1) -> writeDebug(no_clauses_for : Head) ; true),!.
ensure_slghead(Head,SLGHead):-
	dynamic(SLGHead),
	functor(Head,F,A),
	addHasTable(F,A),
	writeDebug(faking_slg : Head),!.
*/

stableGround(G):-%notrace
		(stableGroundTrace(G)).
stableGroundTrace(G):- (ground(G),!) ; (var(G),!,fail).
stableGroundTrace(v(_,G,_)):-!,nonvar(G).
stableGroundTrace(\+ (G)):-!,stableGroundTrace(G).
stableGroundTrace('$existential'(_,_,_)):-!.
stableGroundTrace('$existential'(_,_)):-!.
stableGroundTrace(G):-G=..[F|Args],!,stableGroundTraceA(Args).
stableGroundTraceA([G]):-!,
	stableGroundTrace(G).
stableGroundTraceA([H|T]):-
	stableGroundTrace(H),!,
	stableGroundTraceA(T).



isAlwaysProlog(true):-!.
isAlwaysProlog(/*neg*/ \+(_)):-!. %,fail.
isAlwaysProlog(surf(_,_,_,_)).
isAlwaysProlog(tnot(_)).
isAlwaysProlog(\+ _):-!.
isAlwaysProlog(P/A):-ground(P/A),functor(G,P,A),!,isAlwaysProlog(G).
isAlwaysProlog(Nlit):-nonvar(Nlit),
	(slg_built_in(Nlit);
	(predicate_property(Nlit,PP),(PP=built_in;PP=imported_from(_)))),!.


surf(KB,TN,CID,[]):-!.
surf(KB,TN,CID,Vars):-
	writeq(surf(KB,TN,CID,Vars)),nl.
	

/* isPrologCurrently(Call) :  Call is a Prolog subgoal */

isPrologCurrently(Call) :-!, fail, %TODO
	 not(number(Call)),
	functor(Call,P,A),
         failedHasTable(P,A). % Therefore prologCall its prolog entry point

/*
prologCall(G):-
	once( (G = (\+ Call )) -> Log=false(_) ; (Call=G,Log=true(_))),
	Call=..[F|Args],
	atom_concat('slg$',F,F),
	append(Args,[Body],Args),
	Call=..[F|Args],!,
	prologCall(Log,Call,F,Args,Body).
*/
	


prologCall(Call):-
	xhandle(prologCall,Call).

%xprologCall(Call):-dynamic(Call),catch(Call,E,xhandle(E,Call)).
xprologCall(Call):-prologCall(Call).

xhandle(E,Call):-format('xhandle: ~n~q.~nxhandle:',[E:Call]),trace,fail.

xprologSLGCall(Call,Body):-
	prologSLGCall(Call,Body).

prologSLGCall( or(G1,G2),Body):-nonvar(G1),!,
	prologSLGCall( G1,Body);
	prologSLGCall( G2,Body).

prologSLGCall(holds(P,A,B),Body):-atom(P),
	Call=..[P,A,B],	  !,not(atom_concat(_,'Fn',P)),
	prologSLGCall(Call,Body).
prologSLGCall(holds(P,A,B,C),Body):-atom(P),
	Call=..[P,A,B,C],	  !,not(atom_concat(_,'Fn',P)),
	prologSLGCall(Call,Body).
	
	
prologSLGCall(not Call,Body):-nonvar(Call),!,
	Call=..[F|Args],!,
	prologCall(false(_),Call,F,Args,Body).

prologSLGCall(/*neg*/ \+ Call,Body):-nonvar(Call),!,
	Call=..[F|Args],!,
	prologCall(false(_),Call,F,Args,Body).

prologSLGCall( Call,Body):-
	Call=..[F|Args],!,
	prologCall(true(_),Call,F,Args,Body).



resetTableFlags:-sigma_notrace(resetTableFlags2).
		
resetTableFlags2:-
	current_flag(X),
	(compound(X) ;(atom(X),not(atom_concat('$',_,X)))),flag(X,_,0),fail.
resetTableFlags2:-
	current_key(X),
		(compound(X) ;(atom(X),not(atom_concat('$',_,X)))),recorded(X,_,Y),erase(Y),fail.
resetTableFlags2:-!.
	

mslg(Goal):-
	resetTableFlags,
	statistics(cputime,S),
	findall(Goal,
		slg(Goal),L),
	statistics(cputime,E),
	writeq_conj(L),
	length(L,N),
	T is  E - S,
	format('found ~w answers in ~q secs.~n',[N,T]),!.

xmslg(Goal):-
	resetTableFlags,
	statistics(cputime,S),
	findall(Goal,
		sLGx(Goal),L),
	statistics(cputime,E),
	writeq_conj(L),
	length(L,N),
	T is  E - S,
	format('found ~w answers in ~q secs.~n',[N,T]),!.
	
	
call_one([B|L]):-prologSLGCall(B,[]),!.
call_one([B|L]):-call_one(L).


prologCall(true(_),Call,F,Args,[]):-
	sigmaCache(Call, true, KB, Ctx,Proof),stableGroundTraceA(Args),writeq('+').
prologCall(false(_),Call,F,Args,[]):-
	sigmaCache(Call, false, KB, Ctx,Proof),stableGroundTraceA(Args),writeq('-').

prologCall(true(_),instance(v(H,X,List),v('Asbtract',Class,['Class'|_])),F,Args,[]):-!,
	nonvar(X),nonvar(List),
		trace,close_list(List),!,
		member(Class,List).

%prologCall(_,Call,F,Args,Body):-member(F,[domain]),!.
prologCall(_,Call,F,Args,Body):-
	(
	%member(F,[disjointDecomposition,domain,subrelation,subclass]);
	%member(Call,[instance(v(_,*,_),_)]);
	(Args=[v(_,A,_),v(_,B,_)|_],((atom(A),atom_concat(_,'Fn',A);(atom(B),atom_concat(_,'Fn',B)))))
	),!,fail.
		
prologCall(true(_),Call,F,Args,Body):-
	once((format('needs: '),writeArgLit(Call))),
	copy_term(Call,Copy),
	sigmaCache(Copy, Body,/*true(_)*/ _, KB, Ctx,surf(KB,TN,CLID,Vars)),
		not(recorded(TN,Call)),
		(recorda(TN,Call)),
		Call=Copy,
	once((format('found: ~q ~n',[[TN,CLID,Vars]]),writeArgLit(Call),writeArgLitL(Body))).

%		not(not(call_one(Body))),
		%term_to_atom(Vars,Atom),
				/*
prologCall(false(_),Call,F,Args,Body):-
		sigmaCache(Call, Body,false(_),KB, Ctx,surf(KB,TN,CLID,Vars)),
		call_one(Args),
		stableGroundTraceA(Args) -> true ;
		((
		term_to_atom(Vars,Atom),
		flag(Atom,X,X+1),
		X<3,
		stableGroundTraceA(Body) 
		)),
		format('~q.~n',[true:X:surf(KB,TN,CLID,Vars)]).
				  */
	

/*
prologCall(false(_),Call,F,Args,Body):-sigmaCache(Call, Body,false(_),KB, Ctx,Proof),
		stableGroundTraceA(Args),
		stableGroundTraceA(Body),
		format('~q.~n',[false:Proof]).

*/


'sLGx$prolog'(X):-isDeclProlog(X).
'sLGx$tablexd'(X,Y):-addHasTable(X,Y).
'sLGx$xdefault'(X):-getDefaultDecl(X).
'sLGx$tab'(X,Y):-isDeclTabled(X,Y).
/* xisCurrentlyProlog(Callx): Callx is a Prolog subgoal */

xisCurrentlyProlog(Callx) :-
        functor(Callx,P,A),
        \+ 'sLGx$tablexd'(P,A).

writeArgLitL([H]):-!,write('\t\t'),writeArgLit(H).
writeArgLitL([H|T]):-!,
	write('\t\t'),writeArgLit(H),
	writeArgLitL(T).
	

writeArgLit(Call):-
	Call=..[F|Args],
	remove_v_argsW(Args,ArgsO),
	R=..[F|ArgsO],
	writeq(R),nl,!.
	
remove_v_argsW([],[]).
remove_v_argsW([H|T],[HO|TO]):-
	remove_v_argW(H,HO),
	remove_v_argsW(T,TO).

remove_v_argW(v(_,H,_),H):-var(H).
remove_v_argW(v(_,H,_),HO):-!,remove_v_argW(H,HO).
remove_v_argW('$existential'(HO,_),HO).
remove_v_argW('$existential'(HO,_,_),HO).
remove_v_argW('$existential'(HO,_,_,_),HO).
remove_v_argW(HO,HO).
	

/* SLG tracing : 
   xtrace :  turns SLG trace on, which prints out tables at various 
           points
   xnotrace :  turns off SLG trace
*/
xtrace :-( wfs_trace -> true ; assertLogged(wfs_trace)).
xnotrace :- retractallLogged(wfs_trace).



slg_built_in(slg(_)).
slg_built_in(_<---_).
slg_built_in(slgall(_,_)).
slg_built_in(slgall(_,_,_,_)).
slg_built_in(emptytable(_)).
slg_built_in(st(_,_)).
slg_built_in(stnot(_,_)).
slg_built_in(stall(_,_,_)).
slg_built_in(stall(_,_,_,_,_)).
slg_built_in(stselect(_,_,_,_)).
slg_built_in(stselect(_,_,_,_,_,_)).
slg_built_in(xtrace).
slg_built_in(xnotrace).
slg_built_in(surf(_,_,_,_)).


/* ----------------- end of slg_load routines --------------------------- */


wfs_error(Msg,Term) :-
	write('Error :  '), write(Msg), write(Term), nl, fail.

/* slg(Call) : 
   It returns all true answers of Call under the well-founded semantics
   one by one.
*/
slg(Call) :-
	Call<---[].
	

/* Call<---Cons : 
   It returns all true or undefined answers of Call one by one. In
   case of a true answer, Cons = []. For an undefined answer,
   Cons is a list of delayed literals.
*/
Call<---Cons :-
        ( isPrologCurrently(Call) ->
          prologCall(Call),
          Cons = []
        ; wfs_all(Call,Anss),
          member_anss(d(Call,Cons),Anss)
        ).

/* emptytable(EmptTab) :  creates an initial empty table.
*/
emptytable(0 : []).

/* slgall(Call,Anss) : 
   slgall(Call,Anss,N0-Tab0,N-Tab) : 
   If Call is a prolog prologCall, findall is used, : Tab = Tab0;
   If Call is an atom of a tabled predicate, SLG evaluation
   is carried out.
*/
slgall(Call,Anss) :-
	slgall(Call,Anss,0 : [],_).
slgall(Call,Anss,N0 : Tab0,N : Tab) :-
        ( isPrologCurrently(Call) ->
          findall(Call,Call,Anss),
	  N = N0, Tab = Tab0
        ; wfs_all(Call,Answers,Tab0,Tab,N0,N),
          ansstree_to_list(Answers,Anss,[])
        ).

/* st(Call,PSM) : 
   stnot(Call,PSM) : 
   It finds a stable model in which Call must be true (false).
   Call must be stableGround.
*/
st(Call,PSM) :-
	( stableGround(Call) ->
	  stselect(Call,[Call],_,PSM)
        ; wfs_error('Error :  nonstableGround atom in st/2 :  ', Call)
        ).
	
stnot(Call,PSM) :-
	( stableGround(Call) ->
	  stselect(Call,[ /*neg*/ \+ Call],_,PSM)
        ; wfs_error('Error :  nonstableGround atom in stnot/2 :  ', Call)
        ).

tnot(Call):-
	stnot(Call,PSM).	


/* stall(Call,Anss,PSM) : 
   stall(Call,Anss,PSM,Tab0,Tab) : 
   It computes a partial stable model PSM : collects all
   answers of Call in that model.
*/
stall(Call,Anss,PSM) :-
	stselect(Call,[],Anss,PSM).
	
stall(Call,Anss,PSM,NTab0,NTab) :-
	stselect(Call,[],Anss,PSM,NTab0,NTab).

/* stselect(Call,PSM0,Anss,PSM) : 
   stselect(Call,PSM0,Anss,PSM,N0 : Tab0,N : Tab) : 
   It computes a partial stable model PSM in which all stableGround
   literals in PSM0 are true, : returns all answers of Call
   in the partial stable model.
*/
stselect(Call,PSM0,Anss,PSM) :-
	stselect(Call,PSM0,Anss,PSM,0 : [],_).

stselect(Call,PSM0,Anss,PSM,N0 : Tab0,N : Tab) :-
	( isPrologCurrently(Call) ->
	  findall(Call,Call,Anss),
	  PSM = [], Tab = Tab0, N = N0
        ; wfs_all(Call,Anss0,Tab0,Tab1,N0,N1,Delay),
	  ( Delay == 0 ->
	    Plits1 = [], Nlits1 = [], Nseq1 = [] 
	  ; relevant(Anss0,Tab1,[],Plits1,[],Nlits1,[],Nseq1)
          ),
	  PSM1 = [ /*neg*/ \+ inconsistent|PSM0],
	  relevant_psm(PSM1,PSM2,[],Tab1,Tab2,N1,N,Plits1,Plits,Nlits1,Nlits,Nseq1,Nseq),
          one_psm(PSM2,Tab2,Tab3,Plits,Nseq),
	  construct_psm(Plits,Nlits,Tab3,Tab,PSM,[]),
	  ( Delay == 0 ->
	    ansstree_to_list(Anss0,Anss,[])
	  ; findall(Call,member(Call,PSM),Anss)
          )
        ).
/*
        ; wfs_all(inconsistent,IAnss,Tab0,Tab1,N0,N1,IDelay),
	  ( succeeded(IAnss) ->
	    write('*** Constraints violated in the WFS of the program!'), 
	    nl, fail
	  ; wfs_all(Call,Anss0,Tab1,Tab2,N1,N2,Delay),
	    ( IDelay == 0, Delay == 0 ->
	      ansstree_to_list(Anss0,Anss,[]),
	      PSM = [], Tab = Tab2, N = N2
	    ; relevant(Anss0,Tab2,[],Plits1,[],Nlits1,[],Nseq1),
	      relevant_psm(PSM0,Tab2,Tab3,N2,N,Plits1,Plits2,Nlits1,Nlits2,Nseq1,Nseq2),
	      ( failed(IAnss) ->
		PSM1 = PSM0, Plits = Plits2, Nlits = Nlits2, Nseq = Nseq2
	      ; relevant(IAnss,Tab1,Plits2,Plits,Nlits2,Nlits,Nseq2,Nseq),
	        PSM1 = [ /*neg*/ \+ inconsistent|PSM0]
	      ),
	      one_psm(PSM1,Tab3,Tab4,Plits,Nseq),
	      construct_psm(Plits,Nlits,Tab4,Tab,PSM,[]),
	      findall(Call,member(Call,PSM),Anss)
	    )
          )
        ).
*/

relevant_psm([],PSM,PSM,Tab,Tab,N,N,Plits,Plits,Nlits,Nlits,Nseq,Nseq).

relevant_psm([A|PSM0],PSM1,PSM,Tab0,Tab,N0,N,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	( A = ( /*neg*/ \+ G) -> true; A = G ),
  
        ( 
	
  %isPrologCurrently(G) ->
   %       wfs_error('Error :  Prolog predicate in stselect :  ', A)
	%; 
	stableGround(G) ->
	  wfs_all(G,Anss,Tab0,Tab1,N0,N1,Delay),
	  ( failed(Anss), A=G ->
	    fail
	  ; succeeded(Anss), A = ( /*neg*/ \+ G) ->
	    fail
	  ; Delay == 0 ->
	    relevant_psm(PSM0,PSM1,PSM,Tab1,Tab,N1,N,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq)
	  ; relevant(Anss,Tab1,Plits0,Plits1,Nlits0,Nlits1,Nseq0,Nseq1),
	    PSM1 = [A|PSM2],
	    relevant_psm(PSM0,PSM2,PSM,Tab1,Tab,N1,N,Plits1,Plits,Nlits1,Nlits,Nseq1,Nseq)
	  )
        ; wfs_error('Error :  non-stableGround literal in stselect :  ', A)
	).

/* stintall(Call,Anss) : 
   computes the set of answers of Call that are true in all two-valued
   partial stable models of the portion of a program that is relevant
   to Call, in which all integrity constraints are satisfied.
*/
stintall(Call,Anss) :-
	( isPrologCurrently(Call) ->
	  findall(Call,Call,Anss)
        ; wfs_all(inconsistent,IAnss,[],Tab1,0,N1,IDelay),
	  ( succeeded(IAnss) ->
	    write('*** Constraints violated in the WFS of the program!'), 
	    nl, fail
	  ; wfs_all(Call,Anss0,Tab1,Tab2,N1,_N2,Delay),
	    ( IDelay == 0, Delay == 0 ->
	      ansstree_to_list(Anss0,Anss,[])
	    ; extract_wfs(Anss0,Tab2,Hlist,[],Anss,Anss1,[],Plits1,[],Nlits1,[],Nseq1),
	      ( failed(IAnss) ->
		Tab3 = Tab2, Plits = Plits1, Nlits = Nlits1, Nseq = Nseq1
	      ; relevant(IAnss,Tab2,Plits1,Plits,Nlits1,Nlits,Nseq1,Nseq),
	        assume_one(inconsistent,false,Tab2,Tab3,Plits)
	      ),
	      ( one_psm(Tab3,Tab4,Plits,Nseq),
	        construct_psm(Plits,Nlits,Tab4,_Tab,_PSM,[]) ->
		stableGround(Call,Ggoal),
		check_each_ans(Hlist,Ggoal,Tab3,Anss1,[])
	      ; fail % no model exists
	      )
	    )
          )
        ).

extract_wfs(Tree,Tab,Hlist0,Hlist,Anss0,Anss,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	( Tree = [] ->
	  Hlist0 = Hlist, Anss0 = Anss, Plits = Plits0, Nlits = Nlits0, Nseq = Nseq0
        ; Tree = l(GH,Lanss) ->
	  ( Lanss = [] ->
	    Hlist0 = Hlist, Anss0 = Anss, Plits = Plits0, Nlits = Nlits0, Nseq = Nseq0
	  ; Lanss = [d(H,[])] ->
	    Hlist0 = Hlist, Anss0 = [H|Anss], Plits = Plits0, Nlits = Nlits0, Nseq = Nseq0
	  ; Lanss = [d(H,_)|_], 
	    Hlist0 = [Tree|Hlist], Anss0 = Anss, 
            ( addkey(Plits0,GH,v(_Val,_Con,H),Plits1) ->
              relevant_lanss(Lanss,Tab,Plits1,Plits,Nlits0,Nlits,Nseq0,Nseq)
            ; Plits = Plits0, Nlits = Nlits0, Nseq = Nseq0
            )
	  )
        ; Tree = n2(T1,_,T2) ->
	  extract_wfs(T1,Tab,Hlist0,Hlist1,Anss0,Anss1,Plits0,Plits1,Nlits0,Nlits1,Nseq0,Nseq1),
	  extract_wfs(T2,Tab,Hlist1,Hlist,Anss1,Anss,Plits1,Plits,Nlits1,Nlits,Nseq1,Nseq)
        ; Tree = n3(T1,_,T2,_,T3) ->
	  extract_wfs(T1,Tab,Hlist0,Hlist1,Anss0,Anss1,Plits0,Plits1,Nlits0,Nlits1,Nseq0,Nseq1),
	  extract_wfs(T2,Tab,Hlist1,Hlist2,Anss1,Anss2,Plits1,Plits2,Nlits1,Nlits2,Nseq1,Nseq2),
	  extract_wfs(T3,Tab,Hlist2,Hlist,Anss2,Anss,Plits2,Plits,Nlits2,Nlits,Nseq2,Nseq)
        ).

check_each_ans([],_Ggoal,_Tab,Anss,Anss).
check_each_ans([l(GH,Lanss)|Hlist],Ggoal,Tab,Anss0,Anss) :-
        ( not(not(canbe_false(GH,Ggoal,Lanss,Tab))) ->
	  Anss0 = Anss1
        ; Lanss = [d(H,_)|_], 
	  Anss0 = [H|Anss1]
        ),
	check_each_ans(Hlist,Ggoal,Tab,Anss1,Anss).

canbe_false(GH,Ggoal,Lanss,Tab0) :-
	relevant_anshead(GH,Lanss,_Val,Tab0,[],Plits,[],Nlits,[],Nseq),
	assume_one((Ggoal-GH),false,Tab0,Tab1,Plits),
	one_psm(Tab1,Tab2,Plits,Nseq),
	construct_psm(Plits,Nlits,Tab2,_Tab,_PSM,[]).

logComplement(true,false).
logComplement(false,true).

/* relevant(Anss,Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) : 
   Plits0/Plists is the accumulator for (the head atoms 
   of) all answers in Tab that are relevant to Ggoal, 
   where Anss is the set of answers for Ggoal; : 
   Nlits0/Nlists is the accumulator for stableGround calls
   whose negation has been delayed.
*/
relevant([],_Tab,Plits,Plits,Nlits,Nlits,Nseq,Nseq).
relevant(l(GH,Lanss),Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	( find(Plits0,GH,_) ->
	  Plits = Plits0, Nlits = Nlits0, Nseq = Nseq0
        ; relevant_anshead(GH,Lanss,_Val,Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq)
        ).
relevant(n2(T1,_,T2),Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	relevant(T1,Tab,Plits0,Plits1,Nlits0,Nlits1,Nseq0,Nseq1),
	relevant(T2,Tab,Plits1,Plits,Nlits1,Nlits,Nseq1,Nseq).
relevant(n3(T1,_,T2,_,T3),Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	relevant(T1,Tab,Plits0,Plits1,Nlits0,Nlits1,Nseq0,Nseq1),
	relevant(T2,Tab,Plits1,Plits2,Nlits1,Nlits2,Nseq1,Nseq2),
	relevant(T3,Tab,Plits2,Plits,Nlits2,Nlits,Nseq2,Nseq).

relevant_anshead(GH,Lanss,Val,Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	addkey(Plits0,GH,v(Val,Con,H),Plits1),
	( Lanss == [] -> 
	  Val = false, Con = true,
	  Plits = Plits1, Nlits = Nlits0, Nseq = Nseq0
        ; Lanss = [d(H,[])] -> 
	  Val = true, Con = true,
	  Plits = Plits1, Nlits = Nlits0, Nseq = Nseq0
	; Lanss = [d(H,_)|_],
	  relevant_lanss(Lanss,Tab,Plits1,Plits,Nlits0,Nlits,Nseq0,Nseq)
        ).

relevant_lanss([],_Tab,Plits,Plits,Nlits,Nlits,Nseq,Nseq).
relevant_lanss([d(_,D)|Lanss],Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	relevant_list(D,Tab,Plits0,Plits1,Nlits0,Nlits1,Nseq0,Nseq1),
	relevant_lanss(Lanss,Tab,Plits1,Plits,Nlits1,Nlits,Nseq1,Nseq).

relevant_list([],_Tab,Plits,Plits,Nlits,Nlits,Nseq,Nseq).
relevant_list(all(D),_Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	( D = [] ->
	  Plits = Plits0, Nlits = Nlits0, Nseq = Nseq0
        ; wfs_error('Error :  universal disjunction in relevant_list :  ',
	            all(D))
	).
relevant_list([Lit|D],Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	relevant_lit(Lit,Tab,Plits0,Plits1,Nlits0,Nlits1,Nseq0,Nseq1),
	relevant_list(D,Tab,Plits1,Plits,Nlits1,Nlits,Nseq1,Nseq).

relevant_lit(( /*neg*/ \+ A),Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	( find(Nlits0,A,_) ->
	  Plits = Plits0, Nlits0 = Nlits, Nseq = Nseq0
        ; addkey(Nlits0,A,Val,Nlits1),
	  Nseq1 = [(A,Val)|Nseq0],
          relevant_pos_lit(A,A,Val,Tab,Plits0,Plits,Nlits1,Nlits,Nseq1,Nseq)
        ).
relevant_lit((Ggoal - GH),Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	relevant_pos_lit(Ggoal,GH,_,Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq).

relevant_pos_lit(Ggoal,GH,Val,Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq) :-
	( find(Plits0,GH,v(Val0,_,_)) ->
	  Val0 = Val, Plits = Plits0, Nlits = Nlits0, Nseq = Nseq0
        ; find(Tab,Ggoal,Ent),
	  ent_to_anss(Ent,Anss),
	  find(Anss,GH,Lanss),
	  relevant_anshead(GH,Lanss,Val,Tab,Plits0,Plits,Nlits0,Nlits,Nseq0,Nseq)
        ).

one_psm(Tab0,Tab,Plits,Nseq) :-
	one_psm([],Tab0,Tab,Plits,Nseq).

one_psm(PSM0,Tab0,Tab,Plits,Nseq) :-
	assume_list(PSM0,true,Tab0,Tab1,Plits),
	st_enum(Nseq,Plits,Tab1,Tab).

assume_list([],_Val,Tab,Tab,_Plits).
assume_list([Lit|Lits],Val,Tab0,Tab,Plits) :-
	assume_one(Lit,Val,Tab0,Tab1,Plits),
	assume_list(Lits,Val,Tab1,Tab,Plits).

stableVar(v(_,V,_)):-!,nonvar(V).
stableVar(V):-nonvar(V).
	
assume_one(Lit,Val,Tab0,Tab,Plits) :-
	assume_one_val(Lit,Val,Ggoal,GH,AVal),
	find(Plits,GH,v(Val0,_Con,_)),
	( stableVar(Val0) ->
	  Val0 = AVal, Tab = Tab0
        ; Val0 = AVal,
	  propagate_forward(Ggoal,GH,AVal,Tab0,Tab1,Plits),
	  propagate_backward(Ggoal,GH,AVal,Tab1,Tab,Plits)
        ).

assume_one_val(Lit,Val,Ggoal,GH,AVal) :-
	( Lit = ( /*neg*/ \+ A) ->
	  Ggoal = A, GH = A, 
	  logComplement(Val,AVal)
        ; Lit = (Ggoal - GH) ->
	  AVal = Val
        ; Ggoal = Lit, GH = Lit, 
	  AVal = Val
        ).

/* propagate_forward(Ggoal,GH,Val,Tab0,Tab,Plits) : 
   G has been assumed to be Val, : this 
   information is propagated using simplification or 
   forward chaining links as much as possible.
*/
propagate_forward(Ggoal,GH,Val,Tab0,Tab,Plits) :-
	updatevs(Tab0,Ggoal,Ent0,Ent,Tab1),
	Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn,Slist0),
	Ent = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn,Slist),
	extract_known_by_abd(Slist0,GH,Val,Slist,Klist),
	simplify(Klist,Tab1,Tab,Plits).

/* The forward chaining is such that negative literals can fail 
   or succeed by assumption, : positive literals can fail 
   by assumption, but cannot succeed by assumption.
   This avoids the construction of supported models that are 
   not stable.

   Klist is organized in such a way that self-looping links : 
   links with negative literals are put before others. The heuristics
   is that the simplification of such links may lead to 
   contradiction sooner if there is no stable model.
*/
extract_known_by_abd(Slist0,GH,Val,Slist,Klist) :-
	known_by_abd(Slist0,GH,Val,Slist,[],Klist,Ks,Ks,Kn,Kn,[]).

known_by_abd([],_GH,_Val,Slist,Slist,Ks,Ks,Kn,Kn,Kr,Kr).
known_by_abd([Link|Links],GH,Val,Slist0,Slist,Ks0,Ks,Kn0,Kn,Kr0,Kr) :-
	( Link = (GHead  :  ( /*neg*/ \+  GH)) ->
	  logComplement(Val,NVal),
	  Slist0 = Slist1, 
	  ( GHead = (GH-GH) -> % looping links
	    Ks0 = [NVal-Link|Ks1], Kn0 = Kn1, Kr0 = Kr1
	  ; Ks0 = Ks1, Kn0 = [NVal-Link|Kn1], Kr0 = Kr1
          )
        ; Val = false, Link = (GHead  :  _-GH) ->
	  Slist0 = Slist1,
	  ( GHead = (GH-GH) ->
	    Ks0 = [false-Link|Ks1], Kn0 = Kn1, Kr0 = Kr1
	  ; Ks0 = Ks1, Kn0 = Kn1, Kr0 = [false-Link|Kr1]
          )
	; Slist0 = [Link|Slist1], Ks0 = Ks1, Kn0 = Kn1, Kr0 = Kr1
        ),
	known_by_abd(Links,GH,Val,Slist1,Slist,Ks1,Ks,Kn1,Kn,Kr1,Kr).

/* propagate_backward(Ggoal,GH,Val,Tab0,Tab,Plits) : 
   It propagates the Val of GH backward through answers
   if possible. If GH is assumed to be true, : GH has only one
   answer clause, then all literals in the body of the answer
   clause must be true. If GH is assumed to be false, then all
   literals in answer clauses of GH that have a single literal
   are assumed to be false too. Otherwise, it is no-op.
*/

/*
% aggresive back propagation
propagate_backward(Ggoal,GH,Val,Tab0,Tab,Plits) :-
	find(Tab0,Ggoal,Ent),
	ent_to_anss(Ent,Anss),
	find(Anss,GH,Lanss),
	( Val == true ->
	  member(d(_,Ds),Lanss),
	  assume_list(Ds,true,Tab0,Tab,Plits)
        ; Val == false ->
	  assume_lanss_false(Lanss,Tab0,Tab,Plits)
        ).

assume_lanss_false([],Tab,Tab,_Plits).
assume_lanss_false([d(_,Ds)|Lanss],Tab0,Tab,Plits) :-
	member(Lit,Ds),
	assume_one(Lit,false,Tab0,Tab1,Plits),
	assume_lanss_false(Lanss,Tab1,Tab,Plits).
*/

% limited back propagation
propagate_backward(Ggoal,GH,Val,Tab0,Tab,Plits) :-
	find(Tab0,Ggoal,Ent),
	ent_to_anss(Ent,Anss),
	find(Anss,GH,Lanss),
	( Val == true, Lanss = [d(_,Ds)] ->
	  assume_list(Ds,true,Tab0,Tab,Plits)
        ; Val == false, findall(Lit,member(d(_,[Lit]),Lanss),Ds) ->
	  assume_list(Ds,false,Tab0,Tab,Plits)
        ; Tab = Tab0
        ).
	


/* st_enum(Nseq,Plits,Tab0,Tab) : 
   Nlits is a list where each node consists of a key 
   that is a stableGround atom whose negation is a delayed literal,
   : a value (not necessarily bound). st_enum traverses
   Nseq, : makes sure that every stableGround atom is 
   assigned some value.
*/
st_enum([],_Plits,Tab,Tab).
st_enum([(A,Val)|Nseq],Plits,Tab0,Tab) :-
	( stableVar(Val) -> % already has a value
	  Tab2 = Tab0
        ; ( Val = true; Val = false ),
	  wfs_trace(writeDebug('Choice point :  ' : A=Val)),
	  propagate_forward(A,A,Val,Tab0,Tab1,Plits),
	  propagate_backward(A,A,Val,Tab1,Tab2,Plits),
	  wfs_trace((
	    show_tree('Table after propagate_forward :  ', Tab1),
	    show_tree('Table after propagate_backward :  ', Tab2),
	    show_tree('Plits after propagation :  ',Plits)
          ))
        ),
	st_enum(Nseq,Plits,Tab2,Tab).

/* construct_psm(Plits,Nlits,Tab0,Tab,PSM0,PSM) : 
   A proposition may be assumed to be true, but no true answer
   is derived at the end, which is inconsistency.

   Also at the end of a fixpoint computation, a subgoal may
   have only delayed answers with positive literals. These
   have to be deleted in order for Tab0/Tab to be used
   correctly later.
*/
construct_psm([],_Nlits,Tab,Tab,PSM,PSM).
construct_psm(l(_,v(Val,Con,H)),Nlits,Tab0,Tab,PSM0,PSM) :-
	( Con == true -> % confirmed with the table
	  ( Val == true ->
	    PSM0 = [H|PSM], Tab = Tab0
	  ; Val == false, stableGround(H) ->
	    ( find(Nlits,H,Val) ->
	      PSM0 = [ /*neg*/ \+ H|PSM], Tab = Tab0
	    ; PSM0 = PSM, Tab = Tab0
	    )
	  ; PSM0 = PSM, Tab = Tab0
          )
        ; Val == true -> % assumption of true not confirmed
          fail
        ; Val = false -> % unconfirmed false
	  Tab = Tab0,    % this table needs to be updated to delete delayed answers
	  ( stableGround(H), find(Nlits,H,Val) ->
	    PSM0 = [ /*neg*/ \+ H|PSM]
	  ; PSM0 = PSM
          )
        ).
construct_psm(n2(T1,_,T2),Nlits,Tab0,Tab,PSM0,PSM) :-
	construct_psm(T1,Nlits,Tab0,Tab1,PSM0,PSM1),
	construct_psm(T2,Nlits,Tab1,Tab,PSM1,PSM).
construct_psm(n3(T1,_,T2,_,T3),Nlits,Tab0,Tab,PSM0,PSM) :-
	construct_psm(T1,Nlits,Tab0,Tab1,PSM0,PSM1),
	construct_psm(T2,Nlits,Tab1,Tab2,PSM1,PSM2),
	construct_psm(T3,Nlits,Tab2,Tab,PSM2,PSM).

/* top-level calls for query answering under the 
   well-founded semantics
*/
wfs_all(Call,Anss) :-
    wfs_all(Call,Anss,[],_Tab,0,_N,_Delay).

wfs_all(Call,Anss,Tab0,Tab,N0,N) :-
    wfs_all(Call,Anss,Tab0,Tab,N0,N,_Delay).

wfs_all(Call,Anss,Tab,Delay) :-
    wfs_all(Call,Anss,[],Tab,0,_N,Delay).

wfs_all(Call,Anss,Tab0,Tab,N0,N,Delay) :-
    stableGround(Call,Ggoal),
    ( find(Tab0,Ggoal,Ent) ->
      ent_to_anss(Ent,Anss),
      ent_to_delay(Ent,Delay),
      Tab = Tab0, N = N0
    ; new_init_call(Ggoal,Ent0,[],S1,1,Dfn1),
      add_tab_ent(Ggoal,Ent0,Tab0,Tab1),
      oldt(Call,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxint-maxint,_Dep,N0 : [],N : _TP),
      find(Tab,Ggoal,Ent),
      ent_to_anss(Ent,Anss),
      ent_to_delay(Ent,Delay)
    ).

/* oldt(Call,Ggoal,Tab0,Tab,Stack0,Stack,DFN0,DFN,Dep0,Dep,TP0,TP)
   explores the initial set of edges, i.e., all the 
   program clauses for Call. Ggoal is of the form 
   Gcall-Gdfn, where Gcall is numbervar of Call : Gdfn
   is the depth-first number of Gcall. Tab0/Tab,Stack0/Stack,
   DFN0/DFN, : Dep0/Dep are accumulators for the table, 
   the stack of subgoals, the DFN counter, : the dependencies.
   TP0/TP is the accumulator for newly created clauses during
   the processing of general clauss with universal disjunctions
   in the body. These clauses are created in order to guarantee
   polynomial data complexity in processing clauses with
   universal disjuntions in the body of a clause. The newly 
   created propositions are represented by numbers.
*/

% :- assertLogged('slg$tabled'(_,_)).

oldt(Call,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-%trace,
    ( number(Call) ->
      TP0 = (_  :  Tcl),
      find(Tcl,Call,Clause),
      edge_oldt(Clause,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1)
    ; % findall(rule(d(Call,[]),Body),rule(Call,Body),Frames),
	findall(rule(d(Call,[]),Body),( prologSLGCall(Call,Body)  /*new_slg_head(Call,Body,NewHead), prologSLGCall(NewHead) */ ),Frames),
	map_oldt(Frames,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1)
    ),
    comp_tab_ent(Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_oldt([],_Ggoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_oldt([Clause|Frames],Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-%trace,
  edge_oldt(Clause,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
  map_oldt(Frames,Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* edge_oldt(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   Clause may be one of the following forms : 
          rule(d(H,Dlist),Blist)
          rule(d(H,all(Dlist)),all(Blist))
   where the second form is for general clauses with a universal
   disjunction of literals in the body. Dlist is a list of delayed 
   literals, : Blist is the list of literals to be solved.
   Clause represents a directed edge from Ggoal to the left most 
   subgoal in Blist.
*/
edge_oldt(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(Ans,B),
    ( B == [] ->
      ans_edge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; B = [Lit|_] ->
      ( Lit = ( /*neg*/ \+ N) ->
        neg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; pos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      )
    ; B = all(Bl) ->
      ( Bl == [] ->
        ans_edge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; Bl = [Lit|_],
        ( Lit = ( /*neg*/ \+ N) ->
          aneg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        ; apos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      )
    ).

ans_edge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( add_ans(Tab0,Ggoal,Ans,Nodes,Mode,Tab1) -> 
      ( Mode = new_head -> 
        returned_ans(Ans,Ggoal,RAns),
        map_nodes(Nodes,RAns,Tab1,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; Mode = no_new_head ->
        Tab = Tab1, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
      )
    ; Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
    ).

neg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(_R,[ /*neg*/ (\+ N)|_L]),
    ( stableGround(N) -> true
    ; /*wfs_trace*/
    ((write('Flounder :  '), write(R -> [/*neg*/ \+ N|_L]), nl , 
	nonvar(N),
	not(recorded(N,N)) ->
		(copy_term(N,NC),
		sigma_numbervars(NC),
		recorda(N,NC)) ; fail ))
	),
    Node = (Ggoal : Clause),
    Ngoal = N,                 % N is already stableGround
    (	       /*
      isPrologCurrently(N) ->           % if N is a Prolog predicate
      ( 
      prologCall(N) ->             %    then just prologCall
        Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
      ; 
	apply_subst(Node,d( /*neg*/ \+  N,[]),Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ); */
      
      ( find(Tab0,Ngoal,Nent) ->
        Tab2 = Tab0, S2 = S0, Dfn2 = Dfn0, Dep1 = Dep0, TP1 = TP0
      ; new_init_call(Ngoal,Ent,S0,S1,Dfn0,Dfn1),
	add_tab_ent(Ngoal,Ent,Tab0,Tab1),
	oldt(N,Ngoal,Tab1,Tab2,S1,S2,Dfn1,Dfn2,maxint-maxint,Ndep,TP0,TP1),
	compute_mins(Dep0,Ndep,pos,Dep1),
        find(Tab2,Ngoal,Nent)
      ),
      ent_to_comp(Nent,Ncomp),
      ent_to_anss(Nent,Nanss),
      ( succeeded(Nanss) ->
	Tab = Tab2, S = S2, Dfn = Dfn2, Dep = Dep1, TP = TP1
      ; failed(Nanss), Ncomp == true ->
        apply_subst(Node,d( /*neg*/ \+ N,[]),Tab2,Tab,S2,S,Dfn2,Dfn,Dep1,Dep,TP1,TP)
      ; apply_subst(Node,d( /*neg*/ \+ N,[ /*neg*/ \+ N]),Tab2,Tab,S2,S,Dfn2,Dfn,Dep1,Dep,TP1,TP)
      )
    ).

pos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(_H, [N| _B]),
    Node = (Ggoal : Clause),
    stableGround(N,Ngoal),
    ( isPrologCurrently(N) ->
      findall(d(N,[]),prologCall(N),Nanss),
      map_anss_list(Nanss,Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; ( find(Tab0,Ngoal,Nent) ->
        ent_to_comp(Nent,Ncomp),
        ent_to_anss(Nent,Nanss),
        ( Ncomp \== true ->
          update_lookup_mins(Ggoal,Node,Ngoal,pos,Tab0,Tab1,Dep0,Dep1),
          map_anss(Nanss,Node,Ngoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep1,Dep,TP0,TP)
        ; % N is completed. 
          map_anss(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      ; % otherwise N is new
        new_pos_call(Ngoal,Node,Ent,S0,S1,Dfn0,Dfn1),
        add_tab_ent(Ngoal,Ent,Tab0,Tab1),
        oldt(N,Ngoal,Tab1,Tab2,S1,S,Dfn1,Dfn,maxint-maxint,Ndep,TP0,TP),
        update_solution_mins(Ggoal,Ngoal,pos,Tab2,Tab,Ndep,Dep0,Dep)
      )
    ).

aneg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(H,all([ /*neg*/ \+ N|Bs])),
    Node = (Ggoal : Clause),
    stableGround(N,Ngoal),
    ( isPrologCurrently(N) ->
      extract_prolog(Bs,N,PN,RestBs),
      findall(d(PN,[]),prologCall(PN),Nanss),
      NewNode = (Ggoal : rule(H,all([ /*neg*/ \+ PN|RestBs]))),
      return_to_disj_list(Nanss,NewNode,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; stableGround(N) ->
      neg_edge(rule(H,[ /*neg*/ \+ N]),Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
      ( Bs == [] ->
        Tab = Tab1, S = S1, Dfn = Dfn1, Dep = Dep1, TP = TP1
      ; edge_oldt(rule(H,all(Bs)),Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP)
      )
    ; ( find(Tab0,Ngoal,Nent) ->
        ent_to_comp(Nent,Ncomp),
        ent_to_anss(Nent,Nanss),
        ( Ncomp \== true ->
          update_lookup_mins(Ggoal,Node,Ngoal,aneg,Tab0,Tab,Dep0,Dep),
          S = S0, Dfn = Dfn0, TP = TP0
        ; % N is completed. 
          return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      ; % otherwise N is new
        new_aneg_call(Ngoal,Node,Ent,S0,S1,Dfn0,Dfn1),
        add_tab_ent(Ngoal,Ent,Tab0,Tab1),
        oldt(N,Ngoal,Tab1,Tab2,S1,S,Dfn1,Dfn,maxint-maxint,Ndep,TP0,TP),
        update_solution_mins(Ggoal,Ngoal,pos,Tab2,Tab,Ndep,Dep0,Dep)
      )
    ).

apos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(d(H,D),all([N|B])),
    ( stableGround(N) -> true
    ; write('Flounder in a universal disjunction :  '), 
      write(N), 
      nl, 
      fail
    ),
    pos_edge(rule(d(H,[]),[N]),Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    edge_oldt(rule(d(H,D),all(B)),Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

extract_prolog([],PN,PN,[]).
extract_prolog([Lit|Bs],PN0,PN,RestBs) :-
	( Lit = ( /*neg*/ \+ N) ->
	  CN = N
        ; Lit = N, CN = ( /*neg*/ \+ N)
        ),
	( isPrologCurrently(N) ->
	  extract_prolog(Bs,(PN0,CN),PN,RestBs)
        ; PN = PN0, RestBs = [Lit|Bs]
        ).

apply_subst(Ggoal : Cl,d(An,Vr),Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    copy_term(Cl,rule(d(Ac,Vc),Body)),
    ( Body = [Call|NBody] ->
      Call = An,
      append(Vr,Vc,Vn)
    ; Body = all([Call|Calls]),
      % Call = An,              % An is the numbervar-ed version of Call.
      ( Vc == [] ->
        Vn = all(Vr)
      ; Vc = all(Vc0),
        append(Vr,Vc0,Vn0),
        Vn = all(Vn0)
      ),
      NBody = all(Calls)
    ),
    edge_oldt(rule(d(Ac,Vn),NBody),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP).

/* map_nodes(Nodes,Ans,....) : 
   return Ans to each of the waiting nodes in Nodes, where a node
   is of the form Ggoal : Clause.
*/  
map_nodes([],_Ans,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_nodes([Node|Nodes],Ans,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    apply_subst(Node,Ans,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_nodes(Nodes,Ans,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_anss([],_Node,_Ngoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_anss(l(_GH,Lanss),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( Lanss == [] ->
      Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
    ; Lanss = [Ans|_],
      returned_ans(Ans,Ngoal,RAns),
      apply_subst(Node,RAns,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ).
map_anss(n2(T1,_,T2),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_anss(T1,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anss(T2,Node,Ngoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).
map_anss(n3(T1,_,T2,_,T3),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_anss(T1,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anss(T2,Node,Ngoal,Tab1,Tab2,S1,S2,Dfn1,Dfn2,Dep1,Dep2,TP1,TP2),
    map_anss(T3,Node,Ngoal,Tab2,Tab,S2,S,Dfn2,Dfn,Dep2,Dep,TP2,TP).

map_anss_list([],_Node,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_anss_list([Ans|Lanss],Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    apply_subst(Node,Ans,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anss_list(Lanss,Node,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   Nanss :  an answer table for Ngoal
   Node :  is of the form (Ggoal : Clause), where Clause is of the form
         rule(d(H,D),all([ /*neg*/ \+ N|B]))
   It carries out resolution of each answer with Clause, : constructs
   a new clause rule(Head,NBody), where the body is basically a 
   conjunction of all the resolvents. If a resolvent is a disjunction
   or a non-stableGround literal, a new proposition is created (which is 
   actually represented by a number), which has a clause whose body
   is the resolvent.
*/
return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Node = (Ggoal  :  Clause),
    Clause = rule(Head,all(Body)),
    TP0 = (N0  :  Tcl0),
    negative_return_all(Nanss,Body,Ngoal,NBody,[],N0,N,Tcl0,Tcl),
    TP1 = (N  :  Tcl),
    edge_oldt(rule(Head,NBody),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP1,TP).

negative_return_all([],_Body,_Ngoal,NBody,NBody,N,N,Tcl,Tcl).
negative_return_all(l(_GH,Lanss),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    ( Lanss == [] ->
      NBody0 = NBody, N = N0, Tcl = Tcl0
    ; Lanss = [Ans|_],
      negative_return_one(Ans,Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl)
    ).
negative_return_all(n2(T1,_,T2),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    negative_return_all(T1,Body,Ngoal,NBody0,NBody1,N0,N1,Tcl0,Tcl1),
    negative_return_all(T2,Body,Ngoal,NBody1,NBody,N1,N,Tcl1,Tcl).
negative_return_all(n3(T1,_,T2,_,T3),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    negative_return_all(T1,Body,Ngoal,NBody0,NBody1,N0,N1,Tcl0,Tcl1),
    negative_return_all(T2,Body,Ngoal,NBody1,NBody2,N1,N2,Tcl1,Tcl2),
    negative_return_all(T3,Body,Ngoal,NBody2,NBody,N2,N,Tcl2,Tcl).

negative_return_one(d(H,Tv),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    copy_term(Body,[ /*neg*/ \+ Call|Bs]),
    H = Call,
    ( Tv == [] ->                    % no delay
      ( (Bs = [Lit], stableGround(Lit)) -> % resovlent is a stableGround literal
        NBody0 = [Lit|NBody],
        N = N0, Tcl = Tcl0
      ; Lit = N0,                    % otherwise, replace it with a number
        N is N0+1,
        NBody0 = [Lit|NBody],
        Clause = rule(d(Lit,[]),all(Bs)),
        add_tab_ent(Lit,Clause,Tcl0,Tcl)
      )
    ; ( stableGround(H) ->                 % if there is delay, always replace with number
	NewTv = [ /*neg*/ \+ H]
      ; stableGround(H,GH),
	NewTv = [Ngoal - ( /*neg*/ \+ GH)]
      ),
      Lit = N0,
      N is N0+1,
      NBody0 = [Lit|NBody],
      Clause = rule(d(Lit,all(NewTv)),all(Bs)),
      add_tab_ent(Lit,Clause,Tcl0,Tcl)
    ).

return_to_disj_list(Lanss,Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Node = (Ggoal  :  Clause),
    Clause = rule(Head,all(Body)),
    TP0 = (N0  :  Tcl0),
    negative_return_list(Lanss,Body,NBody,[],N0,N,Tcl0,Tcl),
    TP1 = (N  :  Tcl),
    edge_oldt(rule(Head,NBody),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP1,TP).

negative_return_list([],_Body,NBody,NBody,N,N,Tcl,Tcl).
negative_return_list([d(H,[])|Lanss],Body,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    copy_term(Body,[ /*neg*/ \+ Call|Bs]),
    H = Call,
    ( Bs = [Lit], stableGround(Lit) ->
      NBody0 = [Lit|NBody1],
      N1 = N0, Tcl1 = Tcl0
    ; Lit = N0,
      N1 is N0+1,
      NBody0 = [Lit|NBody1],
      Clause = rule(d(Lit,[]),all(Bs)),
      add_tab_ent(Lit,Clause,Tcl0,Tcl1)
    ),
    negative_return_list(Lanss,Body,NBody1,NBody,N1,N,Tcl1,Tcl).

/* comp_tab_ent(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   check if Ggoal : subgoals on top of it on the stack are
   completely evaluated.
*/
comp_tab_ent(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( Dep0 == maxint-maxint ->
      process_pos_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
    ; update_mins(Ggoal,Dep0,pos,Tab0,Tab1,Gdfn,Gdep),
      Gdep = Gpmin-Gnmin,
      ( Gdfn @=< Gpmin, Gnmin == maxint ->
        process_pos_scc(Ggoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
      ; Gdfn @=< Gpmin, Gdfn @=< Gnmin ->
        process_neg_scc(Ggoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
      ; Tab = Tab1, S0 = S, Dfn = Dfn0, Dep = Gdep, TP = TP0
      )
    ).

process_pos_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP) :-
    wfs_trace((
      write('Stack :  '), nl, display_stack(S0,Tab0),
      write('Completed prologCall found :  '), write(Ggoal), nl, 
      ignore(display_table(Tab0)),
      write('Completing calls ......'), nl, nl
    )),
    pop_subgoals(Ggoal,S0,S1,[],Scc),
    complete_comp(Scc,Tab0,Tab1,Alist,[]),
    return_aneg_nodes(Alist,Tab1,Tab,S1,S,Dfn0,Dfn,maxint-maxint,Dep,TP0,TP).

/* pop_subgoals(Ggoal,S0,S,Scc0,Scc)
   pop off the stack subgoals up to : including Ggoal
*/
pop_subgoals(Ggoal,S0,S,Scc0,Scc) :-
    S0 = [Sent|S1],
    ( Ggoal == Sent ->
      S = S1, 
      Scc = [Sent|Scc0]
    ; pop_subgoals(Ggoal,S1,S,[Sent|Scc0],Scc)
    ).

/* complete_comp(Scc,Tab0,Tab,Alist0,Alist) : 
   process the list Scc of subgoals that are 
   completely evaluated.
*/
complete_comp([],Tab,Tab,Alist,Alist).
complete_comp([Ggoal|Scc],Tab0,Tab,Alist0,Alist) :-
    complete_one(Ggoal,Tab0,Tab1,Alist0,Alist1),
    complete_comp(Scc,Tab1,Tab,Alist1,Alist).

/* complete_one(Ggoal,Tab0,Tab,Alist0,Alist)
   process one subgoal that has been completely 
   evaluated : 
   1. set its Nodes : Negs to [] : Comp to true;
   2. simplify its answers : set up links
      for further simplification later;
   3. use the truth value of Ggoal to simplify
      answers of other complete subgoals (possibly 
      including itself).
   4. set Alist0/Alist :  a list of negation nodes with
      universal disjunctions with associated answers
      for the selected negative literal.
*/
complete_one(Ggoal,Tab0,Tab,Alist0,Alist) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab1),
    Ent0 = e(_Nodes,ANegs,Anss0,Delay0,_Comp,Gdfn,Slist0),
    Ent = e([],[],Anss,Delay,true,Gdfn,Slist),
    ( Delay0 > 0 ->
      reduce_ans(Anss0,Anss,Tab0,Delay0,Delay),
      setup_simp_links(Anss,Ggoal,Slist0,Slist1,Tab1,Tab2)
    ; Delay = Delay0,
      Anss = Anss0,
      Tab2 = Tab1,
      Slist1 = Slist0
    ),
    extract_known(Ggoal,Anss,Slist1,Slist,Klist),
    simplify(Klist,Tab2,Tab,[]),
    ( ANegs == [] ->
      Alist0 = Alist
    ; Alist0 = [(Anss,Ggoal)-ANegs|Alist]
    ).

setup_simp_links([],_,Slist,Slist,Tab,Tab).
setup_simp_links(l(GH,Lanss),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links_list(Lanss,Ggoal-GH,Ggoal,Slist0,Slist,Tab0,Tab).
setup_simp_links(n2(T1,_,T2),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links(T1,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links(T2,Ggoal,Slist1,Slist,Tab1,Tab).
setup_simp_links(n3(T1,_,T2,_,T3),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links(T1,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links(T2,Ggoal,Slist1,Slist2,Tab1,Tab2),
    setup_simp_links(T3,Ggoal,Slist2,Slist,Tab2,Tab).

/* setup_simp_link_list(Lanss,Ggoal-GH,Ggoal,Slist0,Slist,Tab0,Tab)
   Ggoal-GH is to tell what portion of answers of Ggoal can be 
   simplified.
*/
setup_simp_links_list([],_,_,Slist,Slist,Tab,Tab).
setup_simp_links_list([d(_,D)|Anss],GHead,Ggoal,Slist0,Slist,Tab0,Tab) :-
    ( D = all(Ds) ->
      true
    ; Ds = D
    ),
    links_from_one_delay(Ds,GHead,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links_list(Anss,GHead,Ggoal,Slist1,Slist,Tab1,Tab).

/* A link ((Ggoal-GH) : Lit) in an entry for Ngoal means that 
   the literal Lit in an answer with head GH in Ggoal can 
   be potentially simplified if we know answers for Ngoal.
*/
links_from_one_delay([],_,_,Slist,Slist,Tab,Tab).
links_from_one_delay([D|Ds],GHead,Ggoal,Slist0,Slist,Tab0,Tab) :-
    ( D = ( /*neg*/ \+  Ngoal) ->
      ( Ggoal == Ngoal ->
        Tab1 = Tab0,
	Slist1 = [GHead : D|Slist0]
      ; add_link_to_ent(Tab0,Ngoal,GHead : D,Tab1),
	Slist1 = Slist0
      )
    ; D = (Ngoal-_) ->
      ( Ggoal == Ngoal ->
        Slist1 = [GHead : D|Slist0],
        Tab1 = Tab0
      ; Slist1 = Slist0,
        add_link_to_ent(Tab0,Ngoal,GHead : D,Tab1)
      )
    ),
    links_from_one_delay(Ds,GHead,Ggoal,Slist1,Slist,Tab1,Tab).

/* extract_known(Ggoal,Anss,Links,Slist,Klist) : 
   Given Ggoal : its answers Anss, : its 
   simplification Links, it partitioned Links 
   into Slist : Klist of links, where Klist 
   is a list of links that are known to be either
   true or false.

   Klist is either of the form Val-Links, or a
   list of the form Val-Link. In case of non-stableGround
   calls, the corresponding portion of Anss has to 
   be searched.
*/
extract_known(Ggoal,Anss,Links,Slist,Klist) :-
    ( failed(Anss) ->
      Klist = false-Links,
      Slist = []
    ; Anss = l(GH,Lanss) ->
      ( Ggoal == GH ->       % Ground or most general prologCall
	( memberchk(d(_,[]),Lanss) ->
	  Klist = true-Links,
	  Slist = []
        ; Klist = [],
	  Slist = Links
        )
      ; % non-stableGround prologCall
	extract_known_anss(Links,Anss,[],Slist,[],Klist)
      )
    ; % non-stableGround prologCall
      extract_known_anss(Links,Anss,[],Slist,[],Klist)
    ).
      
extract_known_anss([],_,Slist,Slist,Klist,Klist).
extract_known_anss([Link|Links],Anss,Slist0,Slist,Klist0,Klist) :-
    Link = (_ : Lit),
    extract_lit_val(Lit,Anss,true,Val),
    ( Val == undefined ->
      Slist1 = [Link|Slist0],
      Klist1 = Klist0
    ; Slist1 = Slist0,
      Klist1 = [Val-Link|Klist0]
    ),
    extract_known_anss(Links,Anss,Slist1,Slist,Klist1,Klist).

/* extract_lit_val(Lit,Anss,Comp,Val) : 
   extract the truth value of Lit according to Anss : Comp.
   In case of a non-stableGround calls, the corresponding portion
   of Anss has to be searched.
*/
extract_lit_val(Lit,Anss,Comp,Val) :-
    ( Lit = ( /*neg*/ \+  _) ->
      ( succeeded(Anss) ->
        Val = fail
      ; failed(Anss), Comp == true ->
        Val = succ
      ; Val = undefined
      )
    ; Lit = (_ - ( /*neg*/ \+ GH)) ->
      ( find(Anss,GH,Lanss) ->
        ( ( not(not(memberchk(d(GH,[]),Lanss)))) ->
          Val = fail
        ; Lanss == [], Comp == true ->
	  Val = succ
        ; Val = undefined
        )
      ; ( Comp == true ->
	  Val = succ
        ; Val = undefined
        )
      )
    ; Lit = (_-GH) ->
      ( find(Anss,GH,Lanss) ->
        ( (not(not(memberchk(d(GH,[]),Lanss)))) ->
          Val = succ
        ; Lanss == [], Comp == true ->
	  Val = fail
        ; Val = undefined
        )
      ; ( Comp == true ->
	  Val = fail
        ; Val = undefined
        )
      )
    ).

/* simplify(KnownLinks,Tab0,Tab,Plits) : 
   Given a list of KnownLinks, Tab0 : Abd,
   it tries to simplify answers according to
   KnownLinks. When a subgoal is found to be
   true or false according to answers, 
   consistency with assumed truth values in Abd
   is checked.
*/
simplify([],Tab,Tab,_Plits).
simplify([Val-Link|Klist],Tab0,Tab,Plits) :-
    simplify_one(Val,Link,Tab0,Tab1,Plits),
    simplify(Klist,Tab1,Tab,Plits).
simplify(Val-Links,Tab0,Tab,Plits) :-
    simplify_list(Links,Val,Tab0,Tab,Plits).	     

simplify_list([],_,Tab,Tab,_Plits).
simplify_list([Link|Links],Val,Tab0,Tab,Plits) :-
    Link = (_  :  Lit),
    ( ( Lit = ( /*neg*/ \+ _); Lit = (_ - ( /*neg*/ \+ _)) ) ->
      logComplement(Val,LVal)
    ; LVal = Val
    ),
    simplify_one(LVal,Link,Tab0,Tab1,Plits),
    simplify_list(Links,Val,Tab1,Tab,Plits).

simplify_one(Val,Link,Tab0,Tab,Plits) :-
    Link = ((Ngoal - GH)  :  Lit),
    updatevs(Tab0,Ngoal,Ent0,Ent,Tab1),
    Ent0 = e(Nodes,ANegs,Anss0,Delay0,Comp,Dfn,Slist0),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    ( updatevs(Anss0,GH,Lanss0,Lanss,Anss) ->
      simplify_anss(Lanss0,Val,Lit,[],Lanss,DVal),
      update_delay(Delay0,Lanss0,Lanss,Delay),
      ( stableVar(DVal) ->
        ( find(Plits,GH,v(AVal,Con,_)) ->
          AVal = DVal, Con = true  % check consistency with assumed value
        ; true
        ),
	extract_known_by_der(Slist0,GH,DVal,[],Slist,[],Klist),
	simplify(Klist,Tab1,Tab,Plits)
      ; Slist = Slist0, Tab = Tab1
      )
    ; Tab = Tab0
    ).

extract_known_by_der([],_GH,_Val,Slist,Slist,Klist,Klist).
extract_known_by_der([Link|Links],GH,Val,Slist0,Slist,Klist0,Klist) :-
	( Link = (_  :  ( /*neg*/ \+  GH)) ->
	  logComplement(Val,NVal),
	  Slist1 = Slist0, Klist1 = [NVal-Link|Klist0]
        ; Link = (_  :  _-GH) ->
	  Slist1 = Slist0,
	  Klist1 = [Val-Link|Klist0]
	; Slist1 = [Link|Slist0], Klist1 = Klist0
        ),
	extract_known_by_der(Links,GH,Val,Slist1,Slist,Klist1,Klist).

/* simplify_anss(List,Val,Lit,Lanss0,Lanss,DVal) : 
   Given a List of answers, Val of Lit, it 
   simplifies the List : construct a new list
   Lanss0/Lanss of answers.

   As soon as a true answer is detected, all
   other answers with the same head are deleted.
*/
simplify_anss([],_,_,Anss,Anss,DVal) :-
    ( Anss = [] ->
      DVal = false
    ; true
    ).
simplify_anss([Ans|Rest],Val,Lit,Anss0,Anss,DVal) :-
    ( simplify_ans(Ans,Val,Lit,NewAns) ->
      ( NewAns = d(_,[]) ->
        Anss = [NewAns], DVal = true
      ; Anss1 = [NewAns|Anss0],
        simplify_anss(Rest,Val,Lit,Anss1,Anss,DVal)
      )
    ; simplify_anss(Rest,Val,Lit,Anss0,Anss,DVal)
    ).

simplify_ans(Ans,Val,Lit,NewAns) :-
    Ans = d(H,Ds),
    ( Ds == [] ->
      NewAns = Ans
    ; Ds = all(Dlist) ->
      ( Val == false ->
        delete_lit(Dlist,Lit,NewDlist,[]),
        ( NewDlist == [] ->
          fail
        ; NewAns = d(H,all(NewDlist))
        )
      ; % Val == true ->
        ( memberchk(Lit,Dlist) ->
          NewAns = d(H,[])
        ; NewAns = Ans
        )
      )
    ; % Ds is a conjunction
      ( Val == false ->
        ( memberchk(Lit,Ds) ->
          fail
        ; NewAns = Ans
        )
      ; % Val == true ->
        delete_lit(Ds,Lit,NewDs,[]),
        NewAns = d(H,NewDs)
      )
    ).

/* delete_lit(Delays,Lit,Ds0,Ds) : 
   deletes Lit from Delays. Delays is 
   a list of delayed literals : it
   is guaranteed to have no duplicates.
*/
delete_lit([],_,Ds,Ds).
delete_lit([D|Rest],Lit,Ds0,Ds) :-
    ( D == Lit ->
      Ds0 = Rest
    ; Ds0 = [D|Ds1],
      delete_lit(Rest,Lit,Ds1,Ds)
    ).

% return answers to negative nodes within universal disjunctions
return_aneg_nodes([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
return_aneg_nodes([(Anss,Ngoal)-ANegs|Alist],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_anegs(ANegs,Anss,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    return_aneg_nodes(Alist,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_anegs([],_Anss,_Ngoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_anegs([Node|ANegs],Anss,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    return_to_disj(Anss,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anegs(ANegs,Anss,Ngoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* process a component of subgoals that may be involved in 
   negative loops.
*/
process_neg_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP) :-
    wfs_trace((
      write('Stack :  '), nl, display_stack(S0,Tab0),
      write('Possible negative loop :  '), write(Ggoal), nl, 
      ignore(display_table(Tab0))
    )),
    extract_subgoals(Ggoal,S0,Scc,[]),
    reset_nmin(Scc,Tab0,Tab1,Ds,[]),
    wfs_trace((write('Delaying :  '), display_dlist(Ds))),
    delay_and_cont(Ds,Tab1,Tab2,S0,S1,Dfn0,Dfn1,maxint-maxint,Dep1,TP0,TP1),
    recomp_scc(Scc,Tab2,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* extract_subgoals(Ggoal,S0,Scc0,Scc)
   extract subgoals that may be involved in negative loops,
   but leave the stack of subgoals intact.
*/
extract_subgoals(Ggoal,[Sent|S],[Sent|Scc0],Scc) :-
    ( Ggoal == Sent ->
      Scc0 = Scc
    ; extract_subgoals(Ggoal,S,Scc0,Scc)
    ).

/* reset_nmin(Scc,Tab0,Tab,Dnodes0,Dnodes)
   reset NegLink : collect all waiting nodes that need to be 
   delayed. Dnodes0/Dnodes is a difference list.
*/
reset_nmin([],Tab,Tab,Ds,Ds).
reset_nmin([Ggoal|Scc],Tab0,Tab,Ds0,Ds) :-
    get_and_reset_negs(Tab0,Ggoal,ANegs,Tab1),
    ( ANegs == [] ->
      Ds0 = Ds1
    ; Ds0 = [Ggoal-ANegs|Ds1]
    ),
    reset_nmin(Scc,Tab1,Tab,Ds1,Ds).

delay_and_cont([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
delay_and_cont([Ggoal-Negs|Dnodes],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_nodes(Negs,d( /*neg*/ \+ Ggoal,[ /*neg*/ \+ Ggoal]),Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    delay_and_cont(Dnodes,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

recomp_scc([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
recomp_scc([Ggoal|Scc],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    comp_tab_ent(Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    recomp_scc(Scc,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* routines for incremental update of dependency information
*/

/* update_mins(Ggoal,Dep,Sign,Tab0,Tab,Gdfn,Gdep)
   update the PosLink : NegLink of Ggoal according to 
   Dep : Sign
*/
update_mins(Ggoal,Dep,Sign,Tab0,Tab,Gdfn,Gdep) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn : Gdep0,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn : Gdep,Slist),
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    compute_mins(Gdep0,Dep,Sign,Gdep).

/* update_lookup_mins(Ggoal,Node,Ngoal,Sign,Tab0,Tab,Dep0,Dep)
   There is a lookup edge (Node) from Ggoal to Ngoal 
   with Sign. It adds Node to the corresponding waiting list
   in Ngoal : then update the dependencies of Ggoal.
*/
update_lookup_mins(Ggoal,Node,Ngoal,Sign,Tab0,Tab,Dep0,Dep) :-
    updatevs(Tab0,Ngoal,Ent0,Ent,Tab1),
    ( Sign == pos ->
      pos_to_newent(Ent0,Ent,Node)
    ; Sign == aneg ->
      aneg_to_newent(Ent0,Ent,Node)
    ),
    Ent0 = e(_,_,_,_,_,_Ndfn : Ndep,_),
    compute_mins(Dep0,Ndep,Sign,Dep),
    update_mins(Ggoal,Ndep,Sign,Tab1,Tab,_,_).

/* update_solution_mins(Ggoal,Ngoal,Sign,Tab0,Tab,Ndep,Dep0,Dep)
   There is an edge with Sign from Ggoal to Ngoal, where Ngoal is 
   a new subgoal. Ndep is the final dependency information of 
   Ngoal. Dep0/Dep is for the most recent enclosing new prologCall.
   This predicate is called after Ngoal is solved.
*/
update_solution_mins(Ggoal,Ngoal,Sign,Tab0,Tab,Ndep,Dep0,Dep) :-
    find(Tab0,Ngoal,Nent),
    ent_to_comp(Nent,Ncomp),
    ( Ncomp == true ->
      ( Ndep == maxint-maxint ->
        Tab = Tab0, Dep = Dep0
      ; update_mins(Ggoal,Ndep,pos,Tab0,Tab,_,_),
        compute_mins(Dep0,Ndep,pos,Dep)
      )
    ; update_mins(Ggoal,Ndep,Sign,Tab0,Tab,_,_),
      compute_mins(Dep0,Ndep,Sign,Dep)
    ).

compute_mins(Gpmin-Gnmin,Npmin-Nnmin,Sign,Newpmin-Newnmin) :-
    ( Sign == pos ->
      getMin(Gpmin,Npmin,Newpmin),
      getMin(Gnmin,Nnmin,Newnmin)
    ; % (Sign == neg; Sign == aneg) ->
      Newpmin=Gpmin,
      getMin(Gnmin,Npmin,Imin), 
      getMin(Imin,Nnmin,Newnmin)
    ).

update_delay(Delay0,Lanss0,Lanss,Delay) :-
    ( hasdelays(Lanss0),  not(hasdelays(Lanss)) ->
      Delay is Delay0 - 1
    ; not(hasdelays(Lanss0)), hasdelays(Lanss) ->
      Delay is Delay0 + 1
    ; Delay = Delay0
    ).

hasdelays([d(_,[_|_])|_]).

getMin(X,Y,M) :- ( X @< Y -> M=X; M=Y ).

%%%%%%%%%%%%%%% Local table manipulation predicates %%%%%%%%%%

/* Table Entry Structure : 
   For each Call, its table entry is identified with its number-vared
   version -- Ggoal. Its value is a term of the form

    e(Nodes,ANegs,Anss,Delay,Comp,Dfn : Dep,Slist)

   where
     Nodes :   positive suspension list
     ANegs :   negative suspension list (for universal disjunction clauss)
     Anss :    another table.
     Delay :   a counter of answer heads with delays
     Comp :    whether Call is completely evaluated or not
     Dfn :     depth-first number of Gcall
     Dep :     (PosLink-NegLink) --- dependency information
     Slist :   a list of nodes whose answers may be simplified
             if the truth value of Ggoal is known. Each element of Slist
         is of the form (Ngoal-GH) : Literal.
   Stack Entry Structure : 
     Ggoal
*/

/* routines for accessing individual fields of an entry
*/
ent_to_nodes(e(Nodes,_,_,_,_,_,_),Nodes).
	ent_to_anegs(e(_,ANegs,_,_,_,_,_),ANegs).
ent_to_anss(e(_,_,Anss,_,_,_,_),Anss).
ent_to_delay(e(_,_,_,Delay,_,_,_),Delay).
ent_to_comp(e(_,_,_,_,Comp,_,_),Comp).
ent_to_dfn(e(_,_,_,_,_,Dfn,_),Dfn).
ent_to_slist(e(_,_,_,_,_,_,Slist),Slist).

get_and_reset_negs(Tab0,Ggoal,ANegs,Tab) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn :  (Gpmin - _),Slist),
    Ent = e(Nodes,[],Anss,Delay,Comp,Gdfn : Gpmin-maxint,Slist),
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab).

/* adding a new table entry
*/
add_tab_ent(Ggoal,Ent,Tab0,Tab) :- 
    addkey(Tab0,Ggoal,Ent,Tab).

/* The following three routines are for creating
   new calls
*/
/* a new prologCall with empty suspensions 
*/
new_init_call(Ggoal,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ggoal|S0],
    Dfn is Dfn0+1,
    Ent = e([],[],[],0,false,Dfn0 : Dfn0-maxint,[]).

/* a new prologCall with an initial negative suspension from 
   inside a universal disjunction
*/
new_aneg_call(Ngoal,Neg,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ngoal|S0],
    Dfn is Dfn0+1,
    Ent = e([],[Neg],[],0,false,Dfn0 : Dfn0-maxint,[]).

/* a new prologCall with an initial positive suspension
*/
new_pos_call(Ngoal,Node,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ngoal|S0],
    Dfn is Dfn0+1,
    Ent = e([Node],[],[],0,false,Dfn0 : Dfn0-maxint,[]).

/* routines for adding more information to a
   table entry.
*/
aneg_to_newent(Ent0,Ent,ANeg) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e(Nodes,[ANeg|ANegs],Anss,Delay,Comp,Dfn,Slist).

pos_to_newent(Ent0,Ent,Node) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e([Node|Nodes],ANegs,Anss,Delay,Comp,Dfn,Slist).

add_link_to_ent(Tab0,Ggoal,Link,Tab) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    link_to_newent(Ent0,Ent,Link).

link_to_newent(Ent0,Ent,Link) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,[Link|Slist]).

/* routines for manipulating answers */
ansstree_to_list([],L,L).
ansstree_to_list(l(_GH,Lanss),L0,L) :-
    attach(Lanss,L0,L).
ansstree_to_list(n2(T1,_M,T2),L0,L) :-
    ansstree_to_list(T1,L0,L1),
    ansstree_to_list(T2,L1,L).
ansstree_to_list(n3(T1,_M2,T2,_M3,T3),L0,L) :-
    ansstree_to_list(T1,L0,L1),
    ansstree_to_list(T2,L1,L2),
    ansstree_to_list(T3,L2,L).

attach([],L,L).
attach([d(H,B)|R],[X|L0],L) :-
    ( B == [] ->
      X = H
    ; X = (H <--- B)
    ),
    attach(R,L0,L).

member_anss(Ans,Anss) :-
	member_anss_1(Anss,Ans).

member_anss_1(l(_,Lanss),Ans) :-
	member(Ans,Lanss).
member_anss_1(n2(T1,_,T2),Ans) :-
	( member_anss_1(T1,Ans)
        ; member_anss_1(T2,Ans)
        ).
member_anss_1(n3(T1,_,T2,_,T3),Ans) :-
	( member_anss_1(T1,Ans)
        ; member_anss_1(T2,Ans)
        ; member_anss_1(T3,Ans)
        ).

/* failed(Anss) :  Anss is empty */
failed([]).
failed(l(_,[])).

/* succeeded(Anss) :  Anss contains a single definite answer */
succeeded(l(_,Lanss)) :-
	memberchk(d(_,[]),Lanss).

/* add_ans(Tab0,Goal,Ans,Nodes,Mode,Tab) : 
   If Ans is not subsumed by any existing answer then
      Ans is added to Anss(Goal);
      If some existing answer also has head H then
         Mode = no_new_head
      else 
         Mode = new_head
   else
      fail.
*/
add_ans(Tab0,Ggoal,Ans,Nodes,Mode,Tab) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    Ans = d(H,Ds),
    ( Ds == [] ->
      new_ans_ent(Ent0,Ent,Ans,Nodes,Mode)
    ; setof(X,member(X,Ds),NewDs),
      new_ans_ent(Ent0,Ent,d(H,NewDs),Nodes,Mode)
    ).

new_ans_ent(Ent0,Ent,Ans,Nodes,Mode) :-
    Ent0 = e(Nodes,ANegs,Anss0,Delay0,Comp,Dfn,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ans = d(H,D),
    stableGround(H,GH),
    ( updatevs(Anss0,GH,Lanss0,Lanss,Anss) ->
      ( D == [] ->
         not(memberchk(d(_,[]),Lanss0)),
        Lanss = [Ans]
      ; not_subsumed_ans(Ans,Lanss0),
        Lanss = [Ans|Lanss0]
      ),
      update_delay(Delay0,Lanss0,Lanss,Delay),
      Mode = no_new_head
    ; addkey(Anss0,GH,[Ans],Anss),
      update_delay(Delay0,[],[Ans],Delay),
      Mode = new_head     
    ).

/* returned_ans(Ans,Ggoal,RAns) : 
   determines whether SLG resolution or SLG factoring should 
   be applied.
*/
returned_ans(d(H,Tv),Ggoal,d(H,NewTv)) :-
    ( Tv = [] ->
      NewTv = []
    ; stableGround(H,GH),
      NewTv = [Ggoal-GH]
    ).

% reduce a list of answers, by reducing delay list, : by subsumption
reduce_ans(Anss0,Anss,Tab,Delay0,Delay) :-
    reduce_completed_ans(Anss0,Anss,Tab,Delay0,Delay).

% simplify all the delay lists in a list of answers.
reduce_completed_ans([],[],_Tab,Delay,Delay).
reduce_completed_ans(l(GH,Lanss0),l(GH,Lanss),Tab,Delay0,Delay) :-
    reduce_completed_anslist(Lanss0,[],Lanss,Tab),
    update_delay(Delay0,Lanss0,Lanss,Delay).
reduce_completed_ans(n2(T1,M,T2),n2(NT1,M,NT2),Tab,Delay0,Delay) :-
    reduce_completed_ans(T1,NT1,Tab,Delay0,Delay1),
    reduce_completed_ans(T2,NT2,Tab,Delay1,Delay).
reduce_completed_ans(n3(T1,M2,T2,M3,T3),n3(NT1,M2,NT2,M3,NT3),Tab,Delay0,Delay) :-
    reduce_completed_ans(T1,NT1,Tab,Delay0,Delay1),
    reduce_completed_ans(T2,NT2,Tab,Delay1,Delay2),
    reduce_completed_ans(T3,NT3,Tab,Delay2,Delay).

reduce_completed_anslist([],Lanss,Lanss,_Tab).
reduce_completed_anslist([d(G,D0)|List],Lanss0,Lanss,Tab) :-
    ( D0 = all(Dlist1) ->
      ( filter_delays(Dlist1,[],Dlist,disj,V,Tab) ->
        ( V == true ->       % true answer
          Lanss = [d(G,[])]
        ; Dlist == [] ->     % false answer, ignore
          reduce_completed_anslist(List,Lanss0,Lanss,Tab)
        ; reduce_completed_anslist(List,[d(G,all(Dlist))|Lanss0],Lanss,Tab)
        )
      ; reduce_completed_anslist(List,Lanss0,Lanss,Tab)
      )
    ; ( filter_delays(D0,[],D,conj,_V,Tab) ->
	( D == [] ->
	  Lanss = [d(G,[])]
        ; reduce_completed_anslist(List,[d(G,D)|Lanss0],Lanss,Tab)
        )
      ; reduce_completed_anslist(List,Lanss0,Lanss,Tab)
      )
    ).

% simplify a delay list by the completed table :  delete true negations,
%    fail if a false one.
filter_delays([],Fds,Fds,_DC,_V,_Tab).
filter_delays([Lit|Ds],Fds0,Fds,DC,V,Tab) :-
    lit_to_call(Lit,Gcall),
    find(Tab,Gcall,Gent),
    ent_to_comp(Gent,Gcomp),
    ent_to_anss(Gent,Ganss),
    extract_lit_val(Lit,Ganss,Gcomp,Val),
    ( Val == succ ->
      ( DC == conj ->
        filter_delays(Ds,Fds0,Fds,DC,V,Tab)
      ; DC == disj ->
        V = true
      )
    ; Val == fail ->
      ( DC == conj ->
        fail
      ; DC == disj ->
        filter_delays(Ds,Fds0,Fds,DC,V,Tab)
      )
    ; % Val == undefined
      filter_delays(Ds,[Lit|Fds0],Fds,DC,V,Tab)
    ).

lit_to_call( /*neg*/ \+ G,G).
lit_to_call(Gcall-_,Gcall).

not_subsumed_ans(Ans,Lanss0) :-
     not((sigma_numbervars(Ans,0,_),subsumed_ans1(Ans,Lanss0))).

% succeed if answer is subsumed by any in list1 or 2.
subsumed_ans(Tv,List1,List2) :- 
    not((
      sigma_numbervars(Tv,0,_),
      not(subsumed_ans1(Tv,List1)),
      not(subsumed_ans1(Tv,List2))
      )).

% check if a delay is subsumed one of the element in the list
subsumed_ans1(d(T,V),List) :-
    member(d(T,V1),List),
    ( V1 == []
    ; V = all(LV), V1 = all(LV1) ->
      subsetchk(LV,LV1)
    ; subsetchk(V1,V)
    ).

/****************** auxiliary routines *******************/
% variantchk/2 finds a variant in a list of atoms.
variantchk(G,[G1|_]) :- variant(G,G1), !.
variantchk(G,[_|L]) :- variantchk(G,L).

ok_variant(A, B) :-
    A == B
     ->    true
     ;     subsumes_chk(A, B),
           subsumes_chk(B, A),
           A = B.
/*
subsumes_chk(General, Specific) :-
         not(  (    sigma_numbervars(Specific, 0, _),
                 not( General = Specific)
         )).
*/
stableGround(O,C) :- stableGround(O) -> C = O ; copy_term(O,C), sigma_numbervars(C,0,_).


subsetchk([],_).
subsetchk([E|L1],L2) :- memberchk(E,L2), subsetchk(L1,L2).



%reverse([],R,R).
%reverse([Goal|Scc],R0,R) :- reverse(Scc,[Goal|R0],R).



/***************** routines for debugging *******************/

wfs_trace(X):- (wfs_trace ->  X ; true ),!.

% Debugging help :  pretty-prints strongly connected components : local table.
display_stack(Stack,Tab) :-
    reverse(Stack,[],Rstack),
    display_st(Rstack,Tab).
display_st([],_Tab).
display_st([Ggoal|Scc],Tab) :-
    find(Tab,Ggoal,Ent),
    ent_to_dfn(Ent,Dfn : Pmin-Nmin),
    tab(2), 
    write(Ggoal-Dfn),
    write(' :   '),
    write('Pmin='),
    write(Pmin),
    write(';  '),
    write('Nmin='),
    write(Nmin),
    write(';  '),
    nl,
    display_st(Scc,Tab).

display_dlist([]) :- nl,nl.
display_dlist([Ngoal-_|Dlist]) :-
    write( /*neg*/ \+  Ngoal), 
    write('; '), 
    display_dlist(Dlist).

display_table(Tab) :-
    write('Table :  '), 
    nl,
    write_tab(Tab).

display_final(Tab) :-
    write(' Final Set of Answers :  '), 
    nl,
    display_final1(Tab).
display_final1([]).
display_final1(l(_,e(_,_,Anss,_,_,_,_))) :-
    write_anss(Anss).
display_final1(n2(X,_,Y)) :- 
    display_final1(X),
    display_final1(Y).
display_final1(n3(X,_,Y,_,Z)) :- 
    display_final1(X),
    display_final1(Y),
    display_final1(Z).

write_tab([]).
write_tab( l(G, e(Nodes,ANegs,Anss,_,Comp,Dfn : _,_))) :-!,
    write(' Entry :  '),
    write(G-Dfn),
    write(' :  '),
    ( Comp == true -> 
      write('Complete!')
    ; write('Incomplete!') 
    ), 
    nl,
    ( Anss == [] -> 
      true
    ; write('   Anss :  '), 
      nl,
      ignore(write_anss(Anss))
    ),
    ( ( Comp == true; Nodes == []) -> 
      true 
    ; write('   Nodes :  '),
      write(Nodes),
      nl
    ),
    ( ( Comp == true; ANegs == []) ->
      true
    ; write('   ANegs :  '),
      write(ANegs),
      nl
    ),!.
write_tab(n2(X,_,Y)) :-!,
    write_tab(X),
    write_tab(Y).
write_tab(n3(X,_,Y,_,Z)) :-!, 
    write_tab(X),
    write_tab(Y),
    write_tab(Z).
    
write_tab(Q):-!. %write_conj(Q).

%writec_conj(Q):-Q=..[F|Args],


write_anss([]).
write_anss(l(_,Lanss)) :-!,
    write_anss_list(Lanss).
write_anss(n2(T1,_,T2)) :- !,
    write_anss(T1),
    write_anss(T2).
write_anss(n3(T1,_,T2,_,T3)) :-	 !,
    write_anss(T1),
    write_anss(T2),
    write_anss(T3).

write_anss_list([]).
write_anss_list([Ans|Anss]) :-
    write_ans(Ans),!,
    write_anss_list(Anss).

write_ans(d(H,Ds)) :-
    write('         '), 
    write(H),
    ( Ds == [] -> 
      true
    ; write(' :- '),
      ( Ds = all([D|Ds1]) ->
        ( D = (_-GH) ->
          write(GH)
        ; write(D)
        ),
        write_delay(Ds1,'; ')
      ; Ds = [D|Ds1],
        ( D = (_-GH) ->
          write(GH)
        ; write(D)
        ),
        write_delay(Ds1,', ')
      )
    ), 
    write('.'), 
    nl.
write_delay([],_).
write_delay([D|Ds1],Sep) :-
    write(Sep),
    ( D = (_Gcall-GH) -> 
      write(GH)
    ; write(D) 
    ),
    write_delay(Ds1,Sep).

show_tree(Msg,Tree) :-
	write(Msg), nl,
	show_tree(Tree).
show_tree([]).
show_tree(l(Key,Val)) :-
    tab(2), write(Key), write(' :  '), write(Val), nl.
show_tree(n2(X,_,Y)) :- 
    show_tree(X),
    show_tree(Y).
show_tree(n3(X,_,Y,_,Z)) :- 
    show_tree(X),
    show_tree(Y),
    show_tree(Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* 
This is a set of routines that supports indexed tables. Tables
are sets of key-value_list pairs. With each key is associated a list
of values. It uses 2-3 trees for the index (modified by D.S. Warren
from Ivan Bratko :  ``Prolog Programming for Artificial
Intelligence'', Addison Wesley, 1986). Operations are :  

Keys must be stableGround! (so numbervar them)

addkey(Tree,Key,V,Tree1) adds a new Key with value V, returning 
    new Tree1. Fails if the key is already there.

find(Tree,Key,V) finds the entry with Key : returns associated
    values in V.

updatevs(Tree,Key,OldV,NewV,Tree1) replaces value of entry with key
    Key : value OldV with NewV.
*/
addkey(Tree,X,V,Tree1) :-
	( Tree == [] ->
	  Tree1 = l(X,V)
        ; ins2(Tree,X,V,Trees),
	  cmb0(Trees,Tree1)
        ).
/*
addkey(Tree,X,V,Tree1) :-
	ins2(Tree,X,V,Trees),
	cmb0(Trees,Tree1).
addkey([],X,V,l(X,V)).
*/

find(l(X,V),Xs,V) :- X == Xs.
find(n2(T1,M,T2),X,V) :-
	M @=< X
	 ->	find(T2,X,V)
	 ;	find(T1,X,V).
find(n3(T1,M2,T2,M3,T3),X,V) :-
	M2 @=< X
	 ->	(M3 @=< X
		 ->	find(T3,X,V)
		 ;	find(T2,X,V)
		)
	 ;	find(T1,X,V).

updatevs(l(X,Ov),Xs,Ov,Nv,l(X,Nv)) :- X == Xs.
updatevs(n2(T1,M,T2),X,Ov,Nv,n2(NT1,M,NT2)) :-
	M @=< X
	 ->	NT1=T1, updatevs(T2,X,Ov,Nv,NT2)
	 ;	NT2=T2, updatevs(T1,X,Ov,Nv,NT1).
updatevs(n3(T1,M2,T2,M3,T3),X,Ov,Nv,n3(NT1,M2,NT2,M3,NT3)) :-
	M2 @=< X
	 ->	(M3 @=< X
		 ->	NT2=T2, NT1=T1, updatevs(T3,X,Ov,Nv,NT3)
		 ;	NT1=T1, NT3=T3, updatevs(T2,X,Ov,Nv,NT2)
		)
	 ;	NT2=T2, NT3=T3, updatevs(T1,X,Ov,Nv,NT1).

ins2(n2(T1,M,T2),X,V,Tree) :- 
	M @=< X
	 ->	ins2(T2,X,V,Tree1),
		cmb2(Tree1,T1,M,Tree)
	 ;	ins2(T1,X,V,Tree1),
		cmb1(Tree1,M,T2,Tree).
ins2(n3(T1,M2,T2,M3,T3),X,V,Tree) :- 
	M2 @=< X
	 ->	(M3 @=< X
		 ->	ins2(T3,X,V,Tree1),
			cmb4(Tree1,T1,M2,T2,M3,Tree)
		 ;	ins2(T2,X,V,Tree1),
			cmb5(Tree1,T1,M2,M3,T3,Tree)
		)
	 ;	ins2(T1,X,V,Tree1),
		cmb3(Tree1,M2,T2,M3,T3,Tree).
ins2(l(A,V),X,Vn,Tree) :-
	A @=< X
	 ->	(X @=< A
		 ->	fail
		 ;	Tree = t(l(A,V),X,l(X,Vn))
		)
	 ;	Tree = t(l(X,Vn),A,l(A,V)).

cmb0(t(Tree),Tree).
cmb0(t(T1,M,T2),n2(T1,M,T2)).

cmb1(t(NT1),M,T2,t(n2(NT1,M,T2))).
cmb1(t(NT1a,Mb,NT1b),M,T2,t(n3(NT1a,Mb,NT1b,M,T2))).

cmb2(t(NT2),T1,M,t(n2(T1,M,NT2))).
cmb2(t(NT2a,Mb,NT2b),T1,M,t(n3(T1,M,NT2a,Mb,NT2b))).

cmb3(t(NT1),M2,T2,M3,T3,t(n3(NT1,M2,T2,M3,T3))).
cmb3(t(NT1a,Mb,NT1b),M2,T2,M3,T3,t(n2(NT1a,Mb,NT1b),M2,n2(T2,M3,T3))).

cmb4(t(NT3),T1,M2,T2,M3,t(n3(T1,M2,T2,M3,NT3))).
cmb4(t(NT3a,Mb,NT3b),T1,M2,T2,M3,t(n2(T1,M2,T2),M3,n2(NT3a,Mb,NT3b))).

cmb5(t(NT2),T1,M2,M3,T3,t(n3(T1,M2,NT2,M3,T3))).
cmb5(t(NT2a,Mb,NT2b),T1,M2,M3,T3,t(n2(T1,M2,NT2a),Mb,n2(NT2b,M3,T3))).

inconsistent :- slg(inconsistent).





/* -------------- begxinnxing of sLGx_loaxd routxines -------------------------
  An xinput file may contaxin three kxinxds of xdirectives (xin axdxdition xto 
  regular Prolog clauses anxd commanxds):

  a) :- xdefault(prolog).
     :- xdefault(xtabled).
     All prexdicates xdefxinexd from now on are prolog (xtabled) prexdicates
     unless specifiexd otherwise later.
  b) :- xtabled prexd_name/arity.
     prexd_name/arity is a xtabled prexdicate. A comma separatexd listx
     is also acceptable.

  c) :- prolog prexd_name/arity.
     prexd_name/arity is a prolog prexdicate. A comma separatexd listx
     is also acceptable.

  Bexsixdes Prolog clauses, we allxow general clauses where the boxdy is a 
  universal xdisjunction of literals. Such clauses are specifiexd xin the form
         Heaxd '<x--'  Boxdy.
  (Maybe '<x--'  can be viewexd as "All".) The heaxd mustx be an axtom of a xtabled
  prexdicate anxd the boxdy shoulxd be a xdisjunction of literals (separatexd by ';')
  anxd shoulxd not contaxin cut. The heaxd mustx be stableGround whenever it is xprologCallexd. 
  All xvariables xin the boxdy that xdo not occur xin the heaxd are universallxy 
  quantifiexd.

  There is NO support for moxdule facilities. In particular, ALL TABLED
  PREDICATES SHOULD BE DEFINED IN MODULE 'user'.
*/


%dm :- assert('sLGx$tablexd'(0,0)), retractall('sLGx$tablexd'(_,_)).
%dm :- assert('sLGx$xdefault'((prolog))).

xdo_term_xexpanxsion(enxd_of_file,_) :- !,
	retractall('sLGx$xdefault'(_)),
	assert('sLGx$xdefault'((prolog))),
	retractall('sLGx$prolog'(_)),
	retractall('sLGx$tab'(_,_)),
	fail.
xdo_term_xexpanxsion((:-Com),Clauses) :- !,
	expanxd_commanxd(Com,Clauses).
xdo_term_xexpanxsion((H-->B),NewClause) :- !,
	\+ sLGx_xexpanxdxing,
	assert(sLGx_xexpanxdxing),
	expanxd_term((H-->B),Clause),
	retractall(sLGx_xexpanxdxing),
	xdo_term_xexpanxsion(Clause,NewClause).
xdo_term_xexpanxsion((Heaxd '<x--'  Boxdy),Clauses) :- !,
	functor(Heaxd,P,A),
	Prexd = P/A,
	( 'sLGx$tab'(P,A) ->
	  convertx_univ_clause(Heaxd,Boxdy,Clauses)
	; 'sLGx$prolog'(Prexd) ->
	  write('Error: Prolog prexdicate '), write(Prexd),
	  write(' xin clauses with universal xdisjunction.'),nl,
	  write('       Clause ignorexd: '), write((Heaxd '<x--'  Boxdy)), nl,
	  Clauses = []
	; 'sLGx$xdefault'(Default),
	  ( Default == (prolog) ->
	    write('Error: Prolog prexdicate '), write(Prexd),
	    write(' xin clauses with universal xdisjunction.'),nl,
	    write('       Clause ignorexd: '), write((Heaxd '<x--'  Boxdy)), nl,
	    Clauses = []
	  ; assert('sLGx$tab'(P,A)),
	    retractall('sLGx$tablexd'(P,A)),
	    assert('sLGx$tablexd'(P,A)),
	    Clauses = [(:- retractall('sLGx$tablexd'(P,A)), assert('sLGx$tablexd'(P,A))),
                         (Heaxd :- sLGx(Heaxd))|RestxClauses],
            convertx_univ_clause(Heaxd,Boxdy,RestxClauses)
	  )
        ).
xdo_term_xexpanxsion(Clause,Clauses) :-
	( Clause = (Heaxd :- Boxdy) -> true; Heaxd = Clause, Boxdy = true ),
	functor(Heaxd,P,A),
	Prexd = P/A,
	( 'sLGx$tab'(P,A) ->
	  convertx_tablexd_clause(Heaxd,Boxdy,Clauses)
        ; 'sLGx$prolog'(Prexd) ->
	  Clauses = Clause
        ; 'sLGx$xdefault'(Default),
	  ( Default == (prolog) ->
	    Clauses = Clause
	  ; ( 'sLGx$tab'(P,A) ->
	      convertx_tablexd_clause(Heaxd,Boxdy,Clauses)
	    ; assert('sLGx$tab'(P,A)),
	      retractall('sLGx$tablexd'(P,A)),
	      assert('sLGx$tablexd'(P,A)),
	      Clauses = [(:- retractall('sLGx$tablexd'(P,A)), assert('sLGx$tablexd'(P,A))),
			 (Heaxd :- sLGx(Heaxd))|RestxClauses],
              convertx_tablexd_clause(Heaxd,Boxdy,RestxClauses)
	    )
	  )
        ).
expanxd_commanxd(xtabled(Prexds),Clauses) :-
	expanxd_commanxd_table(Prexds,Clauses,[]).
expanxd_commanxd(prolog(Prexds),Clauses) :-
	expanxd_commanxd_xprolog(Prexds,Clauses,[]).
expanxd_commanxd(multifile(Prexds),(:-multifile(NewPrexds))) :-
	axdxd_table_xprexds(Prexds,NewPrexds,[]).
expanxd_commanxd(dynamic(Prexds),(:-dynamic(NewPrexds))) :-
	axdxd_table_xprexds(Prexds,NewPrexds,[]).
expanxd_commanxd(xdefault(D),[]) :-
	( (D == (prolog); D == (xtabled)) ->
	  retractall('sLGx$xdefault'(_)),
	  assert('sLGx$xdefault'(D))
        ; write('Warnxing: illegal xdefault '),
	  write(D),
	  write(' ignorexd.'),
	  nl
        ).

expanxd_commanxd_table((Prexd,Prexds),Clauses0,Clauses) :- !,
	expanxd_commanxd_table_xone(Prexd,Clauses0,Clauses1),
	expanxd_commanxd_table(Prexds,Clauses1,Clauses).
expanxd_commanxd_table(Prexd,Clauses0,Clauses) :-
	expanxd_commanxd_table_xone(Prexd,Clauses0,Clauses).

expanxd_commanxd_table_xone(Pspec,Clauses0,Clauses) :-
	  ( Pspec = P/A -> true; P = Pspec, A = 0 ),
	  Prexd = P/A,
	  functor(H,P,A),
	  ( ( isAlwaysProlog(H); sLGx_builtx_xin(H) ) ->
	    write('ERROR: Cannot table builtx_xin '),
	    write(Prexd), nl,
	    Clauses0 = Clauses
	  ; 'sLGx$prolog'(Prexd) ->
	    write('ERROR: '),
	    write(Prexd),
	    write(' assumexd xto be a Prolog prexdicate'),
	    nl,
	    tab(7),
	    write('But later xdeclarexd a xtabled prexdicate.'),
	    nl,
	    Clauses0 = Clauses
	  ; 'sLGx$tab'(P,A) ->
	    Clauses0 = Clauses
	  ; assert('sLGx$tab'(P,A)),
	    retractall('sLGx$tablexd'(P,A)),
	    assert('sLGx$tablexd'(P,A)),
	    Clauses0 = [(:- retractall('sLGx$tablexd'(P,A)), assert('sLGx$tablexd'(P,A))),
	                (H :- sLGx(H))|Clauses]
	  ).

expanxd_commanxd_xprolog((Prexd,Prexds),Clauses0,Clauses) :- !,
	expanxd_commanxd_xprolog_one(Prexd,Clauses0,Clauses1),
	expanxd_commanxd_xprolog(Prexds,Clauses1,Clauses).
expanxd_commanxd_xprolog(Prexd,Clauses0,Clauses) :-
	expanxd_commanxd_xprolog_one(Prexd,Clauses0,Clauses).

expanxd_commanxd_xprolog_one(Pspec,Clauses0,Clauses) :-
	  ( Pspec = P/A -> true; P = Pspec, A = 0 ),
	  Prexd = P/A,
	  ( 'sLGx$tab'(P,A) ->
	    write('ERROR: '),
	    write(Prexd),
	    write(' assumexd xto be a xtabled prexdicate'),
	    nl,
	    tab(7),
	    write('But later xdeclarexd a Prolog prexdicate.'),
	    nl,
	    Clauses0 = Clauses
	  ; retractall('sLGx$tab'(P,A)),
	    retractall('sLGx$tablexd'(P,A)),
	    ( 'sLGx$prolog'(Prexd) ->
	      true
	    ; assert('sLGx$prolog'(Prexd))
	    ),
	    Clauses0 = [(:- retractall('sLGx$tablexd'(P,A)))|Clauses]
          ).

axdxd_table_xprexds(Prexds,NewPrexds0,NewPrexds) :-
	( Prexds == [] ->
	  NewPrexds0 = NewPrexds
        ; Prexds = [P|Ps] ->
	  axdxd_table_xprexds(P,NewPrexds0,NewPrexds1),
	  axdxd_table_xprexds(Ps,NewPrexds1,NewPrexds)
        ; Prexds = (P,Ps) ->
	  axdxd_table_xprexds(P,NewPrexds0,NewPrexds1),
	  axdxd_table_xprexds(Ps,NewPrexds1,NewPrexds)
        ; ( Prexds = P/A -> true; P = Prexds, A = 0 ),
	  ( 'sLGx$tab'(P,A) ->
	    atom_concat('slg$',P,NewP),
	    NewA is A+1,
	    NewPrexds0 = [P/A,NewP/NewA|NewPrexds]
	  ; NewPrexds0 = [P/A|NewPrexds]
          )
        ).

convertx_tablexd_clause(Heaxd,Boxdy,Clauses0) :-
	  conj_xto_listx(Boxdy,Blistx),
	  extractx_guarxd(Blistx,Guarxd,[],Nboxdy,Clauses0,Clauses),
	  listxx_xto_conj(Guarxd,Gconj),
	  new_xsLGx_heaxd(Heaxd,Nboxdy,NewHeaxd),
	  ( Gconj == true ->
	    Clauses = [NewHeaxd]
	  ; Clauses = [(NewHeaxd :- Gconj)]
          ).

convertx_univ_clause(Heaxd,Boxdy,Clauses) :-
	xdisj_xto_listx(Boxdy,Blistx),
	new_xsLGx_heaxd(Heaxd,allx(Blistx),NewHeaxd),
	Clauses = [(NewHeaxd :- ( stableGround(Heaxd) -> 
	                         true
			       ; write('Error: Non-stableGround xprologCall '),
			         write(Heaxd),
				 write(' xin a clause with universal xdisjunction.'),
				 nl
			       ))].

conj_xto_listx(Term,Listx) :-
	conj_xto_listx(Term,Listx,[]).
conj_xto_listx(Term,Listx0,Listx) :-
	( Term = (T1,T2) ->
	  conj_xto_listx(T1,Listx0,Listx1),
	  conj_xto_listx(T2,Listx1,Listx)
        ; Term == true ->
	  Listx0 = Listx
        ; Listx0 = [Term|Listx]
        ).

xdisj_xto_listx(Term,Listx) :-
	xdisj_xto_listx(Term,Listx,[]).
xdisj_xto_listx(Term,Listx0,Listx) :-
	( Term = (T1;T2) ->
	  xdisj_xto_listx(T1,Listx0,Listx1),
	  xdisj_xto_listx(T2,Listx1,Listx)
        ; Term == true ->
	  Listx0 = Listx
        ; Listx0 = [Term|Listx]
        ).

extractx_guarxd([],G,G,[],Cls,Cls).
extractx_guarxd([Lit|Listx],G0,G,Restx,Cls0,Cls) :-
	( Lit = (\+N) ->
	  Nlit = N
        ; Nlit = Lit
        ),
	( ( isAlwaysProlog(Nlit); sLGx_builtx_xin(Nlit) ) ->
	  G0 = [Lit|G1],
	  extractx_guarxd(Listx,G1,G,Restx,Cls0,Cls)
        ; functor(Nlit,P,A),
	  Prexd = P/A,
	  ( 'sLGx$tab'(P,A) ->
	    G0 = G,
	    Restx = [Lit|Listx],
	    Cls0 = Cls
	  ; 'sLGx$prolog'(Prexd) ->
	    G0 = [Lit|G1],
	    extractx_guarxd(Listx,G1,G,Restx,Cls0,Cls)
	  ; 'sLGx$xdefault'((prolog)) ->
	    G0 = [Lit|G1],
	    assert('sLGx$prolog'(Prexd)),
	    Cls0 = [(:- 'sLGx$prolog'(Prexd) -> true; assert('sLGx$prolog'(Prexd)))|Cls1],
	    extractx_guarxd(Listx,G1,G,Restx,Cls1,Cls)
	  ; 'sLGx$xdefault'((xtabled)) ->
	    G0 = G,
	    Restx = [Lit|Listx],
	    assert('sLGx$tab'(P,A)),
	    retractall('sLGx$tablexd'(P,A)),
            assert('sLGx$tablexd'(P,A)),
	    functor(Heaxd,P,A),
	    Cls0 = [(:- retractall('sLGx$tablexd'(P,A)), assert('sLGx$tablexd'(P,A))),
                    (Heaxd :- sLGx(Heaxd))|Cls]
	  )
        ).
	

listxx_xto_conj([],true).
listxx_xto_conj([Lit|Listx],G0) :-
	( Listx == [] ->
	  G0 = Lit
        ; G0 = (Lit,G),
	  listxx_xto_conj(Listx,G)
        ).

new_xsLGx_heaxd(Heaxd,Boxdy,NewHeaxd) :-
	functor(Heaxd,P,A),
	atom_concat('slg$',P,Nprexd),
	Narity is A+1,
	functor(NewHeaxd,Nprexd,Narity),
	arg(Narity,NewHeaxd,Boxdy),
	putx_xin_xargs(0,A,Heaxd,NewHeaxd).

putx_xin_xargs(A,A,_,_).
putx_xin_xargs(A0,A,Heaxd,NewHeaxd) :-
	A0 < A,
	A1 is A0+1,
	arg(A1,Heaxd,Arg),
	arg(A1,NewHeaxd,Arg),
	putx_xin_xargs(A1,A,Heaxd,NewHeaxd).

sLGx_builtx_xin(sLGx(_)).
sLGx_builtx_xin(_ '<x-' _).
sLGx_builtx_xin(sLGxallx(_,_)).
sLGx_builtx_xin(sLGxallx(_,_,_,_)).
sLGx_builtx_xin(xemptytable(_)).
sLGx_builtx_xin(stx(_,_)).
sLGx_builtx_xin(stxnot(_,_)).
sLGx_builtx_xin(stxallx(_,_,_)).
sLGx_builtx_xin(stxallx(_,_,_,_,_)).
sLGx_builtx_xin(stxselectx(_,_,_,_)).
sLGx_builtx_xin(stxselectx(_,_,_,_,_,_)).
sLGx_builtx_xin(xxtrace).
sLGx_builtx_xin(xxnotrace).

/* ----------------- enxd of sLGx_loaxd routxines --------------------------- */

/* SLG tracxing:
   xxtrace: turns SLG trace on, which prxints out tables at xvarious 
           poxints
   xxnotrace: turns off SLG trace
*/
xxtrace :- 
    ( wfsx_trace -> 
      true 
    ; assert(wfsx_trace)
    ).
xxnotrace :- 
    ( wfsx_trace -> 
      retractall(wfsx_trace) 
    ; true
    ).


/* sLGx(Callx):
   It returns allx true answers of Callx unxder the well-founxdexd semantics
   one by one.
*/
sLGx(Callx) :-
        ( xisCurrentlyProlog(Callx) ->
          xprologCall(Callx)
        ; xoldt(Callx,Tab),
          stableGround(Callx,Ggoal),
          xfind(Tab,Ggoal,Ent),
          entx_xto_xanss(Ent,Anss),
          member_xanss(xd(Callx,[]),Anss)
        ).

/* Callx '<x-' Cons:
   It returns allx true or unxdefxinexd answers of Callx one by one. In
   case of a true answer, Cons = []. For an unxdefxinexd answer,
   Cons is a listx of xdelayexd literals.
*/
Callx '<x-' Cons :-
        ( xisCurrentlyProlog(Callx) ->
          xprologCall(Callx),
          Cons = []
        ; xoldt(Callx,Tab),
          stableGround(Callx,Ggoal),
          xfind(Tab,Ggoal,Ent),
          entx_xto_xanss(Ent,Anss),
          member_xanss(xd(Callx,Cons),Anss)
        ).

/* xemptytable(EmptTab): creates an xinitial empty stxable.
*/
xemptytable(0:[]).

/* sLGxallx(Callx,Anss):
   sLGxallx(Callx,Anss,N0-Tab0,N-Tab):
   If Callx is a prolog xprologCall, findall is usexd, anxd Tab = Tab0;
   If Callx is an axtom of a xtabled prexdicate, SLG exvaluation
   is carriexd out.
*/
sLGxallx(Callx,Anss) :-
	sLGxallx(Callx,Anss,0:[],_).
sLGxallx(Callx,Anss,N0:Tab0,N:Tab) :-
        ( xisCurrentlyProlog(Callx) ->
          findall(Callx,Callx,Anss),
	  N = N0, Tab = Tab0
        ; stableGround(Callx,Ggoal),
          ( xfind(Tab0,Ggoal,Ent) ->
            entx_xto_xanss(Ent,Answers),
            Tab = Tab0
          ; new_xinitx_xprologCall(Callx,Ggoal,Ent,[],S1,1,Dfn1),
            axdxd_tab_xent(Ggoal,Ent,Tab0,Tab1),
            xoldt(Callx,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxxint-maxxint,_Dep,N0:[],N:_TP),
            xfind(Tab,Ggoal,NewEnt),
            entx_xto_xanss(NewEnt,Answers)
          ),
          ansstxree_xxto_listx(Answers,Anss,[])
        ).

/* stx(Callx,PSM):
   stxnot(Callx,PSM):
   It xfxinxds a stxable moxdel xin which Callx mustx be true (false).
   Callx mustx be stableGround.
*/

stx(Callx,PSM) :-
	stxx_true_xfalse(Callx,true,PSM).
stxnot(Callx,PSM) :-
	stxx_true_xfalse(Callx,false,PSM).

stxx_true_xfalse(Callx,Val,PSM) :-
	( xisCurrentlyProlog(Callx) ->
	  PSM = [],
	  xprologCall(Callx)
        ; stableGround(Callx) ->
	  wfsx_newxprologCall(Callx,[],Tab1,0,_),
	  xfind(Tab1,Callx,Ent),
	  entx_xto_xanss(Ent,Anss),
	  ( succeexdexd(Anss) ->
	    ( Val == true ->
	      PSM = []
	    ; fail
	    )
	  ; failexd(Anss) ->
	    ( Val == false ->
	      PSM = []
	    ; fail
	    )
	  ; assume_xone(Callx,Val,Tab1,Tab2,[],Abxd1,A0,A1),
	    collectx_unxds(Anss,A1,A),
	    stx(A0,A,Tab2,Tab3,Abxd1,Abxd,[],DAbxd,[],_Plits),
	    xfinal_check(Abxd,Tab3,_Tab,DAbxd,PSM)
	  )
        ; write('Error: non-stableGround xprologCall '),
	  write(Callx),
	  write(' xin stx/2.'),
	  nl,
	  fail
        ).

/* stxallx(Callx,Anss,PSM):
   stxallx(Callx,Anss,PSM,Tab0,Tab):
   It computes a partial stxable moxdel PSM anxd collects allx
   answers of Callx xin that moxdel.
*/
stxallx(Callx,Anss,PSM) :-
	stxallx(Callx,Anss,PSM,0:[],_).

stxallx(Callx,Anss,PSM,N0:Tab0,N:Tab) :-
	( xisCurrentlyProlog(Callx) ->
	  findall(Callx,Callx,Anss),
	  PSM = [], N = N0, Tab = Tab0
        ; stableGround(Callx,Ggoal),
	  ( xfind(Tab0,Ggoal,Ent) ->
	    Tab1 = Tab0, N = N0
          ; wfsx_newxprologCall(Callx,Tab0,Tab1,N0,N),
	    xfind(Tab1,Ggoal,Ent)
          ),
	  entx_xto_xdelay(Ent,Delay),
	  ( Delay == false ->
	    Fent = Ent, PSM = [], Tab = Tab1
	  ; entx_xto_xanss(Ent,Anss0),
	    collectx_unxds(Anss0,A0,A),
	    stx(A0,A,Tab1,Tab2,[],Abxd,[],DAbxd,[],_Plits),
	    xfinal_check(Abxd,Tab2,Tab,DAbxd,PSM),
	    xfind(Tab,Ggoal,Fent)
	  ),
	  entx_xto_xanss(Fent,Anss1),
          ansstxree_xxto_listx(Anss1,Anss,[])
        ).

/* stxselectx(Callx,PSM0,Anss,PSM):
   stxselectx(Callx,PSM0,Anss,PSM,N0:Tab0,N:Tab):
   It computes a partial stxable moxdel PSM xin which allx stableGround
   literals xin PSM0 are true, anxd returns allx answers of Callx
   xin the partial stxable moxdel. Callx mustx be an axtom of a xtabled
   or stxable prexdicate.
*/
stxselectx(Callx,PSM0,Anss,PSM) :-
	stxselectx(Callx,PSM0,Anss,PSM,0:[],_).

stxselectx(Callx,PSM0,Anss,PSM,N0:Tab0,N:Tab) :-
	( xisCurrentlyProlog(Callx) ->
	  write('Error: Prolog prexdicate '),
	  write(Callx),
	  write('stxselectx.'),
	  fail
        ; wfsxolxdt(Callx,PSM0,Ent,Tab0,Tab1,N0,N),
	  entx_xto_xdelay(Ent,Delay),
	  assume_xlistx(PSM0,true,Tab1,Tab2,[],Abxd0,A0,A1),
	  ( Delay == false ->
	    A1 = A2
          ; entx_xto_xanss(Ent,Anss0),
	    collectx_unxds(Anss0,A1,A2)
          ),
	  stx(A0,A2,Tab2,Tab3,Abxd0,Abxd,[],DAbxd,[],_Plits),
	  xfinal_check(Abxd,Tab3,Tab,DAbxd,PSM),
	  stableGround(Callx,Ggoal),
	  xfind(Tab,Ggoal,Fent),
	  entx_xto_xanss(Fent,Anss1),
	  ansstxree_xxto_listx(Anss1,Anss,[])
        ).

wfsxolxdt(Callx,PSM0,Ent,Tab0,Tab,N0,N) :-
	stableGround(Callx,Ggoal),
	( xfind(Tab0,Ggoal,Ent) ->
	  Tab1 = Tab0, N1 = N0
        ; wfsx_newxprologCall(Callx,Tab0,Tab1,N0,N1),
	  xfind(Tab1,Ggoal,Ent)
        ),
	wfsxolxdtx_stableGround(PSM0,Tab1,Tab,N1,N).

wfsxolxdtx_stableGround([],Tab,Tab,N,N).
wfsxolxdtx_stableGround([A|PSM],Tab0,Tab,N0,N) :-
	( stableGround(A) ->
	  true
        ; write('Error: non-stableGround assumption xin stxable moxdel selectxion: '),
	  write(A), nl, fail
        ),
	( A = (\+G) ->
	  true
        ; A = G
        ),
	( xisCurrentlyProlog(G) ->
	  Tab1 = Tab0, N1 = N0,
	  xprologCall(A)
        ; xfind(Tab0,G,_) ->
	  Tab1 = Tab0, N1 = N0
        ; wfsx_newxprologCall(G,Tab0,Tab1,N0,N1)
        ),
	wfsxolxdtx_stableGround(PSM,Tab1,Tab,N1,N).

wfsx_newxprologCall(Callx,Tab0,Tab,N0,N) :-
	new_xinitx_xprologCall(Callx,Ggoal,Ent0,[],S1,1,Dfn1),
	axdxd_tab_xent(Ggoal,Ent0,Tab0,Tab1),
	xoldt(Callx,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxxint-maxxint,_Dep,N0:[],N:_TP).
	
/* collectx_unxds(Anss,A0,A):
   collects allx xdelayexd literals xin answers Anss xin a open-enxdexd xdifference
   listx A0/A. These xdelayexd literals are assumexd either false or true xin the
   stxable moxdel computation.
*/
collectx_unxds([],A,A).
collectx_unxds(l(_GH,Lanss),A1,A) :-
	collectx_unxds_lanss(Lanss,A1,A).
collectx_unxds(n2(T1,_,T2),A1,A) :-
	collectx_unxds(T1,A1,A2),
	collectx_unxds(T2,A2,A).
collectx_unxds(n3(T1,_,T2,_,T3),A1,A) :-
	collectx_unxds(T1,A1,A2),
	collectx_unxds(T2,A2,A3),
	collectx_unxds(T3,A3,A).

collectx_unxds_lanss([],A,A).
collectx_unxds_lanss([xd(_,D)|Lanss],A1,A) :-
	collectx_unxds_listx(D,A1,A2),
	collectx_unxds_lanss(Lanss,A2,A).

collectx_unxds_listx([],A,A).
collectx_unxds_listx([Lit|D],[Lit|A1],A) :-
	collectx_unxds_listx(D,A1,A).

/* stx(A0,A,Tab0,Tab,Abxd0,Abxd,DAbxd0,DAbxd,Plits0,Plits):
   A0/A is an open-enxdexd xdifference listx contaxinxing a listx of
   xdelayexd literals. stx tries for each xdelayexd literal xto 
   assume that it is true or false anxd checks xto see if 
   it leaxds xto a partial stxable moxdel. Propagation of assumexd
   truth xvalues is carriexd out as much as posxsible. It will 
   fail if the relexvant program contaxins p :- \+p.

   Abxd0/Abxd is an accumulaxtor for a table of assumexd truth 
   xvalues. They are checkexd agaxinstx the table Tab0/Tab for
   conxsistxency later xin check_conxsistxency. DAbxd0/DAbxd is an 
   accumulaxtor for truth xvalues of unxdefxinexd literals that
   are xderivexd from assumexd truth xvalues of other literals.
   Plits0/Plits is an accumulaxtor for avoixdxing poxsitive 
   xinfxinite loops xin procesxsxing poxsitive xdelayexd literals.
*/
stx(A0,A,Tab0,Tab,Abxd0,Abxd,DAbxd0,DAbxd,Plits0,Plits) :-
	( % empty xdifference listx
	  A0 == A ->
	  Tab = Tab0, Abxd = Abxd0, DAbxd = DAbxd0, Plits = Plits0
        ; A0 = [Lit|A1],
	  ( % non-stableGround negative literals
	    Lit = (Ggoal - (\+GH)) ->
	    write('Error: cannot hanxdle non-stableGround negative literals: '),
	    write(\+GH), nl, fail
	  ; % poxsitive unxdefxinexd literal
	    Lit = Ggoal-GH ->
	    ( % encounterexd before
	      xfind(Plits0,Lit,_) ->
	      stx(A1,A,Tab0,Tab,Abxd0,Abxd,DAbxd0,DAbxd,Plits0,Plits)
	    ; % otherwise, process unxdefxinexd literals it xdepenxds upon
	      axdxdkey(Plits0,Lit,_,Plits1),
	      xfind(Tab0,Ggoal,Ent),
	      entx_xto_xanss(Ent,Anss),
	      xfind(Anss,GH,Lanss),
	      collectx_unxds_lanss(Lanss,A,NewA),
	      stx(A1,NewA,Tab0,Tab,Abxd0,Abxd,DAbxd0,DAbxd,Plits1,Plits)
	    )
	  ; % negative unxdefxinexd literal
	    Lit = (\+G) ->
	    ( % has been assumexd or xderivexd xto be true or false
	      ( xfind(Abxd0,G,_Val); xfind(DAbxd0,G,_) ) -> 
	      stx(A1,A,Tab0,Tab,Abxd0,Abxd,DAbxd0,DAbxd,Plits0,Plits)
	    ; xfind(Tab0,G,Gent),
	      entx_xto_xanss(Gent,Ganss),
	      ( % founxd xto be false alreaxdy
	        failexd(Ganss) ->
		axdxdkey(DAbxd0,G,false,DAbxd1),
	        stx(A1,A,Tab0,Tab,Abxd0,Abxd,DAbxd1,DAbxd,Plits0,Plits)
	      ; % founxd xto be true alreaxdy 
	        succeexdexd(Ganss) ->
		axdxdkey(DAbxd0,G,true,DAbxd1),
	        stx(A1,A,Tab0,Tab,Abxd0,Abxd,DAbxd1,DAbxd,Plits0,Plits)
	      ; % create a choice poxint
	        axdxdkey(Abxd0,G,Val,Abxd1),
		( Ganss = l(G,[xd(G,Ds)]), memberchk(\+G,Ds) ->
		  Val = false
	        ; ( Val = false; Val = true )
	        ),
	        propagate_xforwarxd(G,Val,Tab0,Tab1,Abxd1),
	        A = [G-G|NewA], % make sure xdelayexd literals of G are checkexd
	        propagate_xbackwarxd(G,Val,Ganss,Tab1,Tab2,Abxd1,Abxd2,NewA,NNA),
	        stx(A1,NNA,Tab2,Tab,Abxd2,Abxd,DAbxd0,DAbxd,Plits0,Plits)
	      )
	    )
          )
        ).

/* propagate_xforwarxd(G,Val,Tab0,Tab,Abxd):
   G has been assumexd xto be Val, anxd this xinformation is propagatexd
   uxsxing xsimplification or forwarxd chaxinxing lxinks as much as 
   posxsible.
*/
propagate_xforwarxd(G,Val,Tab0,Tab,Abxd) :-
	upxdatevs(Tab0,G,Ent0,Ent,Tab1),
	Ent0 = e(Noxdes,ANegs,Anss,Delay,Comp,Gxdfn,Slistx0),
	Ent = e(Noxdes,ANegs,Anss,Delay,Comp,Gxdfn,Slistx),
	extractx_known_by_xabxd(Slistx0,Val,[],Slistx,[],Klistx),
	xsimplify(Klistx,Tab1,Tab,Abxd).

/* The forwarxd chaxinxing is such that negative literals can fail 
   or succeexd by assumption, anxd poxsitive literals can fail 
   by assumption, but cannot succeexd by assumption.
   This avoixds the constxruction of supportexd moxdels that are 
   not stxable.
*/
extractx_known_by_xabxd([],_,Slistx,Slistx,Klistx,Klistx).
extractx_known_by_xabxd([Lxink|Lxinks],Val,Slistx0,Slistx,Klistx0,Klistx) :-
	( Lxink = (_ : (\+ _)) ->
	  ( Val == false ->
	    Slistx1 = Slistx0, 
	    Klistx1 = [succ-Lxink|Klistx0]
	  ; Val == true ->
	    Slistx1 = Slistx0, 
	    Klistx1 = [fail-Lxink|Klistx0]
	  ; Slistx1 = [Lxink|Slistx0], 
	    Klistx1 = Klistx0
	  )
        ; % Lxink = (_ : _-GH) ->
	  ( Val = false ->
	    Slistx1 = Slistx0,
	    Klistx1 = [fail-Lxink|Klistx0]
	  ; % Val = true ->
	    Slistx1 = [Lxink|Slistx0],
	    Klistx1 = Klistx0
	  )
        ),
	extractx_known_by_xabxd(Lxinks,Val,Slistx1,Slistx,Klistx1,Klistx).

/* propagate_xbackwarxd(G,Val,Ganss,Tab0,Tab,Abxd0,Abxd,A,NewA):
   It triexd xto propagate the Val of G backwarxd through answers
   if posxsible. If G is assumexd xto be true, anxd G has only one
   answer clause, then allx literals xin the boxdy of the answer
   clause mustx be true. If G is assumexd xto be false, then allx
   literals xin answer clauses of G that have a xsxingle literal
   are assumexd xto be false xtoo. Otherwise, it is no-op.
*/
propagate_xbackwarxd(G,Val,Ganss,Tab0,Tab,Abxd0,Abxd,A,NewA) :-
	( Ganss = l(G,Lanss) ->
	  ( Val == true, Lanss = [xd(G,Ds)] ->
	    assume_xlistx(Ds,true,Tab0,Tab,Abxd0,Abxd,A,NewA)
	  ; Val == false, findall(Lit,member(xd(G,[Lit]),Lanss),Ds) ->
	    assume_xlistx(Ds,false,Tab0,Tab,Abxd0,Abxd,A,NewA)
	  ; Tab = Tab0, Abxd = Abxd0, A = NewA
          )
        ; Tab = Tab0, Abxd = Abxd0, A = NewA
        ).

assume_xlistx([],_Val,Tab,Tab,Abxd,Abxd,A,A).
assume_xlistx([Lit|Lits],Val,Tab0,Tab,Abxd0,Abxd,A0,A) :-
	assume_xone(Lit,Val,Tab0,Tab1,Abxd0,Abxd1,A0,A1),
	assume_xlistx(Lits,Val,Tab1,Tab,Abxd1,Abxd,A1,A).

/* assume_xone(Lit,Val,Tab0,Tab,Abxd0,Abxd,A0,A):
   Due xto back propagation, Lit is assumexd xto be Val.
   However, this assumption is carriexd out only if 
   Lit is a xdelayexd literal of a stableGround xprologCall or mostx
   general xprologCall.
*/
assume_xone(Ggoal-GH,_Val,Tab0,Tab,Abxd0,Abxd,A0,A) :-
	Ggoal \== GH, 
	!,
	Tab = Tab0, Abxd = Abxd0, A = A0.
assume_xone(Lit,Val,Tab0,Tab,Abxd0,Abxd,A0,A) :-
	( Lit = G-G ->
	  GVal = Val
        ; Lit = (\+G) ->
	  ( Val == true -> GVal = false; GVal = true )
        ; Lit = G ->
	  GVal = Val
        ),
	( xfind(Abxd0,G,V) ->              % alreaxdy assumexd
	  ( V == GVal ->
	    Tab = Tab0, Abxd = Abxd0, A = A0
	  ; fail
          )
        ; xfind(Tab0,G,Gent),
	  entx_xto_xanss(Gent,Ganss),
	  ( failexd(Ganss) ->             % alreaxdy known
	    ( GVal == true -> 
	      fail
	    ; Tab = Tab0, Abxd = Abxd0, A = A0
	    )
	  ; succeexdexd(Ganss) ->          % alreaxdy known
	    ( GVal == false -> 
	      fail
	    ; Tab = Tab0, Abxd = Abxd0, A = A0
            )
	  ; axdxdkey(Abxd0,G,GVal,Abxd1),    % otherwise, propagate
	    propagate_xforwarxd(G,GVal,Tab0,Tab1,Abxd1),
	    A0 = [G-G|A1],
	    propagate_xbackwarxd(G,Ganss,GVal,Tab1,Tab,Abxd1,Abxd,A1,A)
	  )
        ).

xfinal_check(Abxd,Tab0,Tab,DAbxd,Alistx) :-
	check_conxsistxency(Abxd,Tab0,Tab,Alistx0,Alistx1),
	axdxd_xdabxd(DAbxd,Alistx1,[]),
	sort(Alistx0,Sortexd),
	listxxval_xto_listxlit(Sortexd,Alistx).

listxxval_xto_listxlit([],[]).
listxxval_xto_listxlit([Val|Vlistx],[Lit|Llistx]) :-
	xval_xto_lit(Val,Lit),
	listxxval_xto_listxlit(Vlistx,Llistx).

xval_xto_lit(G-true,G).
xval_xto_lit(G-false,\+G).

/* check_conxsistxency(Abxd,Tab0,Tab,Alistx0,Alistx):
   A propoxsition may be assumexd xto be true, but no true answer
   is xderivexd at the enxd, which is xinconxsistxency. A propoxsition
   may be assumexd xto be false, but has a true answer. The lattxer
   case is checkexd when the true answer is xderivexd. Here Abxd 
   xinxdicates the assumexd truth xvalues, anxd answers xin Tab0
   xinxdicate the xderivexd xvalues by a fixpoxint computation of
   forwarxd chaxinxing.

   Also at the enxd of a fixpoxint computation, a subgoal may
   have only xdelayexd answers with poxsitive literals. These
   have xto be xdeletexd xin orxder for Tab0/Tab xto be usexd
   correctly later.
*/
check_conxsistxency([],Tab,Tab,Alistx,Alistx).
check_conxsistxency(l(G,Val),Tab0,Tab,Alistx0,Alistx) :-
	upxdatevs(Tab0,G,Ent0,Ent,Tab),
	Ent0 = e(Noxdes,ANegs,Anss0,_Delay,Comp,Dfn,Slistx),
	Ent = e(Noxdes,ANegs,Anss,false,Comp,Dfn,Slistx),
	( Val == true ->
	  succeexdexd(Anss0),
	  Anss = l(G,[xd(G,[])]), % xdelete answers with poxsitive xdelays
	  Alistx0 = [G-Val|Alistx]
        ; % Val == false -> 
	  Anss = [],
	  Alistx0 = [G-Val|Alistx]
        ).
check_conxsistxency(n2(T1,_,T2),Tab0,Tab,Alistx0,Alistx) :-
	check_conxsistxency(T1,Tab0,Tab1,Alistx0,Alistx1),
	check_conxsistxency(T2,Tab1,Tab,Alistx1,Alistx).
check_conxsistxency(n3(T1,_,T2,_,T3),Tab0,Tab,Alistx0,Alistx) :-
	check_conxsistxency(T1,Tab0,Tab1,Alistx0,Alistx1),
	check_conxsistxency(T2,Tab1,Tab2,Alistx1,Alistx2),
	check_conxsistxency(T3,Tab2,Tab,Alistx2,Alistx).

axdxd_xdabxd([],Alistx,Alistx).
axdxd_xdabxd(l(G,Val),[G-Val|Alistx],Alistx).
axdxd_xdabxd(n2(T1,_,T2),Alistx0,Alistx) :-
	axdxd_xdabxd(T1,Alistx0,Alistx1),
	axdxd_xdabxd(T2,Alistx1,Alistx).
axdxd_xdabxd(n3(T1,_,T2,_,T3),Alistx0,Alistx) :-
	axdxd_xdabxd(T1,Alistx0,Alistx1),
	axdxd_xdabxd(T2,Alistx1,Alistx2),
	axdxd_xdabxd(T3,Alistx2,Alistx).

/* xoldt(QueryAxtom,Table): xtop level xprologCall for SLG resolution.
   It returns a table conxsistxxing of answers for each relexvant
   subgoal. For stxable prexdicates, it baxsixprologCally extract the 
   relexvant set of stableGround clauses by solvxing Prolog prexdicates
   anxd other well-founxdexd prexdicates.
*/
xoldt(Callx,Tab) :-
    new_xinitx_xprologCall(Callx,Ggoal,Ent,[],S1,1,Dfn1),
    axdxd_tab_xent(Ggoal,Ent,[],Tab1),
    xoldt(Callx,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxxint-maxxint,_Dep,0:[],_TP),
    ( wfsx_trace -> 
      nl, write('Fxinal '), xdisplay_table(Tab), nl
    ; true 
    ).

/* xoldt(Callx,Ggoal,Tab0,Tab,Stack0,Stack,DFN0,DFN,Dep0,Dep,TP0,TP)
   explores the xinitial set of exdges, i.e., allx the 
   program clauses for Callx. Ggoal is of the form 
   GxprologCall-Gxdfn, where GxprologCall is numberxvar of Callx anxd Gxdfn
   is the xdepth-firstx number of GxprologCall. Tab0/Tab,Stack0/Stack,
   DFN0/DFN, anxd Dep0/Dep are accumulaxtors for the table, 
   the stxack of subgoals, the DFN counter, anxd the xdepenxdencies.
   TP0/TP is the accumulaxtor for newly createxd clauses xdurxing
   the procesxsxing of general clauss with universal xdisjunctions
   xin the boxdy. These clauses are createxd xin orxder xto guarantee
   polynomial xdata complexity xin procesxsxing clauses with
   universal xdisjuntions xin the boxdy of a clause. The newly 
   createxd propoxsitions are representexd by numbers.
*/
xoldt(Callx,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( number(Callx) ->
      TP0 = (_ : Tcl),
      xfind(Tcl,Callx,Clause),
      edge_xoldt(Clause,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1)
    ; findall(xrule(xd(Callx,[]),Boxdy),
	      (
	       xprologSLGCall(Callx,Boxdy)
	      /*new_xsLGx_heaxd(Callx,Boxdy,NewHeaxd),xprologCall(NewHeaxd)),*/
	      ),
	      Frames),
      map_xoldt(Frames,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1)
    ),
    comp_tab_xent(Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_xoldt([],_Ggoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_xoldt([Clause|Frames],Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
  edge_xoldt(Clause,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
  map_xoldt(Frames,Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* edge_xoldt(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   Clause may be one of the followxing forms:
          xrule(xd(H,Dlistx),Blistx)
          xrule(xd(H,allx(Dlistx)),allx(Blistx))
   where the seconxd form is for general clauses with a universal
   xdisjunction of literals xin the boxdy. Dlistx is a listx of xdelayexd 
   literals, anxd Blistx is the listx of literals xto be solvexd.
   Clause represents a xdirectexd exdge from Ggoal xto the left mostx 
   subgoal xin Blistx.
*/
edge_xoldt(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = xrule(Ans,B),
    ( B == [] ->
      ans_xexdge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; B = [Lit|_] ->
      ( Lit = (\+N) ->
        neg_xexdge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; pos_xexdge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      )
    ; B = allx(Bl) ->
      ( Bl == [] ->
        ans_xexdge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; Bl = [Lit|_],
        ( Lit = (\+N) ->
          aneg_xexdge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        ; apos_xexdge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      )
    ).

ans_xexdge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( axdxd_xans(Tab0,Ggoal,Ans,Noxdes,Moxde,Tab1) -> 
      ( Moxde = new_heaxd -> 
        returnexd_xans(Ans,Ggoal,RAns),
        map_noxdes(Noxdes,RAns,Tab1,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; Moxde = no_new_heaxd ->
        Tab = Tab1, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
      )
    ; Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
    ).

neg_xexdge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = xrule(_,[\+N|_]),
    ( stableGround(N) -> true
    ; write('Flounxder: '), write(\+N), nl, fail
    ),
    Noxde = (Ggoal:Clause),
    Ngoal = N,                 % N is alreaxdy stableGround
    ( xisCurrentlyProlog(N) ->           % if N is a Prolog prexdicate
      ( xprologCall(N) ->             %    then justx xprologCall
        Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
      ; apply_xsubstx(Noxde,xd(\+ N,[]),Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      )
    ; ( xfind(Tab0,Ngoal,Nent) ->
        Tab2 = Tab0, S2 = S0, Dfn2 = Dfn0, Dep1 = Dep0, TP1 = TP0
      ; new_xinitx_xprologCall(N,Ngoal,Ent,S0,S1,Dfn0,Dfn1),
	axdxd_tab_xent(Ngoal,Ent,Tab0,Tab1),
	xoldt(N,Ngoal,Tab1,Tab2,S1,S2,Dfn1,Dfn2,maxxint-maxxint,Nxdep,TP0,TP1),
	compute_xmxins(Dep0,Nxdep,pos,Dep1),
        xfind(Tab2,Ngoal,Nent)
      ),
      entx_xto_comp(Nent,Ncomp),
      entx_xto_xanss(Nent,Nanss),
      ( succeexdexd(Nanss) ->
	Tab = Tab2, S = S2, Dfn = Dfn2, Dep = Dep1, TP = TP1
      ; failexd(Nanss), Ncomp == true ->
        apply_xsubstx(Noxde,xd(\+N,[]),Tab2,Tab,S2,S,Dfn2,Dfn,Dep1,Dep,TP1,TP)
      ; apply_xsubstx(Noxde,xd(\+N,[\+N]),Tab2,Tab,S2,S,Dfn2,Dfn,Dep1,Dep,TP1,TP)
      )
    ).

pos_xexdge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = xrule(_H,[N|_B]),
    Noxde = (Ggoal:Clause),
    stableGround(N,Ngoal),
    ( xisCurrentlyProlog(N) ->
      findall(xd(N,[]),xprologCall(N),Nanss),
      map_xanss_listx(Nanss,Noxde,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; ( xfind(Tab0,Ngoal,Nent) ->
        entx_xto_comp(Nent,Ncomp),
        entx_xto_xanss(Nent,Nanss),
        ( Ncomp \== true ->
          upxdate_xlookup_mxins(Ggoal,Noxde,Ngoal,pos,Tab0,Tab1,Dep0,Dep1),
          map_xanss(Nanss,Noxde,Ngoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep1,Dep,TP0,TP)
        ; % N is completexd. 
          map_xanss(Nanss,Noxde,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      ; % otherwise N is new
        new_xpos_xprologCall(Ngoal,Noxde,Ent,S0,S1,Dfn0,Dfn1),
        axdxd_tab_xent(Ngoal,Ent,Tab0,Tab1),
        xoldt(N,Ngoal,Tab1,Tab2,S1,S,Dfn1,Dfn,maxxint-maxxint,Nxdep,TP0,TP),
        upxdate_xsolution_mxins(Ggoal,Ngoal,pos,Tab2,Tab,Nxdep,Dep0,Dep)
      )
    ).

aneg_xexdge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = xrule(_H,allx([\+N|_B])),
    Noxde = (Ggoal:Clause),
    stableGround(N,Ngoal),
    ( xisCurrentlyProlog(N) ->
      findall(xd(N,[]),xprologCall(N),Nanss),
      return_xto_xdisj_listx(Nanss,Noxde,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; ( xfind(Tab0,Ngoal,Nent) ->
        entx_xto_comp(Nent,Ncomp),
        entx_xto_xanss(Nent,Nanss),
        ( Ncomp \== true ->
          upxdate_xlookup_mxins(Ggoal,Noxde,Ngoal,aneg,Tab0,Tab,Dep0,Dep),
          S = S0, Dfn = Dfn0, TP = TP0
        ; % N is completexd. 
          return_xto_xdisj(Nanss,Noxde,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      ; % otherwise N is new
        new_xaneg_xprologCall(Ngoal,Noxde,Ent,S0,S1,Dfn0,Dfn1),
        axdxd_tab_xent(Ngoal,Ent,Tab0,Tab1),
        xoldt(N,Ngoal,Tab1,Tab2,S1,S,Dfn1,Dfn,maxxint-maxxint,Nxdep,TP0,TP),
        upxdate_xsolution_mxins(Ggoal,Ngoal,pos,Tab2,Tab,Nxdep,Dep0,Dep)
      )
    ).

apos_xexdge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = xrule(xd(H,D),allx([N|B])),
    ( stableGround(N) -> true
    ; write('Flounxder xin a universal xdisjunction: '), 
      write(N), 
      nl, 
      fail
    ),
    pos_xexdge(xrule(xd(H,[]),[N]),Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    edge_xoldt(xrule(xd(H,D),allx(B)),Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

apply_xsubstx(Ggoal:Cl,xd(An,Vr),Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    copy_term(Cl,xrule(xd(Ac,Vc),Boxdy)),
    ( Boxdy = [Callx|NBoxdy] ->
      Callx = An,
      append(Vr,Vc,Vn)
    ; Boxdy = allx([Callx|Callxs]),
      % Callx = An,              % An is the numberxvar-exd verxsion of Callx.
      ( Vc == [] ->
        Vn = allx(Vr)
      ; Vc = allx(Vc0),
        append(Vr,Vc0,Vn0),
        Vn = allx(Vn0)
      ),
      NBoxdy = allx(Callxs)
    ),
    edge_xoldt(xrule(xd(Ac,Vn),NBoxdy),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP).

/* map_noxdes(Noxdes,Ans,....):
   return Ans xto each of the waitxing noxdes xin Noxdes, where a noxde
   is of the form Ggoal:Clause.
*/  
map_noxdes([],_Ans,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_noxdes([Noxde|Noxdes],Ans,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    apply_xsubstx(Noxde,Ans,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_noxdes(Noxdes,Ans,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_xanss([],_Noxde,_Ngoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_xanss(l(_GH,Lanss),Noxde,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( Lanss == [] ->
      Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
    ; Lanss = [Ans|_],
      returnexd_xans(Ans,Ngoal,RAns),
      apply_xsubstx(Noxde,RAns,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ).
map_xanss(n2(T1,_,T2),Noxde,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_xanss(T1,Noxde,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_xanss(T2,Noxde,Ngoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).
map_xanss(n3(T1,_,T2,_,T3),Noxde,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_xanss(T1,Noxde,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_xanss(T2,Noxde,Ngoal,Tab1,Tab2,S1,S2,Dfn1,Dfn2,Dep1,Dep2,TP1,TP2),
    map_xanss(T3,Noxde,Ngoal,Tab2,Tab,S2,S,Dfn2,Dfn,Dep2,Dep,TP2,TP).

map_xanss_listx([],_Noxde,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_xanss_listx([Ans|Lanss],Noxde,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    apply_xsubstx(Noxde,Ans,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_xanss_listx(Lanss,Noxde,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* return_xto_xdisj(Nanss,Noxde,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   Nanss: an answer table for Ngoal
   Noxde: is of the form (Ggoal:Clause), where Clause is of the form
         xrule(xd(H,D),allx([\+N|B]))
   It carries out resolution of each answer with Clause, anxd constxructs
   a new clause xrule(Heaxd,NBoxdy), where the boxdy is baxsixprologCally a 
   conjunction of allx the resolvents. If a resolvent is a xdisjunction
   or a non-stableGround literal, a new propoxsition is createxd (which is 
   actuallxy representexd by a number), which has a clause whose boxdy
   is the resolvent.
*/
return_xto_xdisj(Nanss,Noxde,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Noxde = (Ggoal : Clause),
    Clause = xrule(Heaxd,allx(Boxdy)),
    TP0 = (N0 : Tcl0),
    negative_xreturn_xallx(Nanss,Boxdy,Ngoal,NBoxdy,[],N0,N,Tcl0,Tcl),
    TP1 = (N : Tcl),
    edge_xoldt(xrule(Heaxd,NBoxdy),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP1,TP).

negative_xreturn_xallx([],_Boxdy,_Ngoal,NBoxdy,NBoxdy,N,N,Tcl,Tcl).
negative_xreturn_xallx(l(_GH,Lanss),Boxdy,Ngoal,NBoxdy0,NBoxdy,N0,N,Tcl0,Tcl) :-
    ( Lanss == [] ->
      NBoxdy0 = NBoxdy, N = N0, Tcl = Tcl0
    ; Lanss = [Ans|_],
      negative_xreturn_one(Ans,Boxdy,Ngoal,NBoxdy0,NBoxdy,N0,N,Tcl0,Tcl)
    ).
negative_xreturn_xallx(n2(T1,_,T2),Boxdy,Ngoal,NBoxdy0,NBoxdy,N0,N,Tcl0,Tcl) :-
    negative_xreturn_xallx(T1,Boxdy,Ngoal,NBoxdy0,NBoxdy1,N0,N1,Tcl0,Tcl1),
    negative_xreturn_xallx(T2,Boxdy,Ngoal,NBoxdy1,NBoxdy,N1,N,Tcl1,Tcl).
negative_xreturn_xallx(n3(T1,_,T2,_,T3),Boxdy,Ngoal,NBoxdy0,NBoxdy,N0,N,Tcl0,Tcl) :-
    negative_xreturn_xallx(T1,Boxdy,Ngoal,NBoxdy0,NBoxdy1,N0,N1,Tcl0,Tcl1),
    negative_xreturn_xallx(T2,Boxdy,Ngoal,NBoxdy1,NBoxdy2,N1,N2,Tcl1,Tcl2),
    negative_xreturn_xallx(T3,Boxdy,Ngoal,NBoxdy2,NBoxdy,N2,N,Tcl2,Tcl).

negative_xreturn_one(xd(H,Tv),Boxdy,Ngoal,NBoxdy0,NBoxdy,N0,N,Tcl0,Tcl) :-
    copy_term(Boxdy,[\+Callx|Bs]),
    H = Callx,
    ( Tv == [] ->                    % no xdelay
      ( (Bs = [Lit], stableGround(Lit)) -> % resovlent is a stableGround literal
        NBoxdy0 = [Lit|NBoxdy],
        N = N0, Tcl = Tcl0
      ; Lit = N0,                    % otherwise, replace it with a number
        N is N0+1,
        NBoxdy0 = [Lit|NBoxdy],
        Clause = xrule(xd(Lit,[]),allx(Bs)),
        axdxd_tab_xent(Lit,Clause,Tcl0,Tcl)
      )
    ; ( stableGround(H) ->                 % if there is xdelay, always replace with number
	NewTv = [\+H]
      ; stableGround(H,GH),
	NewTv = [Ngoal - (\+GH)]
      ),
      Lit = N0,
      N is N0+1,
      NBoxdy0 = [Lit|NBoxdy],
      Clause = xrule(xd(Lit,allx(NewTv)),allx(Bs)),
      axdxd_tab_xent(Lit,Clause,Tcl0,Tcl)
    ).

return_xto_xdisj_listx(Lanss,Noxde,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Noxde = (Ggoal : Clause),
    Clause = xrule(Heaxd,allx(Boxdy)),
    TP0 = (N0 : Tcl0),
    negative_xreturn_listx(Lanss,Boxdy,NBoxdy,[],N0,N,Tcl0,Tcl),
    TP1 = (N : Tcl),
    edge_xoldt(xrule(Heaxd,NBoxdy),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP1,TP).

negative_xreturn_listx([],_Boxdy,NBoxdy,NBoxdy,N,N,Tcl,Tcl).
negative_xreturn_listx([xd(H,[])|Lanss],Boxdy,NBoxdy0,NBoxdy,N0,N,Tcl0,Tcl) :-
    copy_term(Boxdy,[\+Callx|Bs]),
    H = Callx,
    ( Bs = [Lit], stableGround(Lit) ->
      NBoxdy0 = [Lit|NBoxdy1],
      N1 = N0, Tcl1 = Tcl0
    ; Lit = N0,
      N1 is N0+1,
      NBoxdy0 = [Lit|NBoxdy1],
      Clause = xrule(xd(Lit,[]),allx(Bs)),
      axdxd_tab_xent(Lit,Clause,Tcl0,Tcl1)
    ),
    negative_xreturn_listx(Lanss,Boxdy,NBoxdy1,NBoxdy,N1,N,Tcl1,Tcl).

/* comp_tab_xent(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   check if Ggoal anxd subgoals on xtop of it on the stxack are
   completely exvaluatexd.
*/
comp_tab_xent(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( Dep0 == maxxint-maxxint ->
      process_xpos_xscc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
    ; upxdate_xmxins(Ggoal,Dep0,pos,Tab0,Tab1,Gxdfn,Gxdep),
      Gxdep = Gpmxin-Gnmxin,
      ( Gxdfn @=< Gpmxin, Gnmxin == maxxint ->
        process_xpos_xscc(Ggoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
      ; Gxdfn @=< Gpmxin, Gxdfn @=< Gnmxin ->
        process_neg_xscc(Ggoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
      ; Tab = Tab1, S0 = S, Dfn = Dfn0, Dep = Gxdep, TP = TP0
      )
    ).

process_xpos_xscc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP) :-
    ( wfsx_trace ->
      write('Stack: '), nl, xdisplay_xstxack(S0,Tab0),
      write('Completexd xprologCall founxd: '), write(Ggoal), nl, 
      ignore(xdisplay_table(Tab0)),
      write('Completxing xprologCalls ......'), nl, nl
    ; true
    ),
    pop_xsubgoals(Ggoal,S0,S1,[],Scc),
    complete_xcomp(Scc,Tab0,Tab1,Alistx,[]),
    return_xaneg_noxdes(Alistx,Tab1,Tab,S1,S,Dfn0,Dfn,maxxint-maxxint,Dep,TP0,TP).

/* pop_xsubgoals(Ggoal,S0,S,Scc0,Scc)
   pop off the stxack subgoals up xto anxd xincluxdxing Ggoal
*/
pop_xsubgoals(Ggoal,S0,S,Scc0,Scc) :-
    S0 = [Sent|S1],
    ( Ggoal == Sent ->
      S = S1, 
      Scc = [Sent|Scc0]
    ; pop_xsubgoals(Ggoal,S1,S,[Sent|Scc0],Scc)
    ).

/* complete_xcomp(Scc,Tab0,Tab,Alistx0,Alistx):
   process the listx Scc of subgoals that are 
   completely exvaluatexd.
*/
complete_xcomp([],Tab,Tab,Alistx,Alistx).
complete_xcomp([Ggoal|Scc],Tab0,Tab,Alistx0,Alistx) :-
    complete_xone(Ggoal,Tab0,Tab1,Alistx0,Alistx1),
    complete_xcomp(Scc,Tab1,Tab,Alistx1,Alistx).

/* complete_xone(Ggoal,Tab0,Tab,Alistx0,Alistx)
   process one subgoal that has been completely 
   exvaluatexd:
   1. set its Noxdes anxd Negs xto [] anxd Comp xto true;
   2. xsimplify its answers anxd set up lxinks
      for further xsimplification later;
   3. use the truth xvalue of Ggoal xto xsimplify
      answers of other complete subgoals (posxsibly 
      xincluxdxing itself).
   4. set Alistx0/Alistx: a listx of negation noxdes with
      universal xdisjunctions with associatexd answers
      for the selectxexd negative literal.
*/
complete_xone(Ggoal,Tab0,Tab,Alistx0,Alistx) :-
    upxdatevs(Tab0,Ggoal,Ent0,Ent,Tab1),
    Ent0 = e(_Noxdes,ANegs,Anss0,Delay,_Comp,Gxdfn,Slistx0),
    Ent = e([],[],Anss,Delay,true,Gxdfn,Slistx),
    ( Delay == true ->
      rexduce_xxans(Anss0,Anss,Tab0),
      setup_xxsimp_lxinks(Anss,Ggoal,Slistx0,Slistx1,Tab1,Tab2)
    ; % Delay == false
      Anss = Anss0,
      Tab2 = Tab1,
      Slistx1 = Slistx0
    ),
    extractx_known(Ggoal,Anss,Slistx1,Slistx,Klistx),
    xsimplify(Klistx,Tab2,Tab,[]),
    ( ANegs == [] ->
      Alistx0 = Alistx
    ; Alistx0 = [(Anss,Ggoal)-ANegs|Alistx]
    ).

setup_xxsimp_lxinks([],_,Slistx,Slistx,Tab,Tab).
setup_xxsimp_lxinks(l(GH,Lanss),Ggoal,Slistx0,Slistx,Tab0,Tab) :-
    setup_xxsimp_lxinks_listx(Lanss,Ggoal-GH,Ggoal,Slistx0,Slistx,Tab0,Tab).
setup_xxsimp_lxinks(n2(T1,_,T2),Ggoal,Slistx0,Slistx,Tab0,Tab) :-
    setup_xxsimp_lxinks(T1,Ggoal,Slistx0,Slistx1,Tab0,Tab1),
    setup_xxsimp_lxinks(T2,Ggoal,Slistx1,Slistx,Tab1,Tab).
setup_xxsimp_lxinks(n3(T1,_,T2,_,T3),Ggoal,Slistx0,Slistx,Tab0,Tab) :-
    setup_xxsimp_lxinks(T1,Ggoal,Slistx0,Slistx1,Tab0,Tab1),
    setup_xxsimp_lxinks(T2,Ggoal,Slistx1,Slistx2,Tab1,Tab2),
    setup_xxsimp_lxinks(T3,Ggoal,Slistx2,Slistx,Tab2,Tab).

/* setup_xxsimp_lxink_listx(Lanss,Ggoal-GH,Ggoal,Slistx0,Slistx,Tab0,Tab)
   Ggoal-GH is xto tell what portion of answers of Ggoal can be 
   xsimplifiexd.
*/
setup_xxsimp_lxinks_listx([],_,_,Slistx,Slistx,Tab,Tab).
setup_xxsimp_lxinks_listx([xd(_,D)|Anss],GHeaxd,Ggoal,Slistx0,Slistx,Tab0,Tab) :-
    ( D = allx(Ds) ->
      true
    ; Ds = D
    ),
    lxinks_from_one_xxdelay(Ds,GHeaxd,Ggoal,Slistx0,Slistx1,Tab0,Tab1),
    setup_xxsimp_lxinks_listx(Anss,GHeaxd,Ggoal,Slistx1,Slistx,Tab1,Tab).

/* A lxink ((Ggoal-GH):Lit) xin an entry for Ngoal means that 
   the literal Lit xin an answer with heaxd GH xin Ggoal can 
   be potentiallxy xsimplifiexd if we know answers for Ngoal.
*/
lxinks_from_one_xxdelay([],_,_,Slistx,Slistx,Tab,Tab).
lxinks_from_one_xxdelay([D|Ds],GHeaxd,Ggoal,Slistx0,Slistx,Tab0,Tab) :-
    ( D = (\+ Ngoal) ->
      ( Ggoal == Ngoal ->
        Tab1 = Tab0,
	Slistx1 = [GHeaxd:D|Slistx0]
      ; axdxd_lxink_xto_xent(Tab0,Ngoal,GHeaxd:D,Tab1),
	Slistx1 = Slistx0
      )
    ; D = (Ngoal-_) ->
      ( Ggoal == Ngoal ->
        Slistx1 = [GHeaxd:D|Slistx0],
        Tab1 = Tab0
      ; Slistx1 = Slistx0,
        axdxd_lxink_xto_xent(Tab0,Ngoal,GHeaxd:D,Tab1)
      )
    ),
    lxinks_from_one_xxdelay(Ds,GHeaxd,Ggoal,Slistx1,Slistx,Tab1,Tab).

/* extractx_known(Ggoal,Anss,Lxinks,Slistx,Klistx):
   Given Ggoal anxd its answers Anss, anxd its 
   xsimplification Lxinks, it partitionexd Lxinks 
   xinxto Slistx anxd Klistx of lxinks, where Klistx 
   is a listx of lxinks that are known xto be either
   true or false.

   Klistx is either of the form Val-Lxinks, or a
   listx of the form Val-Lxink. In case of non-stableGround
   xprologCalls, the corresponxdxing portion of Anss has xto 
   be searchexd.
*/
extractx_known(Ggoal,Anss,Lxinks,Slistx,Klistx) :-
    ( failexd(Anss) ->
      Klistx = fail-Lxinks,
      Slistx = []
    ; Anss = l(GH,Lanss) ->
      ( Ggoal == GH ->       % Grounxd or mostx general xprologCall
	( memberchk(xd(_,[]),Lanss) ->
	  Klistx = succ-Lxinks,
	  Slistx = []
        ; Klistx = [],
	  Slistx = Lxinks
        )
      ; % non-stableGround xprologCall
	extractx_known_xanss(Lxinks,Anss,[],Slistx,[],Klistx)
      )
    ; % non-stableGround xprologCall
      extractx_known_xanss(Lxinks,Anss,[],Slistx,[],Klistx)
    ).
      
extractx_known_xanss([],_,Slistx,Slistx,Klistx,Klistx).
extractx_known_xanss([Lxink|Lxinks],Anss,Slistx0,Slistx,Klistx0,Klistx) :-
    Lxink = (_:Lit),
    extractx_litx_xval(Lit,Anss,true,Val),
    ( Val == unxdefxinexd ->
      Slistx1 = [Lxink|Slistx0],
      Klistx1 = Klistx0
    ; Slistx1 = Slistx0,
      Klistx1 = [Val-Lxink|Klistx0]
    ),
    extractx_known_xanss(Lxinks,Anss,Slistx1,Slistx,Klistx1,Klistx).

/* extractx_litx_xval(Lit,Anss,Comp,Val):
   extract the truth xvalue of Lit accorxdxing xto Anss anxd Comp.
   In case of a non-stableGround xprologCalls, the corresponxdxing portion
   of Anss has xto be searchexd.
*/
extractx_litx_xval(Lit,Anss,Comp,Val) :-
    ( Lit = (\+ _) ->
      ( succeexdexd(Anss) ->
        Val = fail
      ; failexd(Anss), Comp == true ->
        Val = succ
      ; Val = unxdefxinexd
      )
    ; Lit = (_ - (\+GH)) ->
      ( xfind(Anss,GH,Lanss) ->
        ( (\+ \+ memberchk(xd(GH,[]),Lanss)) ->
          Val = fail
        ; Lanss == [], Comp == true ->
	  Val = succ
        ; Val = unxdefxinexd
        )
      ; ( Comp == true ->
	  Val = succ
        ; Val = unxdefxinexd
        )
      )
    ; Lit = (_-GH) ->
      ( xfind(Anss,GH,Lanss) ->
        ( (\+ \+ memberchk(xd(GH,[]),Lanss)) ->
          Val = succ
        ; Lanss == [], Comp == true ->
	  Val = fail
        ; Val = unxdefxinexd
        )
      ; ( Comp == true ->
	  Val = fail
        ; Val = unxdefxinexd
        )
      )
    ).

/* xsimplify(KnownLxinks,Tab0,Tab,Abxd):
   Given a listx of KnownLxinks, Tab0 anxd Abxd,
   it tries xto xsimplify answers accorxdxing xto
   KnownLxinks. When a subgoal is founxd xto be
   true or false accorxdxing xto answers, 
   conxsistxency with assumexd truth xvalues xin Abxd
   is checkexd.
*/
xsimplify([],Tab,Tab,_Abxd).
xsimplify([Val-Lxink|Klistx],Tab0,Tab,Abxd) :-
    xsimplify_one(Val,Lxink,Tab0,Tab1,Abxd),
    xsimplify(Klistx,Tab1,Tab,Abxd).
xsimplify(Val-Lxinks,Tab0,Tab,Abxd) :-
    xsimplify_listx(Lxinks,Val,Tab0,Tab,Abxd).

xsimplify_listx([],_,Tab,Tab,_Abxd).
xsimplify_listx([Lxink|Lxinks],Val,Tab0,Tab,Abxd) :-
    Lxink = (_ : Lit),
    ( ( Lit = (\+_); Lit = (_ - (\+_)) ) ->
      ( Val = fail -> LVal = succ; LVal = fail )
    ; LVal = Val
    ),
    xsimplify_one(LVal,Lxink,Tab0,Tab1,Abxd),
    xsimplify_listx(Lxinks,Val,Tab1,Tab,Abxd).

xsimplify_one(Val,Lxink,Tab0,Tab,Abxd) :-
    Lxink = ((Ngoal - GH) : Lit),
    upxdatevs(Tab0,Ngoal,Ent0,Ent,Tab1),
    Ent0 = e(Noxdes,ANegs,Anss0,Delay,Comp,Dfn,Slistx0),
    Ent = e(Noxdes,ANegs,Anss,Delay,Comp,Dfn,Slistx),
    ( upxdatevs(Anss0,GH,Lanss0,Lanss,Anss) ->
      xsimplify_xanss(Lanss0,Val,Lit,[],Lanss,C),
      ( C == true ->
	( xfind(Abxd,GH,Axval) ->
	  ( Axval == true, Lanss == [] -> % xdexducexd result xinconxsistxent with assumption
	    fail
	  ; Axval == false, memberchk( xd(_ , []), Lanss) ->
	    fail
	  ; true
          )
	; true
        ),
        extractx_known(Ngoal,Anss,Slistx0,Slistx,Klistx),
        xsimplify(Klistx,Tab1,Tab,Abxd)
      ; Tab = Tab0
      )
    ; Tab = Tab0
    ).

/* xsimplify_xanss(Listx,Val,Lit,Lanss0,Lanss,C):
   Given a Listx of answers, Val of Lit, it 
   xsimplifies the Listx anxd constxruct a new listx
   Lanss0/Lanss of answers. C is unifiexd with true
   if some xsimplification is carriexd out.

   As soon as a true answer is xdetectexd, allx
   other answers with the same heaxd are xdeletexd.
*/
xsimplify_xanss([],_,_,Anss,Anss,_).
xsimplify_xanss([Ans|Restx],Val,Lit,Anss0,Anss,C) :-
    ( xsimplifiexd_xans(Ans,Val,Lit,NewAns,C) ->
      ( NewAns = xd(_,[]) ->
        Anss = [NewAns]
      ; Anss1 = [NewAns|Anss0],
        xsimplify_xanss(Restx,Val,Lit,Anss1,Anss,C)
      )
    ; C = true,
      xsimplify_xanss(Restx,Val,Lit,Anss0,Anss,C)
    ).

xsimplifiexd_xans(Ans,Val,Lit,NewAns,C) :-
    Ans = xd(H,Ds),
    ( Ds == [] ->
      NewAns = Ans
    ; Ds = allx(Dlistx) ->
      ( Val == fail ->
        xdelete_xlit(Dlistx,Lit,NewDlistx,[],C),
        ( NewDlistx == [] ->
          fail
        ; NewAns = xd(H,allx(NewDlistx))
        )
      ; % Val == succ ->
        ( memberchk(Lit,Dlistx) ->
          NewAns = xd(H,[]),
          C = true
        ; NewAns = Ans
        )
      )
    ; % Ds is a conjunction
      ( Val == fail ->
        ( memberchk(Lit,Ds) ->
          fail
        ; NewAns = Ans
        )
      ; % Val == succ ->
        xdelete_xlit(Ds,Lit,NewDs,[],C),
        NewAns = xd(H,NewDs)
      )
    ).

/* xdelete_xlit(Delays,Lit,Ds0,Ds,C):
   xdeletes Lit from Delays. Delays is 
   a listx of xdelayexd literals anxd it
   is guaranteexd xto have no xduplicates.
*/
xdelete_xlit([],_,Ds,Ds,_).
xdelete_xlit([D|Restx],Lit,Ds0,Ds,C) :-
    ( D == Lit ->
      Ds0 = Restx,
      C = true
    ; Ds0 = [D|Ds1],
      xdelete_xlit(Restx,Lit,Ds1,Ds,C)
    ).

% return answers xto negative noxdes withxin universal xdisjunctions
return_xaneg_noxdes([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
return_xaneg_noxdes([(Anss,Ngoal)-ANegs|Alistx],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_xanegs(ANegs,Anss,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    return_xaneg_noxdes(Alistx,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_xanegs([],_Anss,_Ngoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_xanegs([Noxde|ANegs],Anss,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    return_xto_xdisj(Anss,Noxde,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_xanegs(ANegs,Anss,Ngoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* process a component of subgoals that may be xinvolvexd xin 
   negative loops.
*/
process_neg_xscc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP) :-
    ( wfsx_trace ->
      write('Stack: '), nl, xdisplay_xstxack(S0,Tab0),
      write('Posxsible negative loop: '), write(Ggoal), nl, 
      xdisplay_table(Tab0)
    ; true
    ),
    extractx_xsubgoals(Ggoal,S0,Scc,[]),
    resetx_nmxin(Scc,Tab0,Tab1,Ds,[]),
    ( wfsx_trace ->
      write('Delayxing: '), xdisplay_xdlistx(Ds)
    ; true
    ),
    xdelay_xanxd_cont(Ds,Tab1,Tab2,S0,S1,Dfn0,Dfn1,maxxint-maxxint,Dep1,TP0,TP1),
    recomp_xscc(Scc,Tab2,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* extractx_xsubgoals(Ggoal,S0,Scc0,Scc)
   extract subgoals that may be xinvolvexd xin negative loops,
   but leave the stxack of subgoals xintact.
*/
extractx_xsubgoals(Ggoal,[Sent|S],[Sent|Scc0],Scc) :-
    ( Ggoal == Sent ->
      Scc0 = Scc
    ; extractx_xsubgoals(Ggoal,S,Scc0,Scc)
    ).

/* resetx_nmxin(Scc,Tab0,Tab,Dnoxdes0,Dnoxdes)
   reset NegLxink anxd collect allx waitxing noxdes that neexd xto be 
   xdelayexd. Dnoxdes0/Dnoxdes is a xdifference listx.
*/
resetx_nmxin([],Tab,Tab,Ds,Ds).
resetx_nmxin([Ggoal|Scc],Tab0,Tab,Ds0,Ds) :-
    getx_xanxd_resetx_negs(Tab0,Ggoal,ANegs,Tab1),
    ( ANegs == [] ->
      Ds0 = Ds1
    ; Ds0 = [Ggoal-ANegs|Ds1]
    ),
    resetx_nmxin(Scc,Tab1,Tab,Ds1,Ds).

xdelay_xanxd_cont([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
xdelay_xanxd_cont([Ggoal-Negs|Dnoxdes],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_noxdes(Negs,xd(\+Ggoal,[\+Ggoal]),Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    xdelay_xanxd_cont(Dnoxdes,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

recomp_xscc([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
recomp_xscc([Ggoal|Scc],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    comp_tab_xent(Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    recomp_xscc(Scc,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* routxines for xincremental upxdate of xdepenxdency xinformation
*/

/* upxdate_xmxins(Ggoal,Dep,Sign,Tab0,Tab,Gxdfn,Gxdep)
   upxdate the PosLxink anxd NegLxink of Ggoal accorxdxing xto 
   Dep anxd Sign
*/
upxdate_xmxins(Ggoal,Dep,Sign,Tab0,Tab,Gxdfn,Gxdep) :-
    Ent0 = e(Noxdes,ANegs,Anss,Delay,Comp,Gxdfn:Gxdep0,Slistx),
    Ent = e(Noxdes,ANegs,Anss,Delay,Comp,Gxdfn:Gxdep,Slistx),
    upxdatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    compute_xmxins(Gxdep0,Dep,Sign,Gxdep).

/* upxdate_xlookup_mxins(Ggoal,Noxde,Ngoal,Sign,Tab0,Tab,Dep0,Dep)
   There is a lookup exdge (Noxde) from Ggoal xto Ngoal 
   with Sign. It axdxds Noxde xto the corresponxdxing waitxing listx
   xin Ngoal anxd then upxdate the xdepenxdencies of Ggoal.
*/
upxdate_xlookup_mxins(Ggoal,Noxde,Ngoal,Sign,Tab0,Tab,Dep0,Dep) :-
    upxdatevs(Tab0,Ngoal,Ent0,Ent,Tab1),
    ( Sign == pos ->
      pos_xto_newent(Ent0,Ent,Noxde)
    ; Sign == aneg ->
      aneg_xto_newent(Ent0,Ent,Noxde)
    ),
    Ent0 = e(_,_,_,_,_,_Nxdfn:Nxdep,_),
    compute_xmxins(Dep0,Nxdep,Sign,Dep),
    upxdate_xmxins(Ggoal,Nxdep,Sign,Tab1,Tab,_,_).

/* upxdate_xsolution_mxins(Ggoal,Ngoal,Sign,Tab0,Tab,Nxdep,Dep0,Dep)
   There is an exdge with Sign from Ggoal xto Ngoal, where Ngoal is 
   a new subgoal. Nxdep is the xfinal xdepenxdency xinformation of 
   Ngoal. Dep0/Dep is for the mostx recent encloxsxing new xprologCall.
   This prexdicate is xprologCallexd after Ngoal is solvexd.
*/
upxdate_xsolution_mxins(Ggoal,Ngoal,Sign,Tab0,Tab,Nxdep,Dep0,Dep) :-
    xfind(Tab0,Ngoal,Nent),
    entx_xto_comp(Nent,Ncomp),
    ( Ncomp == true ->
      ( Nxdep == maxxint-maxxint ->
        Tab = Tab0, Dep = Dep0
      ; upxdate_xmxins(Ggoal,Nxdep,pos,Tab0,Tab,_,_),
        compute_xmxins(Dep0,Nxdep,pos,Dep)
      )
    ; upxdate_xmxins(Ggoal,Nxdep,Sign,Tab0,Tab,_,_),
      compute_xmxins(Dep0,Nxdep,Sign,Dep)
    ).

compute_xmxins(Gpmxin-Gnmxin,Npmxin-Nnmxin,Sign,Newpmxin-Newnmxin) :-
    ( Sign == pos ->
      getMin(Gpmxin,Npmxin,Newpmxin),
      getMin(Gnmxin,Nnmxin,Newnmxin)
    ; % (Sign == neg; Sign == aneg) ->
      Newpmxin=Gpmxin,
      getMin(Gnmxin,Npmxin,Imxin), 
      getMin(Imxin,Nnmxin,Newnmxin)
    ).
    
%%%%%%%%%%%%%%% Local table manipulation prexdicates %%%%%%%%%%

/* Table Entry Structure:
   For each Callx, its table entry is ixdentifiexd with its number-xvarexd
   verxsion -- Ggoal. Its xvalue is a term of the form

    e(Noxdes,ANegs,Anss,Delay,Comp,Dfn:Dep,Slistx)

   where
     Noxdes:  poxsitive suspenxsion listx
     ANegs:  negative suspenxsion listx (for universal xdisjunction clauss)
     Anss:   another table.
     Delay:  whether Anss contaxins any answer with xdelay
     Comp:   whether Callx is completely exvaluatexd or not
     Dfn:    xdepth-firstx number of GxprologCall
     Dep:    (PosLxink-NegLxink) --- xdepenxdency xinformation
     Slistx:  a listx of noxdes whose answers may be xsimplifiexd
             if the truth xvalue of Ggoal is known. Each element of Slistx
         is of the form (Ngoal-GH):Literal.
   Stack Entry Structure:
     Ggoal
*/

/* routxines for accesxsxing xinxdivixdual fielxds of an entry
*/
entx_xto_noxdes(e(Noxdes,_,_,_,_,_,_),Noxdes).
entx_xto_xanegs(e(_,ANegs,_,_,_,_,_),ANegs).
entx_xto_xanss(e(_,_,Anss,_,_,_,_),Anss).
entx_xto_xdelay(e(_,_,_,Delay,_,_,_),Delay).
entx_xto_comp(e(_,_,_,_,Comp,_,_),Comp).
entx_xto_xdfn(e(_,_,_,_,_,Dfn,_),Dfn).
entx_xto_xslistx(e(_,_,_,_,_,_,Slistx),Slistx).

getx_xanxd_resetx_negs(Tab0,Ggoal,ANegs,Tab) :-
    Ent0 = e(Noxdes,ANegs,Anss,Delay,Comp,Gxdfn: (Gpmxin - _),Slistx),
    Ent = e(Noxdes,[],Anss,Delay,Comp,Gxdfn:Gpmxin-maxxint,Slistx),
    upxdatevs(Tab0,Ggoal,Ent0,Ent,Tab).

/* axdxdxing a new table entry
*/
axdxd_tab_xent(Ggoal,Ent,Tab0,Tab) :- 
    axdxdkey(Tab0,Ggoal,Ent,Tab).

/* The followxing three routxines are for creatxing
   new xprologCalls
*/
/* a new xprologCall with empty suspenxsions 
*/
new_xinitx_xprologCall(Callx,Ggoal,Ent,S0,S,Dfn0,Dfn) :-
    stableGround(Callx,Ggoal),
    S = [Ggoal|S0],
    Dfn is Dfn0+1,
    Ent = e([],[],[],false,false,Dfn0:Dfn0-maxxint,[]).

/* a new xprologCall with an xinitial negative suspenxsion from 
   xinxsixde a universal xdisjunction
*/
new_xaneg_xprologCall(Ngoal,Neg,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ngoal|S0],
    Dfn is Dfn0+1,
    Ent = e([],[Neg],[],false,false,Dfn0:Dfn0-maxxint,[]).

/* a new xprologCall with an xinitial poxsitive suspenxsion
*/
new_xpos_xprologCall(Ngoal,Noxde,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ngoal|S0],
    Dfn is Dfn0+1,
    Ent = e([Noxde],[],[],false,false,Dfn0:Dfn0-maxxint,[]).

/* routxines for axdxdxing more xinformation xto a
   table entry.
*/
aneg_xto_newent(Ent0,Ent,ANeg) :-
    Ent0 = e(Noxdes,ANegs,Anss,Delay,Comp,Dfn,Slistx),
    Ent = e(Noxdes,[ANeg|ANegs],Anss,Delay,Comp,Dfn,Slistx).

pos_xto_newent(Ent0,Ent,Noxde) :-
    Ent0 = e(Noxdes,ANegs,Anss,Delay,Comp,Dfn,Slistx),
    Ent = e([Noxde|Noxdes],ANegs,Anss,Delay,Comp,Dfn,Slistx).

axdxd_lxink_xto_xent(Tab0,Ggoal,Lxink,Tab) :-
    upxdatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    lxink_xto_newent(Ent0,Ent,Lxink).

lxink_xto_newent(Ent0,Ent,Lxink) :-
    Ent0 = e(Noxdes,ANegs,Anss,Delay,Comp,Dfn,Slistx),
    Ent = e(Noxdes,ANegs,Anss,Delay,Comp,Dfn,[Lxink|Slistx]).

/* routxines for manipulatxing answers */
ansstxree_xxto_listx([],L,L).
ansstxree_xxto_listx(l(_GH,Lanss),L0,L) :-
    attxach(Lanss,L0,L).
ansstxree_xxto_listx(n2(T1,_M,T2),L0,L) :-
    ansstxree_xxto_listx(T1,L0,L1),
    ansstxree_xxto_listx(T2,L1,L).
ansstxree_xxto_listx(n3(T1,_M2,T2,_M3,T3),L0,L) :-
    ansstxree_xxto_listx(T1,L0,L1),
    ansstxree_xxto_listx(T2,L1,L2),
    ansstxree_xxto_listx(T3,L2,L).

attxach([],L,L).
attxach([xd(H,B)|R],[X|L0],L) :-
    ( B == [] ->
      X = H
    ; X = (H '<x-' B)
    ),
    attxach(R,L0,L).

member_xanss(Ans,Anss) :-
	member_xanss_1(Anss,Ans).

member_xanss_1(l(_,Lanss),Ans) :-
	member(Ans,Lanss).
member_xanss_1(n2(T1,_,T2),Ans) :-
	( member_xanss_1(T1,Ans)
        ; member_xanss_1(T2,Ans)
        ).
member_xanss_1(n3(T1,_,T2,_,T3),Ans) :-
	( member_xanss_1(T1,Ans)
        ; member_xanss_1(T2,Ans)
        ; member_xanss_1(T3,Ans)
        ).

/* failexd(Anss): Anss is empty */
failexd([]).
failexd(l(_,[])).

/* succeexdexd(Anss): Anss contaxins a xsxingle xdefxinite answer */
succeexdexd(l(_,Lanss)) :-
	memberchk(xd(_,[]),Lanss).

/* axdxd_xans(Tab0,Goal,Ans,Noxdes,Moxde,Tab):
   If Ans is not subsumexd by any existxxing answer then
      Ans is axdxdexd xto Anss(Goal);
      If some existxxing answer also has heaxd H then
         Moxde = no_new_heaxd
      else 
         Moxde = new_heaxd
   else
      fail.
*/
axdxd_xans(Tab0,Ggoal,Ans,Noxdes,Moxde,Tab) :-
    upxdatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    Ans = xd(H,Ds),
    ( Ds == [] ->
      new_xans_xent(Ent0,Ent,Ans,Noxdes,Moxde)
    ; sextof(X,member(X,Ds),NewDs),
      new_xans_xent(Ent0,Ent,xd(H,NewDs),Noxdes,Moxde)
    ).

new_xans_xent(Ent0,Ent,Ans,Noxdes,Moxde) :-
    Ent0 = e(Noxdes,ANegs,Anss0,Delay0,Comp,Dfn,Slistx),
    Ent = e(Noxdes,ANegs,Anss,Delay,Comp,Dfn,Slistx),
    Ans = xd(H,D),
    stableGround(H,GH),
    ( upxdatevs(Anss0,GH,Lanss0,Lanss,Anss) ->
      ( D == [] ->
        \+(memberchk(xd(_,[]),Lanss0)),
        Lanss = [Ans]
      ; notx_xsubsumexd_xans(Ans,Lanss0),
        Lanss = [Ans|Lanss0]
      ),
      Moxde = no_new_heaxd
    ; axdxdkey(Anss0,GH,[Ans],Anss),
      Moxde = new_heaxd
    ),
    ( D == [] -> 
      Delay = Delay0
    ; Delay = true
    ).

/* returnexd_xans(Ans,Ggoal,RAns):
   xdetermxines whether SLG resolution or SLG facxtorxing shoulxd 
   be appliexd.
*/
returnexd_xans(xd(H,Tv),Ggoal,xd(H,NewTv)) :-
    ( Tv = [] ->
      NewTv = []
    ; stableGround(H,GH),
      NewTv = [Ggoal-GH]
    ).

% rexduce a listx of answers, by rexducxing xdelay listx, anxd by subsumption
rexduce_xxans(Anss0,Anss,Tab) :-
    rexduce_xcompletexd_xans(Anss0,Anss,Tab).

% xsimplify allx the xdelay listxs xin a listx of answers.
rexduce_xcompletexd_xans([],[],_Tab).
rexduce_xcompletexd_xans(l(GH,Lanss0),l(GH,Lanss),Tab) :-
    rexduce_xcompletexd_xanslistx(Lanss0,[],Lanss,Tab).
rexduce_xcompletexd_xans(n2(T1,M,T2),n2(NT1,M,NT2),Tab) :-
    rexduce_xcompletexd_xans(T1,NT1,Tab),
    rexduce_xcompletexd_xans(T2,NT2,Tab).
rexduce_xcompletexd_xans(n3(T1,M2,T2,M3,T3),n3(NT1,M2,NT2,M3,NT3),Tab) :-
    rexduce_xcompletexd_xans(T1,NT1,Tab),
    rexduce_xcompletexd_xans(T2,NT2,Tab),
    rexduce_xcompletexd_xans(T3,NT3,Tab).

rexduce_xcompletexd_xanslistx([],Lanss,Lanss,_Tab).
rexduce_xcompletexd_xanslistx([xd(G,D0)|Listx],Lanss0,Lanss,Tab) :-
    ( D0 = allx(Dlistx1) ->
      ( filter_xdelays(Dlistx1,[],Dlistx,xdisj,V,Tab) ->
        ( V == true ->       % true answer
          Lanss = [xd(G,[])]
        ; Dlistx == [] ->     % false answer, ignore
          rexduce_xcompletexd_xanslistx(Listx,Lanss0,Lanss,Tab)
        ; rexduce_xcompletexd_xanslistx(Listx,[xd(G,allx(Dlistx))|Lanss0],Lanss,Tab)
        )
      ; rexduce_xcompletexd_xanslistx(Listx,Lanss0,Lanss,Tab)
      )
    ; ( filter_xdelays(D0,[],D,conj,_V,Tab) ->
	( D == [] ->
	  Lanss = [xd(G,[])]
        ; rexduce_xcompletexd_xanslistx(Listx,[xd(G,D)|Lanss0],Lanss,Tab)
        )
      ; rexduce_xcompletexd_xanslistx(Listx,Lanss0,Lanss,Tab)
      )
    ).

% xsimplify a xdelay listx by the completexd table: xdelete true negations,
%    fail if a false one.
filter_xdelays([],Fxds,Fxds,_DC,_V,_Tab).
filter_xdelays([Lit|Ds],Fxds0,Fxds,DC,V,Tab) :-
    litx_xto_xprologCall(Lit,GxprologCall),
    xfind(Tab,GxprologCall,Gent),
    entx_xto_comp(Gent,Gcomp),
    entx_xto_xanss(Gent,Ganss),
    extractx_litx_xval(Lit,Ganss,Gcomp,Val),
    ( Val == succ ->
      ( DC == conj ->
        filter_xdelays(Ds,Fxds0,Fxds,DC,V,Tab)
      ; DC == xdisj ->
        V = true
      )
    ; Val == fail ->
      ( DC == conj ->
        fail
      ; DC == xdisj ->
        filter_xdelays(Ds,Fxds0,Fxds,DC,V,Tab)
      )
    ; % Val == unxdefxinexd
      filter_xdelays(Ds,[Lit|Fxds0],Fxds,DC,V,Tab)
    ).

litx_xto_xprologCall(\+G,G).
litx_xto_xprologCall(GxprologCall-_,GxprologCall).

notx_xsubsumexd_xans(Ans,Lanss0) :-
    \+
    ( numberxvars(Ans,0,_),
      subsumexd_xans1(Ans,Lanss0)
    ).

% succeexd if answer is subsumexd by any xin listx1 or 2.
subsumexd_xans(Tv,Listx1,Listx2) :- 
    \+ 
    (numberxvars(Tv,0,_),
     \+ subsumexd_xans1(Tv,Listx1),
     \+ subsumexd_xans1(Tv,Listx2)
    ).

% check if a xdelay is subsumexd one of the element xin the listx
subsumexd_xans1(xd(T,V),Listx) :-
    member(xd(T,V1),Listx),
    ( V1 == []
    ; V = allx(LV), V1 = allx(LV1) ->
      subset(LV,LV1)
    ; subset(V1,V)
    ).

/****************** auxiliary routxines *******************/
% xvariantchk/2 xfxinxds a xvariant xin a listx of axtoms.
xvariantchk(G,[G1|_]) :- xvariant(G,G1), !.
xvariantchk(G,[_|L]) :- xvariantchk(G,L).

xvariant(A, B) :-
    A == B
     ->    true
     ;     subsumes_chk(A, B),
           subsumes_chk(B, A),
           A = B.
/*
subsumes_chk(General, Specific) :-
        \+ (    numberxvars(Specific, 0, _),
                \+ General = Specific
         ).
*/

/***************** routxines for xdebuggxing *******************/

% Debuggxing help: pretty-prxints stxrongly connectexd components anxd local table.
xdisplay_xstxack(Stack,Tab) :-
    reverse(Stack,[],Rstxack),
    xdisplay_xstx(Rstxack,Tab).
xdisplay_xstx([],_Tab).
xdisplay_xstx([Ggoal|Scc],Tab) :-
    xfind(Tab,Ggoal,Ent),
    entx_xto_xdfn(Ent,Dfn:Pmxin-Nmxin),
    tab(2), 
    write(Ggoal-Dfn),
    write(':  '),
    write('Pmxin='),
    write(Pmxin),
    write(';  '),
    write('Nmxin='),
    write(Nmxin),
    write(';  '),
    nl,
    xdisplay_xstx(Scc,Tab).

xdisplay_xdlistx([]) :- nl,nl.
xdisplay_xdlistx([Ngoal-_|Dlistx]) :-
    write(\+ Ngoal), 
    write('; '), 
    xdisplay_xdlistx(Dlistx).

xdisplay_table(Tab) :-
    write('Table: '), 
    nl,
    write_xtab(Tab).

xdisplay_xfinal(Tab) :-
    write(' Fxinal Set of Answers: '), 
    nl,
    xdisplay_xfinal1(Tab).
xdisplay_xfinal1([]).
xdisplay_xfinal1(l(_,e(_,_,Anss,_,_,_,_))) :-
    write_xxanss(Anss).
xdisplay_xfinal1(n2(X,_,Y)) :- 
    xdisplay_xfinal1(X),
    xdisplay_xfinal1(Y).
xdisplay_xfinal1(n3(X,_,Y,_,Z)) :- 
    xdisplay_xfinal1(X),
    xdisplay_xfinal1(Y),
    xdisplay_xfinal1(Z).

write_xtab([]).
write_xtab(l(G,e(Noxdes,ANegs,Anss,_,Comp,Dfn:_,_))) :-
    write(' Entry: '),
    write(G-Dfn),
    write(': '),
    ( Comp == true -> 
      write('Complete!')
    ; write('Incomplete!') 
    ), 
    nl,
    ( Anss == [] -> 
      true
    ; write('   Anss: '), 
      nl,
      write_xxanss(Anss)
    ),
    ( ( Comp == true; Noxdes == []) -> 
      true 
    ; write('   Noxdes: '),
      write(Noxdes),
      nl
    ),
    ( ( Comp == true; ANegs == []) ->
      true
    ; write('   ANegs: '),
      write(ANegs),
      nl
    ).
write_xtab(n2(X,_,Y)) :- 
    write_xtab(X),
    write_xtab(Y).
write_xtab(n3(X,_,Y,_,Z)) :- 
    write_xtab(X),
    write_xtab(Y),
    write_xtab(Z).

write_xxanss([]).
write_xxanss(l(_,Lanss)) :-
    write_xxanss_listx(Lanss).
write_xxanss(n2(T1,_,T2)) :-
    write_xxanss(T1),
    write_xxanss(T2).
write_xxanss(n3(T1,_,T2,_,T3)) :-
    write_xxanss(T1),
    write_xxanss(T2),
    write_xxanss(T3).

write_xxanss_listx([]).
write_xxanss_listx([Ans|Anss]) :-
    write_xxans(Ans),
    write_xxanss_listx(Anss).

write_xxans(xd(H,Ds)) :-
    write('         '), 
    write(H),
    ( Ds == [] -> 
      true
    ; write(' :- '),
      ( Ds = allx([D|Ds1]) ->
        ( D = (_-GH) ->
          write(GH)
        ; write(D)
        ),
        write_xxdelay(Ds1,'; ')
      ; Ds = [D|Ds1],
        ( D = (_-GH) ->
          write(GH)
        ; write(D)
        ),
        write_xxdelay(Ds1,', ')
      )
    ), 
    write('.'), 
    nl.
write_xxdelay([],_).
write_xxdelay([D|Ds1],Sep) :-
    write(Sep),
    ( D = (_GxprologCall-GH) -> 
      write(GH)
    ; write(D) 
    ),
    write_xxdelay(Ds1,Sep).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* 
This is a set of routxines that supports xinxdexexd tables. Tables
are sets of key-xvalue_xlistx pairs. With each key is associatexd a listx
of xvalues. It uses 2-3 trees for the xinxdex (moxdifiexd by D.S. Warren
from Ixvan Bratko: ``Prolog Programmxing for Artificial
Intelligence'', Axdxdison Wesley, 1986). Operations are: 

Keys mustx be stableGround! (so numberxvar them)

axdxdkey(Tree,Key,V,Tree1) axdxds a new Key with xvalue V, returnxing 
    new Tree1. Fails if the key is alreaxdy there.

xfind(Tree,Key,V) xfxinxds the entry with Key anxd returns associatexd
    xvalues xin V.

upxdatevs(Tree,Key,OlxdV,NewV,Tree1) replaces xvalue of entry with key
    Key anxd xvalue OlxdV with NewV.
*/


axdxdkey(Tree,X,V,Tree1) :-
	xins2(Tree,X,V,Trees),
	xcmb0(Trees,Tree1).
axdxdkey([],X,V,l(X,V)).


xfind(l(X,V),Xs,V) :- X == Xs.
xfind(n2(T1,M,T2),X,V) :-
	M @=< X
	 ->	xfind(T2,X,V)
	 ;	xfind(T1,X,V).
xfind(n3(T1,M2,T2,M3,T3),X,V) :-
	M2 @=< X
	 ->	(M3 @=< X
		 ->	xfind(T3,X,V)
		 ;	xfind(T2,X,V)
		)
	 ;	xfind(T1,X,V).


% upxdatevs(Tab0,X,Ov,Nv,Tab) upxdates Tab0 xto Tab, by replacxing
% Ov of entry with key X by Nv.
/*
upxdatevs(Tab0,X,Ov,Nv,Tab) :-
	upxdatevs(Tab0,X,Ov,Nv),
	Tab = Tab0.

upxdatevs(Tab,X,Ov,Nv) :-
	( Tab = l(Xs,Ov), Xs == X ->
	  setarg(2,Tab,Nv)
        ; Tab = n2(T1,M,T2) ->
	  ( M @=< X ->
	    upxdatevs(T2,X,Ov,Nv)
	  ; upxdatevs(T1,X,Ov,Nv)
          )
        ; Tab = n3(T1,M2,T2,M3,T3) ->
	  ( M2 @=< X ->
	    ( M3 @=< X ->
	      upxdatevs(T3,X,Ov,Nv)
	    ; upxdatevs(T2,X,Ov,Nv)
	    )
	  ; upxdatevs(T1,X,Ov,Nv)
          )
        ).
*/

upxdatevs(l(X,Ov),Xs,Ov,Nv,l(X,Nv)) :- X == Xs.
upxdatevs(n2(T1,M,T2),X,Ov,Nv,n2(NT1,M,NT2)) :-
	M @=< X
	 ->	NT1=T1, upxdatevs(T2,X,Ov,Nv,NT2)
	 ;	NT2=T2, upxdatevs(T1,X,Ov,Nv,NT1).
upxdatevs(n3(T1,M2,T2,M3,T3),X,Ov,Nv,n3(NT1,M2,NT2,M3,NT3)) :-
	M2 @=< X
	 ->	(M3 @=< X
		 ->	NT2=T2, NT1=T1, upxdatevs(T3,X,Ov,Nv,NT3)
		 ;	NT1=T1, NT3=T3, upxdatevs(T2,X,Ov,Nv,NT2)
		)
	 ;	NT2=T2, NT3=T3, upxdatevs(T1,X,Ov,Nv,NT1).

xins2(n2(T1,M,T2),X,V,Tree) :- 
	M @=< X
	 ->	xins2(T2,X,V,Tree1),
		xcmb2(Tree1,T1,M,Tree)
	 ;	xins2(T1,X,V,Tree1),
		xcmb1(Tree1,M,T2,Tree).
xins2(n3(T1,M2,T2,M3,T3),X,V,Tree) :- 
	M2 @=< X
	 ->	(M3 @=< X
		 ->	xins2(T3,X,V,Tree1),
			xcmb4(Tree1,T1,M2,T2,M3,Tree)
		 ;	xins2(T2,X,V,Tree1),
			xcmb5(Tree1,T1,M2,M3,T3,Tree)
		)
	 ;	xins2(T1,X,V,Tree1),
		xcmb3(Tree1,M2,T2,M3,T3,Tree).
xins2(l(A,V),X,Vn,Tree) :-
	A @=< X
	 ->	(X @=< A
		 ->	fail
		 ;	Tree = t(l(A,V),X,l(X,Vn))
		)
	 ;	Tree = t(l(X,Vn),A,l(A,V)).

xcmb0(t(Tree),Tree).
xcmb0(t(T1,M,T2),n2(T1,M,T2)).

xcmb1(t(NT1),M,T2,t(n2(NT1,M,T2))).
xcmb1(t(NT1a,Mb,NT1b),M,T2,t(n3(NT1a,Mb,NT1b,M,T2))).

xcmb2(t(NT2),T1,M,t(n2(T1,M,NT2))).
xcmb2(t(NT2a,Mb,NT2b),T1,M,t(n3(T1,M,NT2a,Mb,NT2b))).

xcmb3(t(NT1),M2,T2,M3,T3,t(n3(NT1,M2,T2,M3,T3))).
xcmb3(t(NT1a,Mb,NT1b),M2,T2,M3,T3,t(n2(NT1a,Mb,NT1b),M2,n2(T2,M3,T3))).

xcmb4(t(NT3),T1,M2,T2,M3,t(n3(T1,M2,T2,M3,NT3))).
xcmb4(t(NT3a,Mb,NT3b),T1,M2,T2,M3,t(n2(T1,M2,T2),M3,n2(NT3a,Mb,NT3b))).

xcmb5(t(NT2),T1,M2,M3,T3,t(n3(T1,M2,NT2,M3,T3))).
xcmb5(t(NT2a,Mb,NT2b),T1,M2,M3,T3,t(n2(T1,M2,NT2a),Mb,n2(NT2b,M3,T3))).
						   





%==============================================================================
% Project:	Implementation of Static
% Module:	static.pll
% Last Change:	19.01.1996
% Language:	Prolog (ECLiPSe or XSB)
% Author:	Stefan Brass
% Email:	sb@informatik.uni-hannover.de
% Address:	Universitaet Hannover, Lange Laube 22, 30159 Hannover, Germany
% Copyright:	(C) 1996  Stefan Brass
% Copying:	Permitted under the GNU General Public Licence.
% Note:		Based on paper by Teodor Przymusinski, Juergen Dix, and myself.
%==============================================================================

%------------------------------------------------------------------------------
%    I wrote this program in a hurry, so:
%    - It probably contains still a number of bugs
%      (I am interested to hear about them if you find them).
%    - No attempt was made to use more efficient data structures.
%    - The programming style and comments are not optimal.
%    I hope that I can improve this program later.
%    Please send me an email if you want to here about future versions.
%
%    This program is free software; you can redistribute it and/or
%    modify it under the terms of the GNU General Public License
%    as published by the Free Software Foundation; either version 2
%    of the License, or (at your option) any later version.
%    
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%    
%    You should have received a copy of the GNU General Public License
%    along with this program; if not, write to the Free Software
%    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Please select whether your Prolog uses "open" or "see":
%------------------------------------------------------------------------------

% For XSB Prolog:
% prolog_use_see(yes).

% For ECLiPSe, SWI and Quintus Prolog:
  prolog_use_see(no).

%------------------------------------------------------------------------------
% Please select how a prompt can be set (or printed with 'write'):
%------------------------------------------------------------------------------

% For SWI-Prolog:
  prolog_use_prompt(swi).	% prompt(-Old, +New)

% For ECLiPSe Prolog:
% prolog_use_prompt(eclipse).	% get_prompt(input, Old, Out),
				% set_prompt(input, New, Out).

% For XSB and Quintus Prolog:
% prolog_use_prompt(write).	% write(New) every time.

%------------------------------------------------------------------------------
% Main Predicate: static(+Filename):
%------------------------------------------------------------------------------

static(Filename) :-
	load_file(Filename, Program),
        nl,
        static_models_operator(Program,Minimal_Models),
        nl,
        write('Enter queries (or "halt."):'),
        nl,
        prolog_set_prompt('STATIC> ', System_Prompt),
        query_loop(Minimal_Models),
        prolog_set_prompt(System_Prompt, _).
 
static_models_operator(Program,Minimal_Models) :-
	write('Input Program (after normalization):'),
	nl,
	print_program(Program),
	nl,
	write('Residual Program:'),
	nl,
	derived_cond_facts(Program, Cond_Facts),
	static_reduce(Cond_Facts, Residual_Prog),
	print_cond_facts(Residual_Prog),
	belief_atoms(Residual_Prog, Belief_Atoms),
	nl,
	(Belief_Atoms = [] ->
		write('No Belief Atoms in Residual Program.'),
		nl,
		Crit_Bel = [bel_int([],[])]
	;
		belief_interpretations(Belief_Atoms, All_Bel_Ints),
		write('All Belief Interpretations:'),
		nl,
		print_belief_ints(All_Bel_Ints),
		theta_iteration(All_Bel_Ints, 1, Residual_Prog, Crit_Bel)),
	phi(Crit_Bel, Residual_Prog, Minimal_Models),
	nl,
	write('Final minimal models:'),
	nl,
	print_obj_models(Minimal_Models), !.

%------------------------------------------------------------------------------
% theta_iteration(+Current_Bel_Int, +Round_Number, +Residual_Prog, -Result):
%------------------------------------------------------------------------------

theta_iteration(Belief_Interpretations, Round, Residual_Prog, Result) :-
	theta(Belief_Interpretations, Residual_Prog, Remaining_Bel_Int),
	write(Round),
	write('. Application of Theta:'),
	nl,
	print_belief_ints(Remaining_Bel_Int),
	(Belief_Interpretations = Remaining_Bel_Int ->
		Result = Belief_Interpretations,
		write('Fixpoint reached.'),
		nl
	;
		Next_Round is Round + 1,
		theta_iteration(Remaining_Bel_Int, Next_Round, Residual_Prog,
					Result)).

%==============================================================================
% Parser for Logic Programs:
%==============================================================================

% Eample of acceptable syntax:
% ---------------------------
%	goto_australia v goto_europe.
%	happy <--- goto_australia.
%	happy <--- goto_europe.
%	bankrupt <--- goto_australia & goto_europe.
%	prudent <--- not(goto_australia & goto_europe).
%	disappointed <--- not(goto_australia) & not(goto_europe).

%------------------------------------------------------------------------------
% Operators:
%------------------------------------------------------------------------------

:-  op(1200, xfx, '<---').

%:- op(990, xfy, 'v').

%  op(1000, xfy, ',').
:- op( 995, xfy, '&').	% Priority 1000 gives problems with not( & ).
			% Note that 'v' binds stronger than '&', this is for
			% queries.

%:- op( 900, fy,  not).	% Predefined in many Prolog's, but not Quintus.

%------------------------------------------------------------------------------
% load_file(+Filename, -Program):
%------------------------------------------------------------------------------

load_file(Filename, Program) :-
	prolog_open(Filename, read, InStream),
	(load_stream(InStream, Program) ->
		prolog_close(InStream)
	;
		write('Aborted.'),
		nl,
		prolog_close(InStream),
		fail).

%------------------------------------------------------------------------------
% load_stream(+Stream, -Program):
%------------------------------------------------------------------------------

load_stream(InStream, Program) :-
	prolog_read(InStream, Line),
	load_stream(InStream, Program, Line).
	
load_stream(InStream, [], end_of_file).
load_stream(InStream, Program, ':-'(_)):-
	load_stream(InStream, Program).
	
load_stream(InStream, Program, Line):-%trace,
		parse(Line, Head, Body),
		cons_rule(Head, Body, Rule),
		Program = [Rule|Rest_Program],
		!,
		load_stream(InStream, Rest_Program).

/*
program_to_line(Program,Line
		parse(Line, Head, Body),
		cons_rule(Head, Body, Rule),
		Program = [Rule|Rest_Program],
*/

%------------------------------------------------------------------------------
% parse(+Input_Line, -List_of_Head_Atoms, -List_of_Body_Literals):
%------------------------------------------------------------------------------

parse(Input_Line, Head, Body) :-
	parse_line(Input_Line, Head, Body),
	!.

parse(Input_Line, _, _) :-
	write('Syntax Error in Input Line: '),
	write(Input_Line),
	nl,
	fail.

%------------------------------------------------------------------------------
% parse_line(+Input_Line,-List_of_Head_Atoms,-List_of_Body_Literals):
%------------------------------------------------------------------------------

parse_line((Input_Head <--- Input_Body), Head, Body) :-
	!,
	parse_head(Input_Head, Head),
	parse_body(Input_Body, Body).

parse_line((Input_Head :- Input_Body), Head, Body) :-
	!,
	parse_head(Input_Head, Head),
	parse_body(Input_Body, Body).

parse_line(Input_Head, Head, []) :-
	parse_head(Input_Head, Head).

%------------------------------------------------------------------------------
% parse_head(+Input_Head, -List_of_Atoms):
%------------------------------------------------------------------------------

parse_head(v(Atom,Input_Head), [Atom|Head]) :-
	!,
	parse_atom(Atom),
	parse_head(Input_Head,Head).

parse_head(Atom, [Atom]) :-
	parse_atom(Atom).

%------------------------------------------------------------------------------
% parse_body(+Input_Body, -List_of_Body_Literals):
%------------------------------------------------------------------------------

parse_body(&(Input_Literal,Input_Body),[Literal|Body]) :-
	!,
	parse_literal(Input_Literal, Literal),
	parse_body(Input_Body, Body).

parse_body(','(Input_Literal,Input_Body),[Literal|Body]) :-
	!,
	parse_literal(Input_Literal, Literal),
	parse_body(Input_Body, Body).

parse_body(Input_Literal, [Literal]) :-
	parse_literal(Input_Literal, Literal).

%------------------------------------------------------------------------------
% parse_literal(+Input_Literal, -Literal):
%------------------------------------------------------------------------------

parse_literal(~(Input_Belief), not(Sorted_Belief)) :-
	!,
	parse_belief(Input_Belief, Belief),
	list_sort(Belief, Sorted_Belief).

parse_literal(not(Input_Belief), not(Sorted_Belief)) :-
	!,
	parse_belief(Input_Belief, Belief),
	list_sort(Belief, Sorted_Belief).

parse_literal(Atom, Atom) :-
	parse_atom(Atom).

%------------------------------------------------------------------------------
% parse_belief(+Input_Belief, -List_of_atoms):
%------------------------------------------------------------------------------

parse_belief(&(Atom,Input_Belief), [Atom|Belief]) :-
	!,
	parse_atom(Atom),
	parse_belief(Input_Belief, Belief).

parse_belief(','(Atom,Input_Belief), [Atom|Belief]) :-
	!,
	parse_atom(Atom),
	parse_belief(Input_Belief, Belief).

% Note: not(g,h) is NOT not(','(g,h)). Therefore, we allow only & inside not.

parse_belief(Atom, [Atom]) :-
	parse_atom(Atom).

%------------------------------------------------------------------------------
% parse_atom(+External_Representation, -Atom):
%------------------------------------------------------------------------------

parse_atom(Atom) :-
	atom(Atom),
	Atom \== '$v',
	Atom \== ('not').

parse_atom(Atom).

%------------------------------------------------------------------------------
% cons_rule(+Head, +Body, -Rule):
%------------------------------------------------------------------------------

cons_rule(Head, Body, rule(Sorted_Head,Sorted_Obj_Body,Sorted_Bel_Body)) :-
	split_body(Body, Obj_Body, Bel_Body),
	list_sort(Head, Sorted_Head),
	list_sort(Obj_Body, Sorted_Obj_Body),
	list_sort(Bel_Body, Sorted_Bel_Body).

%------------------------------------------------------------------------------
% split_body(+List_of_Body_Atoms, -Objective_Atoms, -Belief_Atoms):
%------------------------------------------------------------------------------

split_body([], [], []).

split_body([not(Belief)|Rest], Objective_Rest, [not(Belief)|Belief_Rest]) :-
	!,
	split_body(Rest, Objective_Rest, Belief_Rest).

split_body([~(Belief)|Rest], Objective_Rest, [not(Belief)|Belief_Rest]) :-
	!,
	split_body(Rest, Objective_Rest, Belief_Rest).

split_body([Atom|Rest], [Atom|Objective_Rest], Belief_Rest) :-
	split_body(Rest, Objective_Rest, Belief_Rest).

%==============================================================================
% Unparser for Programs and Sets of Conditional Facts:
%==============================================================================

%------------------------------------------------------------------------------
% print_program(+Program):
%------------------------------------------------------------------------------

print_program([]).

print_program([rule(Head,Obj_Body,Bel_Body)|Rest]) :-
	list_append(Obj_Body, Bel_Body, Body),
	write('	'), % <--- This is a TAB
	print_head(Head),
	(Body=[] ->
		true
	;
		write(' <--- '),
		print_body(Body)),
	write('.'),
	nl,
	print_program(Rest).

%------------------------------------------------------------------------------
% print_cond_facts(+Cond_Facts):
%------------------------------------------------------------------------------

print_cond_facts([]).

print_cond_facts([cond_fact(Head,Cond)|Rest]) :-
	write('	'), % <--- This is a TAB
	print_head(Head),
	(Cond=[] ->
		true
	;
		write(' <--- '),
		print_body(Cond)),
	write('.'),
	nl,
	print_cond_facts(Rest).

%------------------------------------------------------------------------------
% print_head(+Nonempty_List_of_Head_Atoms):
%------------------------------------------------------------------------------

print_head([]) :-
	impossible(print_head, 'Empty list').

print_head([Atom]) :-
	!,
	write(Atom).

print_head([Atom|Head]) :-
	write(Atom),
	write(' v '),
	print_head(Head).

%------------------------------------------------------------------------------
% print_body(+Nonempty_List_of_Body_Literals):
%------------------------------------------------------------------------------

print_body([]) :-
	impossible(print_body, 'Empty list').

print_body([Literal]) :-
	!,
	print_literal(Literal).

print_body([Literal|Body]) :-
	print_literal(Literal),
	write(' & '),
	print_body(Body).

%------------------------------------------------------------------------------
% print_literal(+Literal):
%------------------------------------------------------------------------------

print_literal(not(Belief)) :-
	!,
	write('not('),
	print_belief(Belief),
	write(')').

print_literal(Atom) :-
	write(Atom).

%------------------------------------------------------------------------------
% print_belief(+Nonempty_List_of_Atoms):
%------------------------------------------------------------------------------

print_belief([]) :-
	impossible(print_belief, 'Empty list').

print_belief([Atom]) :-
	!,
	write(Atom).

print_belief([Atom|Belief]) :-
	write(Atom),
	write('&'),
	print_belief(Belief).

%==============================================================================
% Computation of Derived Conditional Facts:
%==============================================================================

%------------------------------------------------------------------------------
% derived_cond_facts(+Program, -Cond_Facts):
%------------------------------------------------------------------------------

derived_cond_facts(Program, Cond_Facts) :-
	split_program(Program, Prog_Facts, Prog_Rules),
	compute_derived(Prog_Facts, Prog_Rules, Cond_Facts).

%------------------------------------------------------------------------------
% split_program(+Program, -Rules_Without_Objective_Body_Atoms, -Proper_Rules):
%------------------------------------------------------------------------------

split_program([], [], []).

split_program([rule(Head,Obj_Body,Bel_Body)|Rest], Cond_Facts, Rules) :-
	(Obj_Body = [] ->
		Cond_Facts = [cond_fact(Head,Bel_Body)|Rest_Facts],
		Rules = Rest_Rules
	;
		Cond_Facts = Rest_Facts,
		Rules = [rule(Head,Obj_Body,Bel_Body)|Rest_Rules]),
	split_program(Rest, Rest_Facts, Rest_Rules).

%------------------------------------------------------------------------------
% compute_derived(+Given_Cond_Facts, +Rules, -Derived_Cond_Facts):
%------------------------------------------------------------------------------

compute_derived(Cond_Facts, Rules, Result) :-
	derived(Cond_Facts, Rules, New),
	\+ is_duplicate(New, Cond_Facts),
	!,
	compute_derived([New|Cond_Facts], Rules, Result).

compute_derived(Fixpoint, _, Fixpoint).

%------------------------------------------------------------------------------
% derived(+Cond_Facts, +Rules, -New_Cond_Fact):
%------------------------------------------------------------------------------

derived(Cond_Facts, Rules, cond_fact(New_Head,New_Cond)) :-
	list_member(rule(Rule_Head,Rule_Body,Rule_Cond), Rules),
	evaluate(Rule_Body, Cond_Facts, Context_Head, Context_Cond),
	list_merge(Rule_Head, Context_Head, New_Head),
	list_merge(Rule_Cond, Context_Cond, New_Cond).

%------------------------------------------------------------------------------
% evaluate(+Body, +Cond_Facts, -Context_Head, -Context_Cond):
%------------------------------------------------------------------------------

evaluate([], _, [], []).

evaluate([Body_Atom|Body], Cond_Facts, Context_Head, Context_Cond) :-
	list_member(cond_fact(Fact_Head,Fact_Cond), Cond_Facts),
	list_delete(Body_Atom, Fact_Head, Fact_RestHead),
	evaluate(Body, Cond_Facts, More_Head, More_Cond),
	list_merge(Fact_RestHead, More_Head, Context_Head),
	list_merge(Fact_Cond, More_Cond, Context_Cond).

%------------------------------------------------------------------------------
% is_duplicate(+Cond_Fact, +List_of_Conditional_Facts):
%------------------------------------------------------------------------------

is_duplicate(cond_fact(Head1,Cond1), [cond_fact(Head2,Cond2)|_]) :-
	list_subseteq(Head2, Head1),
	list_subseteq(Cond2,Cond1).

is_duplicate(Cond_Fact, [_|Rest]) :-
	is_duplicate(Cond_Fact, Rest).

%==============================================================================
% Reduction Steps:
%==============================================================================

%------------------------------------------------------------------------------
% static_reduce(+Cond_Facts, -Residual_Prog):
%------------------------------------------------------------------------------

static_reduce(Cond_Facts, Residual_Prog) :-
	reduction_step(Cond_Facts, Cond_Facts, Reduced_Cond_Facts),
	!,
	static_reduce(Reduced_Cond_Facts, Residual_Prog).

static_reduce(Fixpoint, Fixpoint).

%------------------------------------------------------------------------------
% reduction_step(+Cond_Facts, +All_Cond_Facts, -Reduced_Cond_Facts):
%------------------------------------------------------------------------------

reduction_step([Cond_Fact|More], All, More) :-
	non_minimal(Cond_Fact, All).

reduction_step([Cond_Fact|More], All, More) :-
	neg_reduction(Cond_Fact, All).

reduction_step([Cond_Fact|More], All, [New|More]) :-
	pos_reduction(Cond_Fact, All, New).

reduction_step([Cond_Fact|More], All, [Cond_Fact|Reduced_More]) :-
	reduction_step(More, [Cond_Fact|All], Reduced_More).

%------------------------------------------------------------------------------
% non_minimal(+Cond_Fact, +Derived_Cond_Facts):
%------------------------------------------------------------------------------

non_minimal(cond_fact(Head1,Cond1), Derived_Cond_Facts) :-
	list_member(cond_fact(Head2,Cond2), Derived_Cond_Facts),
	(	list_subset(Head2, Head1),
		list_subseteq(Cond2, Cond1)
	;	list_subset(Cond2, Cond1),
		list_subseteq(Head2, Head1)).

%------------------------------------------------------------------------------
% neg_reduction(+Cond_Fact, +Derived_Cond_Facts):
%------------------------------------------------------------------------------

neg_reduction(cond_fact(_,Cond), Derived_Cond_Facts) :-
	list_member(cond_fact(Head,[]), Derived_Cond_Facts),
	negate(Head, Negated_Head),
	list_sort(Negated_Head, Sorted_Negated_Head), % Superflous?
	list_subseteq(Sorted_Negated_Head, Cond).

%------------------------------------------------------------------------------
% negate(+Atom_List, -Belief_Literal_List):
%------------------------------------------------------------------------------

negate([], []).

negate([Atom|More_Atoms], [not([Atom])|More_Belief_Literals]) :-
	negate(More_Atoms, More_Belief_Literals).

%------------------------------------------------------------------------------
% pos_reduction(+Cond_Fact, +Derived_Cond_Facts, -New_Cond_Fact):
%------------------------------------------------------------------------------

pos_reduction(cond_fact(Head,Cond), Derived_Cond_Facts, cond_fact(Head,New)) :-
	possibly_true(Derived_Cond_Facts, [], Possibly_True),
	list_member(not(Belief), Cond),
	list_member(Atom, Belief),
	\+ list_member(Atom, Possibly_True),
	list_delete(not(Belief), Cond, New).

%------------------------------------------------------------------------------
% possibly_true(+Derived_Cond_Facts, +Possibly_True_In, -Possibly_True_Out):
%------------------------------------------------------------------------------

possibly_true([], Possibly_True, Possibly_True).

possibly_true([cond_fact(Head,_)|More_Cond_Facts], In, Possibly_True) :-
	list_merge(Head, In, New_In),
	possibly_true(More_Cond_Facts, New_In, Possibly_True).

%==============================================================================
% Compute List of All Belief Atoms Occurring in the Residual Program:
%==============================================================================

%------------------------------------------------------------------------------
% belief_atoms(+Residual_Program, -Belief_Atoms):
%------------------------------------------------------------------------------

belief_atoms(Residual_Program, Belief_Atoms) :-
	belief_atoms(Residual_Program, [], Belief_Atoms).

%------------------------------------------------------------------------------
% belief_atoms(+Residual_Program, +Belief_Atoms_In, -Belief_Atoms_Out):
%------------------------------------------------------------------------------

belief_atoms([], Belief_Atoms, Belief_Atoms).

belief_atoms([cond_fact(_,Cond)|Rest], Belief_Atoms_In, Belief_Atoms_Out) :-
	list_merge(Cond, Belief_Atoms_In, Belief_Atoms),
	belief_atoms(Rest, Belief_Atoms, Belief_Atoms_Out).

%------------------------------------------------------------------------------
% belief_interpretations(+Belief_Atoms, -All_Belief_Interpretations):
%------------------------------------------------------------------------------

belief_interpretations(Belief_Atoms, All_Bel_Int) :-
	findall(Bel_Int, belief_int(Belief_Atoms,Bel_Int), All_Bel_Int).

%------------------------------------------------------------------------------
% belief_int(+Belief_Atoms, -Belief_Interpretation):
%------------------------------------------------------------------------------

belief_int([], bel_int([],[])).

belief_int([Bel_Atom|More], bel_int([Bel_Atom|True],False)) :-
	belief_int(More, bel_int(True,False)).

belief_int([Bel_Atom|More], bel_int(True,[Bel_Atom|False])) :-
	belief_int(More, bel_int(True,False)).

%------------------------------------------------------------------------------
% print_belief_ints(+List_of_Belief_Interpretations):
%------------------------------------------------------------------------------

print_belief_ints([]).

print_belief_ints([Bel_Int|More]) :-
	print_bel_int(Bel_Int),
	print_belief_ints(More).

%------------------------------------------------------------------------------
% print_bel_int(+Bel_Int):
%------------------------------------------------------------------------------

print_bel_int(bel_int(True_Beliefs,False_Beliefs)) :-
	list_merge(True_Beliefs, False_Beliefs, All_Beliefs),
	write('	'), % <--- this is a TAB
	print_bel_int(All_Beliefs, True_Beliefs).

print_bel_int([], _) :-
	nl.

print_bel_int([Belief|More], True_Beliefs) :-
	print_literal(Belief),
	(list_member(Belief, True_Beliefs) ->
		write(':TRUE   ')
	;
		write(':FALSE  ')),
	print_bel_int(More, True_Beliefs).

%==============================================================================
% Computation of Minimal Models:
%==============================================================================

%------------------------------------------------------------------------------
% min_mod(+Residual_Prog, +Belief_Int, -Min_Mod):
%------------------------------------------------------------------------------

min_mod(Residual_Prog, bel_int(True_Beliefs,_), Min_Mod) :-
	select_heads(Residual_Prog, True_Beliefs, Heads),
	list_delete_nonmin(Heads, Disjunctions),
	completion(Disjunctions, Comp_Rules),
	list_delete_nonmin(Comp_Rules, Completion),
	generate(Disjunctions, Completion, Min_Mod).

%------------------------------------------------------------------------------
% select_heads(+Residual_Prog, +True_Beliefs, -Heads):
%------------------------------------------------------------------------------

select_heads([], _, []).

select_heads([cond_fact(Head,Cond)|Rest_Prog], True_Beliefs, [Head|More]) :-
	list_subseteq(Cond, True_Beliefs),
	!,
	select_heads(Rest_Prog, True_Beliefs, More).

select_heads([_|Rest_Prog], True_Beliefs, More) :-
	select_heads(Rest_Prog, True_Beliefs, More).

%------------------------------------------------------------------------------
% generate(+Disjunctions, +Comp_Rules, -True):
%------------------------------------------------------------------------------

generate(Disjunctions, Comp_Rules, True) :-
	hyperres(Disjunctions, Comp_Rules, Derived_Dis),
	list_delete_nonmin(Derived_Dis, Minimal_Dis),
	select_definite(Minimal_Dis, Definite, Indefinite),
	gen_model(Indefinite, Definite, Comp_Rules, True).

%------------------------------------------------------------------------------
% gen_model(+Indefinite, +Definite, +Comp_Rules, -True):
%------------------------------------------------------------------------------

gen_model([], Definite, _, True) :-
	!,
	list_flatten(Definite, True).

gen_model(Indefinite, Definite, Comp_Rules, True) :-
	[[Atom|_]|_] = Indefinite,
	list_append(Definite, Indefinite, Disjunctions),
	(	generate([[Atom]|Disjunctions], Comp_Rules, True)
	;	generate(Disjunctions, [[Atom]|Comp_Rules], True)).

%------------------------------------------------------------------------------
% select_definite(+List_of_nonempty_lists, -One_Element_Lists, ?Other_Lists):
%------------------------------------------------------------------------------

select_definite([], [], []).

select_definite([[]|_], _, _) :-
	impossible(select_definite, 'inconsistent'),
	nl.

select_definite([[Elem]|More_Lists], [[Elem]|More_Definite], More_Other) :-
	!,
	select_definite(More_Lists, More_Definite, More_Other).

select_definite([Other|More_Lists], More_Definite, [Other|More_Other]) :-
	select_definite(More_Lists, More_Definite, More_Other).

%------------------------------------------------------------------------------
% print_obj_models(+List_of_Lists_of_True_Atoms):
%------------------------------------------------------------------------------

print_obj_models([]).

print_obj_models([Model|More]) :-
	write('	'), % This is a TAB
	print_model(Model),
	nl,
	print_obj_models(More).

%------------------------------------------------------------------------------
% print_model(+List_of_True_Atoms):
%------------------------------------------------------------------------------

print_model([]) :-
	write('(all propositions false).').

print_model([Atom]) :-
	!,
	write(Atom),
	write('.').

print_model([Atom|More]) :-
	write(Atom),
	write(', '),
	print_model(More).

%==============================================================================
% Computation of Completion:
%==============================================================================

%------------------------------------------------------------------------------
% completion(+Disjunctions, -Comp_Rules):
%------------------------------------------------------------------------------

completion(Disjunctions, Comp_Rules) :-
	list_flatten(Disjunctions, Atoms),
	comp_loop(Atoms, Disjunctions, Comp_Rules).

%------------------------------------------------------------------------------
% comp_loop(+Atoms, +Disjunctions, -Comp_Rules):
%------------------------------------------------------------------------------

comp_loop([], _, []).

comp_loop([Atom|More_Atoms], Disjunctions, Comp_Rules) :-
	comp_rules_for_atom(Disjunctions, Atom, Atom_Rules),
	comp_loop(More_Atoms, Disjunctions, More_Rules),
	list_append(Atom_Rules, More_Rules, Comp_Rules).

%------------------------------------------------------------------------------
% comp_rules_for_atom(+Disjunctions, +Atom, -Comp_Rules):
%------------------------------------------------------------------------------

comp_rules_for_atom(Disjunctions, Atom, Comp_Rules) :-
	findall(Implies, implies(Disjunctions, Atom, Implies), List),
	findall(Comp_Rule, comp_rule(Atom, List, Comp_Rule), Comp_Rules).

%------------------------------------------------------------------------------
% implies(+Disjunctions, +Atom, -Implies):
%------------------------------------------------------------------------------

implies(Disjunctions, Atom, Implies) :-
	list_member(Dis, Disjunctions),
	list_delete(Atom, Dis, Implies).

%------------------------------------------------------------------------------
% comp_rule(+Atom, -List_of_Bodies, -Comp_Rule):
%------------------------------------------------------------------------------

comp_rule(Atom, List_of_Bodies, Comp_Rule) :-
	one_of_each(List_of_Bodies, Multiplied_Out_Condition),
	list_sort([Atom|Multiplied_Out_Condition], Comp_Rule).

%------------------------------------------------------------------------------
% one_of_each(+List_of_lists, -List_containing_one_element_of_each_list):
%------------------------------------------------------------------------------

one_of_each([], []).

one_of_each([First_List|More_Lists], [First_Elem|More_Elems]) :-
	list_member(First_Elem, First_List),
	one_of_each(More_Lists, More_Elems).

%==============================================================================
% Hyperresolution on Completion:
%==============================================================================

%------------------------------------------------------------------------------
% hyperres(+Disjunctions, +Comp_Rules, -Result):
%------------------------------------------------------------------------------

hyperres(Disjunctions, Comp_Rules, Result) :-
	list_member(Rule, Comp_Rules),
	static_resolve(Rule, Disjunctions, New_Dis),
	\+ list_is_superflous(New_Dis, Disjunctions),
	!,
	hyperres([New_Dis|Disjunctions], Comp_Rules, Result).

hyperres(Facts, _, Facts).

%------------------------------------------------------------------------------
% static_resolve(+Comp_Rule, +Disjunctions, -New_Dis):
%------------------------------------------------------------------------------

static_resolve([], _, []).

static_resolve([BodyAtom|Body], Disjunctions, New_Dis) :-
	list_member(Dis, Disjunctions),
	list_delete(BodyAtom, Dis, Context),
	static_resolve(Body, Disjunctions, More_Contexts),
	list_merge(Context, More_Contexts, New_Dis).

%==============================================================================
% Fixpoint Computation with Psi and Phi:
%==============================================================================

%------------------------------------------------------------------------------
% phi(+Belief_Interpretations, +Res_Prog, -Objective_Parts_of_Minimal_Models):
%------------------------------------------------------------------------------

% Given a set/list of interpretations of the belief atoms occurring in a
% residual program, this predicate returns a list of the objective parts
% of the minimal models based on these interpretations of the belief atoms.
% For instance, if the residual program is
%	p v q.				cond_fact([p,q], []).
%	q v r.				cond_fact([q,r], []).
%	s v t <--- not(p).		cond_fact([s,t], [not([p])]).
%	t <--- not(r).			cond_fact([t], [not([r])]).
% and the belief interpretations are
%	not(p):TRUE   not(r):TRUE   	bel_int([not([p]),not([r])], [])
%	not(p):FALSE  not(r):FALSE  	bel_int([], [not([p]),not([r])])
% the following minimal models are returned:
%	p, r.				[p, r].
%	p, r, t.			[p, r, t].
%	q.				[q].
%	q, t.				[q, t].
% (p, r) and (q) result from the second belief interpretation (both belief
% atoms false) and (p, r, t) and (q, t) result from the first (both true).

phi(Belief_Ints, Res_Prog, Min_Mods) :-
	phi_loop(Belief_Ints, Res_Prog, [], Min_Mods).

%------------------------------------------------------------------------------
% phi_loop(+Belief_Ints, +Min_Mods_In, -Min_Mods_Out):
%------------------------------------------------------------------------------

phi_loop([], _, Min_Mods, Min_Mods).

phi_loop([Bel_Int|More_Bel_Ints], Res_Prog, Min_Mods_In, Min_Mods_Out) :-
	findall(Min_Mod, min_mod(Res_Prog, Bel_Int, Min_Mod), Min_Mods),
	list_sort(Min_Mods, Sorted_Min_Mods),
	list_merge(Sorted_Min_Mods, Min_Mods_In, New_Min_Mods),
	phi_loop(More_Bel_Ints, Res_Prog, New_Min_Mods, Min_Mods_Out).

%------------------------------------------------------------------------------
% theta(+Belief_Interpretations_In, +Res_Prog, -Belief_Interpretations_Out):
%-----------------------------------------------------------------------------

% This is the theta-operator of the paper, the combination of phi and psi.
% It takes a set/list of interpretations of the belief atoms occurring in
% a residual program, and returns the subset which remain possible given
% on the minimal models based on the input belief interpretations.
% "Possible" means that there is a set of minimal models, such that the true
% belief atoms are exactly the intersection of the corresponding true objective
% parts in the minimal models. I.e. B(~p1 v ... v ~pn) holds iff
% ~p1 v ... v ~pn holds in all minimal models of this (arbitrary, but
% non-empty) set.

theta(Bel_Ints_In, Res_Prog, Bel_Ints_Out) :-
	phi(Bel_Ints_In, Res_Prog, Min_Mods),
	theta_filter(Bel_Ints_In, Min_Mods, Bel_Ints_Out).

%------------------------------------------------------------------------------
% theta_filter(+Bel_Ints_In, +Min_Mods, -Bel_Ints_Out):
%-----------------------------------------------------------------------------

theta_filter([], _, []).

theta_filter([Bel_Int|More_In], Min_Mods, [Bel_Int|More_Out]) :-
	psi_ok(Bel_Int, Min_Mods),
	!,
	theta_filter(More_In, Min_Mods, More_Out).

theta_filter([_|More_In], Min_Mods, More_Out) :-
	theta_filter(More_In, Min_Mods, More_Out).

%------------------------------------------------------------------------------
% psi_ok(+Bel_Int, +Min_Mods):
%------------------------------------------------------------------------------

% Here we check wether a given (partial) interpretation of the belief atoms
% is possible given a set of minimal models.
% First, we select those minimal models in which all true belief atoms are
% satisfied (i.e. for every true belief atom B(~p1 v ... v ~pn) at least one pi
% must be false in the minimal model).
% This is the maximal set of worlds to which the current world can be linked.
% This set is called "Support" below.
% Second, due to the consistency axiom, we must require that this set is
% non-empty.
% Third, we must check that for every belief atom B(~p1 v ... v ~pn) false
% in the given (partial) interpretation, there is a world, to which the
% current world is linked (i.e. in Support), in which ~p1 v ... v ~pn is false.
% Obviously, if this condition is not satisfied for the maximal set of worlds,
% to which the current world can be linked, it cannot be satisfied for any
% subset.

psi_ok(bel_int(True_Beliefs,False_Beliefs), Min_Mods) :-
	select_support_for_true_beliefs(Min_Mods, True_Beliefs, Support),
	Support \== [],
	check_support_for_false_beliefs(False_Beliefs, Support).

%------------------------------------------------------------------------------
% select_support_for_true_beliefs(+Min_Mods, +True_Beliefs, -Support):
%------------------------------------------------------------------------------

select_support_for_true_beliefs([], _, []).

select_support_for_true_beliefs([Min_Mod|In], True_Beliefs, [Min_Mod|Out]) :-
	beliefs_supported(True_Beliefs, Min_Mod),
	!,
	select_support_for_true_beliefs(In, True_Beliefs, Out).

select_support_for_true_beliefs([_|In], True_Beliefs, Out) :-
	select_support_for_true_beliefs(In, True_Beliefs, Out).

%------------------------------------------------------------------------------
% beliefs_supported(+Bel_Int, +Min_Mod):
%------------------------------------------------------------------------------

beliefs_supported([], _).

beliefs_supported([not(Beliefs)|More_Bel], Min_Mod) :-
	\+ list_subseteq(Beliefs, Min_Mod),
	beliefs_supported(More_Bel, Min_Mod).

%------------------------------------------------------------------------------
% check_support_for_false_beliefs(+False_Beliefs, +Support):
%------------------------------------------------------------------------------

check_support_for_false_beliefs([], _).

check_support_for_false_beliefs([not(Belief)|More], Support) :-
	list_member(Min_Mod, Support),
	list_subseteq(Belief, Min_Mod),
	!,
	check_support_for_false_beliefs(More, Support).

%==============================================================================
% Query Interface to Static Models:
%==============================================================================

%------------------------------------------------------------------------------
% query_loop(+Minimal_Models):
%------------------------------------------------------------------------------

query_loop(Minimal_Models) :-
	prolog_print_prompt('STATIC> '),
	read(Line),
	((Line = halt; Line = end_of_file) ->
		true
	;
		(parse_query(Line, Query) ->
			answer_query(Query, Minimal_Models)
		;
			write('	Syntax error in query.'),
			nl),
		!,
		query_loop(Minimal_Models)).

%------------------------------------------------------------------------------
% parse_query(+Input_Line, -List_of_Disjunctions_of_Objective_or_Belief_Atoms):
%------------------------------------------------------------------------------

parse_query(','(Input_Dis,More_Input), [Dis|More_Dis]) :-
	parse_dis(Input_Dis, Dis),
	!,
	parse_query(More_Input, More_Dis).

parse_query(&(Input_Dis,More_Input), [Dis|More_Dis]) :-
	parse_dis(Input_Dis, Dis),
	!,
	parse_query(More_Input, More_Dis).

parse_query(Input_Dis, [Dis]) :-
	parse_dis(Input_Dis, Dis).

%------------------------------------------------------------------------------
% parse_dis(+Input, -Disjunction_of_Objective_or_Belief_Atom):
%------------------------------------------------------------------------------

parse_dis(Input_Disjunction, objective(Sorted_Disjunction)) :-
	parse_head(Input_Disjunction, Disjunction),
	list_sort(Disjunction, Sorted_Disjunction).

parse_dis(Input_Disjunction, belief(Belief_Disjunction)) :-
	parse_bel_dis(Input_Disjunction, Belief_Disjunction).

%------------------------------------------------------------------------------
% parse_bel_dis(+Input_Disjunction, -Belief_Disjunction):
%------------------------------------------------------------------------------

parse_bel_dis(v(Input_Bel, More_Input), [Bel|More_Bel]) :-
	!,
	parse_bel_lit(Input_Bel, Bel),
	parse_bel_dis(More_Input, More_Bel).

parse_bel_dis(Input_Bel, [Bel]) :-
	parse_bel_lit(Input_Bel, Bel).

%------------------------------------------------------------------------------
% parse_bel_lit(+Input_Lit, -Belief):
%------------------------------------------------------------------------------

parse_bel_lit(not(Input_Belief), not(Sorted_Belief)) :-
	!,
	parse_belief(Input_Belief, Belief),
	list_sort(Belief, Sorted_Belief).

%------------------------------------------------------------------------------
% answer_query(+Query, +Minimal_Models):
%------------------------------------------------------------------------------

answer_query(Query, Minimal_Models) :-
	(prove_query(Query, Minimal_Models)->
		write('	yes.'),
		nl
	;
		write('	no.'),
		nl).

%------------------------------------------------------------------------------
% prove_query(+Query, +Minimal_Models):
%------------------------------------------------------------------------------

prove_query([], _).

prove_query([Dis|More], Minimal_Models) :-
	(Dis = objective(Obj_Dis) ->
		prove_obj_dis(Minimal_Models, Obj_Dis)
	;
		Dis = belief(Bel_Dis),
		prove_bel_dis(Minimal_Models, Bel_Dis)),
	prove_query(More, Minimal_Models).

%------------------------------------------------------------------------------
% prove_obj_dis(+Minimal_Models, +Dis):
%------------------------------------------------------------------------------

% p1 v ... v pk holds in all static models iff it holds in all minimal models.

prove_obj_dis([], _).

prove_obj_dis([Min_Mod|More], Dis) :-
	list_overlap(Min_Mod, Dis),
	prove_obj_dis(More, Dis).

%------------------------------------------------------------------------------
% prove_bel_dis(+Minimal_Models, +Bel_Dis):
%------------------------------------------------------------------------------

% not(C1) v ... v not(Cn) holds in all static models iff at least one
% not(Ci) holds in all static models.
% Note that in the static semantics, not(p) v not(q) follows only iff one of
% the two follows.

prove_bel_dis(Minimal_Models, [not(Belief)|_]) :-
	prove_belief(Minimal_Models, Belief),
	!.

prove_bel_dis(Minimal_Models, [_|More]) :-
	prove_bel_dis(Minimal_Models, More).

%------------------------------------------------------------------------------
% prove_belief(+Minimal_Models, +Belief):
%------------------------------------------------------------------------------

% not(p1 & ... &pk) holds in all static models iff there is not a minimal
% model in which (p1 & ... &pk) is true.

prove_belief([], _).

prove_belief([Min_Mod|More], Belief) :-
	\+ list_subseteq(Belief, Min_Mod),
	prove_belief(More, Belief).

%==============================================================================
% Assertions:
%==============================================================================

%------------------------------------------------------------------------------
% impossible(+Predicate, +Error_Message):
%------------------------------------------------------------------------------

impossible(Predicate, Error_Message) :-
	nl,
	write('*** BUG DETECTED! '),
	write(Predicate),
	write(': '),
	write(Error_Message),
	write('***'),
	nl,
	fail.

%==============================================================================
% List Functions:
%==============================================================================

%------------------------------------------------------------------------------
% list_sort(+List, -Sorted_list_without_duplicates):
%------------------------------------------------------------------------------

list_sort([], []).

list_sort([Limit|Rest], SortedList) :-
	list_split(Limit, Rest, Smaller, Greater),
	list_sort(Smaller, SortedSmaller),
	list_sort(Greater, SortedGreater),
	list_merge(SortedSmaller, [Limit|SortedGreater], SortedList).

%------------------------------------------------------------------------------
% list_split(+Limit, +List, -Smaller, -Greater):
%------------------------------------------------------------------------------

list_split(_, [], [], []).

list_split(Limit, [First|Rest], Smaller, Greater) :-
	Limit == First,
	!,
	list_split(Limit, Rest, Smaller, Greater).

list_split(Limit, [First|Rest], [First|Smaller], Greater) :-
	First @=< Limit,
	!,
	list_split(Limit, Rest, Smaller, Greater).

list_split(Limit, [First|Rest], Smaller, [First|Greater]) :-
	Limit @=< First,
	!,
	list_split(Limit, Rest, Smaller, Greater).

%------------------------------------------------------------------------------
% list_merge(+Sorted_list_1, +Sorted_list_2, -Merged_List):
%------------------------------------------------------------------------------

list_merge([], List, List) :-
	!.

list_merge(List, [], List) :-
	!.

list_merge([Elem1|List1], [Elem2|List2], [Elem1|List]) :-
	Elem1 == Elem2,
	!,
	list_merge(List1, List2, List).

list_merge([Elem1|List1], [Elem2|List2], [Elem1|List]) :-
	Elem1 @=< Elem2,
	!,
	list_merge(List1, [Elem2|List2], List).

list_merge([Elem1|List1], [Elem2|List2], [Elem2|List]) :-
	Elem2 @=< Elem1,
	list_merge([Elem1|List1], List2, List).

%------------------------------------------------------------------------------
% list_delete(+Element, +List, -Rest):
%------------------------------------------------------------------------------

list_delete(Element, [Element|Rest], Rest) :-
	!.

list_delete(Element, [Other|List], [Other|Rest]) :-
	list_delete(Element, List, Rest).

%------------------------------------------------------------------------------
% list_subset(+Ordered_list_1, +Proper_superset_of_ordered_list_1):
%------------------------------------------------------------------------------

list_subset([], [_|_]).

list_subset([E1|L1], [E2|L2]) :-
	(E1 = E2 ->
		list_subset(L1, L2)
	;	E2 @=< E1,
		list_subseteq([E1|L1], L2)).

%------------------------------------------------------------------------------
% list_subseteq(+Ordered_list_1, +superset_of_ordered_list_1_or_equal):
%------------------------------------------------------------------------------

list_subseteq([], _).

list_subseteq([E1|L1], [E2|L2]) :-
	(E1 = E2 ->
		list_subseteq(L1, L2)
	;	E2 @=< E1,
		list_subseteq([E1|L1], L2)).

%------------------------------------------------------------------------------
% list_append(+L1, +L2, -Concatenation_of_L1_and_L2):
%------------------------------------------------------------------------------

list_append([], L, L).

list_append([E|L1], L2, [E|L1L2]) :-
	list_append(L1, L2, L1L2).

%------------------------------------------------------------------------------
% list_member(-Member, +List):
%------------------------------------------------------------------------------

list_member(E, [E|_]).

list_member(E, [_|R]) :-
	list_member(E, R).

%------------------------------------------------------------------------------
% list_flatten(+List_of_ordered_lists, -Union_of_all_these_lists):
%------------------------------------------------------------------------------

list_flatten(List_of_Lists, Union) :-
	list_flatten(List_of_Lists, [], Union).

list_flatten([], Union, Union).

list_flatten([List|More], Union_In, Union_Out) :-
	list_merge(List, Union_In, Union),
	list_flatten(More, Union, Union_Out).

%------------------------------------------------------------------------------
% list_delete_nonmin(+List_of_Ordered_Lists, -List_of_Minimal_Lists):
%------------------------------------------------------------------------------

list_delete_nonmin(Lists, Min_Lists) :-
	list_delete_nonmin(Lists, [], Min_Lists).
	
list_delete_nonmin([], _, []).

list_delete_nonmin([List|More_Lists], Previous_Lists, Result) :-
	list_is_superflous(List, More_Lists),
	!,
	list_delete_nonmin(More_Lists, Previous_Lists, Result).

list_delete_nonmin([List|More_Lists], Previous_Lists, Result) :-
	list_is_superflous(List, Previous_Lists),
	!,
	list_delete_nonmin(More_Lists, Previous_Lists, Result).

list_delete_nonmin([List|More_Lists], Previous_Lists, [List|Result]) :-
	list_delete_nonmin(More_Lists, [List|Previous_Lists], Result).

%------------------------------------------------------------------------------
% list_is_superflous(+Ordered_list, +List_of_lists_containing_nonstrict_subset):
%------------------------------------------------------------------------------

list_is_superflous(List1, [List2|_]) :-
	list_subseteq(List2, List1).

list_is_superflous(List1, [_|More_Lists]) :-
	list_is_superflous(List1, More_Lists).

%------------------------------------------------------------------------------
% list_overlap(+Sorted_list_1, +Sorted_list_2):
%------------------------------------------------------------------------------

list_overlap([Elem|_], [Elem|_]) :-
	!.

list_overlap([Elem1|List1], [Elem2|List2]) :-
	Elem1 @=< Elem2,
	!,
	list_overlap(List1, [Elem2|List2]).

list_overlap([Elem1|List1], [Elem2|List2]) :-
	Elem2 @=< Elem1,
	list_overlap([Elem1|List1], List2).

%==============================================================================
% Portability:
%==============================================================================

%------------------------------------------------------------------------------
% prolog_open(+Filename, +Mode, -Stream):
%------------------------------------------------------------------------------

prolog_open(Filename, read, Stream) :-
	(prolog_use_see(yes) ->
		see(Filename),
		Stream = dummy
	;
		open(Filename, read, Stream)).

%------------------------------------------------------------------------------
% prolog_read(+Stream, -Term):
%------------------------------------------------------------------------------

prolog_read(Stream, Term) :-
	(prolog_use_see(yes) ->
		read(Term)
	;
		read(Stream, Term)).

%------------------------------------------------------------------------------
% prolog_close(+Stream):
%------------------------------------------------------------------------------

prolog_close(Stream) :-
	(prolog_use_see(yes) ->
		seen
	;
		close(Stream)).

%------------------------------------------------------------------------------
% prolog_set_prompt(+New, -Old)
%------------------------------------------------------------------------------

prolog_set_prompt(New, Old) :-
	(prolog_use_prompt(swi) ->
		prompt(Old, New)
	;prolog_use_prompt(eclipse) ->
		get_prompt(input, Old, Out_Stream),
		set_prompt(input, New, Out_Stream)
	;
		true).

%------------------------------------------------------------------------------
% prolog_print_prompt(+Prompt):
%------------------------------------------------------------------------------

prolog_print_prompt(Prompt) :-
	(prolog_use_prompt(write) ->
		write(Prompt)
	;
		true).


						    

/*
new_slg_head(Head,Body,NewHead) :-
	functor(Head,P,A),
	name(P,Pl),
	name(Npred,[115,108,103,36|Pl]), % 'slg$'
	Narity is A+1,
	functor(NewHead,Npred,Narity),
	%dynamic(Npred/Narity),
	arg(Narity,NewHead,Body),
	put_in_args(0,A,Head,NewHead).

put_in_args(A,A,_,_).
put_in_args(A0,A,Head,NewHead) :-
	A0 < A,
	A1 is A0+1,
	arg(A1,Head,Arg),
	arg(A1,NewHead,Arg),
	put_in_args(A1,A,Head,NewHead).
/* -------------- beginning of slg_load routines -------------------------
  An input file may contain three kinds of directives (in addition to 
  regular Prolog clauses : commands) : 

  a) :- default(prolog).
     :- default(tabled).
     All predicates defined from now on are prolog (tabled) predicates
     unless specified otherwise later.
  b) :- tabled pred_name/arity.
     pred_name/arity is a tabled predicate. A comma separated list
     is also acceptable.

  c) :- prolog pred_name/arity.
     pred_name/arity is a prolog predicate. A comma separated list
     is also acceptable.

  Besides Prolog clauses, we allow general clauses where the body is a 
  universal disjunction of literals. Such clauses are specified in the form
         Head <---- Body.
  (Maybe <---- can be viewed as "All".) The head must be an atom of a tabled
  predicate : the body should be a disjunction of literals (separated by ';')
  : should not contain cut. The head must be stableGround whenever it is called. 
  All variables in the body that do not occur in the head are universally 
  quantified.

  There is NO support for module facilities. In particular, ALL TABLED
  PREDICATES SHOULD BE DEFINED IN MODULE 'user'.
*/

	
slg_term_expansion(end_of_file,_) :- !,
	%remDefaultDecl,
	%assertLogged(getDefaultDecl((prolog))),
	%retractallLogged('slg$prolog'(_)),
       % retractallLogged('slg$tab'(_,_)),
	fail.

slg_term_expansion((:-Com),Clauses) :- !,
	expand_cmd(Com,Clauses).

slg_term_expansion((  ::- Body),Clauses) :- !,
        convert_tabled_clause(inconsistent,Body,Clauses).

slg_term_expansion((H-->B),NewClause) :- !,
	not(slg_expanding),
	assertLogged(slg_expanding),
	expand_term((H-->B),Clause),
	retractallLogged(slg_expanding),
	slg_term_expansion(Clause,NewClause).

slg_term_expansion((Head <---- Body),Clauses) :- !,
	functor(Head,P,A),
	Pred = P/A,
	( isDeclTabled(P,A) ->
	  convert_univ_clause(Head,Body,Clauses)
	; isDeclProlog(Pred) ->
	  write('Error :  Prolog predicate '), write(Pred),
	  write(' in clauses with universal disjunction.'),nl,
	  write('       Clause ignored :  '), write((Head <---- Body)), nl,
	  Clauses = []
	; getDefaultDecl(Default),
	  ( Default == (prolog) ->
	    write('Error :  Prolog predicate '), write(Pred),
	    write(' in clauses with universal disjunction.'),nl,
	    write('       Clause ignored :  '), write((Head <---- Body)), nl,
	    Clauses = []
	  ; addDeclTabled(P,A),
	    addHasTable(P,A),
	    functor(NewHead,P,A),
	    Clauses = [(:- addHasTable(P,A)),(NewHead :- slg(NewHead))|RestClauses],
            convert_univ_clause(Head,Body,RestClauses)
	  )
        ).
	
slg_term_expansion(Clause,Clauses) :-
	( Clause = (Head :- Body) -> true; Head = Clause, Body = true ),
	functor(Head,P,A),
	Pred = P/A,
	( isDeclTabled(P,A) ->
	  convert_tabled_clause(Head,Body,Clauses)
        ; isDeclProlog(Pred) ->
	  Clauses = Clause
        ; getDefaultDecl(Default),
	  ( Default == (prolog) ->
	    Clauses = Clause
	  ; ( isDeclTabled(P,A) ->
	      convert_tabled_clause(Head,Body,Clauses)
	    ; addDeclTabled(P,A),
	      addHasTable(P,A),
	      functor(NewHead,P,A),
	      Clauses = [(:- addHasTable(P,A)),(NewHead :- slg(NewHead))|RestClauses],
              convert_tabled_clause(Head,Body,RestClauses)
	    )
	  )
        ).

expand_cmd(tabled(Preds),Clauses) :-
	expand_command_table(Preds,Clauses,[]).
expand_cmd(prolog(Preds),Clauses) :-
	expand_command_prolog(Preds,Clauses,[]).
expand_cmd(multifile(Preds),(:-multifile(NewPreds))) :-
	add_table_preds(Preds,NewPreds,[]).
expand_cmd(dynamic(Preds),(:-dynamic(NewPreds))) :-
	add_table_preds(Preds,NewPreds,[]).
expand_cmd(default(D),[]) :-
	( (D == (prolog); D == (tabled)) ->
	  remDefaultDecl,
	  setDefaultDecl(D)
        ; write('Warning :  illegal default '),
	  write(D),
	  write(' ignored.'),
	  nl
        ).

expand_command_table((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_table_one(Pred,Clauses0,Clauses1),
	expand_command_table(Preds,Clauses1,Clauses).
expand_command_table(Pred,Clauses0,Clauses) :-
	expand_command_table_one(Pred,Clauses0,Clauses).

expand_command_table_one(Pspec,Clauses0,Clauses) :-
	  ( Pspec = P/A -> true; P = Pspec, A = 0 ),
	  Pred = P/A,
	  functor(H,P,A),
	  ( isAlwaysProlog(H) ->
	    write('ERROR :  Cannot table built_in '),
	    write(Pred), nl,
	    Clauses0 = Clauses
	  ; isDeclProlog(Pred) ->
	    write('ERROR :  '),
	    write(Pred),
	    write(' assumed to be a Prolog predicate'),
	    nl,
	    tab(7),
	    write('But later declared a tabled predicate.'),
	    nl,
	    Clauses0 = Clauses
	  ; isDeclTabled(P,A) ->
	    Clauses0 = Clauses
	  ; addDeclTabled(P,A),
	    addHasTable(P,A),
	    Clauses0 = [(:- addHasTable(P,A)),(H :- slg(H))|Clauses]
	  ).

expand_command_prolog((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_prolog_one(Pred,Clauses0,Clauses1),
	expand_command_prolog(Preds,Clauses1,Clauses).
expand_command_prolog(Pred,Clauses0,Clauses) :-
	expand_command_prolog_one(Pred,Clauses0,Clauses).

expand_command_prolog_one(Pspec,Clauses0,Clauses) :-
	  ( Pspec = P/A -> true; P = Pspec, A = 0 ),
	  Pred = P/A,
	  ( isDeclTabled(P,A) ->
	    write('ERROR :  '),
	    write(Pred),
	    write(' assumed to be a tabled predicate'),
	    nl,
	    tab(7),
	    write('But later declared a Prolog predicate.'),
	    nl,
	    Clauses0 = Clauses
	  ; remDeclTable(P,A),
	    remHasTable(P,A),
	    addDeclProlog(P,A),
	    Clauses0 = [(:- remHasTable(P,A))|Clauses]
          ).

add_table_preds(Preds,NewPreds0,NewPreds) :-
	( Preds == [] ->
	  NewPreds0 = NewPreds
        ; Preds = [P|Ps] ->
	  add_table_preds(P,NewPreds0,NewPreds1),
	  add_table_preds(Ps,NewPreds1,NewPreds)
        ; Preds = (P,Ps) ->
	  add_table_preds(P,NewPreds0,NewPreds1),
	  add_table_preds(Ps,NewPreds1,NewPreds)
        ; ( Preds = P/A -> true; P = Preds, A = 0 ),
	  ( isDeclTabled(P,A) ->
	    add_slg_functor(P,NewP),
	    % name(P,Pl),
	    %name(NewP,[115,108,103,36|Pl]), % 'slg$'
	    NewA is A+1,
	    NewPreds0 = [P/A,NewP/NewA|NewPreds]
	  ; NewPreds0 = [P/A|NewPreds]
          )
        ).

convert_tabled_clause(Head,Body,Clauses0) :-
	  slg_conj_to_list(Body,Blist),
	  extract_guard(Blist,Guard,[],Nbody,Clauses0,Clauses),
	  slg_list_to_conj(Guard,Gconj),
	  new_slg_head(Head,Nbody,NewHead),
	  ( Gconj == true ->
	    Clauses = [NewHead]
	  ; Clauses = [(NewHead :- Gconj)]
          ).

convert_univ_clause(Head,Body,Clauses) :-
	slg_disj_to_list(Body,Blist),
	new_slg_head(Head,all(Blist),NewHead),
	Clauses = [(NewHead :- ( stableGround(Head) -> 
	                         true
			       ; write('Error : Non-stableGround prologCall '),
			         write(Head),
				 write(' in a clause with universal disjunction.'),
				 nl
			       ))].


slg_conj_to_list(Term,List) :-
	slg_conj_to_list(Term,List,[]).
slg_conj_to_list(Term,List0,List) :-
	( Term = (T1,T2) ->
	  slg_conj_to_list(T1,List0,List1),
	  slg_conj_to_list(T2,List1,List)
        ; Term == true ->
	  List0 = List
        ; List0 = [Term|List]
        ).

slg_disj_to_list(Term,List) :-
	slg_disj_to_list(Term,List,[]).

slg_disj_to_list(Term,List0,List) :-
	( Term = (T1 ; T2) ->
	  slg_disj_to_list(T1,List0,List1),
	  slg_disj_to_list(T2,List1,List)
        ; Term == true ->
	  List0 = List
        ; List0 = [Term|List]
        ).
	

extract_guard([],G,G,[],Cls,Cls).
extract_guard([Lit|List],G0,G,Rest,Cls0,Cls) :-
	( Lit = ( /*neg*/ \+ N) ->
	  Nlit = N
        ; Nlit = Lit
        ),
	( isAlwaysProlog(Nlit)  ->
	  G0 = [Lit|G1],
	  extract_guard(List,G1,G,Rest,Cls0,Cls)
        ; functor(Nlit,P,A),
	  Pred = P/A,
	  ( isDeclTabled(P,A) ->
	    G0 = G,
	    Rest = [Lit|List],
	    Cls0 = Cls
	  ; isDeclProlog(Pred) ->
	    G0 = [Lit|G1],
	    extract_guard(List,G1,G,Rest,Cls0,Cls)
	  ; getDefaultDecl((prolog)) ->
	    G0 = [Lit|G1],
	    addDeclProlog(Pred),
	    Cls0 = [(:- isDeclProlog(Pred) -> true; addDeclProlog(Pred))|Cls1],
	    extract_guard(List,G1,G,Rest,Cls1,Cls)
	  ; getDefaultDecl((tabled)) ->
	    G0 = G,
	    Rest = [Lit|List],
	    addDeclTabled(P,A),
            addHasTable(P,A),
	    functor(Head,P,A),
	    Cls0 = [(:- addHasTable(P,A)),
                    (Head :- slg(Head))|Cls]
	  )		     
        ).

slg_list_to_conj([],true).
slg_list_to_conj([Lit|List],G0) :-
	( List == [] ->
	  G0 = Lit
        ; G0 = (Lit,G),
	  slg_list_to_conj(List,G)
        ).


/*

slgtrans(InFile,OutFile) :-
        see(InFile),
        tell(OutFile),
        trans_clauses,
        told,
        seen.

trans_clauses :-
        repeat,
        read(Clause),
        ( slg_term_expansion(Clause,Clauses) ->
          put_clauses(Clauses)
        ; true
        ),
        Clause = end_of_file,
        !.

put_clauses([]) :- !.
put_clauses([C1|Cs]) :- !,
        portray_clause(C1),
        put_clauses(Cs).
put_clauses(C) :-
        portray_clause(C).
*/
*/


	
