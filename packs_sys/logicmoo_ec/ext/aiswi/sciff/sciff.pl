%----------------------------------------------------------
% "sciff.pl"
%
% Author: Marco Alberti
% Created: Sept. 29, 2003
%
% Modified by: Federico Chesani
% Modified at: Aug. 30, 2005
%
% Modified by: Federico Chesani
% Modified at: Dec. 01, 2005
%
%
% Version: 1.00.01
%----------------------------------------------------------
%:- multifile(fdet/1).
%:- dynamic(fdet/1).



%----------------------------------------------------------
% CONSULTING THE MODULES
%----------------------------------------------------------




:- use_module(library(terms)).
:- use_module(reified_unif).

:- use_module(prolog_version).
:- (is_dialect(swi) 
	-> use_module(swi_specific),
       [my_chr_extensions] % In SWI, my_chr_extensions must be loaded after reified_unif
	 ;
    is_dialect(sicstus)
    -> ensure_loaded(my_chr_extensions),
       use_module(sicstus_specific)
    ;   write('Unrecognized Prolog System'), nl, fail
    ).

:- use_module(library(chr)).
:- use_module(proof_util).
:- use_module(quantif).
:- use_module(ccopy).
:- use_module(ics_quant).
:- use_module(library(lists)).
%         [append/3,
%          delete/3,
%      		nth/4,
%      		member/2]).
:- use_module(library(terms),
         [term_variables/2]).

%:- ensure_loaded(solver).
:- use_module(solver).
%:- use_module(domains).
:- use_module(history_parser).
:- use_module(sokb_parser).
:- use_module(ics_parser).
:- use_module(sciff_options).
:- use_module(debug).
:- use_module(help).
:- use_module(graphviz).

:- ensure_loaded(defaults).
:- use_module(library(clpfd)).  % used by invert_constraint in SWI, and in old SCIFF code (e.g., block world)
:- [pretty_print].

%----------------------------------------------------------
% DECLARATIONS
%----------------------------------------------------------
%handler society_proof.

%option(already_in_store,on).

:- chr_option(debug,on). % Se lo metto on mi da` un errore




%----------------------------------------------------------
% IMPOSING IC/PSIC
%
% Quantify the variables in an imposed IC, and impose the result as a
% PSIC
%----------------------------------------------------------
:- chr_constraint
	ic/2, psic/2.
impose_ics @
    ic(Body1,Head1)
    ==>
    %unfold_nonrecursive(psic(Body1,Head1),PSIC),
    %impose_psic(PSIC).
    convert_to_psic(ic(Body1,Head1),PSIC),
    call(PSIC).
    %psic(BodyPSIC,HeadPSIC).

convert_to_psic(ic(Body1,Head1),PSIC):-
    unfold_nonrecursive(psic(Body1,Head1),NewPSIC),
    (NewPSIC = psic(BodyUnf,HeadUnf)
        ->  quantify_variables_in_ics(ics(BodyUnf,HeadUnf),
                  ics(Body2,Head2)),
            rewrite_body_terms(Body2,Body3),
            BodyPSIC=Body3, HeadPSIC=Head2,
            PSIC=psic(BodyPSIC,HeadPSIC)
        ;   PSIC=true).


%----------------------------------------------------------
% IMPOSING VARIOUS CONSTRAINTS
%----------------------------------------------------------
:- chr_type functarity ---> [any,int].
:- chr_type posexp ---> e(functarity,?,int).
:- chr_type list(T) --->    [] ; [T | list(T)].
:- chr_constraint
    h/3, en/3, note/2, noten/2, fulf/1, viol/1, pending/1, abd/3,
    e/3.
    %e(+functarity,?,?int).
    %ground_time/0.

get_functor(do(_,_,Main,_),[F,A]):- !, functor(Main,F,A).
get_functor(do(_,_,_,Main,_),[F,A]):- !, functor(Main,F,A).
get_functor(Main,[F,A]):- !, functor(Main,F,A).

h(Event,Time):-
    get_functor(Event,F),
    h(F,Event,Time).

e(Event,Time):-
    get_functor(Event,F),
    e(F,Event,Time).

en(Event,Time):-
    get_functor(Event,F),
    en(F,Event,Time).

abd(Event,Time):-
    get_functor(Event,F),
    abd(F,Event,Time).

% In SICStus 4 there is no remove_constraint, so we remove constraints
% with simpagation rules
fulf(e(F,EEvent,ETime)) \ pending(e(F,EEvent,ETime)) <=> true.
viol(en(F,EEvent,ETime)) \ pending(en(F,EEvent,ETime)) <=> true.
viol(e(F,EEvent,ETime)) \ pending(e(F,EEvent,ETime)) <=> true.  
viol(gt_current_time(e(F,Event,Time), _)) \ pending(e(F,Event,Time)) <=> true.


% Adds the [functor,arity] term to expectations & happened events
% in the body of ICs.
rewrite_body_terms([H,NotH,E,NotE,En,NotEn,Abd,A],[H1,NotH,E1,NotE,En1,NotEn,Abd1,A]):-
    rewrite_body_atom(H,H1),
    rewrite_body_atom(E,E1),
    rewrite_body_atom(Abd,Abd1),
    rewrite_body_atom(En,En1).
    
rewrite_body_atom([],[]).
rewrite_body_atom([h(E,T)|LH],[h(F,E,T)|LH1]):-
    get_functor(E,F),
    rewrite_body_atom(LH,LH1).
rewrite_body_atom([e(E,T)|LH],[e(F,E,T)|LH1]):-
    get_functor(E,F),
    rewrite_body_atom(LH,LH1).
rewrite_body_atom([en(E,T)|LH],[en(F,E,T)|LH1]):-
    get_functor(E,F),
    rewrite_body_atom(LH,LH1).
rewrite_body_atom([abd(E,T)|LH],[abd(F,E,T)|LH1]):-
    get_functor(E,F),
    rewrite_body_atom(LH,LH1).







%----------------------------------------------------------
% EVENT CALCULUS and FACTORING
%
% To carefully think about it...
%----------------------------------------------------------
% Questo andrebbe messo come IC.
% Non posso avere 2 eventi nello stesso tempo
sequential_act @
    e([act,1],act(E1),T1) \
    e([act,1],act(E2),T2)
    <=> sciff_option(seq_act,on),
    T2=T1 | E1=E2.


% factoring
% This factoring is specilised for the abductive event calculus
% Questo factoring va legato ad una regola che toglie uno degli abd
% se ce ne sono 2 identici. Nel caso dell'EC c'e` gia` (che vieta 2 eventi
% nello stesso tempo).

%factoring_ec @
%    e(act(X1),T1), e(act(X2),T2)
%    ==>
%    sciff_option(factoring,on),
%    nonvar(X1), nonvar(X2) |
%    reif_unify(e(act(X1),T1),e(act(X2),T2),B),
%    (B=1 ; B=0).


%% Secondo tentativo: fa l'unificazione dei tempi solo se l'azione
%% e` gia` ground
factoring2 @
    e(F,X,T1), e(F,X,T2)
    ==>
    sciff_option(factoring,on),
    ground(X) |
    (eq(T1,T2) ;
    neq(T1,T2), when(?=(T1,T2),T1\==T2)).
% La seconda serve perche' cosi` se e` la stessa var fallisce
% senza dover usare la (poca) propagazione del diverso.
%%    T1 #= T2 #<=> B,
%%    (B=1 ; B=0).






%----------------------------------------------------------
% ITERATIVE DEEPENING: max number of e(act(...)).
%----------------------------------------------------------
:- chr_constraint 
	max_depth/1.
max_depth_e_act @
e([act,1],act(_),_), max_depth(DepthMax) ==>
    %findall_constraints(e(_,act(_),_),L), length(L,N), N>DepthMax | fail.
    max_constraints(e(_,act(_),_),DepthMax).


%%%MARCOM
max_depth_e_act @
h([sono,4],sono(_,_,_,_),_), max_depth(DepthMax) ==>
    findall_constraints(h(_,sono(_,_,_,_),_),L), length(L,N), N>DepthMax | writeln(ciao(N)),fail.
    %max_constraints(e(_,sono(_,_,_,_),_),DepthMax).



%----------------------------------------------------------
% ITERATIVE DEEPENING: max number of violation
%----------------------------------------------------------
:- chr_constraint
	max_viol/1.
max_violations @
viol(_), max_viol(MaxViol) ==> 
    findall_constraints(viol(_),L), %write(L), 
    length(L,N), 
    write(N) 
    | 
    lt(MaxViol,1000), 
    leq(N,MaxViol).





e_consistency @
    e(F,EEvent,ETime),
    en(F,ENEvent,ENTime)
    ==>
    %findall_constraints(gen_phase,[]) |
    %add_arc_graph('E-consistency',e(EEvent,ETime)\=en(ENEvent,ENTime)),
    check_unification(p(EEvent,ETime),p(ENEvent,ENTime),0,e,en).



%
pending_en @
    en(F,Event,Time)
    ==>
    pending(en(F,Event,Time)).


:- chr_constraint
	nondeterministic/1, phase/1, remove_phase/0.

propagation_h @
    h(F,Event1,Time1),
    psic([[h(F,Event2,Time2)|MoreH],NotH,E,NotE,En,NotEn,Abd,A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    ccopy(p([[h(Event2,Time2)|MoreH],NotH,E,NotE,En,NotEn,Abd,A],Head),
      p([[h(Event2a,Time2a)|MoreHa],NotHa,Ea,NotEa,Ena,NotEna,Abda,Aa],Heada)),
    (subsumeschk(p(Event2a,Time2a),p(Event1,Time1)),
     is_term_quantified(h(Event2a,Time2a),forall)
     ->    %reif_unify(p(Event1,Time1),p(Event2a,Time2a),1),
            (Event1=Event2a, Time1=Time2a
                ->  psic([MoreHa,NotHa,Ea,NotEa,Ena,NotEna,Abda,Aa],Heada)
                ;   true)
     ;  reif_unify(p(Event1,Time1),p(Event2a,Time2a),B),
        PSIC=psic([MoreHa,NotHa,Ea,NotEa,Ena,NotEna,Abda,Aa],Heada),
        (B==1 -> call(PSIC) ;
         B==0 -> true ;
        nondeterministic((B=1, call(PSIC);
        B=0))
        )
    ).

propagation_e @
    e(F,Event1,Time1),
    psic([[],NotH,[e(F,Event2,Time2)|MoreE],NotE,En,NotEn,Abd,A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    ccopy(p([[],NotH,[e(Event2,Time2)|MoreE],NotE,En,NotEn,Abd,A],Head),
      p([[],NotHa,[e(Event2a,Time2a)|MoreEa],NotEa,Ena,NotEna,Abda,Aa],Heada)),
   reif_unify(p(Event1,Time1),p(Event2a,Time2a),B),
   status(S,F),
       (draw_graph(S,propagation_e,e(Event1,Time1)=e(Event2,Time2)),
       B=1, psic([[],NotHa,MoreEa,NotEa,Ena,NotEna,Abda,Aa],Heada)
       ;
       draw_graph(S,propagation_e,e(Event1,Time1)\=e(Event2,Time2)),
       B=0).
% End modification
% Original version:
/*
propagation_e @
    e(Event1,Time1),
    psic([H,NotH,[e(Event2,Time2)|MoreE],NotE,En,NotEn,Abd,A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    ccopy(p([H,NotH,[e(Event2,Time2)|MoreE],NotE,En,NotEn,Abd,A],Head),
      p([Ha,NotHa,[e(Event2a,Time2a)|MoreEa],NotEa,Ena,NotEna,Abda,Aa],Heada)),
   reif_unify(p(Event1,Time1),p(Event2a,Time2a),B),
   status(S,F),
       (draw_graph(S,propagation_e,e(Event1,Time1)=e(Event2,Time2)),
       B#=1, psic([Ha,NotHa,MoreEa,NotEa,Ena,NotEna,Abda,Aa],Heada)
       ;
       draw_graph(S,propagation_e,e(Event1,Time1)\=e(Event2,Time2)),
       B#=0).
*/
% End Original version



% Modified by Federico Chesani
% Date 20060404 1230
propagation_note @
    note(Event1,Time1),
    psic([[],NotH,[],[note(Event2,Time2)|MoreNotE],En,NotEn,Abd,A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    ccopy(p([[],NotH,[],[note(Event2,Time2)|MoreNotE],En,NotEn,Abd,A],Head),
    
p([[],NotHa,[],[note(Event2a,Time2a)|MoreNotEa],Ena,NotEna,Abda,Aa],Heada)),
   reif_unify(p(Event1,Time1),p(Event2a,Time2a),B),
       (B=1, psic([[],NotHa,[],MoreNotEa,Ena,NotEna,Abda,Aa],Heada);
       B=0).
% End Modification       
% Original Version:
/*
propagation_note @
    note(Event1,Time1),
    psic([[],NotH,[],[note(Event2,Time2)|MoreNotE],En,NotEn,Abd,A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    ccopy(p([H,NotH,E,[note(Event2,Time2)|MoreNotE],En,NotEn,Abd,A],Head),
    
p([Ha,NotHa,Ea,[note(Event2a,Time2a)|MoreNotEa],Ena,NotEna,Abda,Aa],Heada)),
   reif_unify(p(Event1,Time1),p(Event2a,Time2a),B),
       (B#=1, psic([Ha,NotHa,Ea,MoreNotEa,Ena,NotEna,Abda,Aa],Heada);
       B#=0).
*/

/*
propagation_e @
    e(F,Event1,Time1),
    psic([[],NotH,[e(F,Event2,Time2)|MoreE],NotE,En,NotEn,Abd,A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    ccopy(p([[],NotH,[e(Event2,Time2)|MoreE],NotE,En,NotEn,Abd,A],Head),
      p([[],NotHa,[e(Event2a,Time2a)|MoreEa],NotEa,Ena,NotEna,Abda,Aa],Heada)),
   reif_unify(p(Event1,Time1),p(Event2a,Time2a),B),
   status(S,F),
       (draw_graph(S,propagation_e,e(Event1,Time1)=e(Event2,Time2)),
       B#=1, psic([[],NotHa,MoreEa,NotEa,Ena,NotEna,Abda,Aa],Heada)
       ;
       draw_graph(S,propagation_e,e(Event1,Time1)\=e(Event2,Time2)),
       B#=0).
*/

propagation_en @
    en(F,Event1,Time1),
    psic([[],NotH,[],[],[en(F,Event2,Time2)|MoreEn],NotEn,Abd,A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    ccopy(p([[],NotH,[],[],[en(Event2,Time2)|MoreEn],NotEn,Abd,A],Head),
      p([[],NotHa,[],[],[en(Event2a,Time2a)|MoreEna],NotEna,Abda,Aa],Heada)),
    ccopy( p(  Event1,  Time1),
           p( Event1a, Time1a)),
     reif_unify(p(Event1a,Time1a),p(Event2a,Time2a),B),
       (B=1, psic([[],NotHa,[],[],MoreEna,NotEna,Abda,Aa],Heada);
       B=0).

propagation_noten @
    noten(Event1,Time1),
    psic([[],NotH,[],[],[],[noten(Event2,Time2)|MoreNotEn],Abd,A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    ccopy( p([[], NotH, [], [], [], [noten(Event2, Time2)| MoreNotEn], Abd,A], Head),
           p([[],NotHa,[],[],[],[noten(Event2a,Time2a)|MoreNotEna],Abda,Aa],Heada)),
    ccopy( p(  Event1,  Time1),
           p( Event1a, Time1a)),
	reif_unify(p(Event1a,Time1a),p(Event2a,Time2a),B),
    (	B=1, psic([[],NotHa,[],[],[],MoreNotEna,Abda,Aa],Heada)
    	;
       	B=0).
  

propagation_abd @
    abd(F,Event1,Time1),
    psic([[],NotH,[],[],[],[],[abd(F,Event2,Time2)|MoreAbd],A],Head)
    ==>
    fn_ok(Event1,Event2)
    |
    %ccopy(p([[],NotH,[],[],[],[],[abd(Event2,Time2)|MoreAbd],A],Head),
	%				p([[],NotHa,[],[],[],[],[abd(Event2a,Time2a)|MoreAbda],Aa],Heada)),
	ccopy(p(NotH, Event2, Time2, MoreAbd, A, Head),
		  p(NotHa,Event2a,Time2a,MoreAbda,Aa,Heada)),
    (subsumeschk(p(Event2a,Time2a),p(Event1,Time1)),
     is_term_quantified(abd(Event2a,Time2a),forall)
     -> (Event2a = Event1, Time2a=Time1
            ->  psic([[],NotHa,[],[],[],[],MoreAbda,Aa],Heada)
            ;   true)
     ;  reif_unify(p(Event1,Time1),p(Event2a,Time2a),B),
        (B=1, psic([[],NotHa,[],[],[],[],MoreAbda,Aa],Heada);
        B=0)
    ).





propagation_violated @
    viol(Event1),
    psic([[],NotH,[],[],[],[],[abd([viol,1],viol(Event2),_Time2)|MoreAbd],A],Head)
    ==>
    sciff_option(violation_causes_failure,no),
    fn_ok(Event1,Event2)
    |
    ccopy(p([[],NotH,[],[],[],[],[viol(Event2)|MoreAbd],A],Head),
    
p([[],NotHa,[],[],[],[],[viol(Event2a)|MoreAbda],Aa],Heada)),
    reif_unify(p(Event1),p(Event2a),B),
%       (B#=1, psic([[],NotHa,[],NotEa,ENa,NotEna,MoreAbda,Aa],Heada);
       (B=1, psic([[],NotHa,[],[],[],[],MoreAbda,Aa],Heada);
       B=0).

% Two terms may be unifiable.
% Profiled version, nearly twice as fast

fn_ok(Term1,Term2):- Term1 == Term2,!. % either if they are identical
fn_ok(Term1,Term2):- \+(?=(Term1,Term2)). % or if they are syntactically unifiable

/* old version 
fn_ok(Term1,Term2):-
    nonvar(Term1),
    nonvar(Term2),
    !,
    Term1=..[H|T1],
    Term2=..[H|T2],
    fn_ok_list(T1,T2).
fn_ok(_,_).
*/


fn_ok_list([],[]).
fn_ok_list([H1|T1],[H2|T2]):-
    fn_ok(H1,H2),
    fn_ok_list(T1,T2).


split_list(List,1,[],List):-
    !.
split_list([Head|Tail],N,[Head|Tail1],Rest):-
    N1 is N-1,
    split_list(Tail,N1,Tail1,Rest).

type_position(h,1).
type_position(noth,2).
type_position(e,3).
type_position(note,4).
type_position(en,5).
type_position(noten,6).
  


logical_equivalence @
%    psic(true,Head) MarcoG: questa mi sembra obsoleta ...
    psic([[],[],[],[],[],[],[],[]],Head)
    <=>
    add_arc_graph(logical_equivalence,(true->Head)),
    impose_head1(Head).

naf(Goal):-
    commas_to_list(Goal,GoalList),
    ics_quant:split_body(GoalList,Body),
    psic(Body,[]).


unfold_psic @
    psic([[],[],[],[],[],[],[],Atoms],Head)
    <=>
    %status(S,F),
    %draw_graph(S, unfolding_psic, Atoms),
    unfold([psic(Atoms,Head)]).

unfold([]):-
    !.
unfold([psic([],Head)|MorePSICS]):- !, 
    % MarcoG Bug fix: the cut was after the call to psic.
    % psic is a CHR constraint, that is rewritten to a Prolog predicate
    % that embeds in its definitions the CHR rules that involve the constraint psic.
    % So, by adding a cut after this, we are cutting the choice points inside
    % the rules that involve psic, so sometimes it cuts alternatives in the
    % head!!  
    %psic([[],[],[],[],[],[],[],[]],Head),
    impose_head1(Head),
    unfold(MorePSICS).
unfold([psic([naf(Goal)|Body],Head)|PSICS]):- !,
    commas_to_list(Goal,GoalList),
    append(Head,[GoalList],NewHead),
    psic([[],[],[],[],[],[],[],Body],NewHead),
    unfold(PSICS).
unfold([psic([clp_constraint(Constraint)|MoreBodyAtoms],Head)|MorePSICS]):-
    call(Constraint),
    unfold([psic(MoreBodyAtoms,Head)|MorePSICS]).
unfold([psic([clp_constraint(Constraint)|_MoreBodyAtoms],_Head)|MorePSICS]):- !,
% MarcoG: metto una pezza, perche' il vincolo a volte e` una restriction
% e non riesce a fare st(C) #<=> 0, pero` si puo` fare st(C) #<=> 0
    invert_constraint(Constraint),
    unfold(MorePSICS).
% MarcoG 12 jul 2006: Sometimes the clause generated with
% unfolding contains the explicit quantification of a
% variable. The quantification is different in PSICs.
% I ignore it for the moment, but maybe it should be
% considered better ...
unfold([psic([quant(_,_)|Body],Head)|MorePSICS]):- !,
    unfold([psic(Body,Head)|MorePSICS]).

% Federico 08 Jun 2007: Managing abducibles introduced by previous unfolding
unfold([psic([AnAtom|Body],Head)|PSICS]):-
	is_SCIFF_RESERVED(AnAtom),
	!,
	ic([AnAtom|Body],Head),
	unfold(PSICS).
	
	
%%% MARCOM: Aggiunta ad-hoc per trattare la negazione di holds_at in mutua esclusione %%%
unfold([psic([not_holdsat(F,T)|Body],Head)|PSICS]):- !,
	append_naf(Head, holds_at(F, T), ExtHead),
    append(ExtHead,[[test_holds_at(F, T)]],NewHead),
    %end modified part
    psic([[],[],[],[],[],[],[],Body],NewHead),
    unfold(PSICS).    
    
append_naf([],Goal,[]).
append_naf([Conjunct|MoreConjuncts], Goal, [[naf(Goal)|Conjunct]|MoreExtConjuncts]):-
	append_naf(MoreConjuncts, Goal, MoreExtConjuncts).	
		



unfold([PSIC|MorePSICS]):-
    PSIC=psic([BodyAtom|_],_),
    predicate_property(BodyAtom, dynamic),
    !,
    get_candidate_clauses(BodyAtom,Clauses),
    check_forall(Clauses),
    get_unfolded_psics(Clauses,PSIC,UnfoldedPSICS),
    append(UnfoldedPSICS,MorePSICS,NewPSICS),
    unfold(NewPSICS).

unfold([PSIC|MorePSICS]):-
    PSIC=psic([BodyAtom|_],_),
    is_sicstus_predicate(BodyAtom),!,
    findall(clause(BodyAtom,[]),BodyAtom,Clauses),
    get_unfolded_psics(Clauses,PSIC,UnfoldedPSICS),
    append(UnfoldedPSICS,MorePSICS,NewPSICS),
    unfold(NewPSICS).

% totally unknown predicate: undefined, not built-in.
unfold([PSIC|MorePSICS]):-
    PSIC=psic([BodyAtom|_],_),
    \+predicate_property(BodyAtom, _),!,unfold(MorePSICS).

% MarcoG: The current atom in the PSIC is not a
% defined predicate, nor a built-in. It must have come
% out from unfolding, and must be an H, or an abducible.
% Let us just re-state the IC, so it will be re-parsed
% and quantified by impose_ics. Hope the quantification
% is correct (to do: check if the quantification is correct)


unfold([psic(_,_)|_]):-
    writeln('An error occured while applying unfolding transition.'),
    writeln('Please communicate this error to SCIFF developers as soon as possible!'),
    writeln('Many Thanks!'),
    fail.


unfold_nonrecursive([],[]):- !. % The PSIC could not have any fact matching it
% ie., the PSIC can be removed. We represent this fact with the emptylist
% it is used by the recursive call (get_unfolded_nonrecursive can return
% the emptylist)
unfold_nonrecursive(psic(Body,Head),UnfoldedPSIC):-
    select(Atom,Body,BodyRest),
    predicate_property(Atom,dynamic),
    is_unquantified(Atom), % This way, I am ure it is not invoked by unfolding
    %copy_term(Atom,Atom1), does not strip constraints
    functor(Atom,F,Arity),
    functor(Atom1,F,Arity),  %this strips constraints, in case the IC was called not in the beginning, e.g., by unfold.
    findall((Atom1,BodyClause),clause(Atom1,BodyClause),Clauses),
    nonrecursive(Clauses),!,
    get_unfolded_nonrecursive(Atom,BodyRest,Head,Clauses,[UnfoldedPSIC1]),
    unfold_nonrecursive(UnfoldedPSIC1,UnfoldedPSIC).
unfold_nonrecursive(X,X).

% Currently unfolds eagerly only predicates consisting of exactly one fact.
nonrecursive([(_,true)]).
get_unfolded_nonrecursive(Atom,BodyRest,Head,[(Atom1,_BodyClause)],[UnfoldedPSIC1]):-
    (Atom = Atom1
     ->    UnfoldedPSIC1 = psic(BodyRest,Head)
     ;     UnfoldedPSIC1 = [] % The atom in the body of PSIC does not unify with
                              % the only fact defining it -> the PSIC is removed
    ).

% We assume that all predicates imported from some module are built-in
is_sicstus_predicate(Pred) :- 
    predicate_property(Pred,Prop),
    % Added also interpreted and compiled, so we can invoke predicates defined in
    % SCIFF itself
    memberchk(Prop,[built_in,imported_from(_),interpreted,compiled]).

%is_sicstus_predicate(Pred) :- member(Pred, [ground, var, nonvar, write, nl, =, memberchk, last_state]).
%is_sicstus_predicate(Pred) :- member(Pred, [ground, var, nonvar, write, nl, =, memberchk]).
% End modification

% ORIGINAL VERSION:
/*
unfold([PSIC|MorePSICS]):-
    PSIC=psic([BodyAtom|_],_),
    get_candidate_clauses(BodyAtom,Clauses),
  
    write('Unfolding: BodyAtom:'), nl,
    write(BodyAtom), nl,
    write('Unfolding: Clauses:'), nl,
    write(Clauses), nl,
  
    check_forall(Clauses),
    get_unfolded_psics(Clauses,PSIC,UnfoldedPSICS),
    append(UnfoldedPSICS,MorePSICS,NewPSICS),
    unfold(NewPSICS).
*/
% End original version
% End Modification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invert_constraint(st(reif_unify(X,Y,B))):- !,
    Bneg in 0..1,
    Bneg #= 1-B,
    reif_unify(X,Y,Bneg).
invert_constraint(reif_unify(X,Y,B)):- !,
    Bneg in 0..1,
    Bneg #= 1-B,
    reif_unify(X,Y,Bneg).
invert_constraint(Constraint):-
    (Constraint = st(Restriction)
        ->  once(opposite(Restriction,Opp)),
            st(Opp)
        ;   once(opposite(Constraint,Opp)),
            call(Opp)
    ).

check_forall(Term):-
    term_variables(Term,Variables),
    quantify_unquantified(Variables,forall).

quantify_unquantified([],_).
quantify_unquantified([Var|MoreVars],Quant):-
    get_quant(Var,_),
    !,
    quantify_unquantified(MoreVars,Quant).
quantify_unquantified([Var|MoreVars],Quant):-
    set_quant(Var,Quant),
    quantify_unquantified(MoreVars,Quant).


get_unfolded_psics([],_,[]).
get_unfolded_psics([clause(CHead,CBody)|MoreClauses],
           PSIC,
           UnfoldedPSICS):-
    (MoreClauses = []
        ->  PSIC=psic([Atom1|MoreAtoms1],Head1)
        ;   ccopy(PSIC,psic([Atom1|MoreAtoms1],Head1)) % MG 16 feb 2008: 
                                                % Mi sembra che la ccopy sia necessaria solo se
                                                % ci sono altre clausole
    ),
    (is_term_quantified(Atom1,forall)
     -> (Atom1=CHead
            ->  append(CBody,MoreAtoms1,NewAtoms),
                UnfoldedPSICS=[psic(NewAtoms,Head1)|MoreUnfoldedPSICS]
            ;   UnfoldedPSICS=MoreUnfoldedPSICS
        )
    ;   reif_unify(CHead,Atom1,B),
        (B==1 ->    append(CBody,MoreAtoms1,NewAtoms),
                    UnfoldedPSICS=[psic(NewAtoms,Head1)|MoreUnfoldedPSICS] ;
         B==0 ->    UnfoldedPSICS=MoreUnfoldedPSICS ;
        (
            (B=1,
            append(CBody,MoreAtoms1,NewAtoms),
            UnfoldedPSICS=[psic(NewAtoms,Head1)|MoreUnfoldedPSICS])
        ;
            (B=0,
            UnfoldedPSICS=MoreUnfoldedPSICS)
        ))
    ),
    get_unfolded_psics(MoreClauses,PSIC,MoreUnfoldedPSICS).
       


get_candidate_clauses(Atom,Clauses):-
    functor(Atom,F,N),
    findall(clause(Head,Body),
        (
          functor(Head,F,N),
          clause(Head,Body1),
          fn_ok(Atom,Head),
          (Body1=true->
              Body=[];
              commas_to_list(Body1,Body))
        ),
        Clauses).



impose_head1(Head):-
    quantify_unquantified(Head),
    term_variables(Head,Variables),
    flag_variables(Variables),
    (Head=[Disj]-> impose_head_disjunct(Disj) ;
     Head=[] -> fail ;
        nondeterministic(impose_head(Head))).


quantify_unquantified(Head):-
    ics_scan_head(Head,[],HV1),
    adjust_variable_list(HV1,HV),
    % No more repeated variables now (eliminated when SICS were loaded)
    quantify_unquantified_variables(HV).

quantify_unquantified_variables([]).
quantify_unquantified_variables([variable(Variable,_)|MoreVariables]):-
    get_quant(Variable,_),
    !,
    quantify_unquantified_variables(MoreVariables).
quantify_unquantified_variables([Variable|MoreVariables]):-
    decide_variable_quantification(Variable,Quantification),
    attach_variable_quantification(Variable,Quantification),
    quantify_unquantified_variables(MoreVariables).

  
%impose_head([]):- fail.
impose_head([H|_]):-
    impose_head_disjunct(H).
impose_head([_|T]):-
    impose_head(T).

impose_head_disjunct([]).
impose_head_disjunct([clp_constraint(C)|Tail]):- !,
    st(C), impose_head_disjunct(Tail).
impose_head_disjunct([Head|Tail]):-
    %Head=..[Functor|Arguments],
    %length(Arguments,NofArgs),
    %functor(Head,Functor,NofArgs),
    isabducible(Head),
    !,
    abduce(Head),
    impose_head_disjunct(Tail).
impose_head_disjunct([Head|Tail]):-
    call(Head),
    impose_head_disjunct(Tail).

% duplicated both with 2 and 3 arguments, because they might occur in both ways
isabducible(h(_,_)).
isabducible(h(_,_,_)).  
isabducible(e(_,_,_)).
isabducible(e(_,_)).
isabducible(en(_,_,_)).
isabducible(en(_,_)).
isabducible(note(_,_)).
isabducible(noten(_,_)).
isabducible(abd(_,_,_)).
isabducible(abd(_,_)).

is_SCIFF_RESERVED(noth(_,_)) :- !.
is_SCIFF_RESERVED(noth(_,_,_)) :- !.
is_SCIFF_RESERVED(X) :-
	isabducible(X).


violation @
    h(F,HEvent,HTime),
    pending(en(F,EEvent,ETime)) # _pending
    ==>
    fn_ok(HEvent,EEvent)
    |
    %ccopy(p(EEvent,ETime),p(EEvent1,ETime1)),  MA QUESTA CCOPY E` NECESSARIA?
    case_analysis_violation(F,HEvent,HTime,EEvent,ETime,_pending).
/* Old version
case_analysis_violation(HEvent,HTime,EEvent,ETime,
              EEvent1,ETime1,_pending):-
    reif_unify(p(HEvent,HTime),p(EEvent1,ETime1),1),
    remove_constraint(_pending),
    viol(en(EEvent,ETime)).
case_analysis_violation(HEvent,HTime,_,_,EEvent1,ETime1,_):-
    reif_unify(p(HEvent,HTime),p(EEvent1,ETime1),0).
*/
% New version, more efficient
case_analysis_violation(F,HEvent,HTime,EEvent1,ETime1,_):- 
    get_option(violation_causes_failure, OptFail),
    (OptFail = yes
        ->  reif_unify(p(HEvent,HTime),p(EEvent1,ETime1),0)
        ;   reif_unify(p(HEvent,HTime),p(EEvent1,ETime1),B),
            (   B=0
            ;   B=1,
                %remove_constraint(_pending),
                viol(en(F,EEvent1,ETime1))
            )
    ).





load_ics:-
    findall(ic(Head,Body),
        ics(Head,Body),
        ICSs),
    call_list(ICSs).

history:-
    history_is_empty(yes),
    !.
history:-
    findall(h(Event,Body),
        hap(Event,Body),
        History),
    set_term_quantification(History, existsf),
    call_list(History).


call_list([]).
call_list([IC|MoreICs]):-
    call(IC),
    call_list(MoreICs).


/* Abduction */



abduce(Abducible):-
    term_variables(Abducible,Variables),
    flag_variables(Variables),
    call(Abducible).

 
flag_variables([]).  
flag_variables([Var|MoreVars]):-
    get_quant(Var,Q),
    ((Q=forallf ; Q=existsf) -> true
    ;   flag_quant(Q,NewQ),
        set_quant(Var,NewQ)
    ),
    flag_variables(MoreVars).

flag_quant(exists,existsf).
flag_quant(forall,forallf).

/* old version, less efficient
flag_variables([]). 
flag_variables([Var|MoreVars]):-
    get_quant(Var,existsf),
    !,
    flag_variables(MoreVars).
flag_variables([Var|MoreVars]):-
    get_quant(Var,forallf),
    !,
    flag_variables(MoreVars).
flag_variables([Var|MoreVars]):-
    get_quant(Var,exists),
    !,
    quant(Var,existsf),
    flag_variables(MoreVars).
flag_variables([Var|MoreVars]):-
    get_quant(Var,forall),
    quant(Var,forallf),
    flag_variables(MoreVars).
*/



%MarcoG 9 may 2005
% The 1st clause used to be:
%check_unification(T1,T2,B,_,_):-
%    ccopy(p(T1,T2),p(U1,U2)),
%    reif_unify(U1,U2,B),
%    reif_unify(T1,T2,B),!.
% but in this case
% ?- existsf(U1), U1 #< P, e(un(a),U1), forallf(X), forallf(U2), st(U2,U2#<U1), st(U2,U2#>0), en(un(X),U2).
% results in X=a!!
% I only unify the copies.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modified by Federico, 30-10-2006
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Old version:
/*


check_unification(T1,T2,B,_,_):-
    ccopy(p(T1,T2),p(U1,U2)),
    reif_unify(U1,U2,B),!.
 check_unification(T1,T2,_,S1,S2):-
    T1=..[_|A1],
    T2=..[_|A2],
    R1=..[S1|A1],
    R2=..[S2|A2],
    inconsistent(R1,R2),
    (	current_predicate(printConstraints/0)
     	->	printConstraints
     	;		true
    ),
    fail.
*/


% New Version:
% DANGER!!! This predicate is defined also in sciff_java_gui.pl
% PLEASE DO NOT MODIFY IT UNLESS YOU REALLY KNOW WHAT ARE YOU DOING!
% KEEP COHERENT THE TWO VERSIONS OF THIS PREDICATE !!!
:- chr_constraint
   inconsistent/2.

check_unification(T1,T2,B,e,note):- !,
    reif_unify(T1,T2,B).
check_unification(T1,T2,B,e,en):- !,
    ccopy(T2,U2),
    reif_unify(T1,U2,B).
check_unification(T1,T2,B,_,_):-
    ccopy(p(T1,T2),p(U1,U2)),
    reif_unify(U1,U2,B).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



not_consistency_e @
    e(_,EEvent,ETime),
    note(NotEEvent,NotETime)
    ==>
    check_unification(p(EEvent,ETime),p(NotEEvent,NotETime),0,e,note).


not_consistency_en @
    en(_,EnEvent,EnTime),
    noten(NotEnEvent,NotEnTime)
    ==>
    check_unification(p(EnEvent,EnTime),p(NotEnEvent,NotEnTime),0,en,noten).

:- chr_constraint
	close_history/0.



/*  version SICStus 3.9
propagation_noth @
    close_history,
    psic([[],[NotH|MoreNotH],[],[],[],[],[],A],Head) # _psic
    ==>
    true
    &
    Body=[[],[NotH|MoreNotH],[],[],[],[],[],A],
    ccopy(p(Body,Head),p(Body1,Head1)),
    propagate_noth(Body1,Body2)
    |
    psic(Body2,Head1).
%    pragma
%    passive(_psic).
*/

% Version for SICStus 4. No more tell guards: Check if it is correct!!!
propagation_noth @
    close_history,
    psic([[],[NotH|MoreNotH],[],[],[],[],[],A],Head)
    ==>
    Body=[[],[NotH|MoreNotH],[],[],[],[],[],A],
    ccopy(p(Body,Head),p(Body1,Head1)),
    propagate_noth(Body1,Body2)
    |
    psic(Body2,Head1).



propagate_noth([H,NotH,E,NotE,EN,NotEN,Abd,A],
           [H,NewNotH,E,NotE,EN,NotEN,Abd,A]):-
    noth_propagation(NotH,NewNotH).

noth_propagation([NotH|MoreNotHs],NewNotH):-
    noth_success(NotH),
    !,
    noth_propagation1(MoreNotHs,NewNotH).
noth_propagation([NotH|MoreNotHs],[NotH|MoreNewNotHs]):-
    noth_propagation(MoreNotHs,MoreNewNotHs).

noth_propagation1([],[]).
noth_propagation1([NotH|MoreNotHs],NewNotH):-
    noth_success(NotH),
    !,
    noth_propagation1(MoreNotHs,NewNotH).
noth_propagation1([NotH|MoreNotHs],[NotH|MoreNewNotHs]):-
    noth_propagation1(MoreNotHs,MoreNewNotHs).

noth_success(noth(Event,Time)):-
    universal_variables_in_term(noth(Event,Time),UnivVariables),
    (UnivVariables=[] ->
        \+(has_happened(Event,Time));
        universal_variables_happened(Event,Time,TermList), % In realta` il D8 dice che 
        % dovrei rendere universali tutte le variabili, escluse le esistenziali non flagged. 
        PUniv=..[p|UnivVariables],
        % Qui devo rendere flagged tutte le variabili che sono universali (in teoria tutte escluse le exist non flagged)
        flag_variables(UnivVariables),
        impose_noth_not_unif(TermList,PUniv)).

impose_noth_not_unif([],_).
impose_noth_not_unif([Term|MoreTerms],PUniv):-
    set_restriction(reified_unif:reif_unify(Term,PUniv,0)),
    %reif_unify(Term,PUniv,0),
    impose_noth_not_unif(MoreTerms,PUniv).

universal_variables_in_term(Term,UnivVariables):-
    term_variables(Term,Vars),
    keep_universals(Vars,UnivVariables).

keep_universals([],[]).
keep_universals([Var|MoreVars],[Var|MoreUnivVars]):-
    is_universal(Var),
    !,
    keep_universals(MoreVars,MoreUnivVars).
keep_universals([_|MoreVars],UnivVars):-
    keep_universals(MoreVars,UnivVars).

universal_variables_happened(Event,Time,TermList):-
    findall(Term,
        (
          ccopy(p(Event,Time),p(Event1,Time1)),
          universal_variables_in_term(p(Event1,Time1),UnivVars),
          has_happened(Event1,Time1),
          Term=..[p|UnivVars]
          ),
        TermList).

has_happened(Event,Time):-
    find_constraint(h(_,Event,Time),_).

%% COMPLIANT HISTORY GENERATION: 20 JUL 04

/* versione MarcoA
fulfiller1 @
    close_history,
    pending(e(Event,Time))
    ==>
%    write('Inserted '),write(h(Event,Time)),nl,
  %  write_history,
    h(Event,Time).
*/

pending_e @
    e(F,Event,Time)
    ==>
    pending(e(F,Event,Time)).

%----------------------------------------------------------
% FULFILLMENT RULES
%----------------------------------------------------------
%new fdet version
fulfillment @
    h(F,HEvent,HTime),
    pending(e(F,EEvent,ETime)) # _pending
    ==>
    fn_ok(HEvent,EEvent)
    |
    %ccopy(p(EEvent,ETime),p(EEvent1,ETime1)), Non necessary: all variables existentially quant.
    case_analysis_fulfillment(F,HEvent,HTime,EEvent,ETime,_pending).

case_analysis_fulfillment(F,HEvent,HTime,EEvent,ETime,_):-
    % HEvent=EEvent, HTime=ETime,
    % If we use unification, it triggers all the CHR constraints involving the variables
    % before terminating the execution. With g-SCIFF, it will trigger fulfiller before
    % removing the pending expectation. Better use reif_unify.
    % 4 Aug 07: Actually even with reif_unify fulfiller is triggered too early.
    % Let us anticipate remove_constraint
    %remove_constraint(_pending),
    %term_unify(HEvent,EEvent),
    %eq(HTime,ETime),
    reif_unify(p(HEvent,HTime),p(EEvent,ETime),1),
    (
    	fdet(e(Term,Time)),
    	subsumeschk(e(Term,Time),e(EEvent,ETime))
    ->
    	!
    ;
    	true
    ),
    fulf(e(F,EEvent,ETime)).
case_analysis_fulfillment(_,HEvent,HTime,EEvent1,ETime1,_):-
    reif_unify(p(HEvent,HTime),p(EEvent1,ETime1),0).


fulfillment_allows @
    h(F,do(HX,HRec,HAction,HD),HT),
    pending(e(F,do(Expecter,EX,ERec,EAction,ED),ET)) # _pending
    ==>
    fn_ok(p(do(HX,HRec,HAction,HD),HT),p(do(EX,ERec,EAction,ED),ET))
    |
    %ccopy(p(EEvent,ETime),p(EEvent1,ETime1)), Non necessary: all variables existentially quant.
    case_analysis_fulfillment(F,do(Expecter,HX,HRec,HAction,HD),HT,do(Expecter,EX,ERec,EAction,ED),ET,_pending).

/*
fulfillment @
    h(HEvent,HTime),
    pending(e(EEvent,ETime)) # _pending
    ==>
    fn_ok(HEvent,EEvent)
    |
    %ccopy(p(EEvent,ETime),p(EEvent1,ETime1)), Non necessary: all variables existentially quant.
    case_analysis_fulfillment(HEvent,HTime,EEvent,ETime,_pending).

case_analysis_fulfillment(HEvent,HTime,HEvent,HTime,_pending):-
    remove_constraint(_pending),
    %reif_unify(p(HEvent,HTime),p(EEvent,ETime),1),
    (sciff_option(fdet,on)->!;true),
    fulf(e(EEvent,ETime)).
case_analysis_fulfillment(HEvent,HTime,EEvent1,ETime1,_):-
    reif_unify(p(HEvent,HTime),p(EEvent1,ETime1),0).
*/


/* versione MarcoG */
fulfiller1 @
    (close_history)
    \
    (pending(e(F,Event,Time)))
    <=>
%        write('Inserted '),write(h(Event,Time)),
    sciff_option(fulfiller,on)
    %findall_constraints(nondeterministic(_),[]) bug fix: fulfilment does not use nondeterinistic
    |
    fulf(e(F,Event,Time)),
    h(F,Event,Time),
    %%%MARCOM
    writeln(h(Event,Time)).

%Constraints used by AlLoWS
:- chr_constraint
	gen_phase/0, end_gen_phase/0, remove_exp/0.

fulfiller_rationality @
    (gen_phase) \
    pending(e(F,do(X,X,Rec,Action,D),T))
    <=>
    %sciff_option(fulfiller_rationality,on) |
    fulf(e(F,do(X,X,Rec,Action,D),T)),
    h(F,do(X,Rec,Action,D),T).

fulfiller_chor @
    (gen_phase),
    pending(e(F,do(chor,X,Rec,Action,D),T)) % # _pending
    ==>
    %sciff_option(fulfiller_rationality,on) |
    testing(WS), set_term_quantification(WS,existsf),
    reif_unify(WS,X,Bool),
    (Bool=0;Bool=1),
    (Bool=0
      ->    %remove_constraint(_pending),
            fulf(e(F,do(chor,X,Rec,Action,D),T)),
            h(F,do(X,Rec,Action,D),T)
      ;     true).

end_gen_phase @
    end_gen_phase \
    gen_phase <=> true.

write_history:-
    %findall_constraints(h(send(_,_,_,_),_),L), MarcoG: why only send?
    findall_constraints(h(_,_,_),L),
    print_chr_list(L,'\n').

write_normal_abducibles:-
		findall_constraints(abd(_,_,_),L),
    print_chr_list(L,'\n').

write_positive_expectations:-
		findall_constraints(e(_,_,_),L),
    print_chr_list(L,'\n').

write_violations:-
		findall_constraints(viol(_,_,_),L),
    print_chr_list(L,'\n').

write_events_not_expected:-
		findall_constraints(not_expected(_,_,_),L),
    print_chr_list(L,'\n').

/*
closure_e @
    (close_history)
    \
    (pending(e(Event,Time)) # _pending)
    <=>
    viol(e(Event,Time))
    pragma
    passive(_pending).
*/


closure_e @
    (close_history),
    (pending(e(F,Event,Time)) ) %# _pending)
    ==>
    true | % this guard has been added to avoid a mis-interpretation of the chr compiler
    (get_option(violation_causes_failure, yes)
    		->	fail
    		; 	(%remove_constraint(_pending),
    				 viol(e(F,Event,Time))
    				)
    ).
    %pragma passive(_pending). Does not work with violation_causes_failure -> no

/*
closure_e @
    (close_history)
    \
    (pending(e(Event,Time)) # _pending)
    <=>
    get_option(violation_causes_failure, no)
    |
    viol(e(Event,Time))
    pragma passive(_pending).
*/

:- chr_constraint
	not_expected/1.


not_expected_gen @
		(h(F,HEvent,HTime))
		==>
		sciff_option(allow_events_not_expected, no)
		|
		not_expected(h(F,HEvent,HTime)).



not_expected_remove @
		fulf(e(F,Event, Time))
		\
		not_expected(h(F,Event, Time))
		<=>
		true.

% If an event is expected both by the WS and the Chor,
% then it is no longer not_expected
not_expected_remove_allows1 @
		(fulf(e(F,do(chor,EX,ERec,EAction,ED),ET)),
		fulf(e(F,do(WS,EX,ERec,EAction,ED),ET)))
		\
		not_expected(h(F,do(EX,ERec,EAction,ED),ET))
		<=> testing(WS) |
		true.

% If an event is expected only by the choreography,
% and it does not involve the WS under test, then
% the event is expected
not_expected_remove_allows2 @
		fulf(e(F,do(chor,EX,ERec,EAction,ED),ET))
		\
		not_expected(h(F,do(EX,ERec,EAction,ED),ET))
		<=> %write('******** TRYING ***************'),
            testing(WS), nonvar(EX), EX \= WS, nonvar(ERec), ERec \= WS |
		true.


% violation is imposed only if the corresponding flag is set to true
protocol_e @
		(close_history) \
    (not_expected(h(F,Event,Time)) # _not_expected)
    <=>
    true %added to avoid a possible chr bug
    |
    (	get_option(violation_causes_failure, yes)
    	->	fail
    	;		%remove_constraint(_not_expected),
    			viol(not_expected(h(F,Event,Time)))
    )
    pragma passive(_not_expected).
    

% End Modification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


closure_en @
    (close_history)
    \
    (pending(en(F,Event,Time)) # _pending)
    <=>
    fulf(en(F,Event,Time))
    pragma
    passive(_pending).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODIFIED BY FEDERICO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Removed because of an optimization introduced by MarcoG
% If violation causes failure then failure is imposed immediately
% without adding the chr_constraint viol(_).
% Otherwise the chr_constraint viol(_) is imposed, and no failment is imposed.
%
% Violations are generated by the following chr_rules:
% - case_analysis_violation (EN with a corresponding H)
% - closure_e (close_history, and E without H)
% - ct_time_constraint (E and due to the current_time, no H is possible anymore)
% - protocol_e (H without E)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
failure_for_violation @
    viol(_)
    ==>
    (	current_predicate(printConstraints/0)
     	->	printConstraints
     	;		true
    ),
   get_option(violation_causes_failure, no).
   */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CURRENT TIME MANAGEMENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint
    current_time/1.

current_time_update @
    (h(_,current_time,CT))
    \
    (current_time(_OldCT) # _current_time)
    <=>
    current_time(CT)
    pragma
    passive(_current_time).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modified by Federico, 20061102
%
% Original version:
/*
ct_time_constraint @
    current_time(CT),
    pending(e(Event,Time)) # _pending
    ==>
    impose_time_constraint(Time,CT)
    pragma
    passive(_pending).

constraints
    gt_current_time/2.

impose_time_constraint(Time,CT):-
    Time #>= CT,
    !.
impose_time_constraint(Time,CT):-
    gt_current_time(Time,CT),
    (	current_predicate(printConstraints/0)
     	->	printConstraints
     	;		true
    ),
    fail.
*/

ct_time_constraint @
    current_time(CT),
    pending(e(F,Event,Time)) # _pending
    ==>
    impose_time_constraint(Time,CT, e(F,Event,Time), _pending)
    pragma
    passive(_pending).

impose_time_constraint(Time,CT, e(F,Event,Time), _):-
    ( geq(Time,CT)
    	->	true
    	;		( get_option(violation_causes_failure, yes)
    				->	fail
    				;	%remove_constraint(_pending),	
                        viol(gt_current_time(e(F,Event,Time), failed_to_impose_greater_than(Time, CT)))
    			)
    ).


%end @ phase(deterministic) <=> findall_constraints(nondeterministic(_),[]) | true.
switch2nondet @ phase(deterministic) <=> phase(nondeterministic).
/*switch2det @ phase(nondeterministic) \ nondeterministic(G) <=>
    call(G).
    %phase(deterministic).*/

switch2det @ phase(nondeterministic) , nondeterministic(G) <=>
    call(G),
    phase(deterministic).


remove_phase \ phase(_) <=> true.
remove_phase <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



build(SOKB,ICS,History):-
   translate_sokb(SOKB,'./sokb.pl'),
    translate_ics(ICS,'./ics.pl'),
    translate_history(History,'./history.pl').

build(ExampleName):-
    atom_concat(ExampleName,'/',A1),
    atom_concat(A1,ExampleName,A2),
    atom_concat(A2,'_sokb.pl',SOKB),
    atom_concat(A2,'_ics.txt',ICS),
    atom_concat(A2,'_history.txt',History),
    build(SOKB,ICS,History).
build(ExampleName,SubExample):-
    atom_concat(ExampleName,'/',A1),
    atom_concat(A1,ExampleName,A2),
    atom_concat(A2,'_sokb.pl',SOKB),
    atom_concat(A2,'_ics.txt',ICS),
    atom_concat(A2,'_history_',A3),
    atom_concat(A3,SubExample,A4),
    atom_concat(A4,'.txt',History),
    build(SOKB,ICS,History).

build1(ExampleName):-
    atom_concat('../examples/',ExampleName,A0),
    atom_concat(A0,'/',A1),
    atom_concat('../protocols/',ExampleName,B0),
    atom_concat(B0,'/',B1),
    atom_concat(A1,ExampleName,A2),
    atom_concat(B1,ExampleName,B2),
    atom_concat(B2,'_sokb.pl',SOKB),
    atom_concat(B2,'_ics.txt',ICS),
    atom_concat(A2,'_history.txt',History),
    build(SOKB,ICS,History).
build1(ExampleName,SubExample):-
    atom_concat('../examples/',ExampleName,A0),
    atom_concat(A0,'/',A1),
    atom_concat('../protocols/',ExampleName,B0),
    atom_concat(B0,'/',B1),
    atom_concat(B1,ExampleName,B2),
    atom_concat(A1,ExampleName,A2),
    atom_concat(B2,'_sokb.pl',SOKB),
    atom_concat(B2,'_ics.txt',ICS),
    atom_concat(A2,'_history_',A3),
  
    atom_concat(A3,SubExample,A4),
    atom_concat(A4,'.txt',History),
    build(SOKB,ICS,History).

build2(ExampleName):-
    atom_concat('../examples/',ExampleName,A0),
    atom_concat(A0,'/',A1),
    atom_concat('../protocols/',ExampleName,B0),
    atom_concat(B0,'/',B1),
    atom_concat(A1,ExampleName,_A2),
    atom_concat(B1,ExampleName,B2),
    atom_concat(B2,'_sokb.pl',SOKB),
    atom_concat(B2,'_ics.txt',ICS),
    build3(SOKB,ICS).
  
build3(SOKB,ICS):-
   translate_sokb(SOKB,'./sokb.pl'),
    translate_ics(ICS,'./ics.pl').
/*
 
:- dynamic(numero_soluzioni/1).
 
conta_soluzioni :-
    retractall(numero_soluzioni(X)),
    assert(numero_soluzioni(0)),
    run,
    numero_soluzioni(N),
    N1 is N + 1,
    write('trovata una soluzione ('),write(N1),write(')'),nl,
    retract(numero_soluzioni(N)),
    assert(numero_soluzioni(N1)),
    fail.
  */
run:-
    load_ics,
    current_time(0),
    society_goal,
    history,
    phase(deterministic),
    close_history.

run_no_close:-
    load_ics,
    current_time(0),
    society_goal,
    history,
    phase(deterministic).

try_number(0).
try_number(X):-
    try_number(Y),
    X is Y+1,
    max_bound(B),
    X < B.

iterative_deepening(Goal):-
    try_number(Depth),
    write('***************'), writeln(depth(Depth)),
    max_depth(Depth),
    call(Goal).

iter:-
    init_graph('proof.dot',_Stream),
    statistics(runtime,_),
    load_ics,
    iterative_deepening(
        (society_goal,
        history,
        phase(deterministic),
        once((
            write('grounding time\n'),  
            ground_time,
            write('grounding 1\n'),
            make_choice
            
            ))
        )),
    statistics(runtime,[_,Time]),
    writeln(runtime(Time)).

iter_gsciff:-
    init_graph('proof.dot',_Stream),
    statistics(runtime,_),
    load_ics,
    society_goal,
    N in 0..1000,
    indomain(N), write('************************* new happen ******************************'),
        writeln(N),
    add_history_el(N),
    phase(deterministic),
    close_history,
    write('grounding\n'),
    make_choice,
    statistics(runtime,[_,Time]),
    writeln(runtime(Time)).

iter_gsciff_hist:-
    init_graph('proof.dot',_Stream),
    statistics(runtime,_),
    load_ics,
    society_goal,
    history,
    phase(deterministic),
    N in 0..1000,
    indomain(N), write('************************* new happen ******************************'),
        writeln(N),
    add_history_el_sym(N,_),
    close_history,
    statistics(runtime,[_,Time]),
    writeln(runtime(Time)).


% Questo predicato e` per verificare la proprieta` delle aste combinatorie:
% "Esiste un insieme di bid t.c. io ottengo uno solo fra i due elementi 1 e 2?"
% E` da raffinare, questo e` solo uno sketch di come si potrebbe fare.
iter_gsciff_auction:-
    statistics(runtime,_),
    load_ics,
    society_goal,
    N in 0..1000,
    indomain(N), write('************************* new happen ******************************'),
        writeln(N),
    add_history_el(N),
    \+((
        e(deliver(_Item1)), e(deliver(_Item2))
      )),
    close_history,
    statistics(runtime,[_,Time]),
    writeln(runtime(Time)).

% Qual e` la bid che devo fare per vincere, sapendo che
% 1. voglio minimizzare la cifra che pago
% 2. ipotizzo che l'altro faccia una bid di 5
% 3. io voglio pagare al massimo 10
best_bid(Bid):-
    statistics(runtime,_),
    load_ics,
    history,
    Price in 0..10,
            existsf(Bid), existsf(Tbid),
            e(tell(i,auc,bid(i1,Bid),auction1),Tbid),
            existsf(Price), existsf(Tpay),
            e(tell(i,auc,pay(Price),auction1),Tpay),
            add_history_el(2), make_choice,
    minimize(
        (   % Metto esplicitamente il society goal, perche' altrimenti
            % non posso accedere alle variabili

            indomain(Price),
            close_history, write(Price), nl
        )
        ,Price),
    statistics(runtime,[_,Time]),
    writeln(runtime(Time)).

% Explanation in help.pl
min_viol_closed(Num,OutList):-
    max_viol(Num),
    minimize(
        (   load_ics,
            society_goal,
            history,
            close_history,
            indomain(Num),
            findall_constraints(_,OutList)
        ),Num).


% Explanation in help.pl
min_viol_open(Num,OutList):-
    max_viol(Num),
    minimize(
        (   load_ics,
            society_goal,
            history,
            indomain(Num),
            findall_constraints(_,OutList)
        ),Num).

get_ics_quantified(PSICs):-
    findall(ic(Head,Body),
        ics(Head,Body),
        ICSs),
    convert_to_psic_list(ICSs,PSICs).

convert_to_psic_list([],[]).
convert_to_psic_list([IC|ICs],[PSIC|PSICs]):-
    convert_to_psic(IC,PSIC),
    convert_to_psic_list(ICs,PSICs).

allows(Param):-
    (var(Param) -> Param = false ; true), % Default behaviour: fail if not conformant
    % load_ics, I re-write the impose_ics because I don't want to redo it for
    %           each possible history (since it is expensive), so I redo only
    %           the necessary parts
    get_ics_quantified(PSICs),
    call_list(PSICs),
    %current_time(0),
    gen_phase,  % activates rationality
    history,
    society_goal,
    phase(deterministic),
    end_gen_phase,  % deactivates rationality, backtrackable
    (Param = verbose -> nl, writeln_debug('Trying history'),
    										write_history, write_normal_abducibles,
    										write_positive_expectations,
    										write_violations,
    										write_events_not_expected
        ;   true),
    (remove_exp,
     society_goal, % reactivate ics, doing a SCIFF (no g-SCIFF) computation
     %load_ics,
     %phase(deterministic), MG: Adding this (together with remove_phase) performs
                % stronger reasoning. However, in some instances
                % it can slow down very much the computation.
                % Better perform stronger reasoning in the early stages (generate)
                % and faster in other stages. Remember that in the first stages
                % we look for all solutions (where deterministic checking is
                % useful), while in the second we look for one solution
     call_list(PSICs),
     
     close_history
     -> fail   % causes backtracking
      ; writeln_debug('AlLoWS failed: no fulfilment for possible history'),nl,
        (get_option(sciff_debug, on) -> write_history,write_normal_abducibles ; true), 
        !, Param=true % deep failure
    ).
allows(_):-
    writeln_debug('AlLoWS succeded: all possible histories compliant'),
    (get_option(allow_events_not_expected, yes)
      ->    writeln_debug('Feeble conformance proven')
      ;     writeln_debug('Strong conformance proven')
    ).


remove_exp_pending @ remove_exp \ pending(_) <=> true.
remove_exp_e  @ remove_exp \ e(_,_,_) <=> true.
remove_exp_en @ remove_exp \ en(_,_,_) <=> true.
remove_exp_fulf @ remove_exp \ fulf(_) <=> true.
remove_exp_ic @ remove_exp \ ic(_,_) <=> true.
remove_exp_psic @ remove_exp \ psic(_,_) <=> true.
%remove_exp_en @ remove_exp \ not_expected(_) <=> true.
end_remove_exp @ remove_exp <=> true.

%iter_gsciff_vickrey:-
%    statistics(runtime,_),
%    load_ics,
%    society_goal,
%    N in 0..1000,
%    indomain(N), write('************************* new happen ******************************'),
%        writeln(N),
%    add_history_el(N),
%    close_history,
%    statistics(runtime,[_,Time]),
%    writeln(runtime(Time)).

add_history_el:-
    existsf(X), existsf(T),
    h(X,T).
add_history_el:-
    existsf(X), existsf(T),
    h(X,T), writeln('************************* new happen ******************************'),
    add_history_el.

add_history_el(0):- !.
add_history_el(N):-
    existsf(X), existsf(T),
    h(X,T),
    N1 is N-1,
    add_history_el(N1).

% generates N happened events ordered in time
add_history_el_sym(0,_):- !.
add_history_el_sym(N,T1):-
    existsf(X), existsf(T),
    h(X,T), T in 0..1000, T #=< T1,
    N1 is N-1,
    add_history_el_sym(N1,T).


proof:-
    init_graph('proof.dot',_Stream),
    statistics(runtime,_),
    load_ics,
    society_goal,
    history,
    once((
        write('grounding time\n'),  
        ground_time,
        write('grounding 1\n'),
        make_choice,
        true
        )),
    statistics(runtime,[_,Time]),
    writeln(runtime(Time)).


test(Pred,Mem,Time):-
    statistics(walltime,[T1,_]),
    call_always_success(Pred),
    statistics(walltime,[T2,_]),
    Time is T2 - T1,
    statistics(memory,[Mem,_]).
  
call_always_success(Pred):-
    call(Pred),
    !.
call_always_success(_).




clp_constraint(A):-
    call(A).


:- chr_constraint
	cug/1.
cug(X) <=> ground(X)|call(X).




/*
constraints novis/0.

firstfail @
    e(X1,T1), e(X2,T2), ground_time
    ==> fd_var(T1), fd_var(T2), fd_size(T1,S1), fd_size(T2,S2), S1<S2 |
        write(size(S1)),
        indomain(T1).

novisual @ ground_time, novis,
    e(X,T) ==> fd_var(T) | T in -1000..1000, indomain(T).      

visual @ e(X,T), ground_time
    ==> fd_var(T) |  T in -1000..1000,
    fd_min(T,Min),fd_max(T,Max), write(T in Min..Max), indomain(T), nl,
write(T), novis.
*/

:- chr_constraint all_e/1, get_list_e/1.

e(F,X,T) \ all_e(L) <=> notmember(e(F,X,T),L) | all_e([e(F,X,T)|L]).

all_e(L1), get_list_e(L2) <=> L1=L2.

findall_e(L):-
    all_e([]), get_list_e(L), !.

notmember(_,[]).
notmember(A,[H|_]):- A==H, !, fail.
notmember(A,[_|T]):- notmember(A,T).

% MG 9 Feb 2008: I don't use the findall_constraints implemented in my_chr_extensions,
% because it does not retain the variables (it is due to the implementation of
% findall in SICStus)
ground_time:- 
    findall_e(L),
    get_time(L,LT),
    solver_search(LT).

get_time([],[]).
get_time([e(_,_,T)|R],[T|RT]):-
    add_default_domain(T),
    get_time(R,RT).
    


build_and_run(SOKB, ICS, History, noclose) :-
	build(SOKB, ICS, History),
	consult(sokb),
	use_module(history),
	use_module(ics),
	run_no_close.

build_and_run(SOKB, ICS, History, close) :-
	build(SOKB, ICS, History),
	consult(sokb),
	use_module(history),
	use_module(ics),
	run.

project(Project):-
   default_dir(Dir),
   atom_concat(Dir,Project,Path),
   atom_concat(Path,'/',PathSlash),
   retractall(prjDir(_)),
   assert(prjDir(PathSlash)),
   atom_concat(PathSlash,'project.pl',PrjFile),
   compile(PrjFile),
   build_prj(PathSlash).

append_path(_,[],[]):- !.
% If the file is actually a URL, do not change it
append_path(Path,[File|Rest],[FullPath|Rest1]):-
    atom_concat('http://',_,File),!,FullPath=File,
    append_path(Path,Rest,Rest1).
append_path(Path,[File|Rest],[FullPath|Rest1]):-
    atom_concat(Path,File,FullPath),
    append_path(Path,Rest,Rest1).

convert_sokb([],_).
convert_sokb([SOKB|Rest],File):-
    write_debug('Parsing file '), write_debug(SOKB),
    translate_sokb(SOKB,File,write), % for the 1st, write, the others are in append
    writeln_debug(' --> OK'),
    convert_sokb1(Rest,File).
convert_sokb1([],_).
convert_sokb1([SOKB|Rest],File):-
    write_debug('Parsing file '), write_debug(SOKB),
    translate_sokb(SOKB,File,append),
    writeln_debug(' --> OK'),
    convert_sokb1(Rest,File).
    
/*
build(SOKB,ICS,History):-
   translate_sokb(SOKB,'./sokb.pl'),
    translate_ics(ICS,'./ics.pl'),
    translate_history(History,'./history.pl').

build(ExampleName):-
    atom_concat(ExampleName,'/',A1),
    atom_concat(A1,ExampleName,A2),
    atom_concat(A2,'_sokb.pl',SOKB),
    atom_concat(A2,'_ics.txt',ICS),
    atom_concat(A2,'_history.txt',History),
    build(SOKB,ICS,History).
*/

	
