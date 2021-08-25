%
%  epath.pl:  create and manipulate epistemic path terms.
%
%  Copyright 2008, Ryan Kelly
%
%  Syntax for epistemic paths is:
%
%    A           - primitive agent name
%    E1 ; E2     - sequence
%    E1 | E2     - choice
%    ?(C)        - test
%    E*          - iteration
%    !(X:T)      - nondet variable rebind with given type
%    -[X:V|..]   - variable assignment
%

%
%  epath_not_refuted_values(E,VVIn,VVOut)  -  determine possible var values
%
%  If path E is entered with variable bindings from the set VVIn, then
%  it will terminate with bindings from the set VVOut.
%  Each of these is a list mapping vars to a list of possible values, e.g.
%  [X:[a,b,c], Y:[f,g]].  Vars not in the mapping are allowed to take on
%  any value.
%
%  Since this is a propositional domain, we can handle the star operator
%  as a simple fixpoint calculation and be sure it will eventually terminate.
%

epath_not_refuted_values(A,VV,VV) :-
    agent(A).
epath_not_refuted_values(?(P),VVIn,VVOut) :-
    epath_not_refuted_values_test(VVIn,[],P,VVOut),
    vv_valid(VVOut).
epath_not_refuted_values(!(X:T),VVIn,VVOut) :-
    bagof(Val,call(T,Val),Vals),
    vv_update(VVIn,X,Vals,VVOut).
epath_not_refuted_values(-[],VVIn,VVIn).
epath_not_refuted_values(-[X:V|Xs],VVIn,VVOut) :-
    vv_update(VVIn,X,[V],VV2),
    epath_not_refuted_values(-Xs,VV2,VVOut).
epath_not_refuted_values(E1 ; E2,VVIn,VVOut) :-
    epath_not_refuted_values(E1,VVIn,VV2),
    epath_not_refuted_values(E2,VV2,VVOut).
epath_not_refuted_values(E1 | E2,VVIn,VVOut) :-
    ( epath_not_refuted_values(E1,VVIn,VV1) ->
        ( epath_not_refuted_values(E2,VVIn,VV2) ->
            vv_merge(VV1,VV2,VVOut)
        ;
            VVOut = VV1
        )
    ;
        epath_not_refuted_values(E2,VVIn,VVOut)
    ).
epath_not_refuted_values(E*,VVIn,VVOut) :-
    epath_not_refuted_values(E,VVIn,VV2),
    ( VV2 = VVIn ->
        VVOut = VV2
    ;
        epath_not_refuted_values(E*,VV2,VVOut)
    ).


epath_not_refuted_values_test([],_,_,[]).
epath_not_refuted_values_test([X:Vs|Xs],Sofar,P,VVOut) :-
    partition(epath_not_refuted_values_allowed(P,X,Xs,Sofar),Vs,Vs1,_),
    VVOut = [X:Vs1|VVOut2],
    Sofar2 = [X:Vs1|Sofar],
    epath_not_refuted_values_test(Xs,Sofar2,P,VVOut2).

epath_not_refuted_values_allowed(P,X,Others1,Others2,V) :-
    epath_not_refuted_values_allowed_sub1(P,Others1,P1),
    epath_not_refuted_values_allowed_sub1(P1,Others2,P2),
    subs(X,V,P2,P3),
    simplify(P3,P4),
    P4 \= false.

epath_not_refuted_values_allowed_sub1(P,[],P).
epath_not_refuted_values_allowed_sub1(P,[X:Vs|Xs],P2) :-
    member(Val,Vs),
    subs(X,Val,P,P1),
    epath_not_refuted_values_allowed_sub1(P1,Xs,P2).
    

vv_valid([]).
vv_valid([_:Vs|Xs]) :-
    Vs \= [],
    vv_valid(Xs).

vv_update([],X,Vs,[X:Vs]).
vv_update([X1:Vs1|Xs],X2,Vs2,Res) :-
    ( X1 == X2 ->
        Res = [X1:Vs2|Xs]
    ;
        Res = [X1:Vs1|Res2],
        vv_update(Xs,X2,Vs2,Res2)
    ).

vv_merge([],VV2,VV2).
vv_merge([X:Vs|Xs],VV1,Res) :-
    vv_merge1(VV1,X,Vs,VV2),
    vv_merge(Xs,VV2,Res).

vv_merge1([],X,Vs,[X:Vs]).
vv_merge1([X1:Vs1|Xs],X2,Vs2,Res) :-
    ( X1 == X2 ->
        union(Vs1,Vs2,VsU),
        Res = [X1:VsU|Xs]
    ;
        Res = [X1:Vs1|Res2],
        vv_merge1(Xs,X2,Vs2,Res2)
    ).



%
%  epath_enum_vars(E,En)  -  enumerate all possible values of each path 
%                            variable in the epath, reducing it from FODL
%                            to VPDL and hence making it decidable
%
%  We expand any unions produced during enumeration over sequence operators,
%  in the hope that each branch will simplify given the new variable bindings.
%  We try to push var assignments as far to the right as possible, doing subs
%  into tests and simplifying.
%

epath_enum_vars(E1 ; E2,En) :-
    epath_enum_vars(E1,En1),
    flatten_op('|',[En1],Ens1),
    epath_enum_vars(E2,En2),
    flatten_op('|',[En2],Ens2),
    epath_enum_vars_branches(Ens1,Ens2,Ens),
    epath_build('|',Ens,En).
epath_enum_vars(E1 | E2,En) :-
    flatten_op('|',[E1,E2],Es),
    maplist(epath_enum_vars,Es,Ens),
    epath_build('|',Ens,En).
epath_enum_vars(E*,En) :-
    epath_enum_vars(E,EnS),
    epath_build('*',EnS,En).
epath_enum_vars(?(P),?(P)).
epath_enum_vars(-VA,-VA).
epath_enum_vars(!(X:T),En) :-
    bagof(VA,V^(call(T,V),VA=(-[X:V])),VAs),
    epath_build('|',VAs,En).
epath_enum_vars(A,A) :-
    agent(A).
    

epath_enum_vars_branches([],_,[]).
epath_enum_vars_branches([B|Bs],Es,[R|Rs]) :-
    ( epath_ends_with_assign(B,VA,Head) ->
        epath_enum_vars_branches_assign(Es,VA,Head,[],R)
    ;
        epath_enum_vars_branches_noassign(Es,B,[],R)
    ),
    epath_enum_vars_branches(Bs,Es,Rs).


epath_enum_vars_branches_assign([],_,_,Acc,R) :-
    joinlist('|',Acc,R).
epath_enum_vars_branches_assign([E|Es],VA,B,Acc,R) :-
    epath_push_assign(E,VA,Ea),
    epath_build(';',[B,Ea],R1),
    epath_enum_vars_branches_assign(Es,VA,B,[R1|Acc],R).

epath_enum_vars_branches_noassign([],_,Acc,R) :-
    joinlist('|',Acc,R).
epath_enum_vars_branches_noassign([E|Es],B,Acc,R) :-
    epath_build(';',[B,E],R1),
    epath_enum_vars_branches_noassign(Es,B,[R1|Acc],R).
 
%
%  epath_ends_with_assign(E,VA,Head)  -  epath ends with a variable assignment
%
%  This predicate is true when E ends with a unique variable assignment
%  operator.  VA is bound to the assignment and Head is the remainder
%  of the path.
%
epath_ends_with_assign(E1 ; E2,VA,Head) :-
    epath_ends_with_assign(E2,VA2,Head2),
    ( Head2 = (?true) ->
        ( epath_ends_with_assign(E1,VA1,Head1) ->
            Head = Head1, vassign_merge(VA1,VA2,VA)
        ;
            Head = E1, VA=VA2
        )
    ;
        Head = (E1 ; Head2), VA=VA2
    ).
epath_ends_with_assign(E1 | E2,VA,Head) :-
    epath_ends_with_assign(E1,VA,Head1),
    epath_ends_with_assign(E2,VA2,Head2),
    VA == VA2,
    Head = (Head1 | Head2).
epath_ends_with_assign(-(VA),VA,?true).


%
%  epath_push_assign(E,VA,Ep)  -  push a variable assignment as far to the
%                                 right as possible.
%
%  This may involve, for example, pushing it over a test operator and
%  substituting the assigned values into the test formula.
%
epath_push_assign(E1 ; E2,VA,Ep) :-
    epath_push_assign(E1,VA,Ep1),
    ( epath_ends_with_assign(Ep1,VA2,Head) ->
        epath_push_assign(E2,VA2,Ep2),
        epath_build(';',[Head,Ep2],Ep)
    ;
        epath_build(';',[Ep1,E2],Ep)
    ).
epath_push_assign(E1 | E2,VA,Ep) :-
    epath_push_assign(E1,VA,Ep1),
    epath_push_assign(E2,VA,Ep2),
    epath_build('|',[Ep1,Ep2],Ep).
epath_push_assign(E*,VA,(-VA) ; (E*)).
epath_push_assign(!(X:T),VA,Ep) :-
    ( vassign_contains(VA,X) ->
        Ep = (-VA ; !(X:T))
    ;
        Ep = (!(X:T) ; -VA)
    ).
epath_push_assign(-VA2,VA,-VAm) :-
    vassign_merge(VA2,VA,VAm).
epath_push_assign(?(P),VA,?(Q) ; -VA) :-
    vassign_subs(VA,P,Q1),
    simplify(Q1,Q).
epath_push_assign(A,VA,A ; -VA) :-
    agent(A).


%
%  Predicates for manipulating a variable assignment list
%
vassign_merge([],VA,VA).
vassign_merge([(X:V)|Xs],VA2,VA) :-
    vassign_insert(X,V,VA2,VAt),
    vassign_merge(Xs,VAt,VA).

vassign_insert(X,V,[],[X:V]).
vassign_insert(X,V,[(X2:V2)|Xs],VA) :-
    ( X == X2 ->
        VA = [(X2:V2)|Xs]
    ;
        vassign_insert(X,V,Xs,VAs),
        VA = [(X2:V2)|VAs]
    ).

vassign_contains([(X:_)|Xs],Y) :-
    X == Y ; vassign_contains(Xs,Y).

vassign_subs([],P,P).
vassign_subs([(X:V)|Xs],P,Q) :-
    subs(X,V,P,P2),
    vassign_subs(Xs,P2,Q).


%
%  epath_vars(E,Vars)  -  find all path variables (as opposed to formula-level
%                         variables) in the given epistemic path.
%
epath_vars(E1 ; E2,Vars) :-
    epath_vars(E1,Vars1),
    epath_vars(E2,Vars2),
    epath_vars_union(Vars1,Vars2,Vars), !.
epath_vars(E1 | E2,Vars) :-
    epath_vars(E1,Vars1),
    epath_vars(E2,Vars2),
    epath_vars_union(Vars1,Vars2,Vars), !.
epath_vars(E*,Vars) :-
    epath_vars(E,Vars), !.
epath_vars(!(X:T),[X:T]) :- !.
epath_vars(?(_),[]) :- !.
epath_vars(-VA,VA) :- !.
epath_vars(A,[]) :-
    agent(A).

epath_vars_union([],Vars,Vars).
epath_vars_union([X:T|Vars1],Vars2,Vars) :-
    (ismember(X:T,Vars2) ->
        epath_vars_union(Vars1,Vars2,Vars)
    ;
        Vars = [X:T|VarsT],
        epath_vars_union(Vars1,Vars2,VarsT)
    ).

%
%  epath_build(Op,Args,EPath)  -  build an epath, with simplification
%
%  This predicate builds an epath, applying simplifications appropriate
%  to the top-level operator but assuming all argument paths are already
%  simplified.
%

epath_build('|',Es,E) :-
    flatten_op('|',Es,Es0),
    partition('='(?false),Es0,_,Es1),
    simplify_epath_choice_subsumes(Es1,Es2),
    simplify_epath_choice_union(Es2,Es3),
    ( Es3 = [] ->
        E = (?false)
    ;
        joinlist('|',Es3,E)
    ).

epath_build(';',Es,E) :-
    flatten_op(';',Es,Es0),
    ( member(?false,Es0) -> 
        E = (?false)
    ;
        partition('='(?true),Es0,_,Es1),
        ( Es1 = [] ->
            E = (?true)
        ;
            simplify_epath_seq_combine(Es1,Ss),
            ( member(?false,Ss) ->
                E = (?false)
            ;
                joinlist(';',Ss,E)
            )
        )
    ).

epath_build('*',E,Eb) :-
    simplify_star_contents(E,E1),
    ( E1 = (?(P)) ->
        Eb = (?(P))
    ;
        Eb = (E1*)
    ).


%
%  simplify_epath  -  simplify an epistemic path
%
%  we can do this by recursively stripping off the outermost operator,
%  simplifying the argument paths, then apply epath_build.
%
simplify_epath(X,_) :-
    var(X), !, throw(cant_simplify_a_free_epath).
simplify_epath(A,A) :-
    agent(A).
simplify_epath(E1 ; E2,Es) :-
    flatten_op(';',[E1,E2],Eseq),
    maplist(simplify_epath,Eseq,Esimp),
    epath_build(';',Esimp,Es).
simplify_epath(E1 | E2,Es) :-
    flatten_op('|',[E1,E2],Eseq),
    maplist(simplify_epath,Eseq,Esimp),
    epath_build('|',Esimp,Es).
simplify_epath(E*,Es) :-
    simplify_epath(E,E1s),
    epath_build('*',E1s,Es).
simplify_epath(?(P),?(S)) :-
    simplify(P,S).
simplify_epath(!(X:T),!(X:T)).



epath_elim_impossible_branches(A,_,A) :-
    agent(A).
epath_elim_impossible_branches(?(P),VVPoss,?(P1)) :-
    ( epath_not_refuted_values(?(P),VVPoss,_) ->
        P1 = P
    ;
        P1 = false
    ).
epath_elim_impossible_branches(!(X:T),_,!(X:T)).
epath_elim_impossible_branches(-VA,_,-VA).
epath_elim_impossible_branches(E1 ; E2,VVPoss,Er) :-
    ( epath_not_refuted_values(E1,VVPoss,VV2) ->
        epath_elim_impossible_branches(E1,VVPoss,Er1),
        epath_elim_impossible_branches(E2,VV2,Er2),
        (Er1 = ?false ->
            Er = ?false
        ; Er2 = ?false ->
            Er = ?false
        ;
            Er = (Er1 ; Er2)
        )
    ;
        Er = (?false)
    ).
epath_elim_impossible_branches(E1 | E2,VVPoss,Er) :-
    epath_elim_impossible_branches(E1,VVPoss,Er1),
    epath_elim_impossible_branches(E2,VVPoss,Er2),
    (Er1 = ?false ->
       Er = Er2
    ; Er2 = ?false ->
       Er = Er1
    ;
       Er = (Er1 | Er2)
    ).
epath_elim_impossible_branches(E*,VVPoss,Er) :-
    ( epath_not_refuted_values(E*,VVPoss,VV2) ->
        epath_elim_impossible_branches(E,VV2,E2),
        (E2 = ?false ->
            Er = ?false
        ;
            Er = (E2*)
        )
    ;
        Er = (?false)
    ).

%
%  Simplification for operations within a star.
%
simplify_star_contents(E1,E2) :-
    ( simplify_star_contents1(E1,Es) ->
        simplify_star_contents(Es,E2)
    ;
        E2 = E1
    ).

simplify_star_contents1(E*,E).

% Any choices within a star that are simply ?true can be removed,
% as we always have the option of staying in current state.
simplify_star_contents1(E1 | E2,Ep) :-
    flatten_op('|',[E1,E2],Es),
    partition('='(?true),Es,Ts,Es2),
    length(Ts,N), N > 0,
    joinlist('|',Es2,Ep).
%
%  Flatten stars in (B1* | (B2* | B3*)*)* 
simplify_star_contents1(E,Ep) :-
    ( E = ((B1*) | (((B2*) | (B3*))*)) ; E = ((((B1*) | (B2*))*) | (B3*)) ) ->
    Ep = ((B1*) | (B2*) | (B3*)).

%
%  Remove choices that are subsumed by repetition of another branch
simplify_star_contents1(E1 | E2,Ep) :-
    flatten_op('|',[E1,E2],Es),
    simplify_epath_star_subsumes(Es,Ss),
    joinlist('|',Ss,Ep).

simplify_epath_star_subsumes(Es,Ss) :-
    simplify_epath_star_subsumes(Es,[],0,Ss).
 
simplify_epath_star_subsumes([],Acc,1,Acc).
simplify_epath_star_subsumes([E|Es],Acc,HaveSimpd,Ss) :-
    ( member(E2,Acc), epath_subsumes(E2*,E) ->
        simplify_epath_star_subsumes(Es,Acc,1,Ss)
    ;
        partition(epath_subsumes(E*),Acc,Rem,Acc2),
        ( Rem = [] -> NewHaveSimpd = HaveSimpd ; NewHaveSimpd = 1 ),
        simplify_epath_star_subsumes(Es,[E|Acc2],NewHaveSimpd,Ss)
    ).



%
%  simplify branches in a choice by removing any subsumed by another branch
%
simplify_epath_choice_subsumes(Es,Ss) :-
    simplify_epath_choice_subsumes(Es,[],Ss).
 
simplify_epath_choice_subsumes([],Acc,Acc).
simplify_epath_choice_subsumes([E|Es],Acc,Ss) :-
    ( member(E2,Acc), epath_subsumes(E2,E) ->
        simplify_epath_choice_subsumes(Es,Acc,Ss)
    ;
        partition(epath_subsumes(E),Acc,_,Acc2),
        simplify_epath_choice_subsumes(Es,[E|Acc2],Ss)
    ).

%
%  simplify branches in a choice by combining two branches into a single,
%  simpler branch giving their union.
%
simplify_epath_choice_union(Es,Ss) :-
    simplify_epath_choice_union(Es,[],Ss).

simplify_epath_choice_union([],Acc,Acc).
simplify_epath_choice_union([E|Es],Acc,Ss) :-
    ( (select(E1,Acc,Rest), simplify_epath_union(E,E1,Eu)) ->
        simplify_epath_choice_union([Eu|Es],Rest,Ss)
    ;
        simplify_epath_choice_union(Es,[E|Acc],Ss)
    ).


%
%  simplify_epath_seq_combine(Es,Ss)  -  simplify sequence of paths by
%                                        combining adjacent ones.
%
simplify_epath_seq_combine(Es,Ss) :-
    simplify_epath_seq_combine(Es,[],Ss).

simplify_epath_seq_combine([E],Acc,Ss) :-
    reverse([E|Acc],Ss).
simplify_epath_seq_combine([E|Es],Acc,Ss) :-
    ( simplify_epath_combine([E|Es],Es2) ->
        simplify_epath_seq_combine_recheck(Es2,Acc,Ss)
    ;
      simplify_epath_seq_combine(Es,[E|Acc],Ss)
    ).

:- index(simplify_eapth_seq_combine_recheck(0,1,0)).

simplify_epath_seq_combine_recheck(Es,[],Ss) :-
    simplify_epath_seq_combine(Es,[],Ss).
simplify_epath_seq_combine_recheck(Es,[A|Acc],Ss) :-
    ( simplify_epath_combine([A|Es],Es2) ->
        simplify_epath_seq_combine_recheck(Es2,Acc,Ss)
    ;
      simplify_epath_seq_combine(Es,[A|Acc],Ss)
    ).

%
%  epath_subsumes(E1,E2)  -  detect common cases where one epath completely
%                            subsumes another.  That is, all worlds reachable
%                            by path E2 are also reachable by path E1.
%
%  epath_subsumes/2 is det, which we ensure using cuts
%
epath_subsumes(E,E) :- !.
epath_subsumes(E*,E1*) :-
    epath_subsumes(E*,E1), !.
epath_subsumes(E*,E1) :-
    epath_subsumes(E,E1), !.
epath_subsumes(E*,E1) :-
    epath_seq_split(E1,[P1,P2],[]),
    epath_subsumes(E*,P1),
    epath_subsumes(E*,P2), !.
epath_subsumes(E,E1 | E2) :-
    epath_subsumes(E,E1),
    epath_subsumes(E,E2), !.
epath_subsumes(E1 | E2,E) :-
    (epath_subsumes(E1,E) ; epath_subsumes(E2,E)), !.
epath_subsumes(E1 ; E2,E) :-
    epath_seq_split(E,[P1,P2],[]),
    epath_subsumes(E1,P1),
    epath_subsumes(E2,P2), !.

%
%  simplify_epath_union(E1,E2,Eu)  -  simplify E1 and E2 into their union
%                                     (E1 | E2) <=> Eu
%
%  simplify_epath_combine(Es,Esc)  -    simplify E1;E2;P into Ec;P
%
%  This basically allows us to special-case a number of common forms.
%
simplify_epath_union(E1,E2,Eu) :-
    simplify_epath_union1(E1,E2,Eu)
    ;
    simplify_epath_union1(E2,E1,Eu).

%  P1 | (P1 ; P2* ; P2)   =>   P1 ; P2*
simplify_epath_union1(E1,E2,Eu) :-
    P1 = E1,
    epath_seq_split(E2,[P1,P2*,P2],[]),
    epath_build('*',P2,P2S),
    epath_build(';',[P1,P2S],Eu).
%  P1 | (P2 ; P2* ; P1)   =>   P2* ; P1
simplify_epath_union1(E1,E2,Eu) :-
    P1 = E1,
    epath_seq_split(E2,[P2,P2*,P1],[]),
    epath_build('*',P2,P2S),
    epath_build(';',[P2S,P1],Eu).
%  P1 | (P2* ; P2 ; P1)   =>   P2* ; P1
simplify_epath_union1(E1,E2,Eu) :-
    P1 = E1,
    epath_seq_split(E2,[P2*,P2,P1],[]),
    epath_build('*',P2,P2S),
    epath_build(';',[P2S,P1],Eu).
% ?P1 | ?P2   =>   ?(P1 | P2)
simplify_epath_union1(?P1,?P2,?Pu) :-
    fml2cnf(P1 | P2,Pu1),
    simplify(Pu1,Pu).


% P1* ; (P2 ; (P1*))*   =>   (P1* | P2*)*
simplify_epath_combine(E,[Ec|Rest]) :-
    epath_seq_split(E,[P1*,Pr*],Rest),
    epath_seq_split(Pr,[P2,P1*],[]),
    epath_build('|',[P1*,P2*],Ec1),
    epath_build('*',Ec1,Ec).
% (P1* ; P2)* ; P1*   =>   (P1* | P2*)*
simplify_epath_combine(E,[Ec|Rest]) :-
    epath_seq_split(E,[Pr*,P1*],Rest),
    epath_seq_split(Pr,[P1*,P2],[]),
    epath_build('|',[P1,P2],Ec1),
    epath_build('*',Ec1,Ec).
% (P1* ; P2)* ; P1 ; P1*   =>   (P1* | P2*)*
simplify_epath_combine(E,[Ec|Rest]) :-
    epath_seq_split(E,[Pr*,P1,P1*],Rest),
    epath_seq_split(Pr,[P1*,P2],[]),
    epath_build('|',[P1,P2],Ec1),
    epath_build('*',Ec1,Ec).
% P1* ; P2*   =>   P1*   if P1 > P2
simplify_epath_combine(E,[Ec|Rest]) :-
    epath_seq_split(E,[P1*,P2*],Rest),
    ( epath_subsumes(P1,P2), Ec = P1
    ; epath_subsumes(P2,P1), Ec = P2
    ).
% ?P1 ; ?P2   =>   ?(P1 & P2)
simplify_epath_combine(E,[?(Pc)|Rest]) :-
    epath_seq_split(E,[?P1,?P2],Rest),
    fml2cnf(P1 & P2,Pc1),
    simplify(Pc1,Pc).

%
%  epath_seq_split(E,Seqs,Rest)  -  nondeterminstically split sequence of ops
%
%  This predicate nondeterministically splits a series of sequence operators
%  into patterns that match the elements of the list Seqs.  Each free var in
%  Seqs may be given one or more elements from the sequence, while each
%  any entries in Seq that are nonvar will be unified with precisely one
%  element.
%
:- index(epath_seq_split(1,1,0)).

epath_seq_split(E1 ; E2,Seqs,Rest) :-
    flatten_op(';',[E1,E2],Es),
    epath_seq_split_prep(Seqs,Preps),
    epath_seq_split(Es,Preps,Rest),
    epath_seq_split_unprep(Preps,Seqs).

epath_seq_split([E|Es],Seqs,Rest) :-
    epath_seq_split([E|Es],[],Seqs,Rest).

epath_seq_split(Rest,[],[],Rest).
epath_seq_split(Rest,[S-Vs|Todo],[],Rest) :-
    reverse(Vs,VsR),
    ( var(S) ->
        S = VsR
    ;
        joinlist(';',VsR,S)
    ),
    epath_seq_split(Rest,Todo,[],Rest).
epath_seq_split([E|Es],Todo,[S|Seqs],Rest) :-
    ( var(S) ->
        ((Todo = [S2-Vs|Todo2], S2 == S) ->
            (epath_seq_split([E|Es],Todo,Seqs,Rest)
            ;
            epath_seq_split(Es,[S-[E|Vs]|Todo2],[S|Seqs],Rest))
        ;
            epath_seq_split(Es,[S-[E]|Todo],[S|Seqs],Rest)
        )
    ;
      epath_seq_split_unify(S,[E|Es],Es2), epath_seq_split(Es2,Todo,Seqs,Rest)
    ).
epath_seq_split([],Todo,[S],Rest) :-
    var(S), Todo = [S2-_|_], S2 == S, 
    epath_seq_split([],Todo,[],Rest).


epath_seq_split_unify(P,[E|Es],Es) :-
    var(P), P=E, !.
epath_seq_split_unify(P1 ; P2,Es,Es2) :-
    epath_seq_split_unify(P1,Es,Es1),
    epath_seq_split_unify(P2,Es1,Es2), !.
epath_seq_split_unify(E,[E|Es],Es).

epath_seq_split_prep([],[]).
epath_seq_split_prep([S|Seqs],[P|Preps]) :-
    (var(S) -> true ; S=P ),
    epath_seq_split_prep(Seqs,Preps).
epath_seq_split_unprep([],[]).
epath_seq_split_unprep([P|Preps],[S|Seqs]) :-
    ( P = [_|_] -> joinlist(';',P,S) ; P=S ),
    epath_seq_split_unprep(Preps,Seqs).

    

%
%  copy_epath(EIn,EOut)  -  copy an epath, renaming path variables
%
%  This produces a copy of EIn with all path variables replaced by
%  fresh variables.  All free formula variables remain unchanged, while
%  all bound formula variables are also renamed.
%
copy_epath(E,Ec) :-
    epath_vars(E,EVarsT),
    maplist(arg(1),EVarsT,EVars),
    term_variables(E,TVars),
    vdelete_list(TVars,EVars,FVars),
    copy_term(E^FVars,E2^FVars2),
    FVars2=FVars,
    copy_epath_fmls(E2,Ec).

copy_epath_fmls(E1 ; E2,E1c ; E2c) :-
    copy_epath_fmls(E1,E1c),
    copy_epath_fmls(E2,E2c).
copy_epath_fmls(E1 | E2,E1c | E2c) :-
    copy_epath_fmls(E1,E1c),
    copy_epath_fmls(E2,E2c).
copy_epath_fmls(E*,Ec*) :-
    copy_epath_fmls(E,Ec).
copy_epath_fmls(?(P),?(Pc)) :-
    copy_fml(P,Pc).
copy_epath_fmls(!(V:T),!(V:T)).
copy_epath_fmls(-VA,-VA).
copy_epath_fmls(A,A) :-
    agent(A).


%
%  pp_epath(E)  -  pretty-print an epistemic path
%

pp_epath(E) :-
    pp_epath(E,0,0).

pp_epath_list([E],_,_,O1,D1) :-
    pp_epath(E,O1,D1).
pp_epath_list([E1,E2|Es],Op,D,O1,D1) :-
    pp_epath(E1,O1,D1), nl,
    pp_inset(D), write(Op), nl,
    pp_epath_list([E2|Es],Op,D,D1,D1).


pp_epath(E1 ; E2,O,D) :-
    flatten_op(';',[E1,E2],Es),
    D1 is D + 1,
    O1 is O + 1,
    pp_epath_list(Es,';',D,O1,D1).
pp_epath(E1 | E2,O,D) :-
    flatten_op('|',[E1,E2],Es),
    D1 is D + 1,
    O1 is O + 1,
    pp_epath_list(Es,'|',D,O1,D1).
pp_epath(?(P),O,D) :-
    D1 is D + 1,
    pp_inset(O), write('?  '),
    pp_fml(P,0,D1).
pp_epath(!(V:T),O,_) :-
    pp_inset(O), write('!  '), write(V:T).
pp_epath(-VA,O,_) :-
    pp_inset(O), write('-  '), pp_epath_assign(VA).
pp_epath(E*,O,D) :-
    D1 is D + 1,
    pp_inset(O), write('*'), nl,
    pp_epath(E,D1,D1).
pp_epath(A,O,_) :-
    agent(A),
    pp_inset(O), write(A).


pp_epath_assign([]).
pp_epath_assign([(X:V)|Xs]) :-
    write(X), write(' <= '), write(V), write(',  '),
    pp_epath_assign(Xs).


