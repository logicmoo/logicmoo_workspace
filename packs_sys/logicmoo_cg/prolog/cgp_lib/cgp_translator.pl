/*

This file translates CG from CGPro form to FOL .
Different types of referents that the 
Translator handles are incrementally added to the system. 
Allowed types of referents:
- generic [PERSON: *] <-> exists(x,person(x))
- named concept [PERSON: John] <-> exists(x,person(x)&name(x,'John')&word('John'))
- universally quantified concept [PERSON: every] <-> all(Y,person(Y)-> ...)
- collective sets of named concepts [PERSON: {John,Sally}]

*/


:- dynamic(kb_index/1).

:- op(850,fx,~).    % negation
:- op(900,xfy,'#').   % disjunction
:- op(900,xfy,&).   % conjunction
:- op(950,xfy,->).  % implication



:- dynamic seed1/1.

/* This predicate returns the concatenation of two atoms as an atom */
    
a_concat(S1,S2,S):-
    name(S1,L1),name(S2,L2),append(L1,L2,L),
    name(S,L).
    
    
seed1(0).

concat1(S1,S2,S):-
    name(S1,L1),name(S2,L2),append(L1,L2,L),
    name(S,L).


translate_ind(ind(_A,Name,Ind),S):- S=..[Name,Ind].

% translate_pred_id(Id,F):-
% cg(Id,A,B,C),translate_cgraph(cg(Id,A,B,C),F).
translate_cgraph(Id,Formula):- var(Id),!, isCG(Id),translate_cgraph(Id,Formula).
translate_cgraph(Id,Formula):- number(Id),!,cg(Id,Rels,A,B),translate_cgraph(cg(Id,Rels,A,B),Formula).
translate_cgraph(cg(Id,Rels,A,B),Formula):-
    expand_names(cg(Id,Rels,A,B),cg(Id,Rels1,A,B)),
    translate_cg(cg(Id,Rels1,A,B),Formula).

translate_cg(cg(_,Rels,_,_),Formula):-
    get_concepts_pred(Rels,CIDs),
    assign_varsn_pred(CIDs,ListIdsConcs),
    make_formula(Rels,ListIdsConcs,FormulaBody),
    make_cg_prefix1(ListIdsConcs,Formula, FormulaBody),guess_varnames(Formula).

expand_names(cg(Id,Rels,A,B),cg(Id,Rels1,A,B)):-
    get_named_concepts(Rels,CIDs),
    add_relations(CIDs,Relations),
    append(Rels,Relations,Rels1).

add_relations([],[]).
add_relations([Id|Next],[Rel|Next1]):-
    cgc(Id,_,_,Ref,_),ground(Ref),
    member(fs(name,Name),Ref),
    kb_index(X),
    X1 is X+1,
    assert(ind(X1,word,Name)),
    retract(kb_index(X)),
    assert(kb_index(X1)),
    Rel=cgr(name,[Id,X1],_),
    add_relations(Next,Next1).



get_named_concepts([],[]).

get_named_concepts([cgr(_,Ids,_)|Next],List) :- 
    get_named_concepts(Next,List1),
    get_n_concepts(Ids,Ids1),
    append3_pred(Ids1,List1,List).

get_n_concepts([],[]).

get_n_concepts([Id|Next],[Id|Next1]):-
    cgc(Id,simple,_,Ref,_),ground(Ref),
    member(fs(name,_),Ref), 
    \+ member(fs(num,pl),Ref),!,
    get_n_concepts(Next,Next1). 	

get_n_concepts([_|Next],Next1):-
    get_n_concepts(Next,Next1).

get_concepts_pred([],[]).

get_concepts_pred([cgr(_,Ids,_)|Next],List) :- 
    get_concepts_pred(Next,List1),
    append3_pred(Ids,List1,List).
    
assign_vars([],[]).
assign_vars([Id|Next],[Id-_Var|NextVars]):- 
    cgc(Id,simple,_,_,_),!,
    assign_vars(Next,NextVars).

assign_vars([Id|Next],[ind-Id|NextVars]):-
    ind(Id,_,_),
    assign_vars(Next,NextVars).


assign_vars([Id|Next],[cg-Id|NextVars]):-
    cgc(Id,situation,_,_,_),
    assign_vars(Next,NextVars).
assign_varsn_pred([],[]).
assign_varsn_pred([Id|Next],[Id-Var|NextVars]):- 
    cgc(Id,_,_,_,_),!,next_var_pred(Var),
    assign_varsn_pred(Next,NextVars).
assign_varsn_pred([Id|Next],[_Var-Id|NextVars]):-
    assign_varsn_pred(Next,NextVars).


next_var_pred(_):- !.
next_var_pred(?(Var)):- retract(seed1(N)),
    N1 is N+1,
    assert(seed1(N1)),
    concat1('a',N,Var).
    



append3_pred([],L,L).
append3_pred([H|T],L,Lresult):-member(H,L),!,
    append3_pred(T,L,Lresult).

append3_pred([H|T],L,[H|Lresult]):- append3_pred(T,L,Lresult).


make_cg_prefix1([] ,F,F).
make_cg_prefix1([Id-_Var|Next],F,F1):- (Id=ind;Id=cg),!, make_cg_prefix1(Next,F,F1).
make_cg_prefix1([Id-Var|Next],F,F1):-  cgc(Id,simple,Name,Refs,_),ground(Refs),
    member(fs(quant,every),Refs),
    \+ member(fs(number,pl),Refs),!,
    T=..[Name,Var],
    F=..[all,Var,F2],
    F2=..[->, T ,F3],
    make_cg_prefix1(Next,F3,F1).

make_cg_prefix1([_Id-Var|Next],F,F1):-  F2=..[exists,Var,F1], 
    make_cg_prefix1(Next,F,F2).


make_formula([cgr(Rname,Ids,_)|OtherRels],ListIdsConcs,F):- 
    make_formula(OtherRels,ListIdsConcs,F1),!,
    construct_term(Rname,Ids,ListIdsConcs,Term),
    ( F1=[] -> F=Term;
    F=.. [&, Term, F1]).

make_formula([],[Id- _Var|Next],F):-
    make_formula([],Next,F1),
    cgc(Id,_,_Name,Refs,_),
    sing_every(Refs),!,
    F=F1.

make_formula([],[Id-Var|Next],F):-
    make_formula([],Next,F1),
    cgc(Id,_,Name,_,_),!,
    construct_term(Name,[Id],[Id-Var],Term),
    ( F1=[] -> F=Term;
    F=.. [&, Term, F1]).

make_formula([],[_Var-Id|Next],F):-
    make_formula([],Next,F1),
    ind(Id,Type,Ind),!,
    Term=..[Type,Ind],
    ( F1=[] -> F=Term;
    F=.. [&, Term, F1]).


make_formula([],[_Var-Id|Next],F):-
    cgc(Id,situation,_,[_GID],_),
    make_formula([],Next,F).



make_formula([],[],[]).



construct_term(not,[Id],_ListIdsConcs,Term) :- !,
    cgc(Id,situation,_,[GID],_),
    cg(GID,Rels,A,B),
    translate_cgraph(cg(GID,Rels,A,B),Term1),
    Term=..[~, Term1].

construct_term(and,[Id1,Id2],_ListIdsConcs,Term) :- !,
    cgc(Id1,situation,_,[GID1],_),
    cg(GID1,Rels1,A1,B1),
    translate_cgraph(cg(GID1,Rels1,A1,B1),Term1),
    cgc(Id2,situation,_,[GID2],_),
    cg(GID2,Rels2,A2,B2),
    translate_cgraph(cg(GID2,Rels2,A2,B2),Term2),
    Term=..[&, Term1,Term2].

construct_term(Rname,Ids,ListIdsConcs,Term) :- 
    length(Ids,1),
    Ids=[Id],
    cgc(Id,simple,Rname,Refs,_),
    member(fs(num,pl),Refs),
    member(Id-Var,ListIdsConcs),
    M=..[Rname,'X'],
    Term=(every('X',member('X',Var)->M)&set(Var)).
    
    

construct_term(Rname,Ids,ListIdsConcs,Term) :- 
    length(Ids,N), functor(Term,Rname,N),
    insert_args(Term,Ids,ListIdsConcs,1).

insert_args(Term,[Id|Next],ListIdsConcs,Num):- member(Id-Var,ListIdsConcs),
    arg(Num,Term,Var),Num1 is Num + 1,
    insert_args(Term,Next,ListIdsConcs,Num1).

insert_args(Term,[Id|Next],ListIdsConcs,Num):- ind(Id,_Type,Ind),
    arg(Num,Term,Ind),Num1 is Num + 1,
    insert_args(Term,Next,ListIdsConcs,Num1).

insert_args(Term,[Id|Next],ListIdsConcs,Num):- cgc(Id,situation,Dummy,_,_),
    arg(Num,Term,Dummy),Num1 is Num + 1,
    insert_args(Term,Next,ListIdsConcs,Num1).


insert_args(_Term,[],_,_).

sing_every(Refs):-  ground(Refs), 
    member(fs(quant,every),Refs), 
    \+ member(fs(num,pl),Refs).



make_string_f(F,S):-
    functor(F,Name,1),
    arg(1,F,Arg),
    make_string_f(Arg,S4),
    a_concat('(',S4,S5),
    a_concat(S5,')',S6),
    a_concat(Name,S6,S).
    
make_string_f(F,S):-
    functor(F,A,2),
    member(A,[&,'#','->']),!,
    arg(1,F,Arg),
    make_string_f(Arg,S4),
    a_concat(S4,' ',S5),
    a_concat(S5,A,S51),
    a_concat(S51,' ',S52),
    arg(2,F,Arg2),
    make_string_f(Arg2,S6),
    a_concat(S52,S6,S).

make_string_f(F,S):-
    functor(F,Name,2),
    arg(1,F,Arg),
    make_string_f(Arg,S4),
    a_concat(S4,',',S5),
    arg(2,F,Arg2),
    make_string_f(Arg2,S6),
    a_concat(S5,S6,S7),
    a_concat('(',S7,S8),
    a_concat(S8,')',S9),
    a_concat(Name,S9,S).


make_string_f(F,F):- atom(F).





translate_pred_id(Id,F):- check_fol_graph(Id),!,
    cg(Id,A,B,C),translate_cgraph(cg(Id,A,B,C),F1),
    make_string_f(F1,F).

translate_pred_id(_Id,F):- F='This graph can not be translated to First Order Predicate Calculus'.

check_fol_graph(Id):- cg(Id,A,_B,_C),member(Rel,A),Rel=cgr(Name,Args,_),
    member(Id1,Args),
    cgc(Id1,situation,_,_,_),
    \+(member(Name,[and,or,not])),!, fail.

check_fol_graph(_).


:- fixup_exports.
