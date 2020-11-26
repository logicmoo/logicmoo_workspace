
multifile_data(F/A):- multifile(F/A), dynamic(F/A), discontiguous(F/A).

:- multifile_data(cgr/3).
:- multifile_data(cg/4).
:- multifile_data(cgc/5).

:- multifile_data(isa_cg/2).
:- multifile_data(isa_rel/2).
:- multifile_data(ind/3).
:- multifile_data(reldef/3).
:- multifile_data(isa_kind/4).
:- multifile_data(typedef/3).




:- dynamic(ex_c/3).
:- dynamic(sp_c/3).
:- dynamic(broi/1).
:- dynamic(broig/1).
:- dynamic(top/1).
:- dynamic(bottom/1).
:- dynamic(u_conc/3).
:- expects_dialect(sicstus).
:- use_module(library(lists)).
:- dynamic(params/1).

:- include(library('../test/cgworld/CGKB.kb')).
:- include(library('../test/cgworld/Type_Hierarchy.kb')).

isConcept(ID):- grounded(id(g), ID), cgc(ID, _, _, _, _).
isCG(ID):- grounded(id(g), ID), cg(ID, _, _, _).

grounded(_, G):- ground(G), !.
grounded(id(_), _):- !.

isRelation(Name):- cgr(Name, _, _).

isSimpleConcept(ID):- grounded(id(g), ID), cgc(ID, simple, _, _, _).
isSituationConcept(ID):- grounded(id(g), ID), cgc(ID, complex, _, _, _).
isSimpleGraph(ID):- grounded(id(g), ID), cg(ID, _, _, F), member(fs(kind, normal), F).
isContextGraph(ID):- grounded(id(g), ID), cg(ID, _, _, F), member(fs(kind, context), F).
isBofContextGraph(ID):- 	grounded(id(g), ID), cg(ID, _, F, _), member(fs(kind, body_of_context), F).
isBCTDefGraph(ID):- grounded(id(g), ID), cg(ID, _, _, F),
    member(fs(kind, body_of_concept_type_def), F).
isBRTDefGraph(ID):- grounded(id(g), ID), cg(ID, _, _, F),
    member(fs(kind, body_of_rel_type_def), F).
isTypeDefGraph(ID):- grounded(id(g), ID), cg(ID, _, _, F),
    member(fs(kind, typedef), F).
isRelDefGraph(ID):- grounded(id(g), ID), reldef(ID, _, _).
isCLabel(Lbl):- grounded(label, Lbl), cgc(_, simple, Lbl, _, _).
isTLabel(Lbl):- grounded(label, Lbl), cg(Lbl, _, _, F),
    member(fs(kind, typedef), F).



isUsedId(ID):- (
    isConcept(ID);
    isCG(ID);
    isTypeDefGraph(ID);
    isRelDefGraph(ID)
    ).


listId(L):- findall(Id, cgc(Id, _, _, _, _), CgcL),
    findall(Id, cg(Id, _, _, _), CgL), append(CgcL, CgL, L1),
    findall(Id, (cg(Id, _, _, F), member(fs(kind, typedef), F)), TL),
    findall(Id, (cg(Id, _, _, F), member(fs(kind, reldef), F)), RL),
    append(TL, RL, L2), append(L1, L2, L).

broiId(Id):- listId(L), max_el(L, Id),
    X is (1 + '//'(Id , 10000000)) * 10000000,
    max_el([Id, X], XId),
    asserta(broi(XId)).

max_el(L, E):- mel(L, -1, E).
mel([], E, E).
mel([H|T], N, E):- ground(H), (H>=N -> mel(T, H, E));mel(T, N, E).

:- broiId(_X).

newId(Id):- retract(broi(I)), !, sum1(I, Id), asserta(broi(Id)).
sum1(X, X1):- X1 is X+1.

/*supRef(super_referent, referent)*/
supRef([], [fs(type, quest)]).
supRef([], [fs(quant, lambda)]).
supRef([], [fs(quant, every)]).
supRef([fs(num, sing)], [fs(num, sing)]).
supRef([], [fs(name, _)]).
supRef([], [fs(refID, _)]).
supRef([fs(num, plur)], [fs(num, plur)]).
supRef([], [fs(type, def)]).
supRef([], [fs(type, meas)]).
supRef([], [fs(quant, _)]).
supRef([], [fs(set_type, _)]).

/*subRef(subreferent, referent)*/
subRef([], [fs(type, quest)]).
subRef([], [fs(quant, lambda)]).
subRef([], [fs(quant, every)]).
subRef([fs(num, sing)], [fs(num, sing)]).
/*subRef([fs(refID, V)], [fs(num, sing)]).*/
subRef([fs(num, plur)], [fs(num, plur)]).
subRef([fs(name, N)], [fs(name, N)]).
subRef([fs(refID, ID)], [fs(refID, ID)]).
subRef([fs(type, def)], [fs(type, def)]).
subRef([fs(type, meas)], [fs(type, meas)]).
subRef([fs(quant, N)], [fs(quant, N)]).
subRef([fs(set_type, T)], [fs(set_type, T)]).

/*HIERARCY*/
/*sub(X, Y) check if X is subtype of Y*/
sub(X, X).
sub(X, Y):- isa_cg(X, Y), !.
sub(X, Y):- isa_cg(X, Z), sub(Z, Y).

suptype(X, Y, X):- sub(Y, X), !.
suptype(X, Y, Y):- sub(X, Y).

subtype(X, Y, X):- sub(X, Y), !.
subtype(X, Y, Y):- sub(Y, X).

/*subrel(subrel_type, rel_type) this prodcedure may be needless if relation hierarcy introduces with isa_cg/2 clause*/

subrel(X, Y):- X==Y, !;isa_rel(X, Y).
subrel(X, Y):- isa_rel(X, Z), !, subrel(Z, Y).

subrels(X, Y, Y):- subrel(X, Y).
subrels(X, Y, X):- subrel(Y, X).

/*finds common father of the given nodes*/
super(A, B, S):- suptype(A, B, S), !;
    top(X),
    path_up(A, X, S1), path_up(B, X, S2),
    waste(S1, S2, S).

/*path_up(X, Y, P) path from X to Y climbing up*/

path_up(X, X, [X]).
path_up(X, Y, P) :-  
  isa_cg(X, Z), path_up(Z, Y, L), append([X], L, P).

path_up_len(_, _, 0, []).
path_up_len(X, Y, N, P):- isa_cg(X, Z), N1 is (N-1),
    path_up_len(Z, Y, N1, P1), append([X], P1, P).
paths_up_len(X, Y, N, P):-
    findall(P1, path_up_len(X, Y, N, P1), P).


/*finds the first common element of two lists of elements*/
waste([H|L], L1, L2):- non_member(H, L1), !,
    waste(L, L1, L2).
waste([H|_], L1, H):- member(H, L1), !.

min_super_type(A, B, S):- if(A=B, S=A,
    (findall(S1, super(A, B, S1), S2),
    sort(S2, S3), top(X),
    min_hel(X, S3, X, S))).

/*returns element from the list which is minimal consider hierarcy e.g sybtype of all the other*/
min_hel(E, [], Top, E):- E\=Top.
min_hel(E, [H|T], Top, M):- sub(H, E), !, min_hel(H, T, Top, M).
min_hel(E, [_|T], Top, M):- min_hel(E, T, Top, M).

/*C is minimal common supertype, L1 is a path from T1 to C, L2 is the path from T2 to C and T is a path from C to Univ*/
minComSuperType(T1, T1, T1, [], [], _).
minComSuperType(T1, T2, C, L1, L2, T):-
    min_super_type(T1, T2, C), path_up(T1, C, L1),
    path_up(T2, C, L2), top(X), path_up(C, X, T).

/*finds common son of the given nodes*/
below(A, B, S):- subtype(A, B, S), !;bottom(X),
    path_down(A, X, S1),
    path_down(B, X, S2),
    waste(S1, S2, S).

/*path_down(X, Y, P) path from X to Y climbing down*/
path_down(X, X, [X]).
path_down(X, Y, P):- isa_cg(Z, X), path_down(Z, Y, L),
    append([X], L, P).

max_sub_type(A, B, S):- if(A=B, S=A,
    (findall(S1, below(A, B, S1), S2),
    sort(S2, S3), bottom(X), max_hel(X, S3, X, S))).

/*returns element from the list which is maximal consider hierarcy*/
max_hel(E, [], B, E):- E\=B.
max_hel(E, [H|T], B, M):- sub(E, H), !, max_hel(H, T, B, M).
max_hel(E, [_|T], B, M):- max_hel(E, T, B, M).

maxComSubType(T1, T1, T1, [], [], _).
maxComSubType(T1, T2, C, L1, L2, B):-
    max_sub_type(T1, T2, C), path_down(T1, C, L1),
    path_down(T2, C, L2), bottom(X), path_down(C, X, B).

/*returns specialization of two list of referents in case of type/subtype relation in corresponding type labels*/
referents1(Ref1, Ref2, Ref2):- subset(Ref1, Ref2), !.
referents1(Ref1, Ref2, Ref):- subrefs(R1, Ref1), subrefs(R2, Ref2), !,
    if(subset(R1, R2), Ref=R2, ref_subt(R1, R2, Ref)).

:- discontiguous(ref_subt/3).

/*PartialSet(1)*/
ref_subt(R1, R2, R):- if(subt_case1(R1, R2, R), !, subt_case2(R1, R2, R)).
subt_case1(R1, R2, R2):- check_mq(R1, Q1), check_mq(R2, Q2), !,
    brel(R1, N), brel(R2, N), Q2>=Q1,
    if((member(fs(name, N1), R1), member(fs(name, N2), R2)), 		subset(N1, N2), !).
subt_case2(R1, R2, R2):- check_mq(R1, Q1), check_mq(R2, Q2), !, Q2>=Q1,
    brel(R1, N), N2 is (N+1), brel(R2, N2), member(fs(name, _), R2).

subt_case2(R1, R2, R):- check_mq(R1, Q1), check_mq(R2, Q2), Q2>=Q1,
    brel(R2, N), N2 is (N+1), brel(R1, N2), member(fs(name, L), R1),
    brel(L, L1), Q2>=L1,
    append(R2, [fs(name, L)], R).
/*DefiniteSet(1)*/
ref_subt(R1, R2, R2):- check_tn(R1, N1), check_tn(R2, N2), brel(R1, N),
    brel(R2, N), subset(N1, N2).
/*DefiniteSet(2)*/
ref_subt(R1, R2, R2):- check_tn(R1, N1), check_mq(R2, Q2), brel(N1, N), Q2>=N.
/*PartialSet(2)*/
ref_subt(R1, R2, R2):- check_mq(R1, Q1), check_tn(R2, N2), brel(N2, N), N>=Q1.
/*NamedIndividual(2) and IndividualMarker(2)*/
ref_subt(R1, R2, Ref):- checkId(R1, R2, Ref).

check_mq(R, N):- member(fs(type, meas), R), member(fs(quant, N), R).
check_tn(R, L):- member(fs(type, def), R), member(fs(name, L), R).
check_tq(R, N):- member(fs(quant, N), R), member(fs(type, def), R).


/*Named individual case_2 and Individual marker case_2*/
checkId(R1, R2, Ref):- if(checkId1(R1, R2, Rf), Ref=Rf, checkId1(R2, R1, Ref)).
checkId1(R1, R2, Ref):- member(fs(name, N), R1), member(fs(refID, Id), R2),
    ind(Id, N, _), append(R1, R2, Refs), clean(Refs, Ref).

/*returns specialization of two list of referents in case common subtype between corresponding type labels*/
referents2(Ref1, Ref2, Ref):- supsetr(Ref1, Ref2, Ref), !.
referents2(Ref1, Ref2, Ref):- subrefs(R1, Ref1), subrefs(R2, Ref2), !,
    if(supsetr(R1, R2, Refs), Ref=Refs, ref_comt(R1, R2, Ref)).

ref_comt(Ref1, Ref2, Ref):- ref_sp_case(Ref1, Ref2, Ref), !.
ref_comt(Ref1, Ref2, Ref):- ref_comt1(Ref1, Ref2, Ref).
/*check in two directions*/
ref_comt1(Ref1, Ref2, Ref):- if(ref_comt2(Ref1, Ref2, Ref), !,
    ref_comt2(Ref2, Ref1, Ref)).

/*PartialSet(1)*/
:- discontiguous(ref_comt2/3).

ref_comt2(R1, R2, R):- if(comt_case1(R1, R2, R), !, comt_case2(R1, R2, R)).

comt_case1(R1, R2, R2):- check_mq(R1, Q1), check_mq(R2, Q2), !,
    brel(R1, N), brel(R2, N), Q2>=Q1,
    if((member(fs(name, N1), R1), member(fs(name, N2), R2)),
    subset(N1, N2), !).

comt_case2(R1, R2, R2):- check_mq(R1, Q1), check_mq(R2, Q2), !, Q2>=Q1,
    brel(R1, N), N2 is (N+1), brel(R2, N2), member(fs(name, _L), R2).

comt_case2(R1, R2, R):- check_mq(R1, Q1), check_mq(R2, Q2), Q2>=Q1,
    brel(R2, N), N2 is (N+1), brel(R1, N2), member(fs(name, L), R1),
    brel(L, L1), Q2>=L1,
    append(R2, [fs(name, L)], R).

/*DefiniteSet(1)*/
ref_comt2(R1, R2, R2):- check_tn(R1, N1), check_tn(R2, N2), brel(R1, N),
    brel(R2, N), subset(N1, N2).

/*DefiniteSet(2)*/
ref_comt2(R1, R2, R):- check_tn(R1, N1), check_mq(R2, Q2), brel(N1, N), Q2>=N,
    append(R2, [fs(name, N1)], R).

/*PartialSet(2)*/
ref_comt2(R1, R2, R2):- check_mq(R1, Q1), check_tn(R2, N2), brel(N2, N), N>=Q1.

/*NamedIndividual(2) and IndividualMarker(2)*/
ref_comt2(R1, R2, Ref):- checkId(R1, R2, Ref).

%Special case
ref_sp_case(R1, R2, R):- 	member(fs(num, sing), R1), !, member(fs(num, sing), R2),
    member(fs(name, N1), R1), member(fs(name, N2), R2),
    append([N1], [N2], N), append([fs(num, plur)], [fs(name, N)], R).

ref_sp_case(R1, R2, R):- 	check_tn(R1, L1), check_tn(R2, L2), append(L1, L2, L), !,
  if(ref_sp_case1(R1, R2, Rf), (append(Rf, [fs(name, L)], R), !),
    (append([fs(type, def)], [fs(num, plur)], S1),
     append(S1, [fs(name, L)], R))).
ref_sp_case(R1, R2, R):- member(fs(name, L1), R1), member(fs(name, L2), R2),
    append(L1, L2, L), append([fs(num, plur)], [fs(name, L)], R).

ref_sp_case1(R1, R2, R):- check_mq(R1, Q1), check_mq(R2, Q2),
    append([fs(num, plur)], [fs(type, meas)], S1),
    append(S1, [fs(type, def)], S2),
    Q is Q1+Q2, append(S2, [fs(quant, Q)], R).

/*returs superset of two sets; if sets aren't in relation superset/subset then fails*/
supsetr(S1, S2, S2):- subset(S1, S2).
supsetr(S1, S2, S1):- subset(S2, S1).

/*finds subreferents of the list of referents*/
subrefs([], []).
subrefs(L, [H|T]):- subRef(H1, [H]), !,
    subrefs(L1, T), append(H1, L1, L).

brel([], 0).
brel([_|T], N):- brel(T, N1), N is N1+1.

%subset([], _).
%subset([E|Sub], Set):- member(E, Set), !, subset(Sub, Set).

/*ind(IndID, Name, Type) declares that individual with IndID conforms to the Type, this individual also conforms to all super types of the given type*/
conformity(IndID, Type):- ind(IndID, _, Type).
conformity(IndID, Type):- isa_cg(SubType, Type), bottom(X),
    SubType\=X,
    conformity(IndID, SubType).
conformity(_, _):- !, fail.

conformity1(IndName, Type):- ind(_, IndName, Type).
conformity1(IndName, Type):- isa_cg(SubType, Type), bottom(X),
    SubType\=X,
    conformity1(IndName, SubType).
conformity1(_, _):- !, fail.


/*check list of referents and if some referent doesn't conform to the type it is removed and returns new list;check only referents with feauture refID*/
conform([], _, []).
conform([fs(refID, ID)|R], T, [New|R1]):-
    if(conformity(ID, T), New=fs(refID, ID), New=[]),
    conform(R, T, R1).
conform([H|R], T, [H|R1]):- conform(R, T, R1).

conform1([], _, []).
conform1([fs(name, N)|R], T, [New|R1]):-
    if(conformity1(N, T), New=fs(name, N), New=[]),
    conform1(R, T, R1).
conform1([H|R], T, [H|R1]):- conform1(R, T, R1).
check_conformity(R, T, R1):- 	if(conform(R, T, R2), R1=R2, conform1(R, T, R1)).

/*unifies concepts with concept labels in relation subtype/type */
checku(N1, N2, R1, R2, Id):- sub(N1, N2), referents1(R1, R2, R),
    check_conformity(R, N1, NR), exist_cgc(simple, N1, NR, _, Id).


/*unifies two concepts and the result is some specialization of the given concepts*/
%unifyconc(Cid1, Cid1, Cid1).
unifyconc(Cid1, Cid2, Id):-
   (cgc(Cid1, simple, N1, Ref1, _),
    cgc(Cid2, simple, N2, Ref2, _),
    unifysimple(N1, Ref1, N2, Ref2, Id),
    assertz(u_conc(Cid1, Cid2, Id)))
  ;(cgc(Cid1, complex, N1, RGr1, _),
    cgc(Cid2, complex, N2, RGr2, _),
    unifysituation(N1, RGr1, N2, RGr2, Id),
    assertz(u_conc(Cid1, Cid2, Id))).
/*unifyconc(_, _, _):- write('Unification is impossible.'),
    !, fail.*/

/*if the two concepts are previously unified then returns the unified concept id(g) else performs unification*/
unifyconcepts(Cid1, Cid2, Id):-
  (u_conc(Cid1, Cid2, Id);u_conc(Cid2, Cid1, Id)), !.
unifyconcepts(Cid1, Cid2, Id):- unifyconc(Cid1, Cid2, Id).

/*unifies simple concepts*/
unifysimple(N1, Ref1, N2, Ref2, Id):-
    (checku(N1, N2, Ref1, Ref2, Id), !;
    checku(N2, N1, Ref1, Ref2, Id), !;
    if(max_sub_type(N1, N2, N),
    (referents2(Ref1, Ref2, R),
    check_conformity(R, N, NR), !,
    exist_cgc(simple, N, NR, _, Id)),
    (!, fail))).

/*unifies complex concepts*/
unifysituation(N1, S1, N2, S2, Id):-
    (sub(N1, N2), exist_cgc(complex, N1, S1, _, Id)), !;
    (sub(N2, N1), exist_cgc(complex, N2, S2, _, Id)).

/*separate list of relations in graph Gid in two lists R and R1, list R consists of all relations that have concept Cid, list R1 consists of all relations that don't have concept Cid*/
findrels(Gid, Cid, R, R1):- graph_relations(Gid, G), findrel(Cid, G, R, R1).

findrel(_, [], [], []).
findrel(Cid, [cgr(N, L, _)|T], [cgr(N, L, _)|T1], L1):-
    member(Cid, L), !, findrel(Cid, T, T1, L1).
findrel(Cid, [cgr(N, L, _)|T], T1, [cgr(N, L, _)|L1]):-
    findrel(Cid, T, T1, L1).

/*generates separated lists of realtinos of two graphs*/
graphrels(Gid1, Gid2, Cid1, Cid2, C1, R1, C2, R2):-
    findrels(Gid1, Cid1, C1, R1),
    findrels(Gid2, Cid2, C2, R2),
    C1\=[], C2\=[].
graphrels(_, _, _, _, _, _, _, _):-
    write('No compatible triples of relations'), !, fail.

/*,,,,,,,,,,,cg_replace every appearance of X in list with Y*/
find_rep(_, _, [], []).
find_rep(X, Y, [X|T], [Y|T1]):- !, find_rep(X, Y, T, T1).
find_rep(X, Y, [H|T], [H|T1]):- !, find_rep(X, Y, T, T1).

/*cg_replace every appearance of concept X in rel.list with concept Y*/
cg_replace(_, _, [], []).
cg_replace(X, Y, [cgr(N, L, _)|T], [cgr(N, L1, _)|T1]):-
    find_rep(X, Y, L, L1), !,
    cg_replace(X, Y, T, T1).

/*cg_replace when it is considered wheather concepts are related to in comming or out comming arcs*/
replace_in(_, _, [], []).
replace_in(X, Y, [cgr(N, L, _)|T], [cgr(N, L1, _)|T1]):-
    inRel(cgr(N, L, _), InR),
    outRel(cgr(N, L, _), OutR),
    find_rep(X, Y, InR, InR1), append(InR1, OutR, L1),
    !, replace_in(X, Y, T, T1).

replace_out(_, _, [], []).
replace_out(X, Y, [cgr(N, L, _)|T], [cgr(N, L1, _)|T1]):-
    inRel(cgr(N, L, _), InR),
    outRel(cgr(N, L, _), OutR),
    [X]=OutR, append(InR, [Y], L1), !,
    replace_out(X, Y, T, T1).
replace_out(X, Y, [cgr(N, L, _)|T], [cgr(N, L, _)|T1]):-
    replace_out(X, Y, T, T1).

inRel(cgr(_, R, _), InR):- append(InR, [_], R).
outRel(cgr(_, R, _), [OutR]):- append(_, [OutR], R).

/*removes repeated elements in the list*/
clean([], []).
clean([H|T], T1):- member(H, T), !, clean(T, T1).
clean([H|T], [H|T1]):- clean(T, T1).

/*removes repeated relations in the list of relation triples in case of different annotations*/
cleanr([], []).
cleanr([cgr(N, R, _)|T], T1):- member(cgr(N, R, _), T), !,
    cleanr(T, T1).
cleanr([cgr(N, R, _)|T], [cgr(N, R, _)|T1]):- cleanr(T, T1).

/*check if exists graph with relation list R, coreference links CorL, feature kind K, faeture comment C; if exists returns its NGrid else create new graph with NGrid*/
exist_cg(R, CorL, K, C, P, NGrid):-
    ((cg(N, R, CorL, FS), member(fs(kind, K), FS),
    member(fs(comment, C), FS), member(fs(operation, P), FS),
    grounded(id(g), N))-> NGrid=N;
    (newId(NGrid), assertz(cg(NGrid, R, CorL, [fs(kind, K),
    fs(comment, C), fs(operation, P)])))).

/*check if exists concept of type K, name T, features F; if exists returns its NC else create new concept*/
exist_cgc(K, T, F, _, NC):- (cgc(N, K, T, F, _), grounded(id(g), N))->NC=N;
    (newId(NC), assertz(cgc(NC, K, T, F, _))).

graph_relations(GrID, Rel):- grounded(id(g), GrID), cg(GrID, Rel, _, _).
graph_clinks(GrID, CLinks):- grounded(id(g), GrID), cg(GrID, _, CLinks, _).

/*subsumes given concept label with other concept label in coreference link*/
help_link([], _, _, []).
help_link([identity_line(H)|T], Cid, NCid, NL):- find_rep(Cid, NCid, H, H1),
    help_link(T, Cid, NCid, T1), append([identity_line(H1)], T1, NL).

/*JOIN OPERATION*/
/*join/4;Gid1&Gid2 are graphs identificators and Cid1&Cid2 are identificators of concepts to be joined.Gid3&Cid3 are identificators of the resulted graph and concept*/
/*Test join(103, 104, 7, 7, GrID, CID), join(103, 106, 6, 12, GrID, CID),
join(100, 110, 7, 7, GrID, CID), join(103, 100, 7, 7, GrID, CID)*/
join(Gid1, Gid2, Cid1, Cid1, Gid3, Cid1):-
    Param=['join', Gid1, Gid2, Cid1, Cid1, Cid1, Gid3],
    if(exist_params(Param, Gid), Gid3=Gid,
    (
    graph_relations(Gid1, R1), graph_relations(Gid2, R2),
    graph_clinks(Gid1, Link1), graph_clinks(Gid2, Link2),
    append(Link1, Link2, Link),
    append(R1, R2, NRel),
    clean(NRel, NR1), clean_help(NR1, NRels),
    to_string('This graph is reseived from graphs ', Gid1, Com1),
    to_string(' and ', Gid2, Com2), to_string(Com1, Com2, Com3),
    to_string(' performing the join operation on concepts with id(g) ', Cid1, Com4),
    to_string(Com3, Com4, Com5), to_string(' and ', Cid1, Com6),
    to_string(Com5, Com6, Comment),
    exist_cg(NRels, Link, joined, Comment, Param, Gid3),
    assertz(params(Param)))).

join(Gid1, Gid2, Cid1, Cid2, Gid3, Cid3):-
    Param=['join', Gid1, Gid2, Cid1, Cid2, Cid3, Gid3],
    if(exist_params(Param, Gid), Gid3=Gid,
    (
    unifyconcepts(Cid1, Cid2, Cid3), !,
    graph_clinks(Gid1, Link1), graph_clinks(Gid2, Link2),
    help_link(Link1, Cid1, Cid3, L1), help_link(Link2, Cid2, Cid3, L2),
    append(L1, L2, L),
    graphrels(Gid1, Gid2, Cid1, Cid2, C1, R1, C2, R2),
    cg_replace(Cid1, Cid3, C1, NC1),
    cg_replace(Cid2, Cid3, C2, NC2),
    append(NC1, NC2, NC), append(R1, R2, NR),
    append(NC, NR, NRel), !,
    clean(NRel, NR1), clean_help(NR1, NRels),
    to_string('This graph is reseived from graphs ', Gid1, Com1),
    to_string(' and ', Gid2, Com2), to_string(Com1, Com2, Com3),
    to_string(' performing the join operation on concepts with id(g) ', Cid1, Com4),
    to_string(Com3, Com4, Com5), to_string(' and ', Cid2, Com6),
    to_string(Com5, Com6, Comment),
    exist_cg(NRels, L, joined, Comment, Param, Gid3),
    assertz(params(Param)))).


/*join_op/4; Grid1&Grid2 are graphs identificators and L1&L2 are type labels of concepts that have to be joined.Grid3&Cid3 are identificators of the resulted graph and concept*/
/*test join_op(103, 'security', 106, 'corporate_bond', GrID, Cid3).*/
join_op(Grid1, L1, Grid2, L2, Grid3):-
    find_conc(Grid1, Grid2, L1, L2, Cid1, Cid2),
    join(Grid1, Grid2, Cid1, Cid2, Grid3, _).

/*finds concepts from the list of relation with the given type labels*/
find_conc(Grid1, Grid2, L1, L2, Cid1, Cid2):-
    graph_relations(Grid1, R1), graph_relations(Grid2, R2),
    rel_to_list(R1, Ls1), rel_to_list(R2, Ls2),
    (cgc(Cid1, _C1, L1, _F1, _), member(Cid1, Ls1)),
    (cgc(Cid2, _C2, L2, _F2, _), member(Cid2, Ls2)), !.

max_join(Grid1, Grid1, Grid1).
max_join(Grid1, Grid2, NGrid):-
    Param=['max_join', Grid1, Grid2, NGrid],
    if(exist_params(Param, Grid), NGrid=Grid,
    (
    graph_relations(Grid1, Rl1), graph_relations(Grid2, Rl2),
    comn_relsj(Rl1, Rl2, Rel1, Rel2), (Rel1= [];Rel2 =[]),
    compareconsj(Rl1, Rl2, _RelC), all_un(Rl1, Rl2, U),
    graph_clinks(Grid1, Link1), graph_clinks(Grid2, Link2),
    un_process(U, Rl1, Rl2, NR1, NR2),
    un_processl(U, Link1, Link2, L1, L2),
    append(NR1, NR2, NR), clean(NR, NRels),
    clean_help(NRels, R), !,
    (R\=[] -> (to_string('This graph is received from graphs ', Grid1, Com1), to_string(' and ', Grid2, Com2),
to_string(Com1, Com2, Com3),
to_string(Com3, ' performing the maximal join operation', Comment),
    append(L1, L2, L),
    exist_cg(R, L, maximal_joined, Comment, Param, NGrid)),
    assertz(params(Param))))).
max_join(Grid1, Grid2, NGrid):-
    Param=['max_join', Grid1, Grid2, NGrid],
    if(exist_params(Param, Grid), NGrid=Grid,
    (
    graph_relations(Grid1, Rl1), graph_relations(Grid2, Rl2),
    comn_relsj(Rl1, Rl2, Rel1, Rel2),
    razlika(Rl1, Rel1, Rest1), razlika(Rl2, Rel2, Rest2),
    comparetrsj(Rel1, Rel2), all_un(Rel1, Rel2, U),
    graph_clinks(Grid1, Link1), graph_clinks(Grid2, Link2),
    un_process(U, Rel1, Rel2, NR1, NR2),
    un_processl(U, Link1, Link2, L1, L2),
    un_process(U, Rest1, Rest2, NRest1, NRest2),
    append(NR1, NR2, NR), clean(NR, NRels),
    append(NRels, NRest1, S), append(S, NRest2, S1), clean(S1, NRel),
    clean_help(NRel, R), !,
    (R\=[] -> (to_string('This graph is received from graphs ', Grid1, Com1), to_string(' and ', Grid2, Com2),
    to_string(Com1, Com2, Com3),
    to_string(Com3, ' performing the maximal join operation', Comment),
    append(L1, L2, L),
    exist_cg(R, L, maximal_joined, Comment, Param, NGrid)),
    assertz(params(Param))))).

compareconsj(Rel1, Rel2, L):- rel_to_list(Rel1, L1), rel_to_list(Rel2, L2),
    conceptsunify(L1, L2, Lm), !, clean(Lm, L).
conceptsunify([], _, []).
conceptsunify([H|L1], L2, L):- conceptun(H, L2, H1), conceptsunify(L1, L2, T), append(H1, T, L).

conceptun(_, [], []).
conceptun(C, [H|T], UC):- unifyconcepts(C, H, C1), conceptun(C, T, U1), append([C1], U1, UC).
conceptun(C, [_H|T], UC):- conceptun(C, T, UC).


/*GENERALIZATION OPERATION*/
/*finds minimal common supertype of the given types*/
gentype(T1, T2, Type):- suptype(T1, T2, Type), !;
    if(min_super_type(T1, T2, T), Type=T, Type=[]).


/*finds some generalization of two lists of referents*/
genref(R1, R2, R):- suprefs(Ref1, R1), suprefs(Ref2, R2), !,
    sec(Ref1, Ref2, R).

/*returns list S of supreferents of the given list of referents*/
suprefs([], []).
suprefs(L, [H|T]):- supRef(H1, [H]), suprefs(L1, T),
    append(H1, L1, L).

referents(R1, R2, R):- referents1(R1, R2, R).
referents(R1, R2, R):- referents2(R1, R2, R).

/*returs the generalize concept of the given concepts*/
extend_conc(Cid1, Cid2, NCid):-
ex_c(Cid1, Cid2, NCid), !.
extend_conc(Cid1, Cid2, NCid):-
extend_concept(Cid1, Cid2, NCid).

extend_concept(Cid, Cid, Cid).
extend_concept(Cid1, Cid2, NCid):-
    (cgc(Cid1, simple, T1, R1, _),
    cgc(Cid2, simple, T2, R2, _),
    extend_simple(T1, R1, T2, R2, NCid),
    assertz(ex_c(Cid1, Cid2, NCid))), !;
    (cgc(Cid1, complex, T1, R1, _),
    cgc(Cid2, complex, T2, R2, _),
    extend_sit(T1, T2, R1, R2, NCid),
    assertz(ex_c(Cid1, Cid2, NCid))).

extend_simple(T1, R1, T2, R2, NCid):-
    (gentype(T1, T2, T), T\=[],
    genref(R1, R2, R), exist_cgc(simple, T, R, _, NCid));!, fail.

extend_sit(T1, T2, R1, R2, NCid):-
    (sub(T1, T2), exist_cgc(complex, T2, R2, _, NCid));
    (sub(T2, T1), exist_cgc(complex, T1, R1, _, NCid));
    (gentype(T1, T2, S), exist_cgc(complex, S, [], _, NCid));!, fail.

/*returns some generalization of 2 lists of concepts*/
comparel([], _, []).
comparel(_, [], []).
comparel([H|T], [H1|T1], [Id|L]):- extend_conc(H, H1, Id),
    !, comparel(T, T1, L).

help_links([], []).
help_links([identity_line([H1, H2])|T], NL):- 		if((ex_c(H1, _, I1);ex_c(_, H1, I1)), NI1=I1, NI1=H1),
    if((ex_c(H2, _, I2);ex_c(_, H2, I2)), NI2=I2, NI2=H2), help_links(T, T1),
    append([identity_line([NI1, NI2])], T1, NL).

/*generalization(GrID1, GrID1, GenGrID);test generalization(103, 106, GenId), generalization(103, 138, GenId) generalization(103, 104, GenID), generalization(106, 107, GenGrID)...*/

generalization(Gid1, Gid1, Gid1).
generalization(Grid1, Grid2, NGrid):-
    Param=['generalization', Grid1, Grid2, NGrid],
    if(exist_params(Param, Grid), NGrid=Grid,
    (
    comn_rels(Grid1, Grid2, Rel1, Rel2),
    comparetrs(Rel1, Rel2, R1), clean_help(R1, R), !,
    (R\=[] -> (to_string('This graph is received from graphs ', Grid1, Com1), to_string(' and ', Grid2, Com2),
    to_string(Com1, Com2, Com3),
    to_string(Com3, ' performing the generalization operation', Comment), graph_clinks(Grid1, L1), graph_clinks(Grid2, L2),
    append(L1, L2, L3), help_links(L3, L),
    exist_cg(R, L, generalized, Comment, Param, NGrid)),
    assertz(params(Param))))).

/*finds common relations, e.g relations with equal names of graphs Gid1 and Gid2*/
comn_rels(Grid1, Grid2, Rel1, Rel2):-
    graph_relations(Grid1, R1), graph_relations(Grid2, R2),
    comn(R1, R2, Rl2), comn(R2, R1, Rl1),
    cleanr(Rl2, Rel2), cleanr(Rl1, Rel1).

comn([], _, []).
comn([cgr(N, _, _)|T], S, R):- new_list(S, N, S1), comn(T, S, S2), append(S1, S2, R), !.

new_list([], _, []).
new_list([cgr(N, S, _)|T], N, [cgr(N, S, _)|T1]):-
    new_list(T, N, T1).
new_list([_|T], N, L):- new_list(T, N, L).

/*returns new list of triples which are some generalization of the given lists of triples*/
comparetrs([], _, []).
comparetrs([H|T], R2, R):- new_trs(H, R2, S), comparetrs(T, R2, S1), append(S, S1, Rel), clean(Rel, R), !.

new_trs(_, [], []).
new_trs(cgr(N, R1, _), [cgr(N, R2, _)|T], [cgr(N, R, _)|Rl]):- comparel(R1, R2, R),
    new_trs(cgr(N, R1, _), T, Rl).
new_trs(cgr(N, R, _), [_|T], Rl):- new_trs(cgr(N, R, _), T, Rl).

/*generalize_graph(GrID, NGrID)*/
/*test generalize_graph(106, NGrID), generalize_graph(155, NGrID)
    generalize_graph(1057, NGrID).*/
generalize_graph(GrID, NGrid):- Param=['generalize_graph', GrID, NGrid],
    if(exist_params(Param, Grid), NGrid=Grid,
    (
    graph_relations(GrID, R), !,
    gen_help(R, Gn), tripls(R, Rels, Gn), clean_help(Rels, Rel),
    to_string('This graph is received from graph ', GrID, Com1),
    to_string(Com1, ' performing the generalization operation', Comment),
    graph_clinks(GrID, L), help_links1(L, Gn, NL),
    exist_cg(Rel, NL, generalized, Comment, Param, NGrid),
    assertz(params(Param)))).

help_links1([], _, []).
help_links1([identity_line([C1, C2])|T], Gn, L):-
    if(memb_gen(C1, Gn, C3), NC1=C3, NC1=C1),
    if(memb_gen(C2, Gn, C4), NC2=C4, NC2=C2),
    help_links1(T, Gn, T1),
    append([identity_line([NC1, NC2])], T1, L).
memb_gen(C1, [gen(C1, C2)|_], C2).
memb_gen(C1, [_|T], C2):- memb_gen(C1, T, C2).

gen_help(R, PL):- rel_to_list(R, L), clean(L, L1), gen_tl(L1, PL).
gen_tl([], []).
gen_tl([C|T], PL):- apply_lbl(C, L), gen_tl(T, P), append([L], P, PL).

apply_lbl(C, E):- cgc(C, K, T, F, Cm), !, isa_res(T, T1),
    exist_cgc(K, T1, F, Cm, NC), E=gen(C, NC).

isa_res(L, NL):- if((isa_cg(L, L1), top(U), dif(L1, U)), NL=L1, NL=L).

tripls([], [], _).
tripls([cgr(N, R1, _)|T1], [cgr(N, R2, _)|T2], Gn):-
    list_css(R1, R2, Gn), tripls(T1, T2, Gn).
list_css([], [], _).
list_css([C1|R1], [C2|R], Gn):- member(gen(C1, C2), Gn),
    list_css(R1, R, Gn).

/*general_conc_in_graph(GrID, ConcLabel, NGrID); test general_conc_in_graph(106, 'corporate_bond', NGrID),
general_conc_in_graph(103, 'security', NGrID)*/
general_conc_in_graph(Grid, Lbl, NGrid):-
    Param=['gen_conc_gr', Grid, Lbl, NGrid],
    if(exist_params(Param, Grid1), NGrid=Grid1,
    (
    cg(Grid, Rel, _, _), rel_to_list(Rel, List),
    (cgc(Cid, K, Lbl, Fs, _), member(Cid, List)), !,
    isa_cg(Lbl, SLbl), exist_cgc(K, SLbl, Fs, _, Cid1),
    cg_replace(Cid, Cid1, Rel, NR), clean_help(NR, NRel),
    to_string('This graph is received from graph ', Grid, Com1),
    to_string(' by generalizing the concept with label ', Lbl, Com2),
    to_string(Com1, Com2, Comment), graph_clinks(Grid, L),
    help_link(L, Cid, Cid1, NL),
    exist_cg(NRel, NL, generalized, Comment, Param, NGrid),
    assertz(params(Param)))).

/*specialization(GrID1, GrID1, SpecGrID)*/
/*test specialization(103, 106, NGrID)*/
specialization(Grid1, Grid1, Grid1).
specialization(Grid1, Grid2, NGrid):-
    Param=['specialization', Grid1, Grid2, NGrid],
    if(exist_params(Param, Grid), NGrid=Grid,
    (
    comn_rels(Grid1, Grid2, Rel1, Rel2),
    comparetrs1(Rel1, Rel2, NR), clean_help(NR, R), !,
    (R\=[] -> (to_string('This graph is received from graphs ', Grid1, Com1), to_string(' and ', Grid2, Com2),
    to_string(Com1, Com2, Com3),
    to_string(Com3, ' performing the specialization operation', Comment),
    graph_clinks(Grid1, L1), graph_clinks(Grid2, L2),
    append(L1, L2, L3), help_links_sp(L3, L),
exist_cg(R, L, specialized, Comment, Param, NGrid)),
    assertz(params(Param))))).

/*returns new list of triples which are some specialization of the given lists of triples*/
comparetrs1([], _, []).
comparetrs1([H|T], R2, R):- new_trs1(H, R2, S), comparetrs1(T, R2, S1), append(S, S1, R), !.

new_trs1(_, [], []).
new_trs1(cgr(N, R1, _), [cgr(N, R2, _)|T], [cgr(N, R, _)|Rl]):-
    comparel_spec(R1, R2, R),
    new_trs1(cgr(N, R1, _), T, Rl).
new_trs1(cgr(N, R, _), [_|T], Rl):- new_trs1(cgr(N, R, _), T, Rl).

/*returns some specialization of 2 lists of concepts*/
comparel_spec([], _, []).
comparel_spec(_, [], []).
comparel_spec([H|T], [H1|T1], [Id|L]):- spec_conc(H, H1, Id),
    !, comparel_spec(T, T1, L).

spec_conc(Cid1, Cid2, NCid):-
sp_c(Cid1, Cid2, NCid), !.
spec_conc(Cid1, Cid2, NCid):-
spec_concept(Cid1, Cid2, NCid).

spec_concept(Cid, Cid, Cid).
spec_concept(Cid1, Cid2, NCid):-
    (cgc(Cid1, simple, T1, R1, _),
    cgc(Cid2, simple, T2, R2, _),
    spec_simple(T1, R1, T2, R2, NCid),
    assertz(sp_c(Cid1, Cid2, NCid))), !;
    (cgc(Cid1, complex, T1, R1, _),
    cgc(Cid2, complex, T2, R2, _),
    spec_sit(T1, T2, R1, R2, NCid),
    assertz(sp_c(Cid1, Cid2, NCid))).

spec_simple(T1, R1, T2, R2, NCid):-
    (spectype(T1, T2, T), T\=[],
    referents(R1, R2, R), exist_cgc(simple, T, R, _, NCid));!, fail.
spec_sit(T1, T2, R1, R2, NCid):-
    (sub(T1, T2), exist_cgc(complex, T1, R1, _, NCid));
    (sub(T2, T1), exist_cgc(complex, T2, R2, _, NCid));
    (spectype(T1, T2, S), exist_cgc(complex, S, [], _, NCid));!, fail.
help_links_sp([], []).
help_links_sp([identity_line([H1, H2])|T], NL):- 		if((sp_c(H1, _, I1);sp_c(_, H1, I1)), NI1=I1, NI1=H1),
    if((sp_c(H2, _, I2);sp_c(_, H2, I2)), NI2=I2, NI2=H2),
    help_links_sp(T, T1),
    append([identity_line([NI1, NI2])], T1, NL).


/*specialize_graph(GrID, NGrID); test specialize_graph(103, NGrID)*/
specialize_graph(GrID, NGrid):- Param=['spec_graph', GrID, NGrid],
    if(exist_params(Param, Grid1), NGrid=Grid1,
    (
    graph_relations(GrID, R), !,
    gen_help1(R, Sp), tripls1(R, Rh, Sp), clean_help(Rh, Rel),
    to_string('This graph is received from graph ', GrID, Com1),
    to_string(Com1, ' performing the specialization operation', Comment), graph_clinks(GrID, L),
    help_links1_sp(L, Sp, NL),
    exist_cg(Rel, NL, specialized, Comment, Param, NGrid),
    assertz(params(Param)))).

help_links1_sp([], _, []).
help_links1_sp([identity_line([C1, C2])|T], Sp, L):-
    if(memb_sp1(C1, Sp, C3), NC1=C3, NC1=C1),
    if(memb_sp1(C2, Sp, C4), NC2=C4, NC2=C2),
    help_links1_sp(T, Sp, T1),
    append([identity_line([NC1, NC2])], T1, L).
memb_sp1(C1, [spec(C1, C2)|_], C2).
memb_sp1(C1, [_|T], C2):- memb_sp1(C1, T, C2).

gen_help1(R, PL):- rel_to_list(R, L), clean(L, L1), gen_tl1(L1, PL).
gen_tl1([], []).
gen_tl1([C|T], PL):- apply_lbl1(C, L), gen_tl1(T, P), append([L], P, PL).

apply_lbl1(C, E):- cgc(C, K, T, F, Cm), !, isa_res1(T, T1),
    exist_cgc(K, T1, F, Cm, NC), E=spec(C, NC).

isa_res1(L, NL):- if((isa_cg(L1, L), bottom(B), dif(L1, B)), NL=L1, NL=L).

tripls1([], [], _).
tripls1([cgr(N, R1, _)|T1], [cgr(N, R2, _)|T2], Sp):-
    list_css1(R1, R2, Sp), tripls1(T1, T2, Sp).
list_css1([], [], _).
list_css1([C1|R1], [C2|R], Sp):- member(spec(C1, C2), Sp),  list_css1(R1, R, Sp).

/*test special_conc_in_graph(106, corporate_bond, NGrID)*/
special_conc_in_graph(Grid, Lbl, NGrid):-
    Param=['spec_conc_gr', Grid, Lbl, NGrid],
    if(exist_params(Param, Grid1), Grid1=NGrid,
    (
    cg(Grid, Rel, _, _), rel_to_list(Rel, List),
    (cgc(Cid, K, Lbl, Fs, _), member(Cid, List)), !,
    isa_cg(SLbl, Lbl), exist_cgc(K, SLbl, Fs, _, Cid1),
    cg_replace(Cid, Cid1, Rel, NRels), clean_help(NRels, NRel),
    to_string('This graph is received from graph ', Grid, Com1),
    to_string(' by specializing the concept with label ', Lbl, Com2),
    to_string(Com1, Com2, Comment), graph_clinks(Grid, L),
    help_link(L, Cid, Cid1, NL),
    exist_cg(NRel, NL, specialized, Comment, Param, NGrid),
    assertz(params(Param)))).


spectype(T1, T2, T):- if(max_sub_type(T1, T2, Type), T=Type, T=[]).

/*PROJECTION*/
/*V:[Person]<-(agnt)<-[Eat].
U:[Girl]<-(agnt)<-[Eat]->(manr)->[Fast].
projection V->U:[Girl]<-(agnt)<-[Eat]. Projection from some general graph to some specialized graph e.g query to knowledge base*/

/*projections_conc(General_graph, Spesific_graph, Projections); finds all projections from General graph to Specific graph considering relations with equal names*/
/*Test projections_conc(103, 106, Grs), projections_conc(1055, 1057, Grs), projections_conc(1055, 1053, Grs), projections_conc(1058, 106, , Grs),
projections_conc(100, 1053, Grs)*/

projections_conc(Gid1, Gid2, NGids):-
findall(NGid, projection_conc(Gid1, Gid2, NGid), NGids1),
    (NGids1\=[]-> NGids=NGids1;
    write('No projections')).

projection_conc(Gid1, Gid2, NGrid):-
    Param=['projection', Gid1, Gid2, NGrid],
    if(exist_params(Param, Grid), NGrid=Grid,
    (
    if(common_relsp1(Gid1, Gid2, Rel1, Rel2),
    (project1(Rel1, Rel2, NRel), clean_help(NRel, Rel),
    (Rel\=[]->
    (to_string('This graph is received from graphs ', Gid1, Com1), to_string(' and ', Gid2, Com2), to_string(Com1, Com2, Com3),
to_string(Com3, ' by performing the projection operation', Comment),
    graph_clinks(Gid2, L2), clean_links(L2, Rel2, L),
    exist_cg(Rel, L, projected, Comment, Param, NGrid)))),
    proj_simple(Gid1, Gid2, NGrid)))).

proj_simple(Gid1, Gid2, C):- graph_relations(Gid1, R1),
    graph_relations(Gid2, R2), rel_to_list(R1, L1),
    rel_to_list(R2, L2), obh(L1, L2, C).

obh([], _, _):- !, fail.
obh([C1|_], L, C):- obh1(C1, L, C), !.
obh([_|T], L, C):- obh1(T, L, C).

obh1(_, [], _).
obh1(C1, [C2|_], C):- cgc(C1, simple, T1, F1, _), cgc(C2, simple, T2, F2, _),
    max_sub_type(T1, T2, T), !, project_referents(F1, F2),
    exist_cgc(simple, T, F2, _, C).
obh1(C1, [_|T], C):- obh1(C1, T, C).

clean_links(L1, R, L):- rel_to_list(R, Lst), clean(Lst, Lst1), clean_lnk(L1, Lst1, L), !.
clean_lnk([], _, []).
clean_lnk([identity_line([C1, C2])|T], Lst, L):-
    cgp_not_member(C1, Lst), cgp_not_member(C2, Lst),
    clean_lnk(T, Lst, L).
clean_lnk([H|T], Lst, L):- clean_lnk(T, Lst, L1), append([H], L1, L).

common_relsp1(Gid1, Gid2, Rel1, Rel2):- graph_relations(Gid1, Rel1),
    graph_relations(Gid2, R), result_trs(Rel1, R, Rel2).

result_trs(R1, R2, R):- sub_trs(R1, R2, R), connect_rel(R).

sub_trs([], _, []).
sub_trs([cgr(N, _, _)|L], Sp, [E|L1]):- memb(cgr(N, _, _), Sp, E),
    sub_trs(L, Sp, L1).
%sub_trs([_|L], Sp, L1):- sub_trs(L, Sp, L1).
memb(cgr(N, _, _), [cgr(N, R, A)|_], cgr(N, R, A)).
memb(cgr(N, _, _), [_|L], E):- memb(cgr(N, _, _), L, E).

connect_rel(R):- len(R, 1).
connect_rel(R):- append(X, Y, R), ((X\=[], Y\=[])->
    (rel_to_list(X, L1), rel_to_list(Y, L2),
    sec1(L1, L2, R1), !, R1\=[]);fail).

project1([], _, []).
project1([H|T], R2, R):- new_prs(H, R2, S), project1(T, R2, S1), append(S, S1, R), !.

new_prs(_, [], []).
new_prs(cgr(N, R1, _), [cgr(N, R2, _)|T], [cgr(N, R, _)|Rl]):- spec(R1, R2, R),
    new_prs(cgr(N, R1, _), T, Rl).
new_prs(cgr(N, R, _), [_|T], Rl):- new_prs(cgr(N, R, _), T, Rl).

/*returns specialization of two list of concepts
elements of the second list are specializaton of elements
from the first list*/
spec(_, [], []).
spec([], _, []).
spec([H|R1], [H1|R2], [H1|R]):-
    (cgc(H, simple, T1, F1, _), cgc(H1, simple, T2, F2, _),
    sub(T2, T1), project_referents(F1, F2), spec(R1, R2, R)), !;
    (cgc(H, complex, T1, _, _),
    cgc(H1, complex, T2, _, _),
    sub(T2, T1), spec(R1, R2, R)).
/*spec(_, _, [no]).*/

project_referents(F1, F2):- razlika(F1, F2, L), (L=[];L=[fs(quant, lambda)]).
project_referents(F1, _Dmiles_F2):-
    (member_check(fs(quant, every), F1);member_check(fs(num, _), F1)),
    cgp_not_member(fs(quant, lambda), F1), cgp_not_member(fs(type, quest), F1),
    cgp_not_member(fs(type, def), F1), cgp_not_member(fs(type, meas), F1),
    cgp_not_member(fs(name, _), F1), cgp_not_member(fs(refID, _), F1),
    cgp_not_member(fs(set_type, _), F1).

/*extended projection is projection of general graph to specific in which remaining relation triples of specific graph are merged to projection triples; this operation is very useful for projection of query graph to knowledge base*/
extended_projections(Gid1, Gid2, NGids):-
    findall(NGid, ext_proj(Gid1, Gid2, NGid), NGids2),
    clean_help(NGids2, NGids1),
    (NGids1\=[]-> NGids=NGids1;
    write('No projections')).
/*test ext_proj(1058, 106, Gr), ext_proj(1058, 103, Gr)*/
ext_proj(Gid1, Gid2, NGrid):-
    Param=['ext_projection', Gid1, Gid2, NGrid],
    if(exist_params(Param, Grid), NGrid=Grid,
    (
    projection_conc(Gid1, Gid2, PGid),
    graph_relations(PGid, Rel1),
    relsp(Gid1, Gid2, Rel2), append(Rel1, Rel2, NRel),
    clean_help(NRel, Rel),
    to_string('This graph is received from graphs ', Gid1, Com1), to_string(' and ', Gid2, Com2), to_string(Com1, Com2, Com3),
to_string(Com3, ' by performing the extended projection operation', Comment),
    graph_clinks(Gid1, L),
    exist_cg(Rel, L, ext_projected, Comment, Param, NGrid),
    assertz(params(Param)))).

relsp(Gid1, Gid2, Rel2):-
    graph_relations(Gid1, S1), graph_relations(Gid2, S2),
    sec2(S2, S1, Rel1), razlika(S2, Rel1, Rel2).

/*TYPE CONTRACTION*/
/*Subsumes subgraph of the given concept graph with concept using definition of the type of this concept*/
contract_type(Gid, TDef, NGrid):-
    Param=['contract_type', Gid, TDef, NGrid],
    if(exist_params(Param, Grid1), NGrid=Grid1,
    (
    cg(TDef, [cgr(def, [Conc, Lexp], _)], _, [fs(kind, typedef)|_]),
    cgc(Lexp, complex, _, [GTid], _),
    projection_conc(GTid, Gid, PrId),
    findgenus(GTid, Cid), find_prgenus(Cid, PrId, Gen),
    cgc(Gen, simple, _, Ref, _), exist_cgc(simple, Conc, Ref, _, Id1),
    graph_relations(PrId, Rel1), graph_relations(Gid, Rel2),
    razlika(Rel2, Rel1, Rel),
    cg_replace(Gen, Id1, Rel, Rel4), clean_help(Rel4, Rel3),
    to_string('This graph is received from graph ', Gid, Com1), to_string(' by performing the contract_type operation of type: ', Conc, Com2), to_string(Com1, Com2, Comment),
    exist_cg(Rel3, _, cotracted_type, Comment, Param, NGrid),
    assertz(params(Param)))).

type_contraction(Gid, TLbl, NGrid):-
    Param=['type_contraction', Gid, TLbl, NGrid],
    if(exist_params(Param, Grid1), NGrid=Grid1,
    (
    cg(_Top_ID, [cgr(def, [Conc, Lexp], _)], _, [fs(kind, typedef)|_]),
    cgc(Conc, _, TLbl, _Fs, _),
    cgc(Lexp, complex, _, [GTid], _),
    projection_conc(GTid, Gid, PrId),
    findgenus(GTid, Cid), find_prgenus(Cid, PrId, Gen),
    cgc(Gen, simple, _, Ref, _), exist_cgc(simple, TLbl, Ref, _, Id1),
    graph_relations(PrId, Rel1), graph_relations(Gid, Rel2),
    razlika(Rel2, Rel1, Rel),
    cg_replace(Gen, Id1, Rel, Rel4), clean_help(Rel4, Rel3),
    to_string('This graph is received from graph ', Gid, Com1), to_string(' by performing the type_contraction operation of type: ', TLbl, Com2), to_string(Com1, Com2, Comment),
    exist_cg(Rel3, _, type_contracted, Comment, Param, NGrid),
    assertz(params(Param)))).

findgenus(Gid, Cid):- graph_relations(Gid, Rels),
    rel_to_list(Rels, List),
    find_lambda(List, Cid).

rel_to_list([], []).
rel_to_list([cgr(_, L1, _)|T], L):- rel_to_list(T, T1),
    append(L1, T1, L).

find_lambda([C|_], C):- cgc(C, _, _, F, _),
    member(fs(quant, lambda), F), !.
find_lambda([_|T], C):- find_lambda(T, C).

find_prgenus(Gen, PrGraph, PrGen):-
    graph_relations(PrGraph, Rel),
    rel_to_list(Rel, List),
    listfind(Gen, List, PrGen).

listfind(Gen, [H|_], H):- spec([Gen], [H], _).
listfind(Gen, [_|T], G):- listfind(Gen, T, G).

findconc([], _, _):- !, fail.
findconc([H|_], L, H):- member(H, L), !.
findconc([_|T], L, C):- findconc(T, L, C).

razlika([], _, []).
razlika([H|L1], L2, [H|L]):- cgp_not_member(H, L2), !,
    razlika(L1, L2, L).
razlika([_|L1], L2, L):- razlika(L1, L2, L).

/*TYPE EXPANSION*/
/*type_expansion(Graph_id, Typedef_graph_id, NewGraph_id):
replaces some type label of Graph_id with his definition in Typedef_graph_id*/
type_exp(Grid, T, Relg1, Reld1):-
    cgc(Concept, _, T, _Fs, _),
    cg(_, [cgr(def, [Concept, Lexp], _)], _, [fs(kind, typedef)|_]),
    cgc(Lexp, _, _, [Gr], _),
    graph_relations(Gr, Reld),
    graph_relations(Grid, Relg), findgenus(Gr, Gen),
    findc_number(Relg, T, Num), cgc(Num, simple, T, F, _),
    cgc(Gen, _, T1, _, _), exist_cgc(simple, T1, F, _, Id),
    cg_replace(Num, Id, Relg, Relg1),
    cg_replace(Gen, Id, Reld, Reld1).

type_expansion(Grid, T, NGrid):-
    Param=['type_expansion', Grid, T, NGrid],
    if(exist_params(Param, Grid1), NGrid=Grid1,
    (
    type_exp(Grid, T, Rg, Rd),
    sec(Rd, Rg, R),
    (R=[] -> (append(Rd, Rg, Rh), clean_help(Rh, Rel));
    (type_exp1(Rd, Rg, R, Rh), clean_help(Rh, Rel))),
    to_string('This graph is received from graph ', Grid, Com1), to_string(' and definition graph of type with label: ', T, Com2), to_string(Com1, Com2, Com3),
to_string(Com3, ' by performing the type_expansion operation', Comment),
    exist_cg(Rel, _, type_expanded, Comment, Param, NGrid),
    assertz(params(Param)))).
type_exp1(Reld, Relg, Rels, Rel):-
    findrep(Rels, Relg, Rep),
    replacement(Rep, Reld, Reld1),
    append(Relg, Reld1, Rel).

type_expan(Grid, TGrid, Relg1, Reld1):-
    cg(TGrid, [cgr(def, [Concept, Lexp], _)], _, [fs(kind, typedef)|_]),
    cgc(Lexp, _, _, [Gr], _),
    cgc(Concept, _, T, _Fs, _),
    graph_relations(Gr, Reld),
    graph_relations(Grid, Relg), findgenus(Gr, Gen),
    findc_number(Relg, T, Num), cgc(Num, simple, T, F, _),
    cgc(Gen, _, T1, _, _), exist_cgc(simple, T1, F, _, Id),
    cg_replace(Num, Id, Relg, Relg1),
    cg_replace(Gen, Id, Reld, Reld1).

expand_type(Grid, TGrid, NGrid):-
    Param=['expand_type', Grid, TGrid, NGrid],
    if(exist_params(Param, Grid1), NGrid=Grid1,
    (
    type_expan(Grid, TGrid, Rg, Rd),
    sec(Rd, Rg, R),
    (R=[] -> (append(Rd, Rg, Rh), clean_help(Rh, Rel));
    (type_exp1(Rd, Rg, R, Rh), clean_help(Rh, Rel))),
    to_string('This graph is received from graph ', Grid, Com1), to_string(' and type definition graph ', TGrid, Com2),
    to_string(Com1, Com2, Com3),
    to_string(Com3, ' by performing the expand_type operation', Comment),
    exist_cg(Rel, _, expanded_type, Comment, Param, NGrid),
    assertz(params(Param)))).


findc_number(L, T, Num):-
rel_to_list(L, L1), (member(Num, L1), cgc(Num, simple, T, _, _)).

com_rel(_, [], []).
com_rel(cgr(N, [C|C1], _), [cgr(N, [C|C2], _)|L], R):-
    com_rel(cgr(N, [C|C1], _), L, L1),
    append([rep(C1, C2)], L1, R).
com_rel(cgr(N, [C1|C], _), [cgr(N, [C2|C], _)|L], R):-
    com_rel(cgr(N, [C1|C], _), L, L1),
    append([rep(C1, C2)], L1, R).
com_rel(cgr(N, [C|C1], _), [_|L], L1):-
    com_rel(cgr(N, [C|C1], _), L, L1).


findrep([], _, []):- !.
findrep([cgr(N, [C|C1], _)|T], L, R):-
    com_rel(cgr(N, [C|C1], _), L, L1), !, findrep(T, L, L2),
    append(L1, L2, R).
findrep([_|T], L, T1):-  findrep(T, L, T1).

replacement([], _, []).
replacement([rep(C1, C2)|L], Reld, Reld1):-
    (replace1(C1, C2, Reld, R1), !;
    cg_replace(C1, C2, Reld, R1)),
    replacement(L, Reld, R2),
    append(R1, R2, Reld1).

replace1([C1], [C2], R1, R2):- cg_replace(C1, C2, R1, R2).

copygraph(GrID, NGrID):- grounded(id(g), GrID),
    cg(GrID, Rels, Idl, Fs),
    newId(NGrID),
    assertz(cg(NGrID, Rels, Idl, Fs)).


/*Services*/
get_concept(Cid, Cgc):- grounded(id(c), Cid), cgc(Cid, K, T, F, A),
    Cgc=cgc(Cid, K, T, F, A).
get_cgcType(Cid, Type):- grounded(id(c), Cid), cgc(Cid, Type, _, _, _).
get_cgcName(Cid, Name):- grounded(id(c), Cid), cgc(Cid, _, Name, _, _).
get_cgcFeature(Cid, Fs):- grounded(id(c), Cid), cgc(Cid, _, _, Fs, _).
get_cgcComment(Cid, Com):- grounded(id(c), Cid), cgc(Cid, _, _, _, Com).

get_graph(Gid, Cg):- grounded(id(g), Gid), cg(Gid, Rel, CoL, Fs),
    Cg=cg(Gid, Rel, CoL, Fs).
get_cgRelations(Gid, Rel):- grounded(id(g), Gid), cg(Gid, Rel, _, _).
get_cgCoreference(Gid, CoL):- grounded(id(g), Gid), cg(Gid, _, CoL, _).
get_cgComment(Gid, Com):- grounded(id(g), Gid), cg(Gid, _, _, Com).
/*returns list of relations with the given name */
get_RelfromCg(Gid, Rname, Rels):- graph_relations(Gid, R),
    new_list(R, Rname, Rels).
get_RelwithConc(Gid, Cid, Rels):- graph_relations(Gid, R),
    findall(cgr(N, L, A), (member(cgr(N, L, A), R), member(Cid, L)), Rels).

/*unifies V with the value of the F-Feature in Ref*/
getValue(Ref, F, V ):- grounded(ref, Ref), grounded(feature, F),
    var(V), member(fs(F, V), Ref).

/*unifies F with feauture which has value V*/
getFeauture(Ref, F, V):- grounded(ref, Ref), grounded(value, V),
    var(F), member(fs(F, V), Ref).

/*unifies F and V from fs(F, V) with their values*/
getVal(Ref, F, V):- grounded(ref, Ref), var(F),
    var(V), member(fs(F, V), Ref).

/*finds feature fs(F, _) in the list Ref, change its value with fs(F, V)and returns new list NRef*/
setValue(Ref, F, V, NRef):- grounded(ref, Ref),
    grounded(feature, F), grounded(value, V), ( member(fs(F, V), Ref) -> NRef=Ref )
    ;( (member(fs(F, V1), Ref), V1\==V) ->
    changeValue(Ref, F, V, NRef) )
    ; (append(fs(F, V), Ref, Ref1),
    sort(Ref1, NRef)).

/*change value of fs(F, _) from the list Ref with fs(F, V) and returns new list of features Ref1*/
changeValue(Ref, F, V, Ref1):- grounded(ref, Ref), grounded(feature, F), grounded(value, V),
    (member(fs(F, V1), Ref), V1\==V)->
    del(fs(F, _), Ref, Ref2),
    append([fs(F, V)], Ref2, Ref3), sort(Ref3, Ref1).

/*unifies Num with the value of the num-Feature in Ref*/
getRefNum(Ref, Num):- grounded(ref, Ref), var(Num),
    member(fs(num, Num), Ref).

/*changes the num-Feature in RfIn and returns the result as RfOut*/
setRefNum(RfIn, Num, RfOut):- grounded(ref, RfIn), grounded(id(g), Num), var(RfOut),
    find_rep(fs(num, _), fs(num, Num), RfIn, RfOut).


getRefType(Ref, Type):- grounded(ref, Ref), var(Type),
    member(fs(type, Type), Ref).

setRefType(RfIn, Type, RfOut):- grounded(ref, RfIn), grounded(type, Type), var(RfOut),
    find_rep(fs(type, _), fs(type, Type), RfIn, RfOut).

addCgFuture(Gid, F1, V1, Fs):- grounded(id(g), Gid), grounded(feature, F1), grounded(value, V1),
    cg(Gid, _, _, Fs1), append(Fs1, [fs(F1, V1)], Fs).

to_string(S1, S2, S):- name(S1, L1), name(S2, L2), append(L1, L2, L),
    name(S, L).

paths_up(X, Y, P):- findall(P1, path_up(X, Y, P1), P).

paths_down(X, Y, P):- findall(P1, path_down(X, Y, P1), P).

sec([], _, []).
sec([H|T], S2, [H|S]):- member(H, S2), !, sec(T, S2, S).
sec([H|T], S2, S):- non_member(H, S2), !, sec(T, S2, S).

cgp_not_member(E, L):- non_member(E, L).

non_member(E, L):- member(E, L), !, fail.
non_member(_, _).

union1([], L, L).
union1([H|T], L, L1):- member(H, L), !, union1(T, L, L1).
union1([H|T], L, [H|L1]):- union(T, L, L1).


len([], 0).
len([_|T], N):- len(T, N1), N is N1+1.

last_el([H], [H]).
last_el([_|T], E):- last_el(T, E).

not_last(L, E):- last_el(L, E), !, fail.
not_last(_, _).

del(E, [E|T], T1):- del(E, T, T1).
del(E, [H|T], [H|T1]):- del(E, T, T1).
del(_, [], []).

inRels([], []).
inRels([H|T], [H1|T1]):- inRel(H, H1), inRels(T, T1).
    outRels([], []).
outRels([H|T], [H1|T1]):- outRel(H, H1), outRels(T, T1).


not_sub(X, Y):- sub(X, Y), !, fail.
not_sub(_, _).

%dobavka
not_son(X):- isa_cg(X, _), !, fail.
not_son(_).

find_top(X):- isa_cg(_, X), not_son(X), !, assertz(top(X)).

:- find_top(_X).

not_father(X):- isa_cg(_, X), !, fail.
not_father(_).

find_bottom(X):- isa_cg(X, _), not_father(X), !, assertz(bottom(X)).

:- find_bottom(_X).

depth(Y, 0):- top(Y), !.
depth(X, N):- isa_cg(X, Y), !, depth(Y, N1), N is N1+1.

depth1(Y, 0):- bottom(Y).
depth1(X, N):- isa_cg(Y, X), !, depth1(Y, N1), N is N1+1.

%new_join(Grid1, Grid2, NewGrid):-
%cg(Grid1, R1, F1, _), cg(Grid2, R2, F2, _),
%rel_to_list(R1, L1), rel_to_list(R2, L2),
%unification(L1, L2, L).

sec1([], _, []).
sec1([H|T], L, [H|L1]):- member(H, L), sec1(T, L, L1).
sec1([_|T], L, L1):- sec1(T, L, L1).

sec2([], _, []).
sec2([H|T], L, [H|L1]):- member1(H, L), !, sec2(T, L, L1).
sec2([_|T], L, L1):- sec2(T, L, L1).
member1(cgr(N, _, _), [cgr(N, _, _)|_]).
member1(cgr(N, L, _), [_|T]):- member1(cgr(N, L, _), T).

super_type_labels(L, []):- top(L).
super_type_labels(L, S):- isa_cg(L, S1), super_type_labels(S1, S2),
    append([S1], S2, S).
sub_type_labels(L, []):- bottom(L).
sub_type_labels(L, S):- isa_cg(S1, L), sub_type_labels(S1, S2),
    append([S1], S2, S).
/*Replaces all relation wchich point to the same concepts with the same relation but point to different concepts*/
clean_help(R, Rel):- rel_point_to_same_conc(R, RC), razlika(R, RC, RR),
    new_r(RC, NRC), f_rep(NRC, Rep), frepl(Rep, RR, NRR),
    append(NRC, NRR, Rel).

rel_point_to_same_conc(R, NR):- serv(R, R1), help1(R1, R, NR).
/*make list with elements eqrel(name, concept)*/
serv(R1, R):- subserv(R1, R2), !, clean(R2, R).
subserv([], []).
subserv([H|L], L1):- memb_spec(H, L, E), !, subserv(L, L2), append([E], L2, L1).
subserv([_|L], L1):- subserv(L, L1).
memb_spec(cgr(N, [_, C], _), [cgr(N, [_, C], _)|_], E):-
    E=eqrel(N, C).
memb_spec(cgr(N, [C1, C], _), [_|T], E):- memb_spec(cgr(N, [C1, C], _), T, E).

help1([], _, []).
help1([eqrel(N, C)|T], R, L):- get_RelwSCN(C, N, R, L1), help1(T, R, L2),
    append(L1, L2, L).
/*returns all relation with name N and second concept Cid*/
get_RelwSCN(Cid, N, R, Rels):-
    findall(cgr(N, [C1, Cid], A), member(cgr(N, [C1, Cid], A), R), Rels).

new_r([], []).
new_r([H|T], NRel):- new_crel(H, H1), new_r(T, R), append([H1], R, NRel).
new_crel(cgr(N, [C, C1], A), cgr(N, [C, C2], A)):- cgc(C1, Kd, Lb, Fs, _),
    newId(C2), assertz(cgc(C2, Kd, Lb, Fs, rep(C1, C2))).
f_rep(Rel, Rep):- rel_to_list(Rel, Lst),
    findall(rep(C1, C), (member(C, Lst), cgc(C, _, _, _, rep(C1, C)), grounded(id(c), C1)), Rep).
frepl([], R, R).
frepl([rep(C1, C)|T], R, Rel):- cg_replace(C1, C, R, R1), frepl(T, R1, Rel).

member_check(E, [E|_]):- !.
member_check(E, [_|T]):- member_check(E, T).

check_idline(identity_line([C1, C2]), L):- member_check(C1, L);
    member_check(C2, L).

/*procedures of maximal join*/
comn_relsj(R1, R2, Rel1, Rel2):-
    comn(R1, R2, Rl2), comn(R2, R1, Rl1),
    cleanr(Rl2, Rel2), cleanr(Rl1, Rel1).

/*returns new list of triples which are some specialization of the given lists of triples*/
comparetrsj([], _).
comparetrsj([H|T], R2):- new_trsj(H, R2), comparetrsj(T, R2), !.

new_trsj(_, []).
new_trsj(cgr(N, R1, _), [cgr(N, R2, _)|T]):-
    comparel_mj(R1, R2),
    new_trsj(cgr(N, R1, _), T).
new_trsj(cgr(N, R, _), [_|T]):- new_trsj(cgr(N, R, _), T).

comparel_mj([], _).
comparel_mj(_, []).
comparel_mj([H|T], [H1|T1]):- unifyconcepts(H, H1, _),
    !, comparel_mj(T, T1).

all_un(R1, R2, L):- 	findall(u_conc(C1, C2, K), (u_conc(C1, C2, K), memb_rel(C1, R1),
    memb_rel(C2, R2)), L).

memb_rel(C, R):- rel_to_list(R, L), member_check(C, L).
un_process([], R1, R2, R1, R2).
un_process([u_conc(C1, C2, Id)|T], R1, R2, NR1, NR2):- 	cg_replace(C1, Id, R1, R11), cg_replace(C2, Id, R2, R22),
    un_process(T, R11, R22, NR1, NR2).

un_processl([], L1, L2, L1, L2).
un_processl([u_conc(C1, C2, Id)|T], L1, L2, NL1, NL2):-
    help_link(L1, C1, Id, L11), help_link(L2, C2, Id, L22),
    un_processl(T, L11, L22, NL1, NL2).
exist_params(Param, Grid):- params(Param), take_last(Param, Grid),
    grounded(id(g), Grid).
not_exist_params(Param, Grid):- exist_params(Param, Grid), !, fail.
not_exist_params(_, _).
take_last(L, E):- append(_, [E], L).

:- fixup_exports.




/*

Warning: findrel/3, which is referenced by
Warning:    /pack/logicmoo_nlu/prolog/cgworld/cgprolog_operations.pl:1047:14: 3-th clause of findrep/3
Warning: kb_index/1, which is referenced by
Warning:    /pack/logicmoo_nlu/prolog/cgworld/cgprolog_translator.pl:63:13: 2-nd clause of add_relations/2
Warning: referents/3, which is referenced by
Warning:    /pack/logicmoo_nlu/prolog/cgworld/cgprolog_operations.pl:728:0: 1-st clause of spec_simple/5
Warning: reldef/3, which is referenced by
Warning:    /pack/logicmoo_nlu/prolog/cgworld/cgprolog_operations.pl:43:24: 1-st clause of isRelDefGraph/1

*/
