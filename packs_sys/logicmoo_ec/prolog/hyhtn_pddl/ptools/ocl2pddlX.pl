% -----------ocl2pddl -----------
% OCL Pddl translator, for domain
% 11 May, 2000
% Donghong Liu
% Ron Simpson - Converted to work with Jasper and OCL Editor Tool
% January 2001
% University of Huddersfield

:- use_module(library(system)).

:- op(100,xfy,'=>').

convert:-
     produce_sort_info,
     produce_at_list,
     write('\(define \(domain '), % RMS Bug no : after define
     domain_name(Name),           % RMS Hope this will be the new standard
     write(Name),
     write('\)'),nl,
     write('  \(:requirements :strips :equality '),
     write(':typing :conditional-effects\)'),nl,
     do_predicates,
     do_action,
     write('\)'),nl,
     retractall(current_num(_,_)),
     retractall(atom_name_list(_,_)),
     retractall(sort_info(_,_)).
convert.

do_predicates:-
     write('  \(:predicates '),nl,
     predicates(Predls),
     tran_pred(Predls),
     write('\)'),nl,nl.
do_predicates.

do_action:-
     operator(Name,Prev,Nec,Cond),
     retractall(current_num(_,_)),
     Name=..[ActName|Varls],
     write('  \(:action '),
     write(ActName),nl,
     def_vars(Varls),
     tran_op(Prev,Nec,Cond,Parals,Parals2,Precon,PEffects,CEffects),
     write_parameters(Parals),
     write_preconditions(Precon),
     write_effects(PEffects,CEffects,Parals2),
     write('\)'),nl,
     fail.
%% RMS Bug bracket only closed at end of all actions (Corrected)
do_action:-          
     nl,nl.

tran_pred([]):-!.
tran_pred([HPred|Rest]):-
     retractall(current_num(_,_)),
     HPred=..[Name|Sortls],
     write('              \('),
     write(Name),
     write_pred_var(Sortls),
     write('\)'),nl,
     tran_pred(Rest),!.

write_parameters(Parals):-
     write('         :parameters \('),
     sort(Parals,S_Parals),
     write_para(S_Parals),
     write('\)'),nl,!.

write_preconditions(Precon):-
     length(Precon,Len),
     Len>1,
     write('         :precondition '),
     write('\(and '),nl,
     write_pred(Precon),
     write('\)'),nl,!.
write_preconditions(Precon):-
     write('         :precondition '),nl,
     write_pred(Precon),nl,!.

write_effects(PEffects,[],[]):-
     length(PEffects,Len),
     Len==1,
     write('         :effect '),nl,
     write_pred(PEffects),nl,!.
write_effects(PEffects,CEffects,Paras):-
     write('         :effect \(and '),
     write_pred(PEffects),nl,
     write_Ceffects(CEffects,Paras),
     write('\)'),nl,!.

write_Ceffects([],[]):-!.
write_Ceffects(Effects,Paras):-
     length(Effects,L),
     L>1,
     write('\(forall '),
     write('\('),write_para(Paras),write('\)'),nl,
     write('and\('),write_pred(Effects),write('\)'),
     write('\)'),nl,!.
write_Ceffects(Effects,Paras):-
     write('\(forall '),
     write('\('),write_para(Paras),write('\)'),nl,
     write_pred(Effects),
     write('\)'),nl,!.

tran_op(Prev,Nec,Cond,Parals,Parals2,Precon,PEffect,CEffect):-
     get_state(Prev,[],PStates),
     find_para(PStates,[],PPara),
     get_state(Nec,[],NStates),
     find_para(NStates,PPara,Parals),
     get_state(Cond,[],CStates),
     find_para(CStates,[],Parals1),
     subtract(Parals1,Parals,Parals2),
     conv_nec(NStates,[],Precon1,[],PEffect),
     conv_cond(CStates,[],CEffect),
     append(PStates,Precon1,Precon).

get_state([],States,States).
get_state([se(Sort,Obj,State)|Rest],List,States):-
     append(List,State,List1),
     get_state(Rest,List1,States).
get_state([sc(Sort,Obj,State)|Rest],List,States):-
     append(List,[[State]],List1),
     get_state(Rest,List1,States).

conv_nec([],Precon,Precon,Effect,Effect):-!.
conv_nec([[LHS=>RHS]|NStates],PList,Precon,EList,Effect):-
     compare_rhs_lhs(RHS,LHS,[],Add_ls,[],At_ls),
     del_list(LHS,RHS,[],Del_ls),
     append(PList,At_ls,PList1),
     append(PList1, LHS, PList2),
     append(EList,Add_ls,EList1),
     append(EList1,Del_ls,EList2),
     conv_nec(NStates,PList2,Precon,EList2,Effect),!.

conv_cond([],Effect,Effect):-!.
conv_cond([[LHS=>RHS]|CStates],List,Effect):-
     compare_rhs_lhs(RHS,LHS,[],Add_ls,[],At_ls),
     del_list(LHS,RHS,[],Del_ls),
     append(LHS,At_ls,Precon0),
     append(Add_ls,Del_ls,Effect0),
     append(List,[when(Precon0,Effect0)],List1),
     conv_cond(CStates,List1,Effect),!.
	
% compare rhs with lhs, if it already in lhs, skip it
% if it is in atomic invariant, separate it out so that it can be put into
% precondition later
% elst, it is in add list
compare_rhs_lhs([],LHS,Add_list,Add_list,At_list,At_list).
compare_rhs_lhs([HRHS|Rest],LHS,Adls,Add_list,Atls,At_list):-
     member(HRHS,LHS),
     compare_rhs_lhs(Rest,LHS,Adls,Add_list,Atls,At_list),!.
compare_rhs_lhs([HRHS|Rest],LHS,Adls,Add_list,Atls,At_list):-
     HRHS=..[Name|Vars],
     atom_name_list(_,Atnames),
     member(Name,Atnames),
     append(Atls,[HRHS],Atls1),
     compare_rhs_lhs(Rest,LHS,Adls,Add_list,Atls1,At_list),!.
compare_rhs_lhs([HRHS|Rest],LHS,Adls,Add_list,Atls,At_list):-
     append(Adls,[HRHS],Adls1),
     compare_rhs_lhs(Rest,LHS,Adls1,Add_list,Atls,At_list),!.

% del_list is if a state is in lhs but not in rhs
del_list([],RHS,Del_list,Del_list).
del_list([HLHS|Rest],RHS,Dels,Del_list):-
     member(HLHS,RHS),
     del_list(Rest,RHS,Dels,Del_list),!.
del_list([ne(_,_)|Rest],RHS,Dels,Del_list):-
     del_list(Rest,RHS,Dels,Del_list),!.
del_list([HLHS|Rest],RHS,Dels,Del_list):-
     HLHS=..[Name|Vars],
     atom_name_list(_,Atnames),
     member(Name,Atnames),
     del_list(Rest,RHS,Dels,Del_list),!.
del_list([HLHS|Rest],RHS,Dels,Del_list):-
     append(Dels,[not(HLHS)],Dels1),
     del_list(Rest,RHS,Dels1,Del_list),!.

find_para([],Parals,Parals).
find_para([ne(_,_)|Rest],List,Parals):-
    find_para(Rest,List,Parals).
find_para([not(_)|Rest],List,Parals):-
    find_para(Rest,List,Parals).
find_para([[Pre=>Eff]|Rest],List,Parals):-
    find_para(Pre,List,List1),
    find_para(Eff,List1,List2),
    find_para(Rest,List2,Parals).
find_para([HPred|Rest],List,Parals):-
    HPred=..[Name|Varls],
    sort_info(HPred, Is_sort_ls),
    def_vars_ls(Is_sort_ls),
    append(List,Is_sort_ls,List1),
    find_para(Rest,List1,Parals).

def_vars_ls([]):-!.
def_vars_ls([is_of_sort(Var,Sort)|Rest]):-
    var(Var),
    gensym(x,DVar),
    Var=DVar,
    def_vars_ls(Rest),!.
def_vars_ls([is_of_sort(Var,Sort)|Rest]):-
    def_vars_ls(Rest),!.

def_vars([]):-!.
def_vars([Var|Rest]):-
    gensym(x,DVar),
    Var=DVar,
    def_vars(Rest),!.

write_pred_var([]):-!.
write_pred_var([Sort|Rest]):-
     gensym(x,VSort),
     write(' ?'),write(VSort),
     write(' - '), write(Sort),
     write_pred_var(Rest),!.

write_para([]):-!.
write_para([is_of_sort(Var,Sort)|Rest]):-
     write(' ?'),write(Var),
     write(' - '), write(Sort),
     write_para(Rest),!.

write_pred([]):-!.
write_pred([not(Pred)|Rest]):-
      write('\(not '),
      write_pred([Pred]),
      write('\)'),nl,
      write_pred(Rest),!.
write_pred([ne(A,B)|Rest]):-
      write('\(not '),
      write('\(= '),
      write('?'), write(A), write(' ?'), write(B),
      write('\)\)'),
      write_pred(Rest),!.
write_pred([when(Pre,Eff)|Rest]):-
      length(Pre,Lenp),
      length(Eff,Lene),
      Lenp > 1,
      Lene > 1,
      write('\(when '),
      write('\(and '),
      write_pred(Pre),
      write('\)'),
      write('\(and '),
      write_pred(Eff),
      write('\)\)'),nl,
      write_pred(Rest),!.
write_pred([when(Pre,Eff)|Rest]):-
      length(Pre,Lenp),
      length(Eff,Lene),
      Lenp = 1,
      Lene = 1,
      write('\(when '),
      write_pred(Pre),
      write_pred(Eff),
      write('\)'),nl,
      write_pred(Rest),!.
write_pred([when(Pre,Eff)|Rest]):-
      length(Pre,Lenp),
      Lenp > 1,
      write('\(when '),
      write('\(and '),
      write_pred(Pre),
      write('\)'),
      write_pred(Eff),
      write('\)'),nl,
      write_pred(Rest),!.
write_pred([when(Pre,Eff)|Rest]):-
      length(Eff,Lene),
      Lene > 1,
      write('\(when '),
      write_pred(Pre),
      write('\(and '),
      write_pred(Eff),
      write('\)\)'),nl,
      write_pred(Rest),!.
write_pred([Pred|Rest]):-
      write('\('),
      Pred=..[Name|Varls],
      write(Name),write(' '),
      write_pred0(Varls),
      write('\)'),
      write_pred(Rest),!.

write_pred0([H|[]]):-
      write('?'), write(H),!.
write_pred0([H|T]):-
      write('?'), write(H), write(' '),
      write_pred0(T),!.



% ----------------------------------------------------------
domain(default).

not(X):- \+X.
emptylist([]).
member(X,[X|_]).
member(X,[_|L]) :- member(X,L).

append([], Z, Z):-! .
append([A|B], Z, C) :-
        not(not(member(A, Z))) ,
        append(B, Z, C),! .
append([A|B], Z, [A|C]) :-
        append(B, Z, C) .

% subtract(A,B,C): subtract B from A
% -------------------------------------
subtract([],_,[]):-!.
subtract([A|B],C,D) :-
        member(A,C),
        subtract(B,C,D),!.
subtract([A|B],C,[A|D]) :-
        subtract(B,C,D),!.

%.. sort_info(next(Box1,Box2),[is_of_sort(Box1,box),is_of_sort(Box2,box)]).
% from onprecede

produce_sort_info :-
        predicates(L),
        produce_sort_info(L),!.
produce_sort_info([]).
produce_sort_info([X|L]) :-
        generalise(X,X1),
        X =.. [H|T],
        X1 =.. [H|T1],
        produce_sort_ofs(T,T1,S),
        assert(sort_info(X1,S)),
        produce_sort_info(L).

generalise(X,X1) :-
        X =.. [H|T],
        genlist(T,T1),
        X1 =.. [H|T1],!.
genlist([],[]) :- !.
genlist([H|T],[H1|T1]) :-
        genlist(T,T1),!.

produce_sort_ofs([],[],[]).
produce_sort_ofs([T|L],[T1|L1],[is_of_sort(T1,T)|S1]) :-
        produce_sort_ofs(L,L1,S1),!.

% produce a atom_name_list(AtList, Atnames).

produce_at_list:-
  atomic_invariants(AtList),
  headls(AtList,[],Atnames),
  assert(atom_name_list(AtList,Atnames)),!.

% headls(List,[],Pnames) --use for make the list of predicates names 
% from a list of predicates

headls([],Hlist,Hlist).
headls([Hls|Tls],Hlist,PNamels):-
  Hls =.. [Pre|Rest],!,
  headls(Tls, [Pre|Hlist],PNamels).
headls([Hls|Tls],Hlist,PNamels):-
  headls(Tls, Hlist,PNamels),!.


/* generate symbol predicate  (from file futile)*/

gensym(Root,Atom) :-
                        getnum(Root,Num),
                        name(Root,Name1),
                        name(Num,Name2),
                        append(Name1,Name2,Name),
                        name(Atom,Name).

getnum(Root,Num) :-
                        retract(current_num(Root,Num1)),!,
                        Num is Num1+1,
                        asserta(current_num(Root,Num)).

getnum(Root,1) :- asserta(current_num(Root,1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% This is the Task Translation %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_p:-
     planner_task(Num,Goal,Ini),
     write('\(define \(problem '),
     write('task'),% RMS Not sure but I think this must be an identifier
     write(Num),
     write('\)'),nl,
     write('  \(:domain  '),
     domain_name(Name), % RMS Hope this will be the new standard
     write(Name),
     write('\)'),nl,
     write('  \(:objects '),nl,
     write_obj,
     write('\)'),nl,
     write_init(Ini),
     write_goal(Goal),
     write('\)'),nl,
     fail.

% Ron allow an individual selected task to be converted
convert_p_id(ID):-
	write(';'),write('taskid '),write(ID),nl,
     planner_task(ID,Goal,Ini),
     write('\(define \(problem '),
     write('task'),% RMS Not sure but I think this must be an identifier
     write(ID),
     write('\)'),nl,
     write('  \(:domain  '),
     domain_name(Name), % RMS Hope this will be the new standard
     write(Name),
     write('\)'),nl,
     write('  \(:objects '),nl,
     write_obj,
     write('\)'),nl,
     write_init(Ini),
     write_goal(Goal),
     write('\)'),nl,
     fail.

convert_p.

write_obj:-
     objects(Sort,Objs),
     write_obj1(Objs),
     write('- '),
     write(Sort),nl,
     fail.

write_obj.

write_obj1([]):-!.
write_obj1([HObj|Rest]):-
     write(HObj),
     write(' '),
     write_obj1(Rest),!.

write_init(Ini):-
     write('  \(:init '),
     atomic_invariants(Atom),
     write_preds(Atom),
     write_ss(Ini),
     write('\)'),nl,nl,!.

write_goal([se(_,_,Pred)]):-
     length(Pred,Len),
     Len = 1,
     write('  \(:goal '),nl,
     write_preds(Pred),
     write('\)'),nl,nl,!.
write_goal(Goal):-
     write('  \(:goal '),nl,
     write('\(and '),
     write_se(Goal),
     write('\)\)'),nl,nl,!.

write_preds([]):-!.
write_preds([HPred|Rest]):-
      write('\('),
      HPred=..[Name|Varls],
      write(Name),write(' '),
      write_preds1(Varls),
      write('\)'),nl,
      write_preds(Rest),!.

write_preds1([H|[]]):-
      write(H),!.
write_preds1([H|T]):-
      write(H), write(' '),
      write_preds1(T),!.

write_ss([]):-!.
write_ss([ss(_,_,Preds)|Ini]):-
      write_preds(Preds),
      write_ss(Ini),!.

write_se([]):-!.
write_se([se(_,_,Preds)|Goal]):-
      write_preds(Preds),
      write_se(Goal),!.

