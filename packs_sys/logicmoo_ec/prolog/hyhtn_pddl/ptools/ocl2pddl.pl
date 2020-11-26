/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the names of
 * the authors or their employers not be used in advertising or publicity 
 * pertaining to distribution of the software without specific, written 
 * prior permission.
 *
 * The authors and their employers disclaim all warranties with regard to 
 * this software, including all implied warranties of merchantability and 
 * fitness.  In no event shall the authors or their employers be liable 
 * for any special, indirect or consequential damages or any damages 
 * whatsoever resulting from loss of use, data or profits, whether in an 
 * action of contract, negligence or other tortious action, arising out of 
 * or in connection with the use or performance of this software.
 */
				% -----------ocl2pddl.pl -----------
% OCL Pddl translator
% 11 May, 2000
% Donghong Liu
% Ron Simpson - Converted to work with Jasper and OCL Editor Tool
% January 2001
% University of Huddersfield
% revised by DL, September 2001
% revised by DL, Feb 2002
% revised by Ron, March 2002

:- unknown(error,fail).
:- use_module(library(system)).

:- op(100,xfy,'=>').

conv:-
	produce_sort_info,
     produce_at_list,
     write('\(define \(domain '), % RMS Bug no : after define
     domain_name(Name),           % RMS Hope this will be the new standard
     write(Name),
     write('\)'),nl,
     write('  \(:requirements :strips :equality '),
     write(':typing :conditional-effects\)'),nl,
     do_types,                    % RMS dealing with hierarchical types
     do_predicates.
	

convert:-
     produce_sort_info,
     produce_at_list,
     write('\(define \(domain '), % RMS Bug no : after define
     domain_name(Name),           % RMS Hope this will be the new standard
     write(Name),
     write('\)'),nl,
     write('  \(:requirements :strips :equality '),
     write(':typing :conditional-effects\)'),nl,
     do_types,                    % RMS dealing with hierarchical types
     do_predicates,
     do_action,
     write('\)'),nl,
     retractall(current_num(_,_)),
     retractall(atom_name_list(_,_)),
     retractall(sort_info(_,_)).
convert:-
%     produce_sort_info,
%     produce_at_list,
%     write('\(define \(domain '), % RMS Bug no : after define
%     domain_name(Name),           % RMS Hope this will be the new standard
     write('default'), %when domain name not defined
     write('\)'),nl,
     write('  \(:requirements :strips :equality '),
     write(':typing :conditional-effects\)'),nl,
     do_types,                     %RMS dealing with hierarchical type
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
     tab(3),                         % Ron
     write('\)'),nl,nl.
do_predicates.

do_act:-
	retractall(const(_)),
     assert(const([])),
     Name = fetch_jack(A,B),
     operator(Name,Prev,Nec,Cond),
     write(Name),nl,
     retractall(current_num(_,_)),
     Name=..[ActName|Varls],
     nl,nl,write('  \(:action '),
     write(ActName),nl,
     def_vars(Varls),
     do_tran_op(Prev,Nec,Cond,Parals,Precon,Effects).
	

do_action:-
     retractall(const(_)),
     assert(const([])),
     operator(Name,Prev,Nec,Cond),
     retractall(current_num(_,_)),
     Name=..[ActName|Varls],
     nl,nl,write('  \(:action '),
     write(ActName),nl,
     def_vars(Varls),
     tran_op(Prev,Nec,Cond,Parals,Precon,Effects),
     write_parameters(Parals),
     write_preconditions(Precon),
     write_effects(Effects),
     tab(3),                       % Ron
     write('\)'),nl,
     fail.
%% RMS Bug bracket only closed at end of all actions (Corrected)
do_action:-
    retractall(const(_)),
    retractall(current_num(_,_)),!.

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
     write_pred(Precon,14,true),
     tab(14),                          % Ron
     write('\)'),nl,!.
write_preconditions(Precon):-
     write('         :precondition '),nl,
     write_pred(Precon,14,true),nl,!.

write_effects(Effects):-
     length(Effects,Len),
     Len==1,
     write('         :effect '),nl,
     write_pred(Effects,14,true),!.
write_effects(Effects):-
     write('         :effect \(and '),nl,
     write_pred(Effects,14,true),
     tab(14),                         % Ron
     write('\)'),!.

do_tran_op(Prev,Nec,Cond,Parals,Precon,Effect):-
	get_state(Prev,[],PStates),
	write(PStates),nl,
	find_para(PStates,[],PPara),
	write(PPara),nl,
	get_state(Nec,[],NStates),
	write(NStates),nl,
        find_para(NStates,PPara,Parals),
	write(Parals),nl.


tran_op(Prev,Nec,Cond,Parals,Precon,Effect):-
     get_state(Prev,[],PStates),
     find_para(PStates,[],PPara),
     get_state(Nec,[],NStates),
     find_para(NStates,PPara,Parals),
     get_state(Cond,[],CStates),
     conv_nec(Nec,[],Precon1,[],PEffect),
     extend_full(Nec,Parals,[],CEffect0),
     conv_cond(Cond,Parals,CEffect0,CEffect),
     append(PStates,Precon1,Precon),
     append(PEffect,CEffect,Effect).

get_state([],States,States).
get_state([se(Sort,Obj,State)|Rest],List,States):-
     append(List,State,List1),
     get_state(Rest,List1,States).
get_state([sc(Sort,Obj,State)|Rest],List,States):-
     append(List,[[State]],List1),
     get_state(Rest,List1,States).

conv_nec([],Precon,Precon,Effect,Effect):-!.
conv_nec([sc(Sort,Obj,LHS=>RHS)|NStates],PList,Precon,EList,Effect):-
     compare_rhs_lhs(RHS,LHS,[],Add_ls,[],At_ls),
     del_list(LHS,RHS,[],Del_ls),
     append(PList,At_ls,PList1),
     append(PList1, LHS, PList2),
     append(EList,Add_ls,EList1),
     append(EList1,Del_ls,EList2),
     conv_nec(NStates,PList2,Precon,EList2,Effect),!.

extend_full([],Parals,CEffect,CEffect):-!.
extend_full([sc(Sort,Obj,LHS=>RHS)|NStates],Parals,CList,CEffect):-
     substate_classes(Sort,Obj,SS),
     extend_full1(SS,LHS,RHS,Parals,CList,CList1),
     extend_full(NStates,Parals,CList1,CEffect),!.

extend_full1([],LHS,RHS,Parals,CEffect,CEffect):-!.
extend_full1([FSS|SS],LHS,RHS,Parals,CList,CEffect):-
     is_subset(LHS,FSS),
     find_diff(FSS,LHS,Diff),
     not(emptylist(Diff)),
     find_para(Diff,Parals,Para),
     subtract(Para,Parals,Parals1),
     del_list(Diff,RHS,[],CDel_ls),
     append_ext(CList,Parals,Parals1,Diff,CDel_ls,CList1),
     extend_full1(SS,LHS,RHS,Parals,CList1,CEffect),!.
extend_full1([FSS|SS],LHS,RHS,Parals,CList,CEffect):-
     extend_full1(SS,LHS,RHS,Parals,CList,CEffect),!.

append_ext(List,Parals,Parals1,Pre,[],List):-!.
append_ext(List,Parals,[],Pre,CDel_ls,List1):-
     append(List,[when(Pre,CDel_ls)],List1),!.
append_ext(List,Parals,Parals1,Pre,CDel_ls,List1):-
     binding_var(Parals,Parals1,[],NEs),
     append(Pre,NEs,Pre1),
     append(List,[forall(Parals1,when(Pre1,CDel_ls))],List1),!.

binding_var(Parals,[],NEs,NEs):-!.
binding_var(Parals,[is_of_sort(Var,Sort)|Par1],Ls,NEs):-
     binding_var1(Parals,Var,Sort,Ls,Ls1),
     binding_var(Parals,Par1,Ls1,NEs),!.

binding_var1([],Var,Sort,Ls,Ls):-!.
binding_var1([is_of_sort(Var1,Sort)|Par],Var,Sort,Ls,[ne(Var1,Var)|Ls1]):-
    binding_var1(Par,Var,Sort,Ls,Ls1),!.
binding_var1([A|Par],Var,Sort,Ls,Ls1):-
    binding_var1(Par,Var,Sort,Ls,Ls1),!.

conv_cond([],Parals,Effect,Effect):-!.
conv_cond([sc(Sort,Obj,LHS=>RHS)|CStates],Parals,List,Effect):-
     substate_classes(Sort,Obj,SS),
     conv_cond1(SS,LHS,RHS,Parals,List,List1),
     conv_cond(CStates,Parals,List1,Effect),!.

conv_cond1([],LHS,RHS,Parals,List,List):-!.
conv_cond1([FSS|SS],LHS,RHS,Parals,List,Effect):-
     is_subset(LHS,FSS),
     compare_rhs_lhs(RHS,FSS,[],Add_ls,[],At_ls),
     del_list(FSS,RHS,[],Del_ls),
     append(FSS,At_ls,Precon0),
     append(Add_ls,Del_ls,Effect0),
     find_para(Precon0,[],Parals1),
     find_para(Effect0,Parals1,Parals2),
     subtract(Parals2,Parals,Parals3),
     append(List,[forall(Parals3,when(Precon0,Effect0))],List1),
     conv_cond1(SS,LHS,RHS,Parals,List1,Effect),!.
conv_cond1([FSS|SS],LHS,RHS,Parals,List,Effect):-
     conv_cond1(SS,LHS,RHS,Parals,List,Effect),!.

	
% compare rhs with lhs, if it already in lhs, skip it
% if it is in atomic invariant, separate it out so that it can be put into
% precondition later
% elst, it is in add list
compare_rhs_lhs([],LHS,Add_list,Add_list,At_list,At_list).
compare_rhs_lhs([ne(A,B)|Rest],LHS,Adls,Add_list,Atls,At_list):-
     append(Atls,[ne(A,B)],Atls1),
     compare_rhs_lhs(Rest,LHS,Adls,Add_list,Atls1,At_list),!.
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
    def_vars_ls(Is_sort_ls,Is_sort_ls1),
    append(List,Is_sort_ls1,List1),
    find_para(Rest,List1,Parals).

def_vars_ls([],[]):-!.
def_vars_ls([is_of_sort(Var,Sort)|Rest],[is_of_sort(DVar,Sort)|Rest1]):-
    var(Var),
    gensym(x,DVar),
    Var=DVar,
    def_vars_ls(Rest,Rest1),!.
def_vars_ls([is_of_sort(Var,Sort)|Rest],Rest1):-
    objects(Sort,Objls),
    member(Var,Objls),
    retract(const(Conls)),
    assert(const([Var|Conls])),    
    def_vars_ls(Rest,Rest1),!.
def_vars_ls([is_of_sort(Var,Sort)|Rest],[is_of_sort(Var,Sort)|Rest1]):-
    def_vars_ls(Rest,Rest1),!.

def_vars([]):-!.
def_vars([Var|Rest]):-
    gensym(x,DVar),
    Var=DVar,
    def_vars(Rest),!.

write_pred_var([]):-!.
write_pred_var([Sort|Rest]):-
     gensym(x,VSort),
     write(' ?'),write(VSort),
     write(' - '),write_sort(Sort),
%     write(' - '), write(Sort),
     write_pred_var(Rest),!.

% Ron deal with hierarchical sorts
do_types :-
	write('(:types '),
	sorts(primitive_sorts,Sorts),
	write_sub_list(Sorts),
	write(')'),
	nl.
	
write_sort(Sort) :-
	sorts(Sort,Subs),
	!,
	collect_subsorts(Subs,[],AllSubs),
	write('(either '),
	write_sub_list(AllSubs),
	write(') ').
	
write_sort(Sort) :-
	write(Sort).

collect_subsorts([],SoFar,SoFar).
collect_subsorts([Sort|RestSorts],SoFar,Final) :-
	sorts(Sort,NewSubs),
	!,
	collect_subsorts(NewSubs,[],NewSubsSubs),
	append(SoFar,NewSubsSubs,NewSoFar),
	collect_subsorts(RestSorts,NewSoFar,Final).
collect_subsorts([Sort|RestSorts],SoFar,Final) :-
	collect_subsorts(RestSorts,[Sort|SoFar],Final).

write_sub_list([]).
write_sub_list([H]) :-
	write(H).
write_sub_list([H|T]) :-
	write(H),
	write(' '),
	write_sub_list(T).

% Ron End deal with hierarchical sorts

write_para([]):-!.
write_para([is_of_sort(Var,Sort)|Rest]):-
     write(' ?'),write(Var),
     write(' - '), write(Sort),
     write_para(Rest),!.

write_pred([],_,_):-!.
write_pred([not(Pred)|Rest],Indent,NL):-
      tab(Indent),                      % Ron
      write('\(not '),
      write_pred([Pred],0,false),
      write('\)'),
      newline(NL),                                % Ron
      write_pred(Rest,Indent,NL),!.
write_pred([when(Pre,Eff)|Rest],Indent,NL):-
      tab(Indent),                      % Ron
      write_cpred(when(Pre,Eff)),
      write_pred(Rest,Indent,NL),!.
write_pred([ne(A,B)|Rest],Indent,NL):-
      tab(Indent),                      % Ron
      write('\(not '),
      write('\(= '),
      write('?'), write(A), write(' ?'), write(B),
      write('\)\)'), 
      newline(NL),                                % Ron
      write_pred(Rest,Indent,NL),!.
write_pred([forall(Para,CEffect)|Rest],Indent,NL):-
      tab(Indent),                      % Ron
      write('\(forall \('),
      write_para(Para),
      write('\) '),nl,
      write_cpred(CEffect),
      write('\)'),
      newline(NL),                                % Ron
      write_pred(Rest,Indent,NL),!.
write_pred([Pred|Rest],Indent,NL):-
      tab(Indent),                      % Ron
      write('\('),
      Pred=..[Name|Varls],
      write(Name),write(' '),
      write_pred0(Varls),
      write('\)'),
      newline(NL),                                % Ron
      write_pred(Rest,Indent,NL),!.

write_pred0([H|[]]):-
      const(Ls),
      member(H,Ls),
      write(H),!.
write_pred0([H|[]]):-
      write('?'), write(H),!.
write_pred0([H|T]):-
      const(Ls),
      member(H,Ls),
      write(H), write(' '),
      write_pred0(T),!.
write_pred0([H|T]):-
      write('?'), write(H), write(' '),
      write_pred0(T),!.

write_cpred(when(Pre,Eff)):-
      length(Pre,Lenp),
      length(Eff,Lene),
      Lenp > 1,
      Lene > 1,
      write('\(when '),
      write('\(and '),
      write_pred(Pre,18,true),
      write('\)'),nl,
      write('\(and '),
      write_pred(Eff,18,true),
      write('\)\)'),nl,!.
write_cpred(when(Pre,Eff)):-
      length(Pre,Lenp),
      length(Eff,Lene),
      Lenp = 1,
      Lene = 1,
      write('\(when '),
      write_pred(Pre,18,true),nl,
      write_pred(Eff,18,true),
      write('\)'),nl.
write_cpred(when(Pre,Eff)):-
      length(Pre,Lenp),
      Lenp > 1,
      write('\(when '),
      write('\(and '),
      write_pred(Pre,18,true),
      write('\)'),nl,
      write_pred(Eff,18,true),
      write('\)'),nl,!.
write_cpred(when(Pre,Eff)):-
      length(Eff,Lene),
      Lene > 1,
      write('\(when '),
      write_pred(Pre,18,true),nl,
      write('\(and '),
      write_pred(Eff,18,true),
      write('\)\)'),nl,!.

% Ron
newline(true) :- nl,!.
newline(_) :- !.

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

produce_at_list.

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
% Ron 5/10/01 Deal with domains without atomic_invariants
write_init(Ini):-
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


% succeeds if first arg is a subset of second
is_subset([ne(_,_)|Set],Set2) :-
     is_subset(Set,Set2).
is_subset([S|Set],Set2) :-
     member(S,Set2),
     is_subset(Set,Set2).
is_subset([S|Set],Set2) :-
     atomic_invariants(AtList),
     member(S,AtList),
     is_subset(Set,Set2).
is_subset([],_).

% Diff = List1 - List2,
% List2 is a subset of List1 except atoms and nes 
find_diff(List1,List2,Diff):-
	is_statics(List2),
	take_out_st(List1,Diff),!.
find_diff([A|List1],List2,Diff):-
	is_statics(A),
	find_diff(List1,List2,Diff),!.
find_diff([A|List1],List2,Diff):-
	take_out(List2,A,List21),
	find_diff(List1,List21,Diff),!.

% take_out(A,B,C):  take out B from A
% B must exist in A
% -------------------------------------
take_out([B|TA],B,TA):-!.
take_out([A|TA],B,[A|C]) :-
        take_out(TA,B,C),!.

% take_out_st(A,B):  take out statics from A
% -------------------------------------
take_out_st([],[]):-!.
take_out_st([A|TA],B) :-
	is_statics(A),
        take_out_st(TA,B),!.
take_out_st([A|TA],[A|B]) :-
        take_out_st(TA,B),!.

% check if a list or predicate is statics or not
is_statics([]):-!.
is_statics([A|List]):-
   is_statics(A),
   is_statics(List),!.
is_statics(ne(A,B)):-!.
is_statics(is_of_sort(A,B)):-!.
is_statics(is_of_primitive_sort(A,B)):-!.
is_statics(Pred):-
    functor(Pred,FF,NN),
    functor(Pred1,FF,NN),
    atomic_invariants(Atom),
    member(Pred1,Atom),!.
