/******************************************************************/
/* MULTAGNT.PRO                                                   */
/* Brazdil's Simulation of a tutoring setting between two agents  */
/******************************************************************/
/* impl. by     : Yiu Cheung HO                                   */
/*                Department of Computing                         */
/*                King's College London                           */
/*                1989                                            */
/*                                                                */
/*                Thomas Hoppe                                    */
/*                Mommsenstr. 50                                  */
/*                1000 Berlin 12                                  */
/*                F.R.G.                                          */
/*                E-Mail: hoppet@db0tui11.pro                     */
/*                1990                                            */
/*                                                                */
/* reference    : Transfer of Knowledge between Systems,      */
/*        Brazdil, P., Associacao Portuguesa para a Intel-*/
/*        igencia Artificial, Working Paper 87-1, Uni-    */
/*        versity of Porto, 1987.                 */
/*                                                                */
/*        Diagnosis of Misunderstanding, Yiu Cheung HO,   */
/*        Project Report, Final Year One Unit Project     */
/*        88/89, Department of Computing, King's College  */
/*        London, 1989.                   */
/*                                                                */
/*  call        : diagnosis                                       */
/*                                                                */
/******************************************************************/

:- dynamic(parent/2).

/******************************************************************/
/* YAP-, C- and M-Prolog specific declaration of dynamical        */
/* clauses.                                                       */
/******************************************************************/
:- dynamic db_entry/3.
:- dynamic def_theory/2.
:- dynamic digits_of_next_sym/1.
 
:- op(999,xfx,:).
:- op(998,xfx,'<-').

/******************************************************************/
/* User Interface                         */
/******************************************************************/
diagnosis :-
    init,
    get_teacher(teacher),
    nl, nl,
    get_learner,
    locate_error.
 
init :-
    abolish(db_entry,3),
    abolish(def_theory,2),
    multifile(db_entry/3),
    multifile(def_theory/2),
   dynamic(db_entry/3),
   dynamic(def_theory/2).

:- init.

:- [teacher].

:- [learner1].

locate_error :-
    repeat,
    mode(Mode),
    generate_error(Mode).
 
generate_error(manual) :-
    repeat,
    get_question(Question),
    process_question(Question),
    exit_manual,
    !, exit.
generate_error(auto) :-
    select_question(Question),
    process_question(Question),
    exit_auto,
    !, exit.
generate_error(_) :-
    exit.
 
process_question(Question) :-
    what_cannot_do(Ls,Ts,Question <- Answer,[],FaultyStep),
    output_error(Ls,Ts,Question <- Answer,FaultyStep), !.
process_question(Question) :-
    write(' *** The teacher cannot answer the question: '),
    write(Question), nl.
 
output_error(Tl,Tt,Question <- Answer,FaultyStep) :-
    nl,
    write(' Result of Diagnosis:'), nl,
    write(' --------------------'), nl, nl,
    write(' The query is:       '), write(Question),nl,
    write(' Teachers answer is: '), out_answer(Answer),nl,
    write(' Learners theory:    '), write(Tl),nl,
    write(' Teachers theory:    '), write(Tt),nl,
    write(' Faulty Steps:       '), out_faulty(FaultyStep), nl, !.

out_faulty([]) :-
    write('no faulty step'), nl.
out_faulty(Steps) :-
    out_step(Steps), nl.
 
out_step([]) :-
    nl.
out_step([Step|Steps]) :-
    write('                     '),
    write(Step), nl,
    write(Steps).
 
out_answer([]) :-
    write('true'), nl.
out_answer(Ans) :-
    out_ans(Ans).
 
out_ans([]) :-
    nl.
out_ans([val(Var,Val)|T]) :-
    write(Var = Val), nl,
    write('                     '),
    out_ans(T).
 
select_question(Question) :-
    generate_question(Question),
    yes_no(yes, 'confirm',Reply),
    Reply = yes.
select_question(_) :-
    write(' no more questions'), nl, !, fail.
 
generate_question(Question) :-
    db_entry(teacher:_,Question,_),
    make_ground_term(Question),
    nl,
    write(' Question generated: '),
    write(Question),
    nl.
 
get_question(Question) :-
    write(' Input question: '),
    read(Question).
 
mode(auto) :-
    nl, nl,
    yes_no(yes, ' Do you want the system to generate questions ? ',Reply),
    nl,
    Reply = yes,
    !.
mode(manual).
 
exit_manual :-
    yes_no(no, ' Exit manual mode ? ',Reply),
    Reply = yes.
 
exit_auto :-
    yes_no(no, ' Exit auto mode ? ',Reply),
    Reply = yes.
 
exit :-
    yes_no(no, ' Quit ? ',Reply),
    Reply = yes.
 
get_teacher(Teacher) :-
    yes_no(yes, ' Do you want to load the provided teacher KB ? ',Reply),
    load_knowledge_base(Reply,Teacher),
    knowledge_base_list(Reply,[],Teacher,FileList),
    yes_no(no, ' Do you want to load another teacher KB ? ',Reply2),
    more_knowledge(Reply2,FileList).
 
load_knowledge_base(no,_).
load_knowledge_base(yes,File) :-
    nl, consult(File), nl, !.
 
more_knowledge(no,[_|_]).
more_knowledge(no,[]) :-
    write(' *** You have not load any knowledge base yet !'), nl,
    more_knowledge(yes,[]).
more_knowledge(yes,FileList) :-
    repeat,
    ask_file(' Please input the filename of the KB: ',File),
    not_loaded(File,FileList,Load),
    load_knowledge_base(Load,File),
    knowledge_base_list(Load,FileList,File,NewList),
    yes_no(no, ' Do you want to consult more KBs ? ',Reply),
    more_knowledge(Reply,NewList).
 
not_loaded(File,List,no) :-
    member(File,List), !.
not_loaded(_,_,yes).

%yesno(Question):- yesno(Question,no).
%yesno(Question, Default):- format('~N~w? (~w): ',[Question,Default]),get_single_char(YN), (YN = 13 -> Default==yes; member(YN, `yY`)).

yes_no(Default, Message,Reply) :-
    repeat,
    write(' '),
    write(Message),
    (Default == yes -> write(' (Yes/no) ') ; write(' (yes/No) ')),
    get_single_char(In),
    ([In]=`e` -> (!,halt(4)) ; ([In]=`a` -> (!,abort) ;  (In = 13 ->  Reply = Default ; reply(In,Reply)))), !.
 
reply(Reply,yes) :-
    member(Reply,[yes,y,'yes.','y.'|`Yy`]).
reply(Reply,no) :-
    member(Reply,[no,n,'no.','n.'|`Nn`]).
 
ask_file(Message,File) :-
    repeat,
    write(' '),
    write(Message),
    read_in(File), !.
 
knowledge_base_list(yes,List,File,[File|List]).
knowledge_base_list(no,List,_,List).
 
no_knowledge([]) :-
    write(' *** You have not load any knowledge base yet !'), nl.
 
get_learner :-
    ask_file(' Please input the filename for the learner KB: ',File),
    load_knowledge_base(yes,File),
    knowledge_base_list(yes,[],File,List),
    yes_no(no,' Do you want to load another KB for the learner ? ',Reply),
    more_knowledge(Reply,List).
 
/******************************************************************/
/* Brazdil's predicates for evaluating the behavior of "LEARNER"  */
/* and "TEACHER".                         */
/******************************************************************/
can_do(learner:Tl,teacher:Tt,Question,TeachersAnswer) :-
    demo(learner:Tl,Question,LearnersAnswer),
    can_do_1(teacher:Tt,Question,TeachersAnswer,LearnersAnswer).
 
can_do_1(Teacher,Question,TeachersAnswer,LearnersAnswer) :-
    demo(Teacher,Question,TeachersAnswer),
    demo(Teacher,LearnersAnswer,TeachersAnswer),
    demo(Teacher,TeachersAnswer,LearnersAnswer).
 
cannot_do(learner:Tl,teacher:Tt,Question,TeachersAnswer) :-
    \+ demo(learner:Tl,Question,_LearnersAnswer),
    demo(teacher:Tt,Question,TeachersAnswer).
cannot_do(Learner,Teacher,Question,_) :-
    can_do(Learner,Teacher,Question,_), !, fail.
cannot_do(learner:Tl,teacher:Tt,Question,TeachersAnswer) :-
    demo(learner:Tl,Question,_),
% It seems that the condition LearnersAnswer <> TeachersAnswer is missing !
    demo(teacher:Tt,Question,TeachersAnswer).
 
what_cannot_do(_,_,'<-'(Q , _),_,_) :-
    \+ all_ground_term(Q),
    nl, write(' *** You asked a non ground question !'), nl, !, fail.
what_cannot_do(Ls,Ts,'<-'(Q , A),FaultyStep,FaultyStep) :-
    can_do(Ls,Ts,Q,A).
what_cannot_do(Ls,Ts,'<-'(Q , A),F1,['<-'(Q , A)|F1]) :-
    is_faulty_step(Ls,Ts,Q,A).
what_cannot_do(Ls,Ts,'<-'(Q , A),F1,F2) :-
    cannot_do(Ls,Ts,Q,A),
    demo_trace2(Ls,Ts,Q,A,SubSteps),
    what_cannot_do_list(Ls,Ts,SubSteps,F1,F3),
    faulty_step(Q,A,F1,F3,F2).
 
is_faulty_step(Ls,Ts,Q,A) :-
    cannot_do(Ls,Ts,Q,A), !,
    \+ demo_trace2(Ls,Ts,Q,A,_).
 
faulty_step(Q,A,F1,F1,['<-'(Q , A)|F1]).
faulty_step(_Q,_A,_F1,F3,F3).
 
what_cannot_do_list(_,_,[],F,F).
what_cannot_do_list(Ls,Ts,[Step1|RestSteps],F1,F3) :-
    what_cannot_do(Ls,Ts,Step1,F1,F2),
    what_cannot_do_list(Ls,Ts,RestSteps,F2,F3).
 
/******************************************************************/
/*                                                                */
/*  call        : demo(+Theory,+Goal,Conditions)          */
/*                                                                */
/*  arguments   : Theory     = ground term denoting a theory      */
/*                Goal       = ground term or list of ground terms*/
/*                Conditions = substitutions                      */
/*                                                                */
/*  property    : backtrackable                   */
/*                                                                */
/******************************************************************/
/* 'demo' is used to prove the Goal in the background of a Theory */
/* delivering a substitution in Conditions.                       */
/* Bindings of variables and values are explicitly maintained by  */
/* this implementation, thus any subterm of the form "variable(S)"*/
/* must actually be of the form "variabl(<atom>)" where <atom> is */
/* the name of a variable in the Goal.                            */
/* The substitutions in Condition may be either a variable  or a  */
/* list of terms which all have the form val(variable(X),Y) where */
/* X is the name of a variable (an atom) and Y is any term.       */
/* In the case Condition is uninstantiated, demo succeeds iff     */
/* Goal can be proven within the Theory. Condition is then instan-*/
/* tiated with the corresponding substitutions in the form        */
/* described above. On backtracking it will deliver the next      */
/* possible proof with the corresponding substitution, if it      */
/* exists.                            */
/* If Condition is instantiated in the form described above, demo */
/* succeeds, if Goal can be proven with the given substitution.   */
/******************************************************************/
demo(Theory,Goal,Conditions) :-
    var(Conditions),
    !,
    check_goal(Goal),
    copy_vars(Goal,LVars,Goal2,LVars2),
    !,
    show(Theory,Goal2),
    link_vals(LVars,LVars2,Conditions),
    make_ground_term(Conditions).
demo(Theory,Goal,Conditions) :-
    nonvar(Conditions),
    check_conditions(Conditions),
    check_goal(Goal),
    set_vars(Goal,Conditions,Goal2),
    copy_vars(Goal2,_,Goal3,LVars3),
    !,
    show(Theory,Goal3),
    no_new_values(LVars3),
    \+ identified_vars(LVars3),
    !.
 
/******************************************************************/
/* Brazdil's predicate for locating erroneous LEARNER's knowledge */
/******************************************************************/
demo_trace2(Ls,Ts,Goal,Conditions,Steps) :-
    set_vars(Goal,Conditions,Goal2),
    copy_vars(Goal2,_,Goal3,_),
    copy_vars(Goal2,_,Goal4,_),
    !,
    db_entry(Ls,Goal3,_),
    !,
    db_entry(Ts,Goal4,Body),
    make_ground_term(Body),
    set_vars(Body,Conditions,Body2),
    copy_vars(Body2,_,Body3,_),
    show(Ts,Body3),
    make_ground_term(Body3),
    trace_list(Body3,Steps).
 
trace_list([],[]).
trace_list([SubGoal|Rest],[SubGoal <- _|Steps]) :-
    trace_list(Rest,Steps).
 
/******************************************************************/
/*                                                                */
/*  call        : show(+Theory,+Goal)                 */
/*                                                                */
/*  arguments   : Theory     = ground term denoting a theory      */
/*                Goal       = ground term or list of ground terms*/
/*                                                                */
/*  property    : backtrackable                   */
/*                                                                */
/******************************************************************/
/* 'show' is nothing else than an Prolog meta-interpreter working */
/* in the traditional way, except that substitutions are explicit-*/
/* ly represented through terms in the form 'val(<var>,<term>)'.  */
/******************************************************************/
show(_,[]) :- !.
show(Th, not(G)) :-
    !,
    \+ show(Th,G).
show(Th, \+ G) :-
    !,
    \+ show(Th,G).
show(_Th,val(X,Y)) :-
    !,
    is_value(X,Y).
show(Th,[G|Gs]) :-
    !,
    show(Th,G),
    show(Th,Gs).
show(Th,G) :-
    db_entry(Th,G,B),
    show(Th,B).
show(Th,G) :-
    def_theory(Th,ThList),
    member(SubTh,ThList),
    show(SubTh,G).
show(_,G) :- predicate_property(G,built_in),!,call(G).
show(_,G) :- predicate_property(G,unknown),dynamic(G),fail.
show(_,G) :-
    \+ clause(G,_),    
    call(G), !.
 
is_value(X,Y) :-
    var(X), var(Y), !.
is_value(X,_) :-
    var(X), !, fail.
is_value(_,Y) :-
    var(Y), !, fail.
is_value(X,X) :-
    atomic(X), !.
is_value([Head1|Tail1],[Head2|Tail2]) :-
    !,
    is_value(Head1,Head2),
    is_value(Tail1,Tail2).
is_value(X,Y) :-
    \+ atomic(X),
    \+ atomic(Y),
    X =..[F|ArgsX],
    Y =..[F|ArgsY],
    !,
    is_value(ArgsX,ArgsY).
 
/******************************************************************/
/* Variable handling procedures                   */
/******************************************************************/
/*                                                                */
/*  call        : copy_vars(+G,+LVars,-G2,-LVars2)        */
/*                                                                */
/*  arguments   : G      = ground term                */
/*                LVars  = list of variables in G         */
/*                G2     = variablelized term             */
/*                LVars2 = list of variables in G2        */
/*                                                                */
/******************************************************************/
/* 'copy_vars' sets G2 to a copy of G with all variables of the   */
/* form 'variable(<name>)' replaced with uninstantiated Prolog    */
/* variables.                                                     */
/******************************************************************/
copy_vars(variable(G),[G],G2,[G2]).
copy_vars(G,[],G,[]) :-
    atomic(G).
copy_vars(G,LVars,G2,LVars2) :-
    G =.. [F|Args],
    copy_vars_list(Args,[],LVars,Args2,[],LVars2),
    G2 =.. [F|Args2].
 
copy_vars_list([],LVars,LVars,[],LVars2,LVars2).
copy_vars_list([A|As],PV,LV,[A2|A2s],PV2,LV2) :-
    copy_vars(A,AVL,A2,AVL2),
    join_vars(AVL,PV,PVplus,AVL2,PV2,PV2plus),
    copy_vars_list(As,PVplus,LV,A2s,PV2plus,LV2).
 
join_vars([],PV,PV,[],PV2,PV2).
join_vars([X|AVL],PVin,PVout,[X2|AVL2],PV2in,PV2out) :-
    twin_member(X,PVin,X2,PV2in),
    join_vars(AVL,PVin,PVout,AVL2,PV2in,PV2out).
join_vars([X|AVL],PVin,PVout,[X2|AVL2],PV2in,PV2out) :-
    join_vars(AVL,[X|PVin],PVout,AVL2,[X2|PV2in],PV2out).
 
twin_member(Var,[Var|_],Val,[Val|_]).
twin_member(Var,[_|Tail1],Val,[_|Tail2]) :-
    twin_member(Var,Tail1,Val,Tail2).
 
/******************************************************************/
/*                                                                */
/*  call        : link_vals(+LVars1,+LVars2,-Cond)        */
/*                                                                */
/*  arguments   : LVars1 = list of atomic ground terms        */
/*                LVars2 = list of terms (can be Prolog variables)*/
/*                Cond   = combined substitution          */
/*                                                                */
/******************************************************************/
/* 'link_vals' combines each corresponding varible name in LVars1 */
/* with its value in LVars2, to form a list of substituitions of  */
/* the form val(<var>,<term>).                                    */
/******************************************************************/
link_vals([X|LV],[X2|LV2],[val(variable(X),X2)|Conditions]) :-
    link_vals(LV,LV2,Conditions).
link_vals([],[],[]).
 
/******************************************************************/
/*                                                                */
/*  call        : set_vars(+Goal1,+Cond,-Goal2)           */
/*                                                                */
/*  arguments   : Goal1 = a ground goal               */
/*                Cond  = a substitution              */
/*                Goal2 = Goal1 with substituted variables        */
/*                                                                */
/******************************************************************/
/* 'set_vars' substitutes variables depicted by 'variable(<name>)'*/
/* in Goal1 by its value in Goal2, according to the substitution  */
/* Cond.                                                  */
/******************************************************************/
set_vars(Goal,[],Goal).
set_vars(Goal,[val(variable(Var),Val)|Rest],ResultGoal) :-
    atomic(Var),
    substitute(Goal,variable(Var),Val,Goal2),
    !,
    set_vars(Goal2,Rest,ResultGoal).
 
substitute(Var,Var,Val,Val).
substitute(Goal,_,_,Goal) :-
    atomic(Goal),
    !.
substitute([Arg|Tail],Var,Val,[NewArg|NewTail]) :-
    !,
    substitute(Arg,Var,Val,NewArg),
    substitute(Tail,Var,Val,NewTail).
substitute(Goal,Var,Val,FinalGoal) :-
    Goal =..[F|Args],
    substitute(Args,Var,Val,NewArgs),
    FinalGoal =..[F|NewArgs].
 
/******************************************************************/
/*                                                                */
/*  call        : all_ground_term(+Term)              */
/*                                                                */
/*  arguments   : Term = a Prolog term                */
/*                                                                */
/******************************************************************/
/* 'all_ground_term' succeeds if Term is ground, i.e. all vari-   */
/* ables are instantiated. Modification note: In YAP-Prolog       */
/*  all_ground_term(Term) :- ground(Term).            */
/* and in any other DEC10-Prolog dialects             */
/*  all_ground_term(Term) :- numbervars(Term,0,0).        */
/* can be used to speed up the system                 */
/******************************************************************/
all_ground_term(Variable) :-
    var(Variable), !, fail.
all_ground_term(Atomic) :-
    atomic(Atomic), !.
all_ground_term([Head|Tail]) :-
    !,
    all_ground_term(Head),
    all_ground_term(Tail).
all_ground_term(Structure) :-
    Structure =.. [_|Args],
    all_ground_term(Args).
 
check_goal(Goal) :-
    \+ all_ground_term(Goal),
    write(' *** Only ground terms in goal allowed !'), !, fail.
check_goal(Goal) :-
    \+ proper_variable(Goal),
    write(' *** <name> of any variable(<name>) should be atomic ground !'),
    !, fail.
check_goal(_).
 
proper_variable(Atom) :-
    atomic(Atom), !.
proper_variable(variable(Name)) :-
    \+ atomic(Name),
    write(' *** variable('), write(Name), write(') not atomic'), nl,
    !, fail.
proper_variable([Head|Tail]) :-
    !,
    proper_variable(Head),
    proper_variable(Tail).
proper_variable(Structure) :-
    Structure =.. [_|Args],
    proper_variable(Args).
 
check_conditions(Cond) :-
    \+ all_ground_term(Cond),
    write(' *** Only ground terms in conditions allowed !'), !, fail.
check_conditions(Cond) :-
    \+ proper_format(Cond),
    write(' *** Conditions should be either an uninstanziated variable'),
    nl,
    write('     or a list of structures, val(variable(<name>),<value>) !'),
    !, fail.
check_conditions(_).
 
proper_format([]).
proper_format([val(variable(Atom),_)|Tail]) :-
    atomic(Atom),
    proper_format(Tail).

make_ground_term(Body3):- make_ground_term(10, Body3).

make_ground_term(_D,Variable) :-
    var(Variable),
    new_symbol(X),
    Variable = variable(X), !.
make_ground_term(_D,Atom) :-
    atomic(Atom), !.
make_ground_term(D,_) :- D == 0, format(user_error,'~N~q~n',[make_ground_term(D,_)]) , !,fail.
make_ground_term(D,[Head|Tail]) :- % \+ is_list(Head),
    !, D2 is D - 1,
    make_ground_term(D2,Head),
    !,
    make_ground_term(D2,Tail).
make_ground_term(D,Structure) :- compound(Structure),!,
    D2 is D - 1,
    Structure =.. [_|Args],
    make_ground_term(D2,Args).
 
/******************************************************************/
/*                                                                */
/*  call        : no_new_values(+List)                */
/*                                                                */
/*  arguments   : List = a Prolog list                */
/*                                                                */
/******************************************************************/
/* 'no_new_values' succeeds if List is a list of uninstantiated   */
/* variables.                                                 */
/******************************************************************/
no_new_values([]).
no_new_values([X|Xs]) :-
    var(X),
    no_new_values(Xs).
 
/******************************************************************/
/*                                                                */
/*  call        : identified_vars(+List)              */
/*                                                                */
/*  arguments   : List = a list of variables              */
/*                                                                */
/******************************************************************/
/* 'identified_vars' succeeds if there exists at least one vari-  */
/* able in the List, which has been 'unified' with another vari-  */
/* able in the list.                                              */
/******************************************************************/
identified_vars([X|Xs]) :-
    member(Y,Xs),
    same_var(X,Y).
identified_vars([_|T]) :-
    identified_vars(T).
 
/******************************************************************/
/*                                                                */
/*  call        : same_var(+Var1,+Var2)               */
/*                                                                */
/*  arguments   : Var1 = a Prolog variable                */
/*                Var2 = a Prolog variable                */
/*                                                                */
/******************************************************************/
/* 'same_var' succeeds if Var1 and Var2 are unified, but uninstan-*/
/* tiated.                                                */
/******************************************************************/
same_var(dummy,Y) :-
    var(Y), !, fail.
same_var(X,Y) :-
    var(X), var(Y).
 
/******************************************************************/
/* Miscelenous predicates                     */
/******************************************************************/
digits_of_next_sym("1").
 
new_symbol(X) :-
    digits_of_next_sym(LN),
    revzap(LN,[],RLN),
    append("sym",RLN,LS),
    name(X,LS),
    inc_digits(LN,LN2),
    retract(digits_of_next_sym(LN)),
    assert(digits_of_next_sym(LN2)).
 
inc_digits([D1|LDT],[D2|LDT]) :-
    D1 <57, D2 is D1 + 1.
inc_digits([_|LDT],[48|LDT2]) :-
    inc_digits(LDT,LDT2).
inc_digits([],[49]).
 
revzap([H|T],V,R) :-
    revzap(T,[H|V],R).
revzap([],R,R).
 
read_in(W) :-
    ignore_space(C),
    rcl(C,L),
    extract_space(L,L1),
    convert(W,L1).
 
ignore_space(C) :-
    repeat,
    get0(C),
    non_space(C).
 
rcl(10,[]).
rcl(C1,[C1|P]) :-
    proper_char(C1),
    get0(C2),
    rcl(C2,P).
rcl(C1,[C1|P]) :-
    space(C1),
    get0(C2),
    rcl(C2,P).
rcl(_C1,L) :-
    put(7),
    get0(C2),
    rcl(C2,L).
 
convert([],[]).
convert(W,L) :-
    name(W,L).
 
non_space(C) :-
    space(C), !, fail.
non_space(10) :-
    !, fail.
non_space(C) :-
    proper_char(C).
non_space(_) :-
    put(7), !, fail.
 
space(32).
space(9).
 
proper_char(C) :-
    C > 32, C < 128.
 
extract_space(L,L2) :-
    reverse(L,R),
    delete_space(R,R2),
    reverse(R2,L2).
 
delete_space([S|T],L) :-
    space(S),
    delete_space(T,L).
delete_space(L,L).

reverse([],[]).
reverse([X|Y],Z) :-
    reverse(Y,Y1),
    append(Y1,[X],Z).

