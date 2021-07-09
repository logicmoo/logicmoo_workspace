
:-set_prolog_flag(singleton,off).
:-set_prolog_flag(redefined,off).
:- style_check(-singleton).

programs(Ps):-
    Ps=[
    (tcl,10),
    (tcr,10),
    (tcn,10),
    (sgm,5),
    (cs_o,100),
    (cs_r,100),
    (disj,100),
    (gabriel,100),
    (kalah,10),
    (pg,10),
    (peep,50),
    (read,100),
    (atr2,1)].

count:-
    programs(Ps),
    '$member'((P,_),Ps),
    load(P),
    initialize_table,
    top,
    write(P), write(' & '),
    print_table_statistics,
    fail.
count.

    
go(Suite):-
    name(Suite,String),
    myappend("res_bp_",String,FileNameString),
    name(FileName,FileNameString),
    open(FileName,write,S),!,
    run(Suite,S).

go:-
    go(programs).

go(Suite):-
    name(Suite,String),
    myappend("res_bp_",String,FileNameString),
    name(FileName,FileNameString),
    open(FileName,write,S),
    run(Suite,S).

ratios:-
    ratios([res_bp_all,
	    res_bp_clause,
	    res_bp_answer,
	    res_bp_subgoal,
	    res_bp_copy,
	    res_bp_auto
	    ],
	    res_bp_all).

profile_all:-
    tell(res_bp_space),
    programs(Ps),
    '$member'(PN,Ps),
    PN=(P,_),
    initialize_table,
    not(not(profile_one(P))),
    fail.
profile_all:-told.

profile_one(P):-
    load(P),
    start_click,
    top,
    access_counters(I,X,Y,L,H,Trail,C,Tab),
    StackSpace is 4*(L+H+Trail),
    TableSpace is 4*Tab,
    TotalSpace is StackSpace+TableSpace,
%    Term=..[P,StackSpace,TableSpace,TotalSpace],
    Term=..[P,StackSpace],
%    Term=..[P,L,H,Trail],
    write(Term),write('.'),nl.

profile_one:-
    start_click,
    top,
    access_counters(I,X,Y,L,H,Trail,C,Tab),
    StackSpace is 4*(L+H+Trail),
    TableSpace is 4*Tab,
    TotalSpace is StackSpace+TableSpace,
    write(access_counters(StackSpace,TableSpace,TotalSpace)),nl.

compall:-
    programs(Ps),
    '$member'((P,N),Ps),
    not(not(compile(P))),
    fail.
compall.

run(Suite,S):-
    Call=..[Suite,Ps],
    call(Call),
    '$member'((P,N),Ps),
    run_program(P,N,S),
    fail.
run(Suite,S):-close(S).

run_program(P,N,S):-
%    compile(P),
		load(P),!,
    initialize_table,!,
    ntimes(N,T),!,
    write(S,PF),write(S,'('), T1 is T/N, write(S,T1), write(S,').'),nl(S).

ntimes(N,T):-
    statistics(runtime,_),
    ntimes(N),
    statistics(runtime,[_,T1]),
    ntimes_dummy(N),
    statistics(runtime,[_,T2]),
    T is T1-T2.
    
ntimes(N):-N=:=0,!.
ntimes(N):-initialize_table,not_not_top, !, N1 is N-1,ntimes(N1).

ntimes_dummy(N):-N=:=0,!.
ntimes_dummy(N):-initialize_table,not_not_dummy, !, N1 is N-1,ntimes_dummy(N1).

not_not_top:-not_top,!,fail.
not_not_top.

not_top:-initialize_table,top,!,fail.
not_top.

not_not_dummy:-not_dummy,!,fail.
not_not_dummy.

not_dummy:-initialize_table,dummy,!,fail.
not_dummy.

dummy.

combine(Fs):-
    combine(Fs,[]).
combine(Fs).

combine([],L):-
    member(X,L),write(X),write('.'),nl,fail.
combine([F|Fs],L0):-
    readall(F,L,_),
    combine(L0,L,L1),
    combine(Fs,L1).

combine([],L,L).
combine([E1|L1],[E2|L2],[E3|L3]):-
    E1=..[F|Args1],
    E2=..[F|Args2],
    myappend(Args1,Args2,Args3),
    E3=..[F|Args3],
    combine(L1,L2,L3).

ratios([F|Fs],F2):-!,
    readall(F,Progs,_),
    ratios([F|Fs],F2,List),
    print_ratios(Progs,List).
ratios(F1,F2):-
    ratios([F1],F2).


ratios([],F2,[]).
ratios([F|Fs],F2,[ratio(Ratios,Mean)|List]):-
    ratios(F,F2,Ratios,Mean),
    ratios(Fs,F2,List).
    
ratios(F1,F2,Ratios,Mean):-
    readall(F1,L1,_),
    readall(F2,L2,_),
    compute_ratios(L1,L2,Ratios),
    sum_list(Ratios,Sum),
    length(Ratios,N),
    Mean is Sum/N.

readall(F,L,N):-
    see(F),
    read(X),
    functor(X,_,N),
    read_data(X,L).

read_data(end_of_file,L):-!,L=[],seen.
read_data(X,L):-
    L=[X|L1],
    read(NewX),
    read_data(NewX,L1).

compute_ratios([],[],[]).
compute_ratios([X|Xs],[Y|Ys],[Z|Zs]):-
    arg(1,X,T1),
    arg(1,Y,T2),
    Z is T1/T2,
    compute_ratios(Xs,Ys,Zs).

print_ratios(Progs,List):-
    print_ratios(Progs,List,1),
    format("<mean> ~t &",[]),
    print_mean(List).

print_ratios([],List,I).
print_ratios([Prog|Progs],List,I):-
    functor(Prog,Name,_),
    format(" ~w ~t &",Name),
    print_ratios_aux(List,I),
    nl,
    I1 is I+1,
    print_ratios(Progs,List,I1).
    
print_ratios_aux([],I).
print_ratios_aux([ratio(Ratios,Mean)|List],I):-
    nth(Ratios,I,Ratio),
    format("~2f &",[Ratio]),
    print_ratios_aux(List,I).

print_mean([]).
print_mean([ratio(Ratios,Mean)|List]):-
    format("~2f &",Mean), 
    print_mean(List).

nth([X|Xs],1,X):-!.
nth([X|Xs],I,Y):-
    I1 is I-1,
    nth(Xs,I1,Y).

sum_list(L,Sum):-
    sum_list(L,0,Sum).

sum_list([],Sum,Sum).
sum_list([X|Xs],Sum0,Sum):-
    Sum1 is Sum0+X,
    sum_list(Xs,Sum1,Sum).


myappend([],X,X).
myappend([X|Xs],Ys,[X|Zs]):-!,myappend(Xs,Ys,Zs).
myappend(X,B,C):- name(X,A),myappend(A,B,C).

'$member'(X,[X1|Xs]):-
    X=X1.
'$member'(X,[X1|Xs]):-
    '$member'(X,Xs).

write_ln(S,[],Dem,End):- write(S,End),nl(S).
write_ln(S,[X],Dem,End):-!,write_item(S,X),write(S,End),nl(S).
write_ln(S,[X|Xs],Dem,End):-write_item(S,X),write(S,Dem), write_ln(S,Xs,Dem,End).

write_item(S,X):-atom(X),!,atom_length(X,N),
    Tab is 20-N,
    write(S,X),spaces(S,Tab).
write_item(S,X):-write(S,X).

spaces(S,Tab):-Tab>0,!,write(S,' '),Tab1 is Tab-1,spaces(S,Tab1).
spaces(S,Tab).


initialize_structure(Sum,N0,N,Value):-
    N0=<N,!,
    arg(N0,Sum,Value),
    N1 is N0+1,
    initialize_structure(Sum,N1,N,Value).
initialize_structure(Sum,N0,N,Value).

    
summary:-
    command(system('cat profile_sp_refs')),
    command(system('cat profile_bp_refs')),
    command(ratios(profile_sp_refs,profile_bp_refs)),

    command(system('cat profile_sp_space')),
    command(system('cat profile_bp_space')),
    command(ratios(profile_sp_space,profile_bp_space)),
    
    command(system('cat res_sp_cc_orchid')),
    command(system('cat res_bp_cc_orchid')),
    command(ratios(res_sp_cc_orchid,res_bp_cc_orchid)),

    command(system('cat res_sp_cc_maroon')),
    command(system('cat res_bp_cc_maroon')),
    command(ratios(res_sp_cc_maroon,res_bp_cc_maroon)),

    command(system('cat res_sp_nc_orchid')),
    command(system('cat res_bp_cc_orchid')),
    command(ratios(res_sp_nc_orchid,res_bp_cc_orchid)),

    command(system('cat res_sp_nc_maroon')),
    command(system('cat res_bp_cc_maroon')),
    command(ratios(res_sp_nc_maroon,res_bp_cc_maroon)).

command(X):-nl,write(X),nl,
    call(X).

:- go.
