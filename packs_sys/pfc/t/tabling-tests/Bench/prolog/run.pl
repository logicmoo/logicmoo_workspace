go:-
    open(res_bp,write,S),
    run(S).

ratios:-
    ratios(['../bp/res_bp',
	    '../eclipse/res_ecl',
	    '../gnu/res_gnu',
	    '../sics/res_sics',
	    '../swi/res_swi',
	    '../xsb/res_xsb'],
	   '../bp/res_bp').
	   
profile:-
    open(profile_bp_refs,write,S1),
    open(profile_bp_space,write,S2),
    start_click,
    not_not_dummy,
    access_counters(_,Dx,Dy,Dl,Dh,Dt,Dc),
    profile(S1,S2,Dx,Dy,Dl,Dh,Dt,Dc).
profile.

profile_one:-
    start_click,
    top,
    access_counters(I,X,Y,L,H,T,C),
    write(access_counters(I,X,Y,L,H,T,C)),nl.

profile(S1,S2,Dx,Dy,Dl,Dh,Dt,Dc):-
    programs(Ps),
    '$member'(PN,Ps),
    PN=(P,_),
    compile(P),
    load(P),
    start_click,
    not_not_top,
    access_counters(_,X,Y,L,H,T,C),
    Refer_x is X-Dx,
    Refer_y is Y-Dy,
    Local is L-Dl,
    Heap is H-Dh,
    Trail is T-Dt,
    Choice is C-Dc,
    _xy is Refer_x + Refer_y,
    _x2y is Refer_x + Refer_y*2,
    _x3y is Refer_x + Refer_y*3,
    _x4y is Refer_x + Refer_y*4,
    write(S1,P),write(S1,'('),
    write_ln(S1,[Refer_x,Refer_y,_xy,_x2y,_x3y,_x4y],',',').'),
    write(S2,P),write(S2,'('),
    Control is Local+Choice,
    write_ln(S2,[Control,Heap,Trail],',',').'),
    fail.
profile(S1,S2,Dx,Dy,Dl,Dh,Dt,Dc):-close(S1),close(S2).


all_profile:-
    programs(Ps),
    start_click,
    '$member'((P,N),Ps),
    load(P),
    not_not_top,
    fail.
all_profile:-
    print_counters.

compall:-
    programs(Ps),
    '$member'((P,N),Ps),
    compile(P),
    fail.
compall.

run(S):-
    programs(Ps),
    '$member'((P,N),Ps),
    run_program(P,N,S),
    fail.
run(S):-close(S).

run_program(P,N,S):-
%    compile(P),
    load(P),
    ntimes(N,T),!,
    write(S,P),write(S,'('), T1 is T/N, write(S,T1), write(S,').'),nl(S).

ntimes(N,T):-
    statistics(runtime,_),
    ntimes(N),
    statistics(runtime,[_,T1]),
    ntimes_dummy(N),
    statistics(runtime,[_,T2]),
    T is T1-T2.
    
ntimes(N):-N=:=0,!.
ntimes(N):-not_not_top, !, N1 is N-1,ntimes(N1).

ntimes_dummy(N):-N=:=0,!.
ntimes_dummy(N):-not_not_dummy, !, N1 is N-1,ntimes_dummy(N1).

not_not_top:-not_top,!,fail.
not_not_top.

not_top:-top,!,fail.
not_top.

not_not_dummy:-not_dummy,!,fail.
not_not_dummy.

not_dummy:-dummy,!,fail.
not_dummy.

dummy.

ratios([F|Fs],F2):-!,
    readall(F,Progs,_),
    ratios([F|Fs],F2,List),
    print_ratios(Progs,List).
ratios(F1,F2):-
    readall(F1,Progs,_),
    ratios([F1],F2,List),
    print_ratios(Progs,List).

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

programs(Ps):-
    Ps=[(boyer,100),
	 (browse,100),
	 (chat_parser,100),
	 (crypt,1000),
	 (fast_mu,10000),
	 (flatten,1000),
	 (meta_qsort,500),
	 (mu,1000),
	 (nreverse,10000),
	 (poly_10,100),
	 (prover,1000),
	 (qsort,10000),
	 (queens_8,500),
	 (query,1500),
	 (reducer,500),
	 (sendmore,500),
	 (simple_analyzer,10),
	 (tak,100),
	(zebra,500)].

