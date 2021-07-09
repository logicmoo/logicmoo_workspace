% a script for benchmarking, by Neng-Fa Zhou

programs(Ps):-
    Ps=[
	(alpha,10),
	(bridge,10),
	(cars,1000),
	(color,10),
	(eq10,1000),
	(eq20,1000),
	(magic3,10000),
	(magic4,1000),
	(olympic,1000),
	(queens1,10),
	(sendmoney,10000),
	(sudoku81,10000),
        (zebra,10000)
	].

go:-
    open(res_bp,write,S),
    run(S).

/*
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
*/
profile(S1,S2,Dx,Dy,Dl,Dh,Dt,Dc):-
    programs(Ps),
    '$member'(PN,Ps),
    PN=(P,_),
%    compile(P),
    load(P),
%    consult(P),
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
%    consult(P),
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

ratios(F1,F2):-
    readall(F1,L1,_),
    readall(F2,L2,_),
    length(L1,N),
    ratios(L1,L2,Ratios),
    sum(Ratios,0,Sum),
    prod(Ratios,1,Prod),
    output_ratios(Ratios),
    Amean is Sum/N,
    Gmean is exp(1/N*log(Prod)), 
    write_ln(user_output,['<amean>', Amean],' & ',' \\hline \\\\'),
    write_ln(user_output,['<gmean>', Gmean],' & ',' \\hline \\\\').

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

ratios([],[],[]).
ratios([P1|L1],[P2|L2],[P3|L3]):-
    P1=..[Name,Time1|_],
    P2=..[Name,Time2|_],
    Ratio is Time1/Time2,
    P3=..[Name,Ratio],
    ratios(L1,L2,L3).

sum([P|L],Sum0,Sum):-
    P=..[Name,R],
    Sum1 is Sum0+R,
    sum(L,Sum1,Sum).
sum([],Sum,Sum).

prod([P|L],Prod0,Prod):-
    P=..[Name,R],
    Prod1 is Prod0*R,
    prod(L,Prod1,Prod).
prod([],Prod,Prod).

output_ratios([]).
output_ratios([Ratio|Ratios]):-
    Ratio=..[Name,R],
    write_ln(user_output,[Name,R],' & ',' \\hline \\\\'),
    output_ratios(Ratios).

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
    write_item(S,X):-Y is truncate(X*100)/100, write(S,Y).

spaces(S,Tab):-Tab>0,!,write(S,' '),Tab1 is Tab-1,spaces(S,Tab1).
spaces(S,Tab).


initialize_structure(Sum,N0,N,Value):-
    N0=<N,!,
    arg(N0,Sum,Value),
    N1 is N0+1,
    initialize_structure(Sum,N1,N,Value).
initialize_structure(Sum,N0,N,Value).



