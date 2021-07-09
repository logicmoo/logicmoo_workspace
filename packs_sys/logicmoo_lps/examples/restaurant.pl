:- expects_dialect(lps).

%0.531 seconds cpu time
%?-  dumplps

initially
    menu(spring_rolls),
    menu(dumplings),
    menu(peking_duck),
    menu(fried_rice),
    menu(chow_mein),
    menu(mapo_tofu),
    portion(spring_rolls,10),
    portion(dumplings,10),
    portion(peking_duck,10),
    portion(fried_rice,10),
    portion(chow_mein,10),
    portion(mapo_tofu,0),
    biggest_table(id8,8),
    table(id1,2),
    table(id2,2),
    table(id3,3),
    table(id4,4),
    table(id5,4),
    table(id6,5),
    table(id7,6),
    table(id8,8),
    is_free(id1),
    is_free(id2),
    is_free(id3),
    is_free(id4),
    is_free(id5),
    is_free(id6),
    is_free(id7),
    is_free(id8),
    waiting_staff(andy),
    waiting_staff(becca),
    waiting_staff(charlie),
    waiting_staff(danni),
    errand_boy(jim).

observe([sign_in(andy),sign_in(becca)],11).
observe([arrive([person_1,personO,person_3]),arrive([person_4,personT_S,person_6])],12).
observe([],13).
observe([],14).
observe([],15).

lps_number(People,Size):- my_length(People,Size).

lps_same(X,Y):- X==Y.

lps_diff(X,Y):- X\==Y.

lps_less(X,Y):- X<Y.

lps_more(X,Y):- X>Y.

lps_leq(X,Y):- X=<Y.

lps_geq(X,Y):- X>=Y.

lps_inc(X,X1):- X1 is X+1.

lps_dec(X,X1):- X1 is X-1.

lps_write(X):- write(X),nl.


action(report_duty(_)).
action(leave_duty(_)).
action(guide(_,_,_)).
action(reject(_)).
action(reserve(_,_,_)).
action(update_no_show(_,_)).
action(send_errand_boy(_,_)).
action(cook(_)).
action(get_serving(_,_)).
action(bring(_,_,_)).


event(sign_in(_)).
event(sign_out(_)).
event(arrive(_)).
event(book(_,_,_)).
event(order(_,_,_)).
event(return_errand_boy(_,_,_)).

fluent(menu(_)).
fluent(portion(_,_)).
fluent(biggest_table(_,_)).
fluent(table(_,_)).
fluent(is_free(_)).
fluent(waiting_staff(_)).
fluent(errand_boy(_)).
fluent(on_duty(_)).
fluent(staff_status(_,_)).
fluent(blacklisted(_)).
fluent(no_show(_,_)).
fluent(reserved(_,_,_)).
fluent(raw_ing(_)).



leave_duty(Staff)from T1 to _T2 terminates on_duty(Staff) if
    on_duty(Staff)at T1.

leave_duty(Staff)from T1 to _T2 terminates staff_status(Staff,_Status) if
    staff_status(Staff,available)at T1.

guide(What,T_S954,Table)from T_S946 to T_S948 terminates is_free(Table) if
    table(Table,T_S978)at T_S946.

guide(Staff,_People,_TId)from _T1 to _T2 terminates staff_status(Staff,available).

reserve(What,T_S954,Table)from T_S946 to T_S948 terminates is_free(T_S954) if
    table(T_S954,T_S978)at T_S946.

update_no_show(Person,_N)from _T1 to _T2 terminates no_show(Person,_X).

update_no_show(What,T_S954)from T_S946 to T_S948 terminates reserved(T_S958,What,T_S962).

return_errand_boy(What,T_S954,Table)from T_S946 to T_S948 terminates portion(T_S954,T_S962) if
    portion(T_S954,T_S980)at T_S946,
    lps_less(T_S980,1).

get_serving(What,T_S954)from T_S946 to T_S948 terminates portion(What,T_S960).

if sign_in(Staff)from _T1 to T2,waiting_staff(Staff)at T2
then 
    report_duty(Staff)from T3 to _T4,
    tc(T2=<T3).

if sign_out(Staff)from _T1 to T2,waiting_staff(Staff)at T2
then 
    leave_duty(Staff)from T3 to _T4,
    tc(T2=<T3).

if arrive(People)from T1 to T2
then 
    seat(People,T1)from T3 to _T4,
    tc(T2=<T3),
    tc(T3=<T2+5).

if book(Person,_Size,_Time)from _T1 to T2,blacklisted(Person)at T2
then 
    reject(Person)from T3 to _T4,
    tc(T2=<T3).

if book(Person,Size,Time)from _T1 to T2,bookable(Person,Size,Time,TId)at T2
then 
    reserve(Person,TId,Time)from T3 to _T4,
    tc(T2=<T3).

if reserved(_TId,Person,Time)at T1,tc(Time<T1),lps_more(T1,Time)
then 
    strike(Person)from T2 to _T3,
    tc(T1=<T2).

if order(Person,TId,What)from _T1 to T2,menu(What)at T2
then 
    fulfil(Person,TId,What,T2)from T3 to T4,
    tc(T2=<T3),
    tc(T4=<T2+10).


report_duty(Staff)from _T1 to _T2 initiates on_duty(Staff).
report_duty(Staff)from _T1 to _T2 initiates staff_status(Staff,available).

guide(Staff,_People,_TId)from _T1 to _T2 initiates staff_status(Staff,busy).

reserve(O396,Table,O400)from S to E initiates reserved(Table,O396,O400) if
    table(Table,O426)at E.

update_no_show(Person,N)from _T1 to _T2 initiates no_show(Person,N).

return_errand_boy(_Boy,What,Amount)from _T1 to _T2 initiates portion(What,Amount).

get_serving(What,N)from _T1 to _T2 initiates portion(What,N1) if
    lps_dec(N,N1).

free_table(TId,Size)at T if
    table(TId,Size)at T,
    is_free(TId)at T.

free_table(TId,Size)at T if
    table(TId,Size2)at T,
    lps_leq(Size,Size2),
    is_free(TId)at T.

blacklisted(Person)at T if
    no_show(Person,N)at T,
    lps_geq(N,2).

bookable(S,E,O394,O396)at O386 if
    not blacklisted(S)at O386,
    not reserved(O436,S,O440)at O386,
    table(O396,E)at O386,
    not reserved(O396,O480,O482)at O386.

bookable(S,E,O394,O396)at O386 if
    not blacklisted(S)at O386,
    not reserved(O436,S,O440)at O386,
    table(O396,O458)at O386,
    lps_leq(E,O458),
    not reserved(O396,O492,O494)at O386.


my_length([],0) :-
    !.

my_length([S|E],O348) :-
    my_length(E,Table),
    O348 is Table+1.


seat(People,T)from TS to _TE if
    lps_number(People,Size),
    free_table(TId,Size)at TS,
    staff_status(Staff,available)at TS,
    guide(Staff,People,TId)from TS to _T2,
    tc(T=<TS),
    tc(TS=<T+10).

strike(Person)from TS to TE if
    no_show(Person,N)at TS,
    lps_inc(N,N1),
    update_no_show(Person,N1)from TS to TE.

strike(E)from O386 to O388 if
    not no_show(E,O414)at O386,
    update_no_show(E,1)from O386 to O388.

fulfil(Person,TId,What,_T)from TS to TE if
    portion(What,N)at TS,
    lps_geq(N,1),
    get_serving(What,N)from TS to T2,
    bring(Person,TId,What)from T2 to TE.

fulfil(Person,TId,What,T)from TS to TE if
    portion(What,N)at TS,
    lps_less(N,1),
    cook(What)from TS to T2,
    tc(TS=<T+2),
    get_serving(What,N)from T2 to T3,
    bring(Person,TId,What)from T3 to TE.

fulfil(Person,TId,What,_T)from TS to TE if
    errand_boy(Boy)at TS,
    send_errand_boy(Boy,What)from TS to T2,
    return_errand_boy(Boy,What,_Amount)from T3 to T4,
    tc(T2=<T3),
    tc(T3=<T2+3),
    portion(What,N)at T4,
    get_serving(What,N)from T4 to T5,
    bring(Person,TId,What)from T5 to TE.

false
    guide(Staff,People1,TId1)from T1 to T2,
    guide(Staff,People2,TId2)from T1 to T2,
    lps_diff(People1,People2),
    lps_same(TId1,TId2).

false
    reserve(Person1,TId1,Time)from T1 to T2,
    reserve(Person2,TId2,Time)from T1 to T2,
    lps_diff(Person1,Person2),
    lps_same(TId1,TId2).

false
    cook(mapo_tofu)from _T1 to T2,
    not raw_ing(tofu)at T2.

false
    get_serving(What,N)from _T1 to T2,
    portion(What,N)at T2,
    lps_less(N,1).


/** <examples>
?- godc(Timeline).
*/
