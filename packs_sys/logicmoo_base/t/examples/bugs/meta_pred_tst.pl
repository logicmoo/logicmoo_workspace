
:- meta_predicate(colon_module_tst(:)).
:- meta_predicate(zero_module_tst(0)).
:- meta_predicate(plus_module_tst(+)).

:- module_transparent(non_module_tst/1).


colon_module_tst(M:G):- (ignore((fail,M:call(G))), '$current_source_module'(SM),'$current_typein_module'(TM),'context_module'(CM),
   writel_lnl(['$current_source_module'=(SM),'$current_typein_module'=(TM),'context_module'=(CM),m=M])).
colon_module_tst(G):- throw(up(G)).


zero_module_tst(G):- ignore((fail,call(G))), '$current_source_module'(SM),'$current_typein_module'(TM),'context_module'(CM),
   writel_lnl(['$current_source_module'=(SM),'$current_typein_module'=(TM),'context_module'=(CM)]).

plus_module_tst(G):- ignore((fail,call(G))), '$current_source_module'(SM),'$current_typein_module'(TM),'context_module'(CM),
   writel_lnl(['$current_source_module'=(SM),'$current_typein_module'=(TM),'context_module'=(CM)]).

non_module_tst(G):- ignore((fail,call(G))), '$current_source_module'(SM),'$current_typein_module'(TM),'context_module'(CM),
   writel_lnl(['$current_source_module'=(SM),'$current_typein_module'=(TM),'context_module'=(CM)]).


i_am_a_pear(0).
apple:i_am_an_apple(0).
user:i_am_an_orange(0).

writel_lnl(G):-is_list(G),!,maplist(writel_lnl,G).
writel_lnl(G):-writeq(G),nl.


print_module_tsts:- 
 forall((
     member(P,[colon_module_tst,zero_module_tst,plus_module_tst,non_module_tst]),
     member(E,[i_am_an_apple(0),i_am_an_orange(0),i_am_a_pear(0)]),
     member(M,[module_meta_pred_tst,user,apple,orange]),
     member(C,[M:UCALL,M:UMCALL,UCALL,UMCALL])),

 ((UCALL=..[P,E],UMCALL=..[P,M:E]),
   format("~N~n:- writeq(~p),nl,~p,nl.~n",[C,C]))).


end_of_file.

root@ubuntu:/opt/PrologMUD/pack/logicmoo_base/t/examples/bugs# swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.3.26)
Copyright (c) 1990-2016 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- [meta_pred_tst].
true.

?- [meta_pred_tst_each].
module_meta_pred_tst:colon_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

module_meta_pred_tst:colon_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

colon_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

user:colon_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

user:colon_module_tst(user:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

colon_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(user:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

apple:colon_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

apple:colon_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

colon_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

orange:colon_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

orange:colon_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

colon_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

module_meta_pred_tst:colon_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

module_meta_pred_tst:colon_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

colon_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

user:colon_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

user:colon_module_tst(user:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

colon_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(user:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

apple:colon_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

apple:colon_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

colon_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

orange:colon_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

orange:colon_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

colon_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

module_meta_pred_tst:colon_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

module_meta_pred_tst:colon_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

colon_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=module_meta_pred_tst

user:colon_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

user:colon_module_tst(user:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

colon_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(user:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=user

apple:colon_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

apple:colon_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

colon_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=apple

orange:colon_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

orange:colon_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

colon_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=source_mod

colon_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user
m=orange

module_meta_pred_tst:zero_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:zero_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:zero_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:zero_module_tst(user:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(user:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:zero_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:zero_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:zero_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:zero_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:zero_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:zero_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:zero_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:zero_module_tst(user:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(user:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:zero_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:zero_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:zero_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:zero_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:zero_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:zero_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:zero_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:zero_module_tst(user:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(user:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:zero_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:zero_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:zero_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:zero_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

zero_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:plus_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:plus_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:plus_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:plus_module_tst(user:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(user:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:plus_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:plus_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:plus_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:plus_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:plus_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:plus_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:plus_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:plus_module_tst(user:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(user:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:plus_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:plus_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:plus_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:plus_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:plus_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:plus_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:plus_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:plus_module_tst(user:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(user:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:plus_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:plus_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:plus_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:plus_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

plus_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:non_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:non_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(module_meta_pred_tst:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:non_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:non_module_tst(user:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(user:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:non_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:non_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(apple:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:non_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:non_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(orange:i_am_an_apple(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:non_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:non_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(module_meta_pred_tst:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:non_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:non_module_tst(user:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(user:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:non_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:non_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(apple:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:non_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:non_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(orange:i_am_an_orange(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:non_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

module_meta_pred_tst:non_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(module_meta_pred_tst:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:non_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

user:non_module_tst(user:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(user:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:non_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

apple:non_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(apple:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:non_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

orange:non_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

non_module_tst(orange:i_am_a_pear(0))
'$current_source_module'=source_mod
'$current_typein_module'=typein_mod
context_module=user

true.

typein_mod:  ?-

