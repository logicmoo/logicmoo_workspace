
:- module(errors_candc,[warning/2,error/2,inform/2,gold/2]).

:- use_module(semlib(options),[candc_option/2]).

system:warning(S,V):- candc_warning(S,V).
system:error(S,V):- canc_error(S,V).

candc_warning(S,V):-
   candc_option('--warnings',true), !,
   format(user_error,'\033[33mWARNING: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

canc_warning(_S,_V):-
   candc_option('--warnings',false).

gold(S,V):-
   candc_option('--warnings',true), !,
   format(user_error,'GOLD: ',[]),
   format(user_error,S,V),
   format(user_error,'~n',[]).

gold(_S,_V):-
   candc_option('--warnings',false).

inform(S,V):-
   candc_option('--info',true), !,
   format(user_error,'\033[34mINFO: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

inform(_S,_V):-
   candc_option('--info',false).

canc_error(S,V):-
   format(user_error,'\033[31mERROR: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

