
:- use_module(library(logicmoo_utils)).

% debug printing
debugln(X):- debugln_xfrm(X,S), dmsg(S).
fmt_pllm(X):- debugln_xfrm(X,S), fmt(S).

debugln_xfrm(Insts,S):- var(Insts), !, sformat(S,"~p",[Insts]).
debugln_xfrm(i(X),S):- \+ is_list(X) -> debugln_xfrm(X,S) ; maplist(debugln_xfrm,X,Y),atomics_to_string(Y,' ',S).
debugln_xfrm([N|A],S):- is_list(A),!,maplist(debugln_xfrm,[N|A],Y),atomics_to_string(Y,' ',S).
debugln_xfrm((F/A),S):- functor(P,F,A),predicate_property(P,number_of_clauses(Insts)),!,sformat(S,"~w=~:d~n",[(F/A),Insts]).
debugln_xfrm(w(E),S):- sformat(S,'~p',E),!.
debugln_xfrm('$'(E),S):- get_flag(E,Insts),!,sformat(S,"~w=~:d~n",[E,Insts]).
debugln_xfrm(N=V,S):- integer(V),!,sformat(S,"~n\t~w\t= ~:d ",[N,V]).
debugln_xfrm(N=V,S):- !,sformat(S,"~n\t~w\t= ~w ",[N,V]).
debugln_xfrm([N],S):- !, debugln_xfrm(N,S).
debugln_xfrm(C,S):- if_defined(tok_split(_,C,S,_)),!.
debugln_xfrm(C,S):- if_defined(tok_split(C,S,_)),!.
debugln_xfrm(C,S):- compound(C),!,sformat(S,"~p",[C]).
%debugln_xfrm(C,S):- compound(C),compound_name_arguments(C,N,A),debugln_xfrm([N|A],S).
debugln_xfrm(nl,'\n'):-!.
debugln_xfrm([],''):-!.
debugln_xfrm(E,E).

