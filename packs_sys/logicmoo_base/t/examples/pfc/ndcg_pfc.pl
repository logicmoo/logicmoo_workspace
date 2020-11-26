% dcg_mpred: translation of dcg-like grammar rules into pfc rules.

:- op(1200,xfx,'-->>').
:- op(1200,xfx,'--*>>').
% :- op(1200,xfx,'<<--').
:- op(400,yfx,'\').

% :- use_module(library(strings)), use_module(library(lists)).

term_expansion((P -->> Q),(:- ain(Rule))) :-
  mpred_translate_rule((P -->> Q), Rule).
term_expansion((P --*>> Q),(:- ain(Rule))) :-
  mpred_translate_rule((P --*>> Q), Rule).

mpred_translate_rule((LP-->>[]),H) :- !, mpred_t_lp(LP,Id,S,S,H).
mpred_translate_rule((LP-->>RP),(H <- B)):-
   mpred_t_lp(LP,Id,S,SR,H),
   mpred_t_rp(RP,Id,S,SR,B1),
   mpred_tidy(B1,B).


mpred_translate_rule((LP--*>>[]),H) :- !, mpred_t_lp(LP,Id,S,S,H).
mpred_translate_rule((LP--*>>RP),(B ==> H)):-
   mpred_t_lp(LP,Id,S,SR,H),
   mpred_t_rp(RP,Id,S,SR,B1),
   mpred_tidy(B1,B).

mpred_t_lp(X,Id,S,SR,[{make_term(ss(X,Id,(S\SR)),XX)},XX]) :- 
  var(X),
  !.

mpred_t_lp((LP,List),Id,S,SR,Term):- 
   !,
   pfcAppend(List,SR,List2),
   make_term(ss(LP,Id,(S\List2)),Term).

mpred_t_lp(LP,Id,S,SR,Term) :- 
  make_term(ss(LP,Id,(S\SR)),Term).

mpred_t_rp(!,Id,S,S,!) :- !.
mpred_t_rp([],Id,S,S1,S=S1) :- !.
mpred_t_rp([X],Id,S,SR,Term) :- 
  !,
  make_term(ss(word(X),Id,(S\SR)),Term).
mpred_t_rp([X|R],Id,S,SR,(Term,RB)) :- 
  !, 
  mpred_t_rp(R,Id,SR1,SR,RB),
  make_term(ss(word(X),Id,(S\SR1)),term).
mpred_t_rp({T},Id,S,S,{T}) :- !.
mpred_t_rp((T,R),Id,S,SR,(Tt,Rt)) :- !,
   mpred_t_rp(T,Id,S,SR1,Tt),
   mpred_t_rp(R,Id,SR1,SR,Rt).
mpred_t_rp((T;R),Id,S,SR,(Tt;Rt)) :- !,
   mpred_t_or(T,Id,S,SR,Tt),
   mpred_t_or(R,Id,S,SR,Rt).
mpred_t_rp(T,Id,S,SR,Term) :- make_term(ss(T,Id,(S\SR)),Term).

mpred_t_or(X,Id,S0,S,P) :-
   mpred_t_rp(X,Id,S0a,S,Pa),
 ( var(S0a), S0a \== S, !, S0=S0a, P=Pa;
   P=(S0=S0a,Pa) ).

mpred_tidy((P1;P2),(Q1;Q2)) :-
   !,
   mpred_tidy(P1,Q1),
   mpred_tidy(P2,Q2).
mpred_tidy(((P1,P2),P3),Q) :- 
   mpred_tidy((P1,(P2,P3)),Q).
mpred_tidy((P1,P2),(Q1,Q2)) :- 
   !,
   mpred_tidy(P1,Q1),
   mpred_tidy(P2,Q2).
mpred_tidy(A,A) :- !.

compile_mpredg :-
  ((retract((L -->> R)), mpred_translate_rule((L -->> R), PfcRule));
    (retract((L --*>> R)), mpred_translate_rule((L --*>> R), PfcRule))),
  ain(PfcRule),
  fail.
compile_mpredg.

parse(Words) :- 
  parse(Words,Id),
  format("~Nsentence id = ~w",Id),
  show(Id,s(X)).


parse(Words,Id) :- 
  gen_s_tag(Id),
  parse1(Words,Id),
  ain(sentence(Id,Words)).

parse1([],_) :- !.
parse1([H|T],Id) :-
 make_term(ss(word(H),Id,([H|T]\T)),Term),
 do(ain(Term)),
 parse1(T,Id).


showSentences(Id) :- showSentences(Id,_).

showSentences(Id,Words) :-
  sentence(Id,Words),
  make_term(ss(s(S),Id,(Words\[])),Term),
  pfc(Term),
  nl,write(S),
  fail.
showSentences(_,_).

do(X) :- call(X) -> true;true.

show(Id,C) :-
  make_term(ss(C,Id,A\B),Term),
  pfc(Term),
  append(Words,B,A),
  format("~n ~w    :   ~w",[C,Words]),
  fail.

gen_s_tag(s(N2)) :-
  var(V),
  (retract(s_tag(N)); N=0),
  N2 is N+1,
  assert(s_tag(N2)).

make_term(ss(Constituent,Id,String),Term) :-
   Constituent =.. [Name|Args],
   name(Name,Name_string),
   name(Name2,[36|Name_string]),
   append([Name2|Args],[Id,String],Term_string),
   Term =.. Term_string.

append([],X,X).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).
