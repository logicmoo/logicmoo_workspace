% dcg_pfc: translation of dcg-like grammar rules into pfc rules.

:- op(1200,xfx,'-->>').
:- op(1200,xfx,'--*>>').
% :- op(1200,xfx,'<<--').
:- op(400,yfx,'\').

% :- use_module(library(strings)), use_module(library(lists)).

term_expansion((P -->> Q),(:- add(Rule))) :-
  pfc_translate_rule((P -->> Q), Rule).
term_expansion((P --*>> Q),(:- add(Rule))) :-
  pfc_translate_rule((P --*>> Q), Rule).

pfc_translate_rule((LP-->>[]),H) :- !, pfc_t_lp(LP,Id,S,S,H).
pfc_translate_rule((LP-->>RP),(H <= B)):-
   pfc_t_lp(LP,Id,S,SR,H),
   pfc_t_rp(RP,Id,S,SR,B1),
   pfc_tidy(B1,B).


pfc_translate_rule((LP--*>>[]),H) :- !, pfc_t_lp(LP,Id,S,S,H).
pfc_translate_rule((LP--*>>RP),(B => H)):-
   pfc_t_lp(LP,Id,S,SR,H),
   pfc_t_rp(RP,Id,S,SR,B1),
   pfc_tidy(B1,B).

pfc_t_lp(X,Id,S,SR,ss(X,Id,(S\SR))) :- var(X),!.

pfc_t_lp((LP,List),Id,S,SR,ss(LP,Id,(S\List2))):- 
   !,
   pfcAppend(List,SR,List2).

pfc_t_lp(LP,Id,S,SR,ss(LP,Id,(S\SR))).

pfc_t_rp(!,Id,S,S,!) :- !.
pfc_t_rp([],Id,S,S1,S=S1) :- !.
pfc_t_rp([X],Id,S,SR,ss(word(X),Id,(S\SR))) :- !.
pfc_t_rp([X|R],Id,S,SR,(ss(word(X),Id,(S\SR1)),RB)) :- 
  !, 
  pfc_t_rp(R,Id,SR1,SR,RB).
pfc_t_rp({T},Id,S,S,{T}) :- !.
pfc_t_rp((T,R),Id,S,SR,(Tt,Rt)) :- !,
   pfc_t_rp(T,Id,S,SR1,Tt),
   pfc_t_rp(R,Id,SR1,SR,Rt).
pfc_t_rp((T;R),Id,S,SR,(Tt;Rt)) :- !,
   pfc_t_or(T,Id,S,SR,Tt),
   pfc_t_or(R,Id,S,SR,Rt).
pfc_t_rp(T,Id,S,SR,ss(T,Id,(S\SR))).

pfc_t_or(X,Id,S0,S,P) :-
   pfc_t_rp(X,Id,S0a,S,Pa),
 ( var(S0a), S0a \== S, !, S0=S0a, P=Pa;
   P=(S0=S0a,Pa) ).

pfc_tidy((P1;P2),(Q1;Q2)) :-
   !,
   pfc_tidy(P1,Q1),
   pfc_tidy(P2,Q2).
pfc_tidy(((P1,P2),P3),Q) :- 
   pfc_tidy((P1,(P2,P3)),Q).
pfc_tidy((P1,P2),(Q1,Q2)) :- 
   !,
   pfc_tidy(P1,Q1),
   pfc_tidy(P2,Q2).
pfc_tidy(A,A) :- !.

compile_pfcg :-
  ((retract((L -->> R)), pfc_translate_rule((L -->> R), PfcRule));
    (retract((L --*>> R)), pfc_translate_rule((L --*>> R), PfcRule))),
  add(PfcRule),
  fail.
compile_pfcg.

parse(Words) :- 
  parse(Words,Id),
  format("~Nsentence id = ~w",Id),
  show(Id,sentence(X)).


parse(Words,Id) :- 
  gen_s_tag(Id),
  parse1(Words,Id),
  add(sentence(Id,Words)).

parse1([],_) :- !.
parse1([H|T],Id) :-
 do(add(ss(word(H),Id,([H|T]\T)))),
 parse1(T,Id).


showSentences(Id) :- showSentences(Id,_).

showSentences(Id,Words) :-
  sentence(Id,Words),
  pfc(ss(s(S),Id,(Words\[]))),
  nl,write(S),
  fail.
showSentences(_,_).

do(X) :- call(X) -> true;true.

show(Id,C) :-
  pfc(ss(C,Id,A\B)),
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
