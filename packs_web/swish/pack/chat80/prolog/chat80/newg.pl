/* @(#)newg.pl	24.1 2/23/88 */

/*
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/

sentence(B,C,D,E,F) :-
   declarative(B,C,G,E,H),
   terminator(.,G,D,H,F).
sentence(B,C,D,E,F) :-
   wh_question(B,C,G,E,H),
   terminator(?,G,D,H,F).
sentence(B,C,D,E,F) :-
   topic(C,G,E,H),
   wh_question(B,G,I,H,J),
   terminator(?,I,D,J,F).
sentence(B,C,D,E,F) :-
   yn_question(B,C,G,E,H),
   terminator(?,G,D,H,F).
sentence(B,C,D,E,F) :-
   imperative(B,C,G,E,H),
   terminator(!,G,D,H,F).


pp(B,C,D,E,F,F,G,H) :-
   virtual(pp(B,C,D,E),G,H).
pp(pp(B,C),D,E,F,G,H,I,J) :-
   prep(B,G,K,I,L),
   prep_case(M),
   np(C,N,M,O,D,E,F,K,H,L,J).


topic(B,C,D,x(gap,nonterminal,pp(E,compl,F,G),H)) :-
   pp(E,compl,F,G,B,I,D,J),
   opt_comma(I,C,J,H).


opt_comma(B,C,D,E) :-
   ~(',',B,C,D,E).
opt_comma(B,B,C,C).


declarative(decl(B),C,D,E,F) :-
   s(B,G,C,D,E,F).


wh_question(whq(B,C),D,E,F,G) :-
   variable_q(B,H,I,J,D,K,F,L),
   question(I,J,C,K,E,L,G).


np(B,C,D,E,F,G,H,I,I,J,K) :-
   virtual(np(B,C,D,E,F,G,H),J,K).
np(np(B,C,[]),B,D,def,E,F,G,H,I,J,K) :-
   is_pp(F),
   pers_pron(C,B,L,H,I,J,K),
   empty(G),
   role(L,decl,D).
np(np(B,C,D),B,E,F,G,H,I,J,K,L,M) :-
   is_pp(H),
   np_head(C,B,F+N,O,D,J,P,L,Q),
   np_all(R),
   np_compls(N,B,G,O,R,I,P,K,Q,M).
np(part(B,C),3+D,E,indef,F,G,H,I,J,K,L) :-
   is_pp(G),
   determiner(B,D,indef,I,M,K,N),
   ~(of,M,O,N,P),
   s_all(Q),
   prep_case(R),
   np(C,3+plu,R,def,F,Q,H,O,J,P,L).


variable_q(B,C,D,E,F,G,H,x(gap,nonterminal,np(I,C,E,J,K,L,M),N)) :-
   whq(B,C,I,D,F,G,H,N),
   do_trace(L,M).
variable_q(B,C,compl,D,E,F,G,x(gap,nonterminal,pp(pp(H,I),compl,J,K),L)) :-
   prep(H,E,M,G,N),
   whq(B,C,I,O,M,F,N,L),
   do_trace(J,K),
   compl_case(D).
variable_q(B,C,compl,D,E,F,G,x(gap,nonterminal,adv_phrase(pp(H,np(C,np_head(int_det(B),[],I),[])),J,K),L)) :-
   context_pron(H,I,E,F,G,L),
   do_trace(J,K),
   verb_case(D).
variable_q(B,C,compl,D,E,F,G,x(gap,nonterminal,pred(adj,value(H,wh(B)),I),J)) :-
   ~(how,E,K,G,L),
   adj(quant,H,K,F,L,J),
   empty(I),
   verb_case(D).


adv_phrase(B,C,D,E,E,F,G) :-
   virtual(adv_phrase(B,C,D),F,G).
adv_phrase(pp(B,C),D,E,F,G,H,I) :-
   loc_pred(B,F,J,H,K),
   pp(pp(prep(of),C),compl,D,E,J,G,K,I).


pred(B,C,D,E,E,F,G) :-
   virtual(pred(B,C,D),F,G).
pred(B,C,D,E,F,G,H) :-
   adj_phrase(C,D,E,F,G,H).
pred(neg,B,C,D,E,F,G) :-
   s_all(H),
   pp(B,compl,H,C,D,E,F,G).
pred(B,C,D,E,F,G,H) :-
   s_all(I),
   adv_phrase(C,I,D,E,F,G,H).


whq(B,C,D,undef,E,F,G,H) :-
   int_det(B,C,E,I,G,J),
   s_all(K),
   np(D,C,L,M,subj,K,N,I,F,J,H).
whq(B,3+C,np(3+C,wh(B),[]),D,E,F,G,H) :-
   int_pron(D,E,F,G,H).


int_det(B,3+C,D,E,F,G) :-
   whose(B,C,D,E,F,G).
int_det(B,3+C,D,E,F,G) :-
   int_art(B,C,D,E,F,G).


np_head0(B,C,D,E,E,F,G) :-
   virtual(np_head0(B,C,D),F,G).
np_head0(name(B),3+sin,def+proper,C,D,E,F) :-
   name(B,C,D,E,F).
np_head0(np_head(B,C,D),3+E,F+common,G,H,I,J) :-
   determiner(B,E,F,G,K,I,L),
   adjs(C,K,M,L,N),
   noun(D,E,M,H,N,J).
np_head0(B,C,def+proper,D,E,F,x(nogap,nonterminal,gen_marker,G)) :-
   poss_pron(B,C,D,E,F,G).
np_head0(np_head(B,[],C),3+sin,indef+common,D,E,F,G) :-
   quantifier_pron(B,C,D,E,F,G).


gen_marker(B,B,C,D) :-
   virtual(gen_marker,C,D).
gen_marker(B,C,D,E) :-
   ~('''',B,F,D,G),
   an_s(F,C,G,E).


whose(B,C,D,E,F,x(nogap,nonterminal,np_head0(wh(B),C,proper),x(nogap,nonterminal,gen_marker,G))) :-
   ~(whose,D,E,F,G).


question(B,C,D,E,F,G,H) :-
   subj_question(B),
   role(subj,I,C),
   s(D,J,E,F,G,H).
question(B,C,D,E,F,G,H) :-
   fronted_verb(B,C,E,I,G,J),
   s(D,K,I,F,J,H).


det(B,C,D,E,E,F,G) :-
   virtual(det(B,C,D),F,G).
det(det(B),C,D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   det(I,C,B,D).
det(generic,B,generic,C,C,D,D).


int_art(B,C,D,E,F,x(nogap,nonterminal,det(G,C,def),H)) :-
   int_art(B,C,G,D,E,F,H).


subj_question(subj).


subj_question(undef).


yn_question(q(B),C,D,E,F) :-
   fronted_verb(nil,G,C,H,E,I),
   s(B,J,H,D,I,F).


verb_form(B,C,D,E,F,F,G,H) :-
   virtual(verb_form(B,C,D,E),G,H).
verb_form(B,C,D,E,F,G,H,I) :-
   terminal(J,F,G,H,I),
   verb_form(J,B,C,D).


neg(B,C,D,D,E,F) :-
   virtual(neg(B,C),E,F).
neg(aux+B,neg,C,D,E,F) :-
   ~(not,C,D,E,F).
neg(B,pos,C,C,D,D).


fronted_verb(B,C,D,E,F,x(gap,nonterminal,verb_form(G,H,I,J),x(nogap,nonterminal,neg(K,L),M))) :-
   verb_form(G,H,I,N,D,O,F,P),
   verb_type(G,aux+Q),
   role(B,J,C),
   neg(R,L,O,E,P,M).


imperative(imp(B),C,D,E,F) :-
   imperative_verb(C,G,E,H),
   s(B,I,G,D,H,F).


imperative_verb(B,C,D,x(nogap,terminal,you,x(nogap,nonterminal,verb_form(E,imp+fin,2+sin,main),F))) :-
   verb_form(E,inf,G,H,B,C,D,F).


s(s(B,C,D,E),F,G,H,I,J) :-
   subj(B,K,L,G,M,I,N),
   verb(C,K,L,O,M,P,N,Q),
   empty(R),
   s_all(S),
   verb_args(L,O,D,R,T,P,U,Q,V),
   minus(S,T,W),
   my_plus(S,T,X),
   verb_mods(E,W,X,F,U,H,V,J).


subj(there,B,C+be,D,E,F,G) :-
   ~(there,D,E,F,G).
subj(B,C,D,E,F,G,H) :-
   s_all(I),
   subj_case(J),
   np(B,C,J,K,subj,I,L,E,F,G,H).


np_head(B,C,D,E,F,G,H,I,J) :-
   np_head0(K,L,M,G,N,I,O),
   possessive(K,L,M,P,P,B,C,D,E,F,N,H,O,J).


np_compls(proper,B,C,[],D,E,F,F,G,G) :-
   empty(E).
np_compls(common,B,C,D,E,F,G,H,I,J) :-
   np_all(K),
   np_mods(B,C,L,D,E,M,K,N,G,O,I,P),
   relative(B,L,M,N,F,O,H,P,J).


possessive(B,C,D,[],E,F,G,H,I,J,K,L,M,N) :-
   gen_case(K,O,M,P),
   np_head0(Q,R,S,O,T,P,U),
   possessive(Q,R,S,V,[pp(poss,np(C,B,E))|V],F,G,H,I,J,T,L,U,N).
possessive(B,C,D,E,F,B,C,D,E,F,G,G,H,H).


gen_case(B,C,D,x(nogap,terminal,the,E)) :-
   gen_marker(B,C,D,E).


an_s(B,C,D,E) :-
   ~(s,B,C,D,E).
an_s(B,B,C,C).


determiner(B,C,D,E,F,G,H) :-
   det(B,C,D,E,F,G,H).
determiner(B,C,D,E,F,G,H) :-
   quant_phrase(B,C,D,E,F,G,H).


quant_phrase(quant(B,C),D,E,F,G,H,I) :-
   quant(B,E,F,J,H,K),
   number(C,D,J,G,K,I).


quant(B,indef,C,D,E,F) :-
   neg_adv(G,B,C,H,E,I),
   comp_adv(G,H,J,I,K),
   ~(than,J,D,K,F).
quant(B,indef,C,D,E,F) :-
   ~(at,C,G,E,H),
   sup_adv(I,G,D,H,F),
   sup_op(I,B).
quant(the,def,B,C,D,E) :-
   ~(the,B,C,D,E).
quant(same,indef,B,B,C,C).


neg_adv(B,not+B,C,D,E,F) :-
   ~(not,C,D,E,F).
neg_adv(B,B,C,C,D,D).


sup_op(least,not+less).
sup_op(most,not+more).


np_mods(B,C,D,[E|F],G,H,I,J,K,L,M,N) :-
   np_mod(B,C,E,G,O,K,P,M,Q),
   do_trace(R),
   my_plus(R,O,S),
   minus(G,S,T),
   my_plus(O,G,U),
   np_mods(B,C,D,F,T,H,U,J,P,L,Q,N).
np_mods(B,C,D,D,E,E,F,F,G,G,H,H).


np_mod(B,C,D,E,F,G,H,I,J) :-
   pp(D,C,E,F,G,H,I,J).
np_mod(B,C,D,E,F,G,H,I,J) :-
   reduced_relative(B,D,E,F,G,H,I,J).


verb_mods([B|C],D,E,F,G,H,I,J) :-
   verb_mod(B,D,K,G,L,I,M),
   do_trace(N),
   my_plus(N,K,O),
   minus(D,O,P),
   my_plus(K,D,Q),
   verb_mods(C,P,Q,F,L,H,M,J).
verb_mods([],B,C,C,D,D,E,E).


verb_mod(B,C,D,E,F,G,H) :-
   adv_phrase(B,C,D,E,F,G,H).
verb_mod(B,C,D,E,F,G,H) :-
   is_adv(C),
   adverb(B,E,F,G,H),
   empty(D).
verb_mod(B,C,D,E,F,G,H) :-
   pp(B,compl,C,D,E,F,G,H).


adjs([B|C],D,E,F,G) :-
   pre_adj(B,D,H,F,I),
   adjs(C,H,E,I,G).
adjs([],B,B,C,C).


pre_adj(B,C,D,E,F) :-
   adj(G,B,C,D,E,F).
pre_adj(B,C,D,E,F) :-
   sup_phrase(B,C,D,E,F).


sup_phrase(sup(most,B),C,D,E,F) :-
   sup_adj(B,C,D,E,F).
sup_phrase(sup(B,C),D,E,F,G) :-
   sup_adv(B,D,I,F,J),
   adj(quant,C,I,E,J,G).


comp_phrase(comp(B,C,D),E,F,G,H,I) :-
   comp(B,C,F,J,H,K),
%  np_no_do_trace(L),			undefined!
   prep_case(M),
   np(D,N,M,O,compl,L,E,J,G,K,I).


comp(B,C,D,E,F,G) :-
   comp_adv(B,D,H,F,I),
   adj(quant,C,H,J,I,K),
   ~(than,J,E,K,G).
comp(more,B,C,D,E,F) :-
   rel_adj(B,C,G,E,H),
   ~(than,G,D,H,F).
comp(same,B,C,D,E,F) :-
   ~(as,C,G,E,H),
   adj(quant,B,G,I,H,J),
   ~(as,I,D,J,F).


relative(B,[C],D,E,F,G,H,I,J) :-
   is_pred(D),
   rel_conj(B,K,C,F,G,H,I,J).
relative(B,[],C,D,D,E,E,F,F).


rel_conj(B,C,D,E,F,G,H,I) :-
   rel(B,J,K,F,L,H,M),
   rel_rest(B,C,J,D,K,E,L,G,M,I).


rel_rest(B,C,D,E,F,G,H,I,J,K) :-
   conj(C,L,D,M,E,H,N,J,O),
   rel_conj(B,L,M,G,N,I,O,K).
rel_rest(B,C,D,D,E,E,F,F,G,G).


rel(B,rel(C,D),E,F,G,H,I) :-
   xopen(F,J,H,K),
   variable(B,C,J,L,K,M),
   s(D,N,L,O,M,P),
   do_trace(Q),
   minus(N,Q,E),
   close(O,G,P,I).


variable(B,C,D,E,F,x(gap,nonterminal,np(np(B,wh(C),[]),B,G,H,I,J,K),L)) :-
   ~(that,D,E,F,L),
   do_trace(J,K).
variable(B,C,D,E,F,x(gap,nonterminal,np(G,H,I,J,K,L,M),N)) :-
   wh(C,B,G,H,I,D,E,F,N),
   do_trace(L,M).
variable(B,C,D,E,F,x(gap,nonterminal,pp(pp(G,H),compl,I,J),K)) :-
   prep(G,D,L,F,M),
   wh(C,B,H,N,O,L,E,M,K),
   do_trace(I,J),
   compl_case(O).


wh(B,C,np(C,wh(B),[]),C,D,E,F,G,H) :-
   rel_pron(I,E,F,G,H),
   role(I,decl,D).
wh(B,C,np(D,E,[pp(F,G)]),D,H,I,J,K,L) :-
   np_head0(E,D,M+common,I,N,K,O),
   prep(F,N,P,O,Q),
   wh(B,C,G,R,S,P,J,Q,L).
wh(B,C,D,E,F,G,H,I,J) :-
   whose(B,C,G,K,I,L),
   s_all(M),
   np(D,E,F,def,subj,M,N,K,H,L,J).


reduced_relative(B,C,D,E,F,G,H,I) :-
   is_pred(D),
   reduced_rel_conj(B,J,C,E,F,G,H,I).


reduced_rel_conj(B,C,D,E,F,G,H,I) :-
   reduced_rel(B,J,K,F,L,H,M),
   reduced_rel_rest(B,C,J,D,K,E,L,G,M,I).


reduced_rel_rest(B,C,D,E,F,G,H,I,J,K) :-
   conj(C,L,D,M,E,H,N,J,O),
   reduced_rel_conj(B,L,M,G,N,I,O,K).
reduced_rel_rest(B,C,D,D,E,E,F,F,G,G).


reduced_rel(B,reduced_rel(C,D),E,F,G,H,I) :-
   xopen(F,J,H,K),
   reduced_wh(B,C,J,L,K,M),
   s(D,N,L,O,M,P),
   do_trace(Q),
   minus(N,Q,E),
   close(O,G,P,I).


reduced_wh(B,C,D,E,F,x(nogap,nonterminal,np(np(B,wh(C),[]),B,G,H,I,J,K),x(nogap,nonterminal,verb_form(be,pres+fin,B,main),x(nogap,nonterminal,neg(L,M),x(nogap,nonterminal,pred(M,N,O),P))))) :-
   neg(Q,M,D,R,F,S),
   pred(M,N,O,R,E,S,P),
   do_trace(J,K),
   subj_case(G).
reduced_wh(B,C,D,E,F,x(nogap,nonterminal,np(np(B,wh(C),[]),B,G,H,I,J,K),x(nogap,nonterminal,verb(L,M,N,O),P))) :-
   participle(L,N,O,D,E,F,P),
   do_trace(J,K),
   subj_case(G).
reduced_wh(B,C,D,E,F,x(nogap,nonterminal,np(G,H,I,J,K,L,M),x(gap,nonterminal,np(np(B,wh(C),[]),B,N,O,P,Q,R),S))) :-
   s_all(T),
   subj_case(I),
   verb_case(N),
   np(G,H,U,J,subj,T,V,D,E,F,S),
   do_trace(L,M),
   do_trace(Q,R).


verb(B,C,D,E,F,F,G,H) :-
   virtual(verb(B,C,D,E),G,H).
verb(verb(B,C,D+fin,E,F),G,H,C,I,J,K,L) :-
   verb_form(M,D+fin,G,N,I,O,K,P),
   verb_type(M,Q),
   neg(Q,F,O,R,P,S),
   rest_verb(N,M,B,C,E,R,J,S,L),
   verb_type(B,H).


rest_verb(aux,have,B,C,[perf|D],E,F,G,H) :-
   verb_form(I,past+part,J,K,E,L,G,M),
   have(I,B,C,D,L,F,M,H).
rest_verb(aux,be,B,C,D,E,F,G,H) :-
   verb_form(I,J,K,L,E,M,G,N),
   be(J,I,B,C,D,M,F,N,H).
rest_verb(aux,do,B,active,[],C,D,E,F) :-
   verb_form(B,inf,G,H,C,D,E,F).
rest_verb(main,B,B,active,[],C,C,D,D).


have(be,B,C,D,E,F,G,H) :-
   verb_form(I,J,K,L,E,M,G,N),
   be(J,I,B,C,D,M,F,N,H).
have(B,B,active,[],C,C,D,D).


be(past+part,B,B,passive,[],C,C,D,D).
be(pres+part,B,C,D,[prog],E,F,G,H) :-
   passive(B,C,D,E,F,G,H).


passive(be,B,passive,C,D,E,F) :-
   verb_form(B,past+part,G,H,C,D,E,F),
   verb_type(B,I),
   passive(I).
passive(B,B,active,C,C,D,D).


participle(verb(B,C,inf,D,E),F,C,G,H,I,J) :-
   neg(K,E,G,L,I,M),
   verb_form(B,N,O,P,L,H,M,J),
   participle(N,C,D),
   verb_type(B,F).


passive(B+trans).
passive(B+ditrans).


participle(pres+part,active,[prog]).
participle(past+part,passive,[]).


close(B,B,C,D) :-
   virtual(close,C,D).


xopen(B,B,C,x(gap,nonterminal,close,C)).


verb_args(B+C,D,E,F,G,H,I,J,K) :-
   advs(E,L,M,H,N,J,O),
   verb_args(C,D,L,F,G,N,I,O,K).
verb_args(trans,active,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
verb_args(ditrans,B,[arg(C,D)|E],F,G,H,I,J,K) :-
   verb_arg(np,D,L,H,M,J,N),
   object(C,E,L,G,M,I,N,K).
verb_args(be,B,[void],C,C,D,E,F,G) :-
   terminal(there,D,E,F,G).
verb_args(be,B,[arg(pred,C)],D,E,F,G,H,I) :-
   pred_conj(J,C,E,F,G,H,I).
verb_args(be,B,[arg(dir,C)],D,E,F,G,H,I) :-
   verb_arg(np,C,E,F,G,H,I).
verb_args(have,active,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
verb_args(B,C,[],D,D,E,E,F,F) :-
   no_args(B).


object(B,C,D,E,F,G,H,I) :-
   adv(J),
   minus(J,D,K),
   advs(C,L,K,F,M,H,N),
   obj(B,L,D,E,M,G,N,I).


obj(ind,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
obj(dir,[],B,B,C,C,D,D).


pred_conj(B,C,D,E,F,G,H) :-
   pred(I,J,K,E,L,G,M),
   pred_rest(B,J,C,K,D,L,F,M,H).


pred_rest(B,C,D,E,F,G,H,I,J) :-
   conj(B,K,C,L,D,G,M,I,N),
   pred_conj(K,L,F,M,H,N,J).
pred_rest(B,C,C,D,D,E,E,F,F).


verb_arg(np,B,C,D,E,F,G) :-
   s_all(H),
   verb_case(I),
   np(B,J,I,K,compl,H,C,D,E,F,G).


advs([B|C],D,E,F,G,H,I) :-
   is_adv(E),
   adverb(B,F,J,H,K),
   advs(C,D,E,J,G,K,I).
advs(B,B,C,D,D,E,E).


adj_phrase(B,C,D,E,F,G) :-
   adj(H,B,D,E,F,G),
   empty(C).
adj_phrase(B,C,D,E,F,G) :-
   comp_phrase(B,C,D,E,F,G).


no_args(trans).
no_args(ditrans).
no_args(intrans).


conj(conj(B,C),conj(B,D),E,F,conj(B,E,F),G,H,I,J) :-
   conj(B,C,D,G,H,I,J).


noun(B,C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   noun_form(H,B,C).


adj(B,adj(C),D,E,F,G) :-
   terminal(C,D,E,F,G),
   adj(C,B).


prep(prep(B),C,D,E,F) :-
   terminal(B,C,D,E,F),
   prep(B).


rel_adj(adj(B),C,D,E,F) :-
   terminal(G,C,D,E,F),
   rel_adj(G,B).


sup_adj(adj(B),C,D,E,F) :-
   terminal(G,C,D,E,F),
   sup_adj(G,B).


comp_adv(less,B,C,D,E) :-
   ~(less,B,C,D,E).
comp_adv(more,B,C,D,E) :-
   ~(more,B,C,D,E).


sup_adv(least,B,C,D,E) :-
   ~(least,B,C,D,E).
sup_adv(most,B,C,D,E) :-
   ~(most,B,C,D,E).


rel_pron(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   rel_pron(G,B).


name(B,C,D,E,F) :-
   opt_the(C,G,E,H),
   terminal(B,G,D,H,F),
   name(B).


int_art(B,plu,quant(same,wh(B)),C,D,E,F) :-
   ~(how,C,G,E,H),
   ~(many,G,D,H,F).
int_art(B,C,D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   int_art(I,B,C,D).


int_pron(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   int_pron(G,B).


adverb(adv(B),C,D,E,F) :-
   terminal(B,C,D,E,F),
   adverb(B).


poss_pron(pronoun(B),C+D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   poss_pron(I,B,C,D).


pers_pron(pronoun(B),C+D,E,F,G,H,I) :-
   terminal(J,F,G,H,I),
   pers_pron(J,B,C,D,E).


quantifier_pron(B,C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   quantifier_pron(H,B,C).


context_pron(prep(in),place,B,C,D,E) :-
   ~(where,B,C,D,E).
context_pron(prep(at),time,B,C,D,E) :-
   ~(when,B,C,D,E).


number(nb(B),C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   number(H,B,C).


terminator(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   terminator(G,B).


opt_the(B,B,C,C).
opt_the(B,C,D,E) :-
   ~(the,B,C,D,E).


conj(B,list,list,C,D,E,F) :-
   terminal(',',C,D,E,F).
conj(B,list,end,C,D,E,F) :-
   terminal(B,C,D,E,F),
   conj(B).


loc_pred(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   loc_pred(G,B).


~(B,C,D,E,F) :-
   terminal(B,C,D,E,F),
   ~B.

