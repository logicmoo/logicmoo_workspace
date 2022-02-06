
sentence(A,B,C,D,E) :- 
        declarative(A,B,F,D,G),
        terminator('.',F,C,G,E).
sentence(A,B,C,D,E) :- 
        wh_question(A,B,F,D,G),
        terminator(?,F,C,G,E).
sentence(A,B,C,D,E) :- 
        yn_question(A,B,F,D,G),
        terminator(?,F,C,G,E).
sentence(A,B,C,D,E) :- 
        imperative(A,B,F,D,G),
        terminator(!,F,C,G,E).


declarative(decl(A),B,C,D,E) :- 
        s(A,F,B,C,D,E).


wh_question(whq(A,B),C,D,E,F) :- 
        variable_q(A,G,H,I,C,J,E,K),
        question(H,I,B,J,D,K,F).


np(A,B,C,D,E,F,G,H,H,I,J) :- 
        virtual(np(A,B,C,D,E,F,G),I,J).
np(np(A,B,[]),A,C,def,D,E,F,G,H,I,J) :- 
        is_pp(E),
        pers_pron(B,A,K,G,H,I,J),
        empty(F),
        role(K,decl,C).
np(np(A,B,C),A,D,E,F,G,H,I,J,K,L) :- 
        is_pp(G),
        np_head(B,A,E+M,N,C,I,O,K,P),
        np_all(Q),
        np_compls(M,A,F,N,Q,H,O,J,P,L).
np(part(A,B),3+C,D,indef,E,F,G,H,I,J,K) :- 
        is_pp(F),
        determiner(A,C,indef,H,L,J,M),
        terminal(of,L,N,M,O),
        s_all(P),
        prep_case(Q),
        np(B,3+plu,Q,def,E,P,G,N,I,O,K).


variable_q(A,B,C,D,E,F,G,x(gap,nonterminal,np(H,B,D,I,J,K,L),M)) :- 
        whq(A,B,H,C,E,F,G,M),
        trace(K,L).
variable_q(A,B,compl,C,D,E,F,x(gap,nonterminal,pp(pp(G,H),compl,I,J),K)) :- 
        prep(G,D,L,F,M),
        whq(A,B,H,N,L,E,M,K),
        trace(I,J),
        compl_case(C).
variable_q(A,B,compl,C,D,E,F,x(gap,nonterminal,adv_phrase(pp(G,np(B,np_head(int_det(A),[],H),[])),I,J),K)) :- 
        context_pron(G,H,D,E,F,K),
        trace(I,J),
        verb_case(C).
variable_q(A,B,compl,C,D,E,F,x(gap,nonterminal,pred(adj,value(G,wh(A)),H),I)) :- 
        terminal(how,D,J,F,K),
        adj(quant,G,J,E,K,I),
        empty(H),
        verb_case(C).


pp(A,B,C,D,E,E,F,G) :- 
        virtual(pp(A,B,C,D),F,G).
pp(pp(A,B),C,D,E,F,G,H,I) :- 
        prep(A,F,J,H,K),
        prep_case(L),
        np(B,M,L,N,C,D,E,J,G,K,I).


adv_phrase(A,B,C,D,D,E,F) :- 
        virtual(adv_phrase(A,B,C),E,F).
adv_phrase(pp(A,B),C,D,E,F,G,H) :- 
        loc_pred(A,E,I,G,J),
        pp(pp(prep(of),B),compl,C,D,I,F,J,H).


pred(A,B,C,D,D,E,F) :- 
        virtual(pred(A,B,C),E,F).
pred(A,B,C,D,E,F,G) :- 
        adj_phrase(B,C,D,E,F,G).
pred(neg,A,B,C,D,E,F) :- 
        s_all(G),
        pp(A,compl,G,B,C,D,E,F).
pred(A,B,C,D,E,F,G) :- 
        s_all(H),
        adv_phrase(B,H,C,D,E,F,G).


whq(A,B,C,undef,D,E,F,G) :- 
        int_det(A,B,D,H,F,I),
        s_all(J),
        np(C,B,K,L,subj,J,M,H,E,I,G).
whq(A,3+B,np(3+B,wh(A),[]),C,D,E,F,G) :- 
        int_pron(C,D,E,F,G).


int_det(A,3+B,C,D,E,F) :- 
        whose(A,B,C,D,E,F).
int_det(A,3+B,C,D,E,F) :- 
        int_art(A,B,C,D,E,F).


np_head0(A,B,C,D,D,E,F) :- 
        virtual(np_head0(A,B,C),E,F).
np_head0(name(A),3+sin,def+proper,B,C,D,E) :- 
        name(A,B,C,D,E).
np_head0(np_head(A,B,C),3+D,E+common,F,G,H,I) :- 
        determiner(A,D,E,F,J,H,K),
        adjs(B,J,L,K,M),
        noun(C,D,L,G,M,I).
np_head0(A,B,def+proper,C,D,E,x(nogap,nonterminal,gen_marker,F)) :- 
        poss_pron(A,B,C,D,E,F).
np_head0(np_head(A,[],B),3+sin,indef+common,C,D,E,F) :- 
        quantifier_pron(A,B,C,D,E,F).


gen_marker(A,A,B,C) :- 
        virtual(gen_marker,B,C).
gen_marker(A,B,C,D) :- 
        terminal('''',A,E,C,F),
        an_s(E,B,F,D).


whose(A,B,C,D,E,x(nogap,nonterminal,np_head0(wh(A),B,proper),x(nogap,nonterminal,gen_marker,F))) :- 
        terminal(whose,C,D,E,F).


question(A,B,C,D,E,F,G) :- 
        subj_question(A),
        role(subj,H,B),
        s(C,I,D,E,F,G).
question(A,B,C,D,E,F,G) :- 
        fronted_verb(A,B,D,H,F,I),
        s(C,J,H,E,I,G).


det(A,B,C,D,D,E,F) :- 
        virtual(det(A,B,C),E,F).
det(det(A),B,C,D,E,F,G) :- 
        terminal(H,D,E,F,G),
        det(H,B,A,C).
det(generic,A,generic,B,B,C,C).


int_art(A,B,C,D,E,x(nogap,nonterminal,det(F,B,def),G)) :- 
        int_art(A,B,F,C,D,E,G).


subj_qustion(subj).


subj_question(undef).


yn_question(q(A),B,C,D,E) :- 
        fronted_verb(nil,F,B,G,D,H),
        s(A,I,G,C,H,E).


verb_form(A,B,C,D,E,E,F,G) :- 
        virtual(verb_form(A,B,C,D),F,G).
verb_form(A,B,C,D,E,F,G,H) :- 
        terminal(I,E,F,G,H),
        verb_form(I,A,B,C).


neg(A,B,C,C,D,E) :- 
        virtual(neg(A,B),D,E).
neg(aux+A,neg,B,C,D,E) :- 
        terminal(not,B,C,D,E).
neg(A,pos,B,B,C,C).


fronted_verb(A,B,C,D,E,x(gap,nonterminal,verb_form(F,G,H,I),x(nogap,nonterminal,neg(J,K),L))) :- 
        verb_form(F,G,H,M,C,N,E,O),
        verb_type(F,aux+P),
        role(A,I,B),
        neg(Q,K,N,D,O,L).


imperative(imp(A),B,C,D,E) :- 
        imperative_verb(B,F,D,G),
        s(A,H,F,C,G,E).


imperative_verb(A,B,C,x(nogap,terminal,you,x(nogap,nonterminal,verb_form(D,imp+fin,2+sin,main),E))) :- 
        verb_form(D,inf,F,G,A,B,C,E).


s(s(A,B,C,D),E,F,G,H,I) :- 
        subj(A,J,K,F,L,H,M),
        verb(B,J,K,N,L,O,M,P),
        empty(Q),
        s_all(R),
        verb_args(K,N,C,Q,S,O,T,P,U),
        minus(R,S,V),
        plus(R,S,W),
        verb_mods(D,V,W,E,T,G,U,I).


subj(there,A,B+be,C,D,E,F) :- 
        terminal(there,C,D,E,F).
subj(A,B,C,D,E,F,G) :- 
        s_all(H),
        subj_case(I),
        np(A,B,I,J,subj,H,K,D,E,F,G).


np_head(A,B,C,D,E,F,G,H,I) :- 
        np_head0(J,K,L,F,M,H,N),
        possessive(J,K,L,O,O,A,B,C,D,E,M,G,N,I).


np_compls(proper,A,B,[],C,D,E,E,F,F) :- 
        empty(D).
np_compls(common,A,B,C,D,E,F,G,H,I) :- 
        np_all(J),
        np_mods(A,B,K,C,D,L,J,M,F,N,H,O),
        relative(A,K,L,M,E,N,G,O,I).


possessive(A,B,C,[],D,E,F,G,H,I,J,K,L,M) :- 
        gen_case(J,N,L,O),
        np_head0(P,Q,R,N,S,O,T),
        possessive(P,Q,R,U,[pp(poss,np(B,A,D))|U],E,F,G,H,I,S,K,T,M).
possessive(A,B,C,D,E,A,B,C,D,E,F,F,G,G).


gen_case(A,B,C,x(nogap,terminal,the,D)) :- 
        gen_marker(A,B,C,D).


an_s(A,B,C,D) :- 
        terminal(s,A,B,C,D).
an_s(A,A,B,B).


determiner(A,B,C,D,E,F,G) :- 
        det(A,B,C,D,E,F,G).
determiner(A,B,C,D,E,F,G) :- 
        quant_phrase(A,B,C,D,E,F,G).


quant_phrase(quant(A,B),C,D,E,F,G,H) :- 
        quant(A,D,E,I,G,J),
        number(B,C,I,F,J,H).


quant(A,indef,B,C,D,E) :- 
        neg_adv(F,A,B,G,D,H),
        comp_adv(F,G,I,H,J),
        terminal(than,I,C,J,E).
quant(A,indef,B,C,D,E) :- 
        terminal(at,B,F,D,G),
        sup_adv(H,F,C,G,E),
        sup_op(H,A).
quant(the,def,A,B,C,D) :- 
        terminal(the,A,B,C,D).
quant(same,indef,A,A,B,B).


neg_adv(A,not+A,B,C,D,E) :- 
        terminal(not,B,C,D,E).
neg_adv(A,A,B,B,C,C).


sup_op(least,not+less).
sup_op(most,not+more).


np_mods(A,B,C,[D|E],F,G,H,I,J,K,L,M) :- 
        np_mod(A,B,D,F,N,J,O,L,P),
        trace(Q),
        plus(Q,N,R),
        minus(F,R,S),
        plus(N,F,T),
        np_mods(A,B,C,E,S,G,T,I,O,K,P,M).
np_mods(A,B,C,C,D,D,E,E,F,F,G,G).


np_mod(A,B,C,D,E,F,G,H,I) :- 
        pp(C,B,D,E,F,G,H,I).
np_mod(A,B,C,D,E,F,G,H,I) :- 
        reduced_relative(A,C,D,E,F,G,H,I).


verb_mods([A|B],C,D,E,F,G,H,I) :- 
        verb_mod(A,C,J,F,K,H,L),
        trace(M),
        plus(M,J,N),
        minus(C,N,O),
        plus(J,C,P),
        verb_mods(B,O,P,E,K,G,L,I).
verb_mods([],A,B,B,C,C,D,D).


verb_mod(A,B,C,D,E,F,G) :- 
        adv_phrase(A,B,C,D,E,F,G).
verb_mod(A,B,C,D,E,F,G) :- 
        is_adv(B),
        adverb(A,D,E,F,G),
        empty(C).
verb_mod(A,B,C,D,E,F,G) :- 
        pp(A,compl,B,C,D,E,F,G).


adjs([A|B],C,D,E,F) :- 
        pre_adj(A,C,G,E,H),
        adjs(B,G,D,H,F).
adjs([],A,A,B,B).


pre_adj(A,B,C,D,E) :- 
        adj(F,A,B,C,D,E).
pre_adj(A,B,C,D,E) :- 
        sup_phrase(A,B,C,D,E).


sup_phrase(sup(most,A),B,C,D,E) :- 
        sup_adj(A,B,C,D,E).
sup_phrase(sup(A,B),C,D,E,F) :- 
        sup_adv(G,C,H,E,I),
        adj(quant,B,H,D,I,F).


comp_phrase(comp(A,B,C),D,E,F,G,H) :- 
        comp(A,B,E,I,G,J),
        np_no_trace(K),
        prep_case(L),
        np(C,M,L,N,compl,K,D,I,F,J,H).


comp(A,B,C,D,E,F) :- 
        comp_adv(A,C,G,E,H),
        adj(quant,B,G,I,H,J),
        terminal(than,I,D,J,F).
comp(more,A,B,C,D,E) :- 
        rel_adj(A,B,F,D,G),
        terminal(than,F,C,G,E).
comp(same,A,B,C,D,E) :- 
        terminal(as,B,F,D,G),
        adj(quant,A,F,H,G,I),
        terminal(as,H,C,I,E).


relative(A,[B],C,D,E,F,G,H,I) :- 
        is_pred(C),
        rel_conj(A,J,B,E,F,G,H,I).
relative(A,[],B,C,C,D,D,E,E).


rel_conj(A,B,C,D,E,F,G,H) :- 
        rel(A,I,J,E,K,G,L),
        rel_rest(A,B,I,C,J,D,K,F,L,H).


rel_rest(A,B,C,D,E,F,G,H,I,J) :- 
        conj(B,K,C,L,D,G,M,I,N),
        rel_conj(A,K,L,F,M,H,N,J).
rel_rest(A,B,C,C,D,D,E,E,F,F).


rel(A,rel(B,C),D,E,F,G,H) :- 
        island(E,I,G,J),
        variable(A,B,I,K,J,L),
        s(C,M,K,N,L,O),
        trace(P),
        minus(M,P,D),
        dnalsi(N,F,O,H).


variable(A,B,C,D,E,x(gap,nonterminal,np(np(A,wh(B),[]),A,F,G,H,I,J),K)) :- 
        terminal(that,C,D,E,K),
        trace(I,J).
variable(A,B,C,D,E,x(gap,nonterminal,np(F,G,H,I,J,K,L),M)) :- 
        wh(B,A,F,G,H,C,D,E,M),
        trace(K,L).
variable(A,B,C,D,E,x(gap,nonterminal,pp(pp(F,G),compl,H,I),J)) :- 
        prep(F,C,K,E,L),
        wh(B,A,G,M,N,K,D,L,J),
        trace(H,I),
        compl_case(N).


wh(A,B,np(B,wh(A),[]),B,C,D,E,F,G) :- 
        rel_pron(H,D,E,F,G),
        role(H,decl,C).
wh(A,B,np(C,D,[pp(E,F)]),C,G,H,I,J,K) :- 
        np_head0(D,C,L+common,H,M,J,N),
        prep(E,M,O,N,P),
        wh(A,B,F,Q,R,O,I,P,K).
wh(A,B,C,D,E,F,G,H,I) :- 
        whose(A,B,F,J,H,K),
        s_all(L),
        np(C,D,E,def,subj,L,M,J,G,K,I).


reduced_relative(A,B,C,D,E,F,G,H) :- 
        is_pred(C),
        reduced_rel_conj(A,I,B,D,E,F,G,H).


reduced_rel_conj(A,B,C,D,E,F,G,H) :- 
        reduced_rel(A,I,J,E,K,G,L),
        reduced_rel_rest(A,B,I,C,J,D,K,F,L,H).


reduced_rel_rest(A,B,C,D,E,F,G,H,I,J) :- 
        conj(B,K,C,L,D,G,M,I,N),
        reduced_rel_conj(A,K,L,F,M,H,N,J).
reduced_rel_rest(A,B,C,C,D,D,E,E,F,F).


reduced_rel(A,reduced_rel(B,C),D,E,F,G,H) :- 
        island(E,I,G,J),
        reduced_wh(A,B,I,K,J,L),
        s(C,M,K,N,L,O),
        trace(P),
        minus(M,P,D),
        dnalsi(N,F,O,H).


reduced_wh(A,B,C,D,E,x(nogap,nonterminal,np(np(A,wh(B),[]),A,F,G,H,I,J),x(nogap,nonterminal,verb_form(be,pres+fin,A,main),x(nogap,nonterminal,neg(K,L),x(nogap,nonterminal,pred(L,M,N),O))))) :- 
        neg(P,L,C,Q,E,R),
        pred(L,M,N,Q,D,R,O),
        trace(I,J),
        subj_case(F).
reduced_wh(A,B,C,D,E,x(nogap,nonterminal,np(np(A,wh(B),[]),A,F,G,H,I,J),x(nogap,nonterminal,verb(K,L,M,N),O))) :- 
        participle(K,M,N,C,D,E,O),
        trace(I,J),
        subj_case(F).
reduced_wh(A,B,C,D,E,x(nogap,nonterminal,np(F,G,H,I,J,K,L),x(gap,nonterminal,np(np(A,wh(B),[]),A,M,N,O,P,Q),R))) :- 
        s_all(S),
        subj_case(H),
        verb_case(M),
        np(F,G,T,I,subj,S,U,C,D,E,R),
        trace(K,L),
        trace(P,Q).


verb(A,B,C,D,E,E,F,G) :- 
        virtual(verb(A,B,C,D),F,G).
verb(verb(A,B,C+fin,D,E),F,G,B,H,I,J,K) :- 
        verb_form(L,C+fin,F,M,H,N,J,O),
        verb_type(L,P),
        neg(P,E,N,Q,O,R),
        rest_verb(M,L,A,B,D,Q,I,R,K),
        verb_type(A,G).


rest_verb(aux,have,A,B,[perf|C],D,E,F,G) :- 
        verb_form(H,past+part,I,J,D,K,F,L),
        have(H,A,B,C,K,E,L,G).
rest_verb(aux,be,A,B,C,D,E,F,G) :- 
        verb_form(H,I,J,K,D,L,F,M),
        be(I,H,A,B,C,L,E,M,G).
rest_verb(aux,do,A,active,[],B,C,D,E) :- 
        verb_form(A,inf,F,G,B,C,D,E).
rest_verb(main,A,A,active,[],B,B,C,C).


have(be,A,B,C,D,E,F,G) :- 
        verb_form(H,I,J,K,D,L,F,M),
        be(I,H,A,B,C,L,E,M,G).
have(A,A,active,[],B,B,C,C).


be(past+part,A,A,passive,[],B,B,C,C).
be(pres+part,A,B,C,[prog],D,E,F,G) :- 
        passive(A,B,C,D,E,F,G).


passive(be,A,passive,B,C,D,E) :- 
        verb_form(A,past+part,F,G,B,C,D,E),
        verb_type(A,H),
        passive(H).
passive(A,A,active,B,B,C,C).


participle(verb(A,B,inf,C,D),E,B,F,G,H,I) :- 
        neg(J,D,F,K,H,L),
        verb_form(A,M,N,O,K,G,L,I),
        participle(M,B,C),
        verb_type(A,E).


passive(A+trans).
passive(A+ditrans).


participle(pres+part,active,[prog]).
participle(past+part,passive,[]).


dnalsi(A,A,B,C) :- 
        virtual(dnalsi,B,C).


island(A,A,B,x(gap,nonterminal,dnalsi,B)).


verb_args(A+B,C,D,E,F,G,H,I,J) :- 
        advs(D,K,L,G,M,I,N),
        verb_args(B,C,K,E,F,M,H,N,J).
verb_args(trans,active,[arg(dir,A)],B,C,D,E,F,G) :- 
        verb_arg(np,A,C,D,E,F,G).
verb_args(ditrans,A,[arg(B,C)|D],E,F,G,H,I,J) :- 
        verb_arg(np,C,K,G,L,I,M),
        object(B,D,K,F,L,H,M,J).
verb_args(be,A,[void],B,B,C,D,E,F) :- 
        terminal(there,C,D,E,F).
verb_args(be,A,[arg(pred,B)],C,D,E,F,G,H) :- 
        pred_conj(I,B,D,E,F,G,H).
verb_args(be,A,[arg(dir,B)],C,D,E,F,G,H) :- 
        verb_arg(np,B,D,E,F,G,H).
verb_args(have,active,[arg(dir,A)],B,C,D,E,F,G) :- 
        verb_arg(np,A,C,D,E,F,G).
verb_args(A,B,[],C,C,D,D,E,E) :- 
        no_args(A).


object(A,B,C,D,E,F,G,H) :- 
        adv(I),
        minus(I,C,J),
        advs(B,K,J,E,L,G,M),
        obj(A,K,C,D,L,F,M,H).


obj(ind,[arg(dir,A)],B,C,D,E,F,G) :- 
        verb_arg(np,A,C,D,E,F,G).
obj(dir,[],A,A,B,B,C,C).


pred_conj(A,B,C,D,E,F,G) :- 
        pred(H,I,J,D,K,F,L),
        pred_rest(A,I,B,J,C,K,E,L,G).


pred_rest(A,B,C,D,E,F,G,H,I) :- 
        conj(A,J,B,K,C,F,L,H,M),
        pred_conj(J,K,E,L,G,M,I).
pred_rest(A,B,B,C,C,D,D,E,E).


verb_arg(np,A,B,C,D,E,F) :- 
        s_all(G),
        verb_case(H),
        np(A,I,H,J,compl,G,B,C,D,E,F).


advs([A|B],C,D,E,F,G,H) :- 
        is_adv(D),
        adverb(A,E,I,G,J),
        advs(B,C,D,I,F,J,H).
advs(A,A,B,C,C,D,D).


adj_phrase(A,B,C,D,E,F) :- 
        adj(G,A,C,D,E,F),
        empty(B).
adj_phrase(A,B,C,D,E,F) :- 
        comp_phrase(A,B,C,D,E,F).


no_args(trans).
no_args(ditrans).
no_args(intrans).


conj(conj(A,B),conj(A,C),D,E,conj(A,D,E),F,G,H,I) :- 
        conj(A,B,C,F,G,H,I).


noun(A,B,C,D,E,F) :- 
        terminal(G,C,D,E,F),
        noun_form(G,A,B).


adj(A,adj(B),C,D,E,F) :- 
        terminal(B,C,D,E,F),
        adj(B,A).


prep(prep(A),B,C,D,E) :- 
        terminal(A,B,C,D,E),
        prep(A).


rel_adj(adj(A),B,C,D,E) :- 
        terminal(F,B,C,D,E),
        rel_adj(F,A).


sup_adj(adj(A),B,C,D,E) :- 
        terminal(F,B,C,D,E),
        sup_adj(F,A).


comp_adv(less,A,B,C,D) :- 
        terminal(less,A,B,C,D).
comp_adv(more,A,B,C,D) :- 
        terminal(more,A,B,C,D).


sup_adv(least,A,B,C,D) :- 
        terminal(least,A,B,C,D).
sup_adv(most,A,B,C,D) :- 
        terminal(most,A,B,C,D).


rel_pron(A,B,C,D,E) :- 
        terminal(F,B,C,D,E),
        rel_pron(F,A).


name(A,B,C,D,E) :- 
        opt_the(B,F,D,G),
        terminal(A,F,C,G,E),
        name(A).


int_art(A,plu,quant(same,wh(A)),B,C,D,E) :- 
        terminal(how,B,F,D,G),
        terminal(many,F,C,G,E).
int_art(A,B,C,D,E,F,G) :- 
        terminal(H,D,E,F,G),
        int_art(H,A,B,C).


int_pron(A,B,C,D,E) :- 
        terminal(F,B,C,D,E),
        int_pron(F,A).


adverb(adv(A),B,C,D,E) :- 
        terminal(A,B,C,D,E),
        adverb(A).


poss_pron(pronoun(A),B+C,D,E,F,G) :- 
        terminal(H,D,E,F,G),
        poss_pron(H,A,B,C).


pers_pron(pronoun(A),B+C,D,E,F,G,H) :- 
        terminal(I,E,F,G,H),
        pers_pron(I,A,B,C,D).


quantifier_pron(A,B,C,D,E,F) :- 
        terminal(G,C,D,E,F),
        quantifier_pron(G,A,B).


context_pron(prep(in),place,A,B,C,D) :- 
        terminal(where,A,B,C,D).
context_pron(prep(at),time,A,B,C,D) :- 
        terminal(when,A,B,C,D).


number(nb(A),B,C,D,E,F) :- 
        terminal(G,C,D,E,F),
        number(G,A,B).


terminator(A,B,C,D,E) :- 
        terminal(F,B,C,D,E),
        terminator(F,A).


opt_the(A,A,B,B).
opt_the(A,B,C,D) :- 
        terminal(the,A,B,C,D).


conj(A,list,list,B,C,D,E) :- 
        terminal(',',B,C,D,E).
conj(A,list,end,B,C,D,E) :- 
        terminal(A,B,C,D,E),
        conj(A).


loc_pred(A,B,C,D,E) :- 
        terminal(F,B,C,D,E),
        loc_pred(F,A).

