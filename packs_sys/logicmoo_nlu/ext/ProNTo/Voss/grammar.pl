:- ensure_loaded('earley.pl').

%Standard rules and words

:- multifile(word/2).
:- multifile(rule/2).
:- dynamic(word/2).
:- dynamic(rule/2).

rule(s,[vp]).
rule(s,[cp,vp]).
rule(s,[np,vp]).
rule(cp,[c,s]).
rule(np,[d,n]).
rule(np,[np,conj,np]).
rule(np,[d,n,pp]).
rule(vp,[v,np]).
rule(vp,[v,np,pp]).

%rule(vp,[v,pp]).
rule(pp,[p,np]).
rule(d,[]).



word(d,the).
word(d,an).
word(d,a).
word(d,all).
word(d,every).

word(n,cat).
word(n,cats).
word(n,dog).
word(n,dogs).
word(n,elephant).
word(n,elephants).
word(n,me).
word(v,dogs).
word(v,chase).
word(v,chases).
word(v,see).
word(v,sees).
word(v,amuse).
word(v,amuses).

word(p,near).
word(conj,and).
word(c,that).


% constituents build a parse tree in these rules
rule(s(s(NP,VP)),[np(NP),vp(VP)]).
rule(np(np(D,N)),[d(D),n(N)]).
rule(np(np(D,N,PP)),[d(D),n(N),pp(PP)]).
rule(vp(vp(V,NP,PP)),[v(V),np(NP),pp(PP)]).
rule(vp(vp(V,NP)),[v(V),np(NP)]).
rule(pp(pp(P,NP)),[p(P),np(NP)]).
rule(d,[]).
word(d(d(the)),the).
word(n(n(cat)),cat).
word(n(plural),cats).
word(n(n(dog)),dog).
word(v(v(chases)),chases).
word(p(p(near)),near).
word(p(p(with)),with).



