
%%%Examples for truncation ops


member(X,[Y|R]):- member(X,R),member(X,[X]).
member(X,[X|_]).

app([x,a],[b,c],[x,a,b,c]).
app([a],[b,c],[a,b,c]).
app([],[b,c],[b,c]).

ex(app([1,2],[3],[1,2,3]),'+').
ex(app([x,a],[b,c],[x,a,b,c]),'+').
ex(app([a],[b,c],[a,b,c]),'+').
ex(app([],[],[]),'+').
ex(app([p],[],[p]),'+').
ex(app([],[u],[u]),'+').
ex(app([],[x,y],[x,y]),'+').
ex(app([r,s],[],[r,s]),'+').
ex(app([g],[d],[g,d]),'+').
ex(app([9,8,7],[],[9,8,7]),'+').
ex(app([],[6,5,4],[6,5,4]),'+').
ex(app([4,3,5],[8],[4,3,5,8]),'+').
ex(app([r,w],[q,t,s,f,i],[r,w,q,t,s,f,i]),'+').
ex(app([j,k,l,m],[n,o,p,q,r],[j,k,l,m,n,o,p,q,r]),'+').
ex(app([r,s,t],[q,u,v],[t,s,r,q,u,v]),'-').
ex(app([s,t],[q,u,v],[s,r,q,u,v]),'-').


min(A,[A|B]):- min(C,B), ge(E,F).
p(X):- q(X,V1),r(V1,V2),q(V3),s(V3,V1).


column(X):- brick(X), standing(X), is_on(X,Y), ground(Y).

column(X):- brick(X), standing(X), is_on(X,Y), column(Y).

same_height(X,Y):- ground(X), ground(Y).

same_height(X,Y):- brick(X), standing(X), brick(Y), standing(Y), is_on(X,X1), is_on(Y,Y1), 
                   same_height(X1,Y1).


arch(X):- part_of(A,X), part_of(B,X), part_of(C,X), is_on(A,B), is_on(A,C), is_on(B,D), 
          is_on(C,E), ground(D), ground(E), left_of(B,C), does_not_touch(B,C), lying(A), 
          wedge(A), standing(B), standing(C), brick(B), brick(C).



ex(p(a),+).
ex(p(b),+).
ex(p(c),-).

q(a,qa).
q(b,qb).
q(c,qc).
r(qa,x).
r(qb,x).
r(qc,x).
s(sa,qa).
s(sb,qb).
s(sc,qc).
q(sa).
q(sb).




end_of_file.

Try for example

| ?- clear_kb, do_full_kb('examples/ex7.pl').

| ?- clear_kb, init_kb('examples/ex7.pl').
| ?- argument_types.
| ?- show_kb.
| ?- complete_chk.  %% Antwort: no
| ?- ip(A).         %% gibt alle unabgedeckten Bsple zur"uck
| ?- clause_heads, eval_examples.
| ?- show_kb.
| ?- complete_chk.  %% geht jetzt gut
| ?- correct_chk.
| ?- fp(A).         %% gibt inkorrekte Klausel(n) + ihre Instantiierung(en) zur"uck
                    %% in der Form [ID:[Instanz]]
| ?- refinement(ID,_).  %% wobei ID der der inkorrekten Klausel ist -> gibt 
                        %% Spezialisierungen dieser Klausel (in einer Liste)
| ?- flatten_kb.    %% kb funktionsfrei machen
| ?- show_kb.

%% Sei ID1 der der Klausel:
%% app(A,B,C) :- cons_p(D,E,A),x_p(D),cons_p(F,G,E),a_p(F),nil_p(G),
%%               cons_p(H,I,B),b_p(H),cons_p(J,G,I),c_p(J),cons_p(D,K,C),cons_p(F,B,K).
%% ID2 der der Klausel:
%% app(A,B,C) :-                      cons_p(D,E,A),a_p(D),nil_p(E),
%%               cons_p(F,G,B),b_p(F),cons_p(H,E,G),c_p(H),cons_p(D,B,C).

Dann teste:

| ?- absorb(ID1,ID2,J), show_clause(J).
| ?- elem_saturate(ID1,ID2,J1), show_clause(J1).
| ?- saturate(ID1,J2,5), show_clause(J2).
| ?- unflatten_kb.
| ?- show_kb.

