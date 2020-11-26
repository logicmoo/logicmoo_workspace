% MENDEL

% parent generation
ex(colour(a,red),+).
ex(colour(b,yellow),+).

% f1
ex(colour(k(a,a),red),+).
ex(colour(k(a,b),red),+).
ex(colour(k(b,b),yellow),+).

% parent with f1
ex(colour(k(a,k(a,a)),red),+).
ex(colour(k(a,k(a,b)),red),+).
ex(colour(k(a,k(b,b)),red),+).
ex(colour(k(b,k(a,a)),red),+).
ex(colour(k(b,k(a,b)),red),+).
ex(colour(k(b,k(a,b)),yellow),+).
ex(colour(k(b,k(b,b)),yellow),+).

% f2
ex(colour(k(k(a,a),k(a,a)),red),+).
ex(colour(k(k(a,a),k(a,b)),red),+).
ex(colour(k(k(a,a),k(b,b)),red),+).
ex(colour(k(k(a,b),k(a,b)),red),+).
ex(colour(k(k(a,b),k(a,b)),yellow),+).
ex(colour(k(k(a,b),k(b,b)),red),+).
ex(colour(k(k(a,b),k(b,b)),yellow),+).
ex(colour(k(k(b,b),k(b,b)),yellow),+).






end_of_file.

Try:

| ?- clear_kb, do_full_kb('examples/ex5.pl').

| ?- clear_kb, init_kb('examples/ex5.pl').
| ?- show_kb.
| ?- lgg(1,2,J),show_clause(J).
| ?- nr_lgg(1,2,J),show_clause(J).
| ?- lgg(3,4,J),show_clause(J).
| ?- nr_lgg(3,4,J),show_clause(J).
| ?- gti(3,4,J),show_clause(J).  % erlaubt backtracking!
| ?- lgti(3,4,C,_,_).
| ?- lgg(8,9,J),show_clause(J).
| ?- rlgg(8,9,J),show_clause(J).
| ?- rlgg(8,9,cuddly_pet(_),J),show_clause(J).
| ?- gen_msg(8,9,J),show_clause(J).
| ?- rlgg(10,11,J),show_clause(J).
| ?- intra_construct1(14,15,A,B,C),show_clauses([14,15,A,B,C]).
| ?- intra_construct2(16,17,A,B,C),show_clauses([16,17,A,B,C]).


end_of_file.

Try for example

| ?- clear_kb, init_kb('examples/ex5.pl').
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


