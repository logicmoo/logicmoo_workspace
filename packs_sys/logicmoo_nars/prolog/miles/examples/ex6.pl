


has_wings(X):- bird(X).
has_beak(X):- bird(X).
bird(X):- vulture(X).
carnivore(X):- vulture(X).

ex(has_wings(tweety),+).
ex(has_beak(tweety),+).




end_of_file.

Try for example


| ?- clear_kb, do_full_kb('examples/ex6.pl').

| ?- clear_kb, init_kb('examples/ex6.pl').
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

