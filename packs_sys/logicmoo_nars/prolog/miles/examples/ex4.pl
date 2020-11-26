% diverse

% index 1 and 2
%%member(2,[1,2]):- member(2,[2]).
%%member(c,[a,b,c]):- member(c,[b,c]),member(c,[c]).

% index 3 and 4
scene(X):- part_of(X,Y),large(X),red(Y),
           part_of(X,Z),small(Z),green(Z),left_of(Y,Z).
scene(X):- part_of(X,Y),small(X),red(Y),
           part_of(X,Z),large(Z),green(Z),left_of(Y,Z).


% index 5-9
pet(X) :- cat(X).
pet(X) :- dog(X).
small(X) :- cat(X).

cuddly_pet(X):- small(X),fluffy(X),dog(X).
cuddly_pet(X):- fluffy(X),cat(X).

ex(cuddly_pet(cathy_the_cat),+).
ex(cuddly_pet(rosi_the_rabbit),+).
ex(cuddly_pet(tom_the_turtle),-).

% index 10-13
has_wings(p).
has_beak(p).
has_wings(X):- bird(X).
has_beak(X):- bird(X).

% index 14 and 15
%%min(2,[3,2]):- min(2,[2]).
%%min(5,[7,6,5]):- min(5,[6,5]).

% index 16 and 17
%%min1(D,[s(D)|E]):- min1(D,E).
%%min1(F,[s(s(F))|G]):- min1(F,G).


end_of_file.

| ?- clear_kb,do_full_kb('examples/ex4.pl').

Try:


| ?- clear_kb,init_kb('examples/ex4.pl').
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

| ?- clear_kb, init_kb('examples/ex1.pl').
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


