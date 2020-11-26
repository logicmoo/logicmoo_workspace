:- use_module(library(logicmoo_utils)).

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

ensure_loaded_if_exists(X):- exists_source(X)->system:ensure_loaded(X);dmsg(ensure_loaded_if_exists(X)).
use_module_if_exists(X):- exists_source(X)->system:use_module(X);dmsg(use_module_if_exists(X)).
use_module_if_exists(X,Y):- exists_source(X)->system:use_module(X,Y);dmsg(use_module_if_exists(X,Y)).

:- prolog_load_context(directory,Dir), 
asserta((
user:file_search_path(home, Dir) %%%%set appropriately!
)).

rev(A,B):-
    rev(A,B,[]).

rev([],B,B):-!.
rev([X|A],B,C):-
    rev(A,B,[X|C]).

nonmember(Arg,[Arg|_]) :-
        !,
        fail.
nonmember(Arg,[_|Tail]) :-
        !,
        nonmember(Arg,Tail).
nonmember(_,[]).


%writes Question (using write/1) to the terminal, regardless of the current output stream, and reads an answer. The prompt is followed by ? , so you should not put a question mark in the question yourself. The answer is the first character typed in response; anything following on the same line will be thrown away. If the answer is y or Y, yesno/1 succeeds. If the answer is n or N, yesno/1 fails. Otherwise it repeats the question. The user has to explicitly type a y or n before it will stop. Because the rest of the line is thrown away, the user can type yes, Yes, You'd better not, and so forth with exactly the same effect as a plain y. If the user just presses <RET>, that is not taken as yes.
yesno(Question):- yesno(Question,no).
yesno(Question, Default):- format('~N~w? (~w): ',[Question,Default]),get_single_char(YN), (YN = 13 -> Default==yes; member(YN, `yY`)).
%is like yesno/1 except that
%Default may be an atom (the first character of whose name will be used), a string (whose first character will be used) or an ASCII code, and will be written in brackets before the question mark; and
%if the user just presses <RET>, Default will be used as the answer.
%For example, yesno('Do you want an extended trace', yes)         
%prints Do you want an extended trace [yes]? _
/*

ask_chars(+Prompt, +MinLength, +MaxLength, -Answer)
writes Prompt to the terminal, and reads a line of characters from it. This response must contain between MinLength and MaxLength characters inclusive, otherwise the question will be repeated until an answer of satisfactory length is obtained. Leading and/or trailing layout characters are retained in the result, and are counted when determining the length of the answer. The list of character codes read is unified with Answer. Note that a colon and a space (: ) are added to the Prompt, so don't add such punctuation yourself. The end-user can find out what sort of input is required by typing a line that starts with a question mark. Therefore it is not possible to read such a line as data. See prompted_line/2 in library(prompt).
Examples:

          | ?- ask_chars('Label', 1, 8, Answer).
          Label: 213456789
          Please enter between 1 and 8 characters.
          Do not add a full stop unless it is part of the answer.
          Label: four
          
          Answer = "four"
          
          | ?- ask_chars('Heading', 1, 30, Answer).
          Heading: ?
          Please enter between 1 and 30 characters.
          Do not add a full stop unless it is part of the answer.
          Heading:    three leading spaces
          
          Answer = "   three leading spaces"
          
*/         
ask_chars(Label, S, E, Answer):- 
  repeat, 
   format('~N~w: ?',[Label]), 
   read_line_to_string_echo(current_input,Answer),atom_length(Answer,Len),
   (between(S,E,Len) -> ! ; (format("~NPlease enter between ~w and ~w characters.~n",[S,E]),fail)).

unify(X, Y):- unify_with_occurs_check(X,Y).

:- use_module_if_exists(library(ordsets)).

union(X,Y):- ord_union(X,Y).
%subseq(X,Y,Z):- ord_union(X,Y).

'$list_skel'(V) :- var(V), !, fail.
'$list_skel'([]).
'$list_skel'([_|L]) :-
    '$list_skel'(L).

% subseq(Sequence1, SubSequence2, Complement):- 
subseq(AB, A, B) :- '$list_skel'(AB), !,
    '$subseq'(AB, A, B).
subseq(AB, A, B) :- '$list_skel'(A), '$list_skel'(B), !,
    '$subseq'(AB, A, B).
subseq(AB, A, _B) :-
    throw('instantiation error'(AB,A)).

'$subseq'([], [], []).
'$subseq'([X|AB], A, [X|B]) :-
    '$subseq'(AB, A, B).
'$subseq'([X|AB], [X|A], B) :-
    '$subseq'(AB, A, B).

subseq0(AB, A) :- '$list_skel'(AB), !,
    '$subseq'(AB, A, _).
subseq0(AB, A) :-
    throw('instantiation error'(AB,A)).

subseq1(AB, A) :- '$list_skel'(AB), !,
    '$subseq'(AB, A, _),
    A \== AB.
subseq1(AB, A) :-
    throw('instantiation error'(AB,A)).


string_append( A , B , C):- string_concat(A,B,C).

save_predicates(List,Filename):- tell(Filename),listing(List),told.

%basics
%Succeeds when SubSequence and Complement are both subsequences of the list Sequence (the order of corresponding elements being preserved) and every element of Sequence which is not in SubSequence is in the Complement and vice versa. That is,




prompt(X):- format('~N~w ',[X]). 

:- expects_dialect(sicstus).

do_full_kb(KB):- 
 clear_kb, init_kb(KB),
 nth_clause(do_full_kb1,Index,_),
 do_full_kb(_,Index).

do_full_kb(KB,Index):-
 nth_clause(do_full_kb1,Index,Ref),
 clause(do_full_kb1,Goal,Ref),
 do_full_kb(KB,Index,Goal).

do_full_kb(KB,Index,Goal):-
 once((
 ignore((nonvar(KB),clear_kb, init_kb(KB))),
  format('~N===================',[]),
  format('~N======= Nth: ~w  ~p  =======',[Index,Goal]),
  format('~N===================~n',[]),
   show_kb, 
   catch(do_full_call_each(Goal),E,(dumpST,throw(E))),
   show_kb,   
   format('~N==== DONE: ~w ========~n~n',[Index]))).

do_full_call_each((G1,G2)):- !, do_full_call_each(G1),do_full_call_each(G2).
do_full_call_each((G1->G2)):- 
 \+ \+ (( 
   wdmsg(do_call((G1->G2=Vars))), 
  my_do_call(G1),!,term_variables(G2,Vars),my_do_call(G2),!, 
  wdmsg(did_call((+Vars))))), !.
do_full_call_each(G1):- G2 = true,
 \+ \+ (( 
   wdmsg(do_call((G1->G2=Vars))), 
  my_do_call(G1),!,term_variables(G2,Vars),my_do_call(G2),!, 
  wdmsg(did_call((+Vars))))),!.

% my_do_call(G):- !, must_or_rtrace(G).
my_do_call(G):- notrace(ignore(catch(G,_,true))).

do_full_kb1:- 
 argument_types,
 show_kb,
 complete_chk,  %% Antwort: no
 ip(A),         %% gibt alle unabgedeckten Bsple zur"uck
 clause_heads, eval_examples,
 show_kb,
 complete_chk,  %% geht jetzt gut
 correct_chk,
 fp(A),         %% gibt inkorrekte Klausel(n) + ihre Instantiierung(en) zur"uck
                    %% in der Form [ID:[Instanz]]
 refinement(_ID,_),  %% wobei ID der der inkorrekten Klausel ist -> gibt 
                        %% Spezialisierungen dieser Klausel (in einer Liste)
 flatten_kb.    %% kb funktionsfrei machen

do_full_kb1:- 
 intra_construct1(1,2,A,B,C) -> show_clauses([1,2,A,B,C]),
 g2_op(1,2,A,B,C),   %% stellt Fragen
 show_kb,
 identify(4,3,J) -> show_clause(J),
 identify(5,_I,J) -> show_clause(J),
 apply_g2([4,5,10],_A,_BB).

do_full_kb1:- 
 intra_construct1(10,11,_A,_B,_C),
 show_clauses([10,11,13,14,15]),
 g1_op(5,1,I),
 g1_op(5,3,I),
 absorb(5,1,I),
 elem_saturate(5,1,I),
 saturate(5,I,10),
 most_spec_v(5,I,J),
 inv_derivate(5,J),
 show_kb,
 lgg(7,9,J) -> show_clause(J).

do_full_kb1:-  
  nr_lgg(7,9,J) -> show_clause(J),
  get_clause(J,_,_,CL,_),reduce_complete(CL,CL1),
  store_clause(_,CL1,nrlgg,I), show_clause(I).

do_full_kb1:-  
 gen_msg(5,6,J,10) -> show_clause(J),
 gti(8,9,J) -> show_clause(J).

do_full_kb1:- 
 rlgg(5,6,J) -> show_clause(J).


do_full_kb1:- 
 lgg(1,2,J) -> show_clause(J),
 nr_lgg(1,2,J) -> show_clause(J),
 lgg(3,4,J) -> show_clause(J),
 nr_lgg(3,4,J) -> show_clause(J),
 gti(3,4,J) -> show_clause(J),  % erlaubt backtracking!
 lgti(3,4,C,_,_),
 lgg(8,9,J) -> show_clause(J),
 rlgg(8,9,J) -> show_clause(J),
 rlgg(8,9,cuddly_pet(_),J) -> show_clause(J),
 gen_msg(8,9,J) -> show_clause(J),
 rlgg(10,11,J) -> show_clause(J),
 intra_construct1(14,15,A,B,C) -> show_clauses([14,15,A,B,C]),
 intra_construct2(16,17,A,B,C) -> show_clauses([16,17,A,B,C]).


%% Sei ID1 der der Klausel:
%% app(A,B,C) :- cons_p(D,E,A),x_p(D),cons_p(F,G,E),a_p(F),nil_p(G),
%%               cons_p(H,I,B),b_p(H),cons_p(J,G,I),c_p(J),cons_p(D,K,C),cons_p(F,B,K),
%% ID2 der der Klausel:
%% app(A,B,C) :-                      cons_p(D,E,A),a_p(D),nil_p(E),
%%               cons_p(F,G,B),b_p(F),cons_p(H,E,G),c_p(H),cons_p(D,B,C),

% Dann teste:
do_full_kb1:- 
 absorb(ID1,ID2,J) -> show_clause(J),
 elem_saturate(ID1,ID2,J1) -> show_clause(J1),
 saturate(ID1,J2,5) -> show_clause(J2),
 unflatten_kb,
 !.


 
