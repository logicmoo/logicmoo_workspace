/*
SLDNF Draw is covered by the Simplified BSD license:
Copyright (c) 2017, Marco Gavanelli
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies,
either expressed or implied, of the FreeBSD Project.
*/
/** <module> sldnfdraw

Produces a LaTeX drawing of an SLDNF tree

http://endif.unife.it/it/ricerca-1/aree-di-ricerca/informazione/ingegneria-informatica/software/sldnf-draw/sldnf-draw


@author Marco Gavanelli, Lorenzo Campioni, Fabrizio Riguzzi
@license Simplified BSD license
@copyright Marco Gavanelli
*/


% Version 1.4
% Does not crash when printing infinite terms
% Uses lib(var_names) for pretty printing the names of variables.

% Version 1.5
% If the resolvent is too long (over max_resolvent_length/1 characters), prints it in two or more rows

% Version 1.6
% Animations

% Version 1.61
% Bug fixes, predicate names can now contain underscores

% TODO:
% CLP? Minimize?


% Note that it does not handle correctly the cut in the resolvent, but only
% in a clause. Please, do not write ?- p,!. but define e new predicate
% callp:- p,!.

% Given a clause a:-b,!,c, we have that:
% 1.the scope of the cut is a:-b, in the sense that it will cut the
%   alternatives to a and b. This is represented with the list
%   of OpenCuts: it contains all the cuts that are in a clause that
%   has been selected.
% 2.The cut has an effect when it is reached, and its effect lasts
%   until the backtracking goes back to a. Thus, the information about
%   reaching a cut is saved with assert(reached(Cut)).
%   Each cut has a unique name, given by a counter.

% ported to SWI-Prolog by
% Lorenzo Campioni <lorenzo.campioni@student.unife.it>
% adapted to SWISH by
% Fabrizio Riguzzi <fabrizio.riguzzi@unife.it>

:-module(sldnfdraw,
  [draw_goal/1,set_depth/1,animate/1,
  op(900,fy,not)]).

:-use_module(library(apply)).
:-use_module(library(memfile)).

:- dynamic begin_resolvent/1.
:- dynamic end_resolvent/1.
:- dynamic begin_binding/1.
:- dynamic end_binding/1.
:- dynamic counter/1.
:- dynamic reached/2.
:- dynamic maxdepth/1.
:- dynamic current_slide/1.
:- dynamic fail_symbol/1.
:- dynamic cut_symbol/1.
:- dynamic success_symbol/1.
:- dynamic my_clause/2.
:- dynamic animations/1.

:- thread_local sldnf_input_mod/1.
:- dynamic sldnf_input_mod/1.

:-meta_predicate set_depth(:).
:-meta_predicate draw_goal(:).
%:-meta_predicate draw_goal(:,+).
:-meta_predicate draw(:,+,+,+).
:-meta_predicate draw(:,+,+,+,+,-).
:-meta_predicate print_builtin_children(+,:,+,+,+,+,+).
:-meta_predicate print_children(:,:,+,+,+,+,+).

:-meta_predicate clausola_list(:,-,+,-).
:-meta_predicate newclausola(:,-,+,-).
:-meta_predicate read_goal(:,-).
:-meta_predicate count_children(:,-,+).
:-meta_predicate vanilla(:).
:-meta_predicate max_subtree_width(:,-).
:-meta_predicate animate(:).



sldnf_version(1.6).

% Maximum length of the resolvent, in chars (very approximate)
% If the length exceeds this length, the resolvent will be printed on two (or more) lines
:- dynamic max_resolvent_length/1.
max_resolvent_length(25).

/**
 * set_depth(++Depth:int) is det
 *
 * Sets the maximum depth of the SLDNF tree
 */
set_depth(M:D):-
    retract(M:maxdepth(_)),
    assert(M:maxdepth(D)).

maxdepth(20). % Default max depth: 20

animations(no).

/**
 * animate(:Var) is det
 *
 * Sets animation on. The argument is unused (it is there to collect the
 * name of the calling module)
 */
animate(M:_):- retract(M:animations(_)), assert(M:animations(yes)).

%begin_binding('{\\tt ').
%end_binding('}').

%begin_resolvent('{\\color{blue} ').
%end_resolvent('}').

%cut('{\\color{red} (cut)}').
%cut('{\\color{red} $\\nrightarrow$}').
cut_symbol('(cut)').

%fail_symbol("$\\bot$").
%fail_symbol("{\\color{red} false}").
fail_symbol("false").

success_symbol("true").

d(G):- draw_goal(G,"tree.tex").

ds(G):- animations(yes),!,d(G), system('./animateTree.sh').
ds(G):- d(G), system('./drawTree.sh').
draw_goal(M:G,FileName,ListName):-
    init_file(FileName,write,File),
    term_length_chopped(G,Length),
    conv_sq_list(G,GSq0),
    maplist(normalize_not,GSq0,GSq),
    init_cuts(M),
	reset_slide(M),
    (draw(M:GSq,File,Length,ListName) ; true),
    close(File).
/**
 * draw_goal(++File:string) is det.
 * draw_goal(--Tree:string) is det
 *
 * Writes the Latex code of the tree to File or
 * returns it as a string in Tree
 */
draw_goal(M:String):-
    var(String),!,
    new_memory_file(Handle),
    open_memory_file(Handle, write, File),
    read_goal(M:G,ListName),
    term_length_chopped(G,Length),
    conv_sq_list(G,GSq0),
    maplist(normalize_not,GSq0,GSq),
    init_cuts(M),
	  reset_slide(M),
    (draw(M:GSq,File,Length,ListName) ; true),
    close(File),
    open_memory_file(Handle, read, R, [free_on_close(true)]),
    read_string(R, _Length, String),
    close(R).


draw_goal(M:FileName):-
    init_file(FileName,write,File),
    read_goal(M:G,ListName),
    term_length_chopped(G,Length),
    conv_sq_list(G,GSq0),
    maplist(normalize_not,GSq0,GSq),
    init_cuts(M),
	reset_slide(M),
    (draw(M:GSq,File,Length,ListName) ; true),
    close(File).

normalize_not( \+(A),not(A)):-!.

normalize_not(A,A).


draw(M:R,F,Longest,L):-
    M:maxdepth(Depth),
    draw(M:R,F,Longest,Depth,[],L).

% draw(+Resolvent,+Stream,MaxLenghtOfResolvent,MaxDepth,OpenCuts)

draw(_M:[],F,Longest,_,_,_):-
	success_symbol(SuccessSymbol),
    print_string_spaces(F,Longest,SuccessSymbol),
    fail.
draw(_,F,Longest,Depth,_,_):- Depth=<0, !,
    print_string_spaces(F,Longest,"..."),
    fail.
draw(M:[H|R],F,LongestIn,Depth,OpenCuts,ListName):-
    negative(H,G),
    Depth1 is Depth-1,
    write_indented(M,F,Depth,"\\begin{bundle}{"),
    print_resolvent(M,F,[not(G)|R],ListName),
    writeln(F,"}"),
	write_indented(M,F,Depth,"\\chunk{"),
    % NOTA: forse in questo caso non conviene metterlo, cosi` il box diventa giusto.
    %       pero` non si assicura che sia giusto il figlio del box...
    % Compute the maximum length
    term_length_chopped([not(G)|R],ResLen), max(LongestIn,ResLen,Length),
	write_indented(M,F,Depth,"\\begin{bundle}{\\framebox{"),
    (draw(M:[G],F,0,Depth1,[],ListName) ; true),
    write(F,"}}"),
	max_subtree_width(M:[G],MaxWidthTemp),
        max(MaxWidthTemp,Length,RES),
        TMP is 1.3*RES,
	float_to_integer(TMP,MaxWidth),
    (vanilla(M:G)
      ->    print_fail(M,F,Depth,MaxWidth) %MODIFIED: ResLen
      ;     write_indented(M,F,Depth,"\\chunk{"), start_pause(M,F),
                (draw(M:R,F,MaxWidth,Depth1,OpenCuts,ListName); true), end_pause(M,F), write(F,"}")
    ),
    indent(M,F,Depth),writeln(F,"\\end{bundle}"),
	%end_pause(F),
	indent(M,F,Depth),writeln(F,"}\\end{bundle}"),
	%end_pause(F),
    fail.

% Cut !
draw(M:[!|R],F,LongestIn,Depth,[LastCut|OpenCuts],ListName):- !,
    Depth1 is Depth-1,
    term_length_chopped([!|R],ResLen), max(LongestIn,ResLen,Length),
    write_indented(M,F,Depth,"\\begin{bundle}{"),
    print_resolvent(M,F,[!|R],ListName),
    writeln(F,"}"),
	M:current_slide(Slide),
    assert(M:reached(LastCut,Slide)),
    (print_builtin_children(true,M:R,F,Length,Depth1,OpenCuts,ListName);     writeln(F,"\\end{bundle}")),
    fail.

% Conjunction of goals: may occur inside another predicate, like not((G1,G2)).
draw(M:[(G1,G2)|R],F,LongestIn,Depth,OpenCuts,L):-
    draw(M:[G1,G2|R],F,LongestIn,Depth,OpenCuts,L).

% Built-in Predicate
draw(M:[G|R],F,LongestIn,Depth,OpenCuts,ListName):-
    Depth1 is Depth-1,
    not(G = not(_)),
    not(G=(_,_)),
    built_in(G),
    term_length_chopped([G|R],ResLen), max(LongestIn,ResLen,Length),
    write_indented(M,F,Depth,"\\begin{bundle}{"),
    print_resolvent(M,F,[G|R],ListName),
    writeln(F,"}"),
    (print_builtin_children(M:G,M:R,F,Length,Depth1,OpenCuts,ListName);     write_indented(M,F,Depth,"\\end{bundle}")),
    fail.

% User defined predicate
draw(M:[G|R],F,LongestIn,Depth,OpenCuts,ListName):-
    Depth1 is Depth-1,
    not(G = not(_)),
    not(G=(_,_)),
    not(built_in(G)),
%	max_subtree_width([G|R],MaxWidthTemp),
    term_length_chopped([G|R],ResLen), max(LongestIn,ResLen,Length),
	write_indented(M,F,Depth,"\\begin{bundle}{"),
    print_resolvent(M,F,[G|R],ListName),
    writeln(F,"}"),
    (print_children(M:G,M:R,F,Length,Depth1,OpenCuts,ListName);     indent(M,F,Depth), writeln(F,"\\end{bundle}") %,end_pause(M,F)
	),
    fail.

print_children(M:G,M:R,F,Length,Depth,OpenCuts,ListName):-
    %Depth1 is Depth-1,
    term_variables(G,Vars),
    vars_names(Vars,VarNames,ListName),
    count_children(M:G,NumChildren,ListName),
    (NumChildren = 1 -> Len = Length ; Len=0),
    increase_counter(M), get_counter(M,C),
    % Unique name for the node: if a cut is reached, it will have this name
    % First part: the cut may cut the alternatives for the clause
    retract_cut_on_backtracking(M,C),
    clausola_list(M:G,B,ListName,NewListName),
%%% QUI SI POTREBBE:
% 1. mettere un secondo parametro nel predicato dynamic reached: e` il numero della slide in cui il cut viene incontrato (viene incontrato nella draw([!|...)
% 2. cambiare questa clause_is_not_cut in una clause_is_cut (controllare che non ci siano variabili che vengono legate in questo ...)
% 3. la clause_is_cut restituisce anche la slide S in cui viene fatto il taglio
% 4. cambiare la start_pause in una pausa effettuata alla slide S
% 5. sincronizzare anche le slide seguenti (se necessario)
    (clause_is_cut(M,C,SlideCut)	%ATTENZIONE CHE HO TOLTO UNA NEGAZIONE! CONTROLLARE CHE NON CI SIANO DEI BINDING ...
        ->   write_indented(M,F,Depth,"\\chunk{"),start_pause(M,F,SlideCut), cut_symbol(StringCut), write(F,StringCut),write(F,"}"), end_pause(M,F), fail
		;    true),
    (check_body_contains_cut(B,OpenCuts,NewCuts,_AddedCut,C)
        ->    % retract_cut_on_backtracking(AddedCut)
              true
        ;     NewCuts=OpenCuts),
    % Second part: takes care of the alternatives of the predicates
    % in the body
    ( (member(Cut,OpenCuts),M:reached(Cut,SlideCut))
      ->        write_indented(M,F,Depth,"\\chunk{"),start_pause(M,F,SlideCut), cut_symbol(StringCut), write(F,StringCut),write(F,"}"),  end_pause(M,F), fail
      ;         write_indented(M,F,Depth,"\\chunk"),
                print_binding(M,F,VarNames,NewListName),
                write(F,"{"), start_pause(M,F),
                append(B,R,Ris),
                (draw(M:Ris,F,Len,Depth,NewCuts,NewListName) ; indent(M,F,Depth), end_pause(M,F), write(F,"}"), fail)
    )   .

print_children(M:G,_,F,Length,Depth,_,ListName):-
    not(clausola_list(M:G,_,ListName,_)),
    %not(clausola(G,_)),
    print_fail(M,F,Depth,Length),
    fail.

print_builtin_children(M:G,M:R,F,Length,Depth,OpenCuts,ListName):-
%    Depth1 is Depth-1,
    term_variables(G,Vars),
    vars_names(Vars,VarNames,ListName),
    findall(G,call(G),L),length(L,NumChildren),
    (NumChildren = 1 -> Len = Length ; Len=0),
    call(G),
    write_indented(M,F,Depth,"\\chunk"),
    print_binding(M,F,VarNames,ListName),
    write(F,"{"),start_pause(M,F),
    (draw(M:R,F,Len,Depth,OpenCuts,ListName) ; end_pause(M,F), write(F,"}"), fail).

print_builtin_children(G,M:_,F,Length,Depth,_OpenCuts,_):-
    not(call(G)),
    print_fail(M,F,Depth,Length),
    fail.


negative(not(G),G):-!.

negative(\+(G),G).
%%%%%%%%%%%%%%%% Predicates fot cut handling %%%%%%%%%%%%%%%%%

%check_body_contains_cut(+Body,++OpenCuts,-NewCuts,-AddedCut,++Counter)
check_body_contains_cut(B,OpenCuts,NewCuts,AddedCut,Counter):-
    memberchk(!,B),push_cut(OpenCuts,NewCuts,AddedCut,Counter).


% A clause is not cut if the cut of the current
% node has not been reached.
clause_is_not_cut(M,C):-
    not(M:reached(cut(C))).
clause_is_cut(M,C,Slide):-
    M:reached(cut(C),Slide).

push_cut(L,[cut(C)|L],cut(C),C).

% On backtracking, remove the information about the reached cuts
% that are not open
retract_cut_on_backtracking(_,_).
retract_cut_on_backtracking(M,C):-
    retract(M:reached(C,_)), fail.

follows(cut(N),cut(N1)):-
    N>N1.

last_cut([C],C):-!.
last_cut([Cut|Cuts],C):-
    last_cut(Cuts,LastSoFar),
    (follows(LastSoFar,Cut)
        ->  C=LastSoFar
        ;   C=Cut).

init_cuts(M):- retract_all(M:reached(_,_)),
    reset_counter(M).

increase_counter(M):-
    M:counter(C), retract(M:counter(C)), C1 is C+1,
    assert(M:counter(C1)).
get_counter(M,C):- M:counter(C).
reset_counter(M):- retract_all(M:counter(_)),
    assert(M:counter(0)).

%%%%%%%%%%%%%%% End predicates for cut handling %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%% Predicates for animations %%%%%%%%%%%%%%%%%%
start_pause(M,_):- M:animations(no),!.
start_pause(M,F):-
	M:current_slide(S),
	start_pause(M,F,S).
start_pause(M,_,_):- M:animations(no),!.
start_pause(M,F,S):-
	write(F,"\\uncover<"),write(F,S),write(F,"->{"),increase_slide(M).

end_pause(M,_):- M:animations(no),!.
end_pause(_M,F):-
	write(F,"}").

increase_slide(M):-
    M:current_slide(C), retract(M:current_slide(C)), C1 is C+1,
    assert(M:current_slide(C1)).
reset_slide(M):- retract_all(M:current_slide(_)),
    assert(M:current_slide(2)).
%%%%%%%%%%%%%%%% End Predicates for animations %%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%

write_indented(M,F,N,S):- indent(M,F,N), write(F,S).

% Helpful for indentation, so that the LaTeX code is slightly more readable
indent(M,F,N):- nl(F), M:maxdepth(Max), N1 is Max-N, indentloop(F,N1).
indentloop(_,0):- !.
indentloop(F,N):- write(F,' '), N1 is N-1, indentloop(F,N1).


count_children(G,NumChildren,ListName):-
    findall(B,clausola_list(G,B,ListName,_),L),
    length(L,NumChildren).

vars_names([],[],_).
vars_names([X|T],[b(X,N)|TN],ListName):-

    var_name(X,N,ListName),
    vars_names(T,TN,ListName).



print_binding(M,F,X,ListName):-
    write(F,"["),
    (begin_binding(S) -> write(F,S); true),
    print_binding1(M,F,X,ListName),
    (end_binding(Send) -> write(F,Send) ; true),
    write(F,"]").

print_binding1(_M,_F,[],_).
print_binding1(M,F,[b(A,B)|T],ListName):-
    var_name(A,Name,ListName),
    Name = B, !,
    % Avoid writing "X=X" as a binding...
    print_binding1(M,F,T,ListName).
print_binding1(M,F,[b(A,B)|T],ListName):-
    write_var(F,B,ListName), write(F,"/"),
    (acyclic_term(A)
      ->    write_term_no_sqbrack(F,A,1000,ListName)
      ;     M:maxdepth(D), write_term_no_sqbrack(F,A,D,ListName)
    ),
    (T=[] -> true
        ; write(F,", "), print_binding1(M,F,T,ListName)).

write_var(F,V,ListName):-
    var(V),
    var_name(V,VarName,ListName),!,
    split_string(VarName,"#","",[Name,Number]),
    write(F,"$"),
    write(F,Name),
    write(F,"_{"),
    write(F,Number),
    write(F,"}$").
write_var(F,VarName,_):-
    string(VarName),
    split_string(VarName,"#","",[Name,Number]),!,
    write(F,"$"),
    write(F,Name),
    write(F,"_{"),
    write(F,Number),
    write(F,"}$").

% Writes a term replacing the symbols "[" and "]"
% with \lbrack and \rbrack, because you cannot use "["
% inside a label of an arc.
% Stops at depth Depth
% write_term_no_sqbrack(File,Term,Depth):-
write_term_no_sqbrack(F,_,0,_):- !,
    write(F,"\\dots").
write_term_no_sqbrack(F,A,_,ListName):-
    % Named Variables
    var(A), !,
    write_var(F,A,ListName).
write_term_no_sqbrack(F,[],_,_):- !,
    write(F,"\\lbrack\\rbrack ").
write_term_no_sqbrack(F,[H|T],N,ListName):- !,
    write(F,"\\lbrack "),
    print_list_no_sqbrack(F,[H|T],N,ListName),
    write(F,"\\rbrack ").
write_term_no_sqbrack(F,T,_,_):-
    T =.. [Fun], !,
    write_functor(F,Fun).
% infixed operators
write_term_no_sqbrack(F,T,N,ListName):-
    T =.. [Fun,ArgX,ArgY],
    N1 is N-1,
    current_op(_,Associativity,Fun),
    (Associativity = xfy ; Associativity = yfx ; Associativity = xfx),!,
    write_term_no_sqbrack(F,ArgX,N1,ListName),
    pretty_write_op(F,Fun),
    write_term_no_sqbrack(F,ArgY,N1,ListName).
write_term_no_sqbrack(F,T,N,ListName):- !,
    N1 is N-1,
    T =.. [Fun|Arg],
    write_functor(F,Fun),
    write(F,"("),
    print_list_no_sqbrack(F,Arg,N1,ListName),
    write(F,")").

% used only to print correctly functor names containing underscores
% (the underscore in LaTeX has a meaning ...)
write_functor(File,Functor):-
    atom_chars(Functor,CharList),
    convert_underscores(CharList,NewCharList),
    atom_chars(NewFunctor,NewCharList),
    write(File,NewFunctor).

convert_underscores([],[]).
convert_underscores(['_'|T],['\\','_'|T1]):- !,
    convert_underscores(T,T1).
convert_underscores([H|T],[H|T1]):-
    convert_underscores(T,T1).

% If the list is a difference list, we also have the
% case in which the rest is a variable.
%print_list_no_sqbrack(File,Term,Depth)
print_list_no_sqbrack(F,V,N,ListName):-
    var(V), !,
    write_term_no_sqbrack(F,V,N,ListName).
print_list_no_sqbrack(F,[H|T],N,ListName):-
    var(T),!,
    write_term_no_sqbrack(F,H,N,ListName),
    write(F,"$|$"),
    write_term_no_sqbrack(F,T,N,ListName).
print_list_no_sqbrack(F,[H],N,ListName):- !,
    write_term_no_sqbrack(F,H,N,ListName).
print_list_no_sqbrack(F,[H1,H2|T],N,ListName):-
    N1 is N-1,
    write_term_no_sqbrack(F,H1,N1,ListName),write(F,","),
    print_list_no_sqbrack(F,[H2|T],N1,ListName).

pretty_write_op(F,<):- !,
    write(F,$<$).
pretty_write_op(F,=<):- !,
    write(F,$=<$).
pretty_write_op(F,>=):- !,
    write(F,$>=$).
pretty_write_op(F,>):- !,
    write(F,$>$).
pretty_write_op(F,is):- !,
    write(F," is ").
pretty_write_op(F,\=):- !,
    write(F,"$\\backslash=$").
pretty_write_op(F,Op):- !,
    write(F,Op).

print_resolvent(M,F,X,ListName):-
    max_resolvent_length(MaxLength),
	print_resolvent(M,F,X,MaxLength,ListName).
print_resolvent(M,F,X,MaxLength,ListName):-
    (begin_resolvent(S) -> write(F,S); true),
    term_length(X,Len),
    (X=[_,_|_], Len>MaxLength
    ->  write(F,"\\begin{tabular}{c}"),
        print_list_tabular(M,F,X,ListName),
        write(F,"\\end{tabular}")
    ;    print_list(M,F,X,ListName)
    ),
    (end_resolvent(Send) -> write(F,Send) ; true).

% Prints a list in a tabular environment, without exceeding max_resolvent_length in each row
print_list_tabular(M,F,[A],ListName):-!, print_list(M,F,[A],ListName).
print_list_tabular(M,F,X,ListName):-
    append2(First,Rest,X),
    (First = [_]
     -> true
     ;  term_length(First,Len1),
        max_resolvent_length(MaxLength),
        Len1<MaxLength
    ),!,
	print_list(M,F,First,ListName),
    (Rest=[] -> true
     ;  write(F,",\\\\"),
        print_list_tabular(M,F,Rest,ListName)
    ).

% Like append, but provides the solutions in the opposite order
% i.e., if the third argument is ground and the others are not,
% first provides the longest lists as first aruments
append2([H|T],X,[H|S]):-
    append2(T,X,S).
append2([],X,X).


print_list(M,F,[H],ListName):- !,
    M:maxdepth(D),
    write_term_no_sqbrack(F,H,D,ListName).
print_list(M,F,[H1,H2|T],ListName):-
    M:maxdepth(D),
    write_term_no_sqbrack(F,H1,D,ListName),write(F,","),
    print_list(M,F,[H2|T],ListName).

print_fail(M,F,Depth,Longest):-
    write_indented(M,F,Depth,"\\chunk{"),
	start_pause(M,F),
	fail_symbol(FailSymb),
	print_string_spaces(F,Longest,FailSymb),
	end_pause(M,F),
    write(F,"}").

% Prints a string adding "Longest" spaces.
print_string_spaces(F,Longest,String):-
	string_length(String,StringLength),
    NumSpace is ((Longest-StringLength)//2),
	%write(F,NumSpace),
    print_n_spaces(F,NumSpace),
    write(F,String), %writeln(F,"\n"),
    print_n_spaces(F,NumSpace).
	%write(F,NumSpace).

print_n_spaces(_,N):- N=<0,!.
print_n_spaces(F,N):-
    number(N), N>0,
    write(F,"~"),
    N1 is N-1,
    print_n_spaces(F,N1).

vanilla(_:[]).
vanilla(M:[A|B]) :- !, vanilla(M:A), vanilla(M:B).
vanilla(M:not(X)) :- !,
    (vanilla(M:X) -> fail ; true).
vanilla(_M:X) :-
    built_in(X), call(X).
vanilla(M:X) :-
    M:c(X,Body,_),
    vanilla(M:Body).

term_length(G,Length):-
    term_to_string(G,S),
    string_length(S,Length).

term_length_chopped(G,L):-
    max_resolvent_length(MaxTermLength),
    term_length(G,Length),
    (Length=<MaxTermLength
    ->  L=Length
    ;   L=MaxTermLength
    ).

% max_subtree_width(G,Width):-
max_subtree_width(_M:[],0):-!.
max_subtree_width(M:[not(A)|B],Width):-!,
	term_length_chopped([not(A)|B],W1),
	findall(W,
		(	max_subtree_width(M:[A],W)
		),
		LW),
        sum(LW,RES),
        max(RES,W1,Width).
max_subtree_width(M:[A|B],Width):- built_in(A),!,
	term_length_chopped([A|B],W1),
	findall(W,
		(	call(A),
			max_subtree_width(M:B,W)
		),
		LW),
	sum(LW,RES),
        max(RES,W1,Width).


max_subtree_width(M:[A|B],Width):-!,
	term_length_chopped([A|B],W1),
	findall(W,
		(	
            M:c(A,Body,_),
			append(Body,B,Resolvent),
			max_subtree_width(M:Resolvent,W)
		),
		LW),
	sum(LW,RES),
        max(RES,W1,Width).

/*
max_subtree_width([A|B],W):-!,
	max_subtree_width(A,WA),
	max_subtree_width(B,WB,ListName),
	max(WA,WB,W).	% Very approximate; in principle the resolvent is WA+Length(B) while resolving A, then it will be chopped ...
max_subtree_width(not(A),W):- !,max_subtree_width(A,W).
max_subtree_width(A,Width):- !,
	built_in(A),
	findall(W,
		(call(A),
		 term_length_chopped(A,W)
		),LW),
        sum(LW,Width).
max_subtree_width(X,Width) :-
	findall(W,
		(	clausola_list(X,Body,_,_),
			max_subtree_width(Body,W)
		),
		LW),
        sum(LW,Width).
*/



remove_char(Sin,Char,Sout):-
    split_string(Sin,Char,"",SubStrings),
    concat_string(SubStrings,Sout).


clausola(H,BSq):-
    functor(H,F,A), current_predicate(F/A),
    clause(H,B),
    conv_sq_list(B,BSq).
clausola(H,BSq):-
    my_clause(H,BSq).

conv_sq_list((A,B),[A|Bsq]):- !,
    conv_sq_list(B,Bsq).
conv_sq_list(true,[]):- !.

conv_sq_list(X,[X]).



init_file(FileName,write,File):-
    open(FileName,write,File),
    write(File,"% File created by SLDNF Draw version "),
    sldnf_version(Ver),
    writeln(File,Ver),
    writeln(File,"% http://endif.unife.it/it/ricerca-1/aree-di-ricerca/informazione/ingegneria-informatica/software/sldnf-draw"),
    nl(File).

%%%%%%%%%
retract_all(X):-retractall(X).

term_to_string(G,S):- term_string(G,S).
init_swi_list:-nb_setval(lista,[]).

float_to_integer(X,R) :- R is truncate(X).

%%%%%%% NAMING VARIABILIIIIII %%%%%%%%%%%%%%%%%%%%


var_name(X,N,L):- var(X),
	get_by_var(X,L,R),!,
	atom_string(R,N).

var_name(X,N,_):- var(X),term_string(X,S),string_concat(S,"#0",N).


append_element_to_lista((_=Var),V,V):-var(Var),get_by_var(Var,V,_),!.

append_element_to_lista((A=Var),V,R):- var(Var),!,
	same_name_number(A,V,N),
	atom_concat(A,'#',Tem),
	atom_concat(Tem,N,Ris),
	R=[(Ris=Var)|V].

append_element_to_lista(_,R,R).


append_to_lista([],R,R).
append_to_lista([L|Ls],ListName,R):- append_element_to_lista(L,ListName,R1),append_to_lista(Ls,R1,R).

match_el_into_lista(X,R):- nb_getval(lista,V),get_by_var(X,V,R).


get_by_var(X,[(Atom=Y)|_],R):-X==Y,!,R=Atom.
get_by_var(X,[_|L],R):-get_by_var(X,L,R).


get_by_atom(X,[(Atom=_)|_]):-X == Atom,!.
get_by_atom(X,[_|L]):-get_by_atom(X,L).


same_name_number(_,[],0):-!.
same_name_number(X,[(Atom=_)|L],N):- atom_chars(Atom,Chars),atom_chars(X,XC),
	name_head(Chars,H),
	XC=H,!,
	same_name_number(X,L,N1),
	N is N1+1.
same_name_number(X,[_|L],N):- same_name_number(X,L,N).



read_goal(M:T,R):-
  M:query(T,VarNames),
  append_to_lista(VarNames,[],R).

name_head(['#'|_],[]).
name_head([A|R],[A|Head]):- name_head(R,Head).

printvar([]):- !.
printvar([L|Ls]):- write(L),nl,printvar(Ls).

printlista:- nb_getval(lista_prog,V), printvar(V).

read_clausola(Str):- read_term(Str,T,[variable_names(V)]),mymatch(T,H,B),!,clausola_prog(H,B,V).



read_clausole(Str):- at_end_of_stream(Str),!.

read_clausole(Str):- read_clausola(Str),read_clausole(Str).


carica(X):- open(X,read,Str),
	nb_setval(lista_prog,[]),
	read_clausole(Str),
	nb_getval(lista_prog,V),reverse(V,Vrev),nb_setval(lista_prog,Vrev),
	close(Str).


clausola_list(M:Goal,Body,L,RES):-
  newclausola(M:Goal,Body,L,RES).


newclausola(M:Goal,Body,LIST,RES):-
	M:c(Goal,Body,Names),
	append_to_lista(Names,LIST,RES).


clausola_prog(G,B,N):-nb_getval(lista_prog,V),conv_sq_list2(B,Bsq),!,nb_setval(lista_prog,[c(G,Bsq,N)|V]).
clausola_prog(T,[],[]):-nb_getval(lista_prog,V),nb_setval(lista_prog,[c(T,[],[])|V]).

conv_sq_list2((A,B),[A|Bsq]):- !,
    conv_sq_list2(B,Bsq).
conv_sq_list2(true,[]):- !.
conv_sq_list2([],[]):-!.
conv_sq_list2(X,[X]).

mymatch((H:-B),H,Bsq):-conv_sq_list2(B,Bsq),!.
mymatch(H,H,[]).


remove_vars([],[]).
remove_vars([(Atom = _)|Ls],[Atom|Rs]):- remove_vars(Ls,Rs).

%%%%%%% FINE NAMING VARIABILIIIIII %%%%%%%%%%%%%%%%%%%%


built_in(G):-
    predicate_property(G,built_in).


sum(L,S) :- sum(L,0,S).
sum([],S,S).
sum([L|T],A,S):- A1 is A+L,sum(T,A1,S).

max(A,B,A) :- A>=B,!.
max(A,B,B) :- A<B.



indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.
%%%%%

get_var_name(X,Name=X):-
   var_property(X, name(Name)),!.

get_var_name(X,'_'=X).

:- multifile sandbox:safe_primitive/1.

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(sldnfdraw:draw_goal(_),[]).

user:term_expansion(end_of_file, end_of_file) :-
  prolog_load_context(module, M),
  sldnf_input_mod(M),!,
  retractall(sldnf_input_mod(M)),
  style_check(+singleton).

user:term_expansion((:- sldnf), []) :-!,
  prolog_load_context(module, M),
  assert(sldnf_input_mod(M)),
  M:dynamic((prog_on/0,query_on/0,c/3,query/2,reached/1)),
  animations(A),
  assert(M:animations(A)),
  maxdepth(MD),
  assert(M:maxdepth(MD)),
  style_check(-singleton).

user:term_expansion((:- begin_program), []) :-
  prolog_load_context(module, M),
  sldnf_input_mod(M),!,
  assert(M:prog_on).

user:term_expansion((:- end_program), []) :-
  prolog_load_context(module, M),
  sldnf_input_mod(M),!,
  retractall(M:prog_on).

user:term_expansion((:- begin_query), []) :-
  prolog_load_context(module, M),
  sldnf_input_mod(M),!,
  assert(M:query_on).

user:term_expansion((:- end_query), []) :-
  prolog_load_context(module, M),
  sldnf_input_mod(M),!,
  retractall(M:query_on).

user:term_expansion(C,c(H,B,VarNames)):-
  prolog_load_context(module, M),
  sldnf_input_mod(M),
  M:prog_on,!,
  term_variables(C,Vars),
  maplist(get_var_name,Vars,VarNames),
  mymatch(C,H,B),!.

user:term_expansion(C,query(C,VarNames)):-
  prolog_load_context(module, M),
  sldnf_input_mod(M),
  M:query_on,!,
  term_variables(C,Vars),
  maplist(get_var_name,Vars,VarNames).
