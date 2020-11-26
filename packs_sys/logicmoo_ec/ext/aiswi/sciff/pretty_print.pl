:- use_module(prolog_version).
:- (is_dialect(swi) -> use_module(swi_specific) ; use_module(sicstus_specific)).
:- use_module(sciff_options).

:- multifile portray/1.

:- use_module(text_style).

% Predicate portray is called, both in SICStus and in SWI, recursively for each nonvariable
% term. If it succeeds, the host Prolog assumes the term has been printed.
% So, if I want to redefine the printing of the term e([F,A],Atom,T) to E(Atom,T), I should do  the following:
%
%   portray(e(_,A,B)) :- write('E('), print(A), write(','), print(T), write(')').
%
% In fact, print/1 calls portray/1, while write/1 does not (does the default printing).
%
% I want to change the default printing of the variables as well, so my portray predicate does
% a recursive call, instead of calling print/1.

% In SWI it works, but in SICStus variables get printed as '$VAR([codelist....])
% So, in SWI, the portray predicate has a recursive call (and must always succeed,
% while in SICStus it calls print, and may fail (for a general term, ...), so that the default
% print is used.

portray(Var):-
    is_dialect(swi),
    var(Var),!,
    (get_quant(Var,Q)
        ->  set_print_style_of_quantification(Q),
            write(Var), reset_text_style
        ;   write(Var)
    ).


%%% Insert here the redefinition of the printing of terms


portray(e([_,_],A,T)):-
    %functor(A,F,N),
    print(e(A,T)).
portray(h([_,_],A,T)):-
    %functor(A,F,N),
    print(h(A,T)).
portray(en([_,_],A,T)):-
    %functor(A,F,N),
    print(en(A,T)).
portray(abd([_,_],A,T)):-
    %functor(A,F,N),
    print(abd(A,T)).

portray(e(A,T)):-
    foreground_color(green), text_property(bold,1), write('e('), reset_text_style,
    my_print(A),
    foreground_color(green), text_property(bold,1), write(','), reset_text_style,
    my_print(T),
    foreground_color(green), text_property(bold,1), write(')'), reset_text_style.
portray(en(A,T)):-
    foreground_color(green), text_property(bold,1), write('en('), reset_text_style,
    my_print(A),
    foreground_color(green), text_property(bold,1), write(','), reset_text_style,
    my_print(T),
    foreground_color(green), text_property(bold,1), write(')'), reset_text_style.
portray(h(A,T)):-
    foreground_color(blue), text_property(bold,1), write('h('), reset_text_style,
    my_print(A),
    foreground_color(blue), text_property(bold,1), write(','), reset_text_style,
    my_print(T),
    foreground_color(blue), text_property(bold,1), write(')'), reset_text_style.

portray(ic(Body,Head)):-
    get_option(portray_ic,on),
    (get_option(coloring,on) -> true ; write('ic:')),
    nl,
    portray_args(Body),
    nl,
    foreground_color(red), text_property(bold,1),
    write('--->\t'),
    reset_text_style,
    print_disjunction(Head), nl.

portray(psic(Body,Head)):-
    get_option(portray_ic,on),
    (get_option(coloring,on) -> true ; write('psic:')),
    nl,
%    print_body(Body),
    print_psic_body(Body),
    %(portray_args(Body) -> true ;  write('failed portray_args('), write(Body), write(')'),nl, fail),
    nl,
    foreground_color(cyan), text_property(bold,1),
    write('--->\t'),
    reset_text_style,
    print_disjunction(Head), nl.

portray(clp_constraint(C)):- my_print(C).
portray(st(C)):- my_print(C).

% CLP constraints (SICStus)

    portray(clpfd:'t=u IND'(A,B)):-
        print(A), write(#=), print(B).

    portray(clpfd:'x\\=y IND'(A,B)):-
        print(A), write(#\=), print(B).


    portray(clpfd:'t=<u+c'(A,B,C)):-
        (C=0    -> print(A#=<B);
         C= -1   -> print(A#<B);
         C>0    -> print(A#=<B+C);
                   print(A#=<B), print(C) % Negative number: sign written by print(C)
         ).


    portray(clpfd:'t>=u+c'(A,B,C)):-
        (C=0    -> print(A#>=B);
         C=1    -> print(A#>B);
         C>0    -> print(A#>=B+C);
                   print(A#>=B), print(C) % Negative number: sign written by print(C)
         ).

portray(A):-
    is_dialect(swi),
    atomic(A), !, write(A).

portray(List):-
    is_dialect(swi),
    is_list(List),!,    % Questo non va bene se e` una difference list o una lista tipo: [A|p]
    print('['),
    portray_list(List),
    print(']').


portray(Term):-
    is_dialect(swi),
    Term =.. [Op|Args],
    current_op(_,Type,Op),
    (member(Type,[xfx, xfy, yfx, yfy])
        ->  Args = [X,Y],
            portray(X), print(Op), portray(Y)
        ;
     member(Type,[xf,yf])
        ->  Args = [X],
            portray(X), print(Op)
        ;
     member(Type,[fx,fy])
        ->  Args = [X],
            print(Op), portray(X)
        ;   print(Op), print('('), portray_args(Args),
            print(')')
    ).

portray(Term):-
    is_dialect(swi),
    Term =.. [F|Args],
%    one_is_var(Args),
    print(F), print('('), portray_args(Args),
    print(')').


set_print_style_of_quantification(forall):- foreground_color(magenta).
set_print_style_of_quantification(forallf):- foreground_color(magenta), text_property(underlined,1).
set_print_style_of_quantification(exists):- foreground_color(blue).
set_print_style_of_quantification(existsf):- foreground_color(blue), text_property(underlined,1).

portray_args([Term]):-!,
    my_print(Term).
portray_args([A,B|T]):- !,
    my_print(A), write(','), portray_args([B|T]).


portray_list(Var):- var(Var),!, write('|'), write(Var).
portray_list([X]):- !, print(X).
portray_list([A,B|T]):-
    print(A), write(','), portray_list([B|T]).

my_print(X):- 
    (is_dialect(swi), portray(X) -> true ; print(X)).




%%% Auxiliary predicates
is_code_list([]).
is_code_list([H|T]):-
    integer(H),
    is_code_list(T).

print_disjunction([H]):- !,
    portray_args(H).
print_disjunction([H1,H2|T]):-
    portray_args(H1), nl, write('\\/\t'),
    print_disjunction([H2|T]).

print_body([]).
print_body([H|T]):- !,
    print_body(H), write(','),
    print_body(T).
print_body(A):- print(A).

print_psic_body([[]|T]):- !,print_psic_body(T).
print_psic_body([A|T]):- !,portray_args(A),
    (empty_body(T) -> true ; write(','), print_psic_body(T)).

empty_body([]).
empty_body([[]|T]):- empty_body(T).

